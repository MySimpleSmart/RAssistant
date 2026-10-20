# Browser integration checks against the existing UI/server with a fake API.
# Requires the optional chromote package and a local Chrome installation.
# Run from the project directory: Rscript tests/browser.R
main <- function() {
  stopifnot(requireNamespace("chromote",quietly=TRUE))
  root <- normalizePath(".")
  port <- httpuv::randomPort()
  update_marker <- tempfile()
  on.exit(unlink(update_marker),add=TRUE)
  available_marker <- tempfile()
  on.exit(unlink(available_marker),add=TRUE)
  reopen_marker <- tempfile();reopen_url <- tempfile()
  on.exit(unlink(c(reopen_marker,reopen_url)),add=TRUE)
  history_root <- tempfile("ra-browser-history-");dir.create(history_root)
  on.exit(unlink(history_root,recursive=TRUE),add=TRUE)
  run_app <- function(root,port,update_marker,history_root,available_marker,reopen_marker,reopen_url,standalone=FALSE) {
    setwd(root)
    source("tests/helpers.R")
    store_env <- new.env(parent=globalenv())
    for(file in c("R/history.R","R/knowledge.R"))sys.source(file,store_env)
    history <- store_env$.conversation_store(project=root,directory=history_root)
    knowledge <- store_env$.knowledge_store(project=root,directory=history_root)
    available <- list(status="available",version="0.1.2",current="0.1.1",
      notes=c("Recover a disconnected Viewer.","Keep <draft> text & images."))
    update_result <- list(status="current",version="0.1.1",current="0.1.1")
    published <- FALSE
    console_enabled <- FALSE
    console <- list(start=function(){console_enabled<<-TRUE},stop=function(){console_enabled<<-FALSE;TRUE},
      dispose=function(){console_enabled<<-FALSE;TRUE},snapshot=function(redact)"CONSOLE_BROWSER_VALUE_42",
      status=function()list(enabled=console_enabled,ready=TRUE,pending=FALSE,notice="",lines=if(console_enabled)1L else 0L))
    app <- load_assistant(package_info=if(standalone)NULL else list(version="0.1.1",history=history,knowledge=knowledge,
      console=console,prepare_console=function()invisible(NULL),
      poll_update=function(){
        if(!published && file.exists(available_marker)){
          published <<- TRUE;update_result <<- available
        }
        update_result
      },
      update_poll_delay=function()500,
      refresh_update=function(){
        update_result <<- if(identical(update_result$status,"available"))
          list(status="current",version="0.1.1",current="0.1.1") else available
      },
      request_update=function(){cat("requested\n",file=update_marker,append=TRUE);invisible(TRUE)}))
    app$initial <- list(id="fixture",path=file.path(tempdir(),"example.R"),contents="plot(1:3)")
    app$capture_current_plot <- function(directory){
      Sys.sleep(.15)
      path<-tempfile("browser-current-plot-",tmpdir=directory,fileext=".png")
      grDevices::png(path,width=400,height=240)
      par(mar=c(3,3,2,1));hist(1:40,main="Current RStudio plot",col="darkorange")
      invisible(grDevices::dev.off());path
    }
    source_unavailable <- FALSE
    app$active_doc <- function(id=NULL) {
      if(source_unavailable)stop("Simulated RStudio Source lookup failure")
      app$initial
    }
    count <- 0L
    app$ask_ai <- function(system_prompt,prompt,images=character()) {
      count <<- count+1L
      Sys.sleep(.2)
      if(grepl("SIMULATED_FAILURE",prompt,fixed=TRUE))stop("Simulated API failure")
      if(grepl("SIMULATE_SOURCE_LOSS",prompt,fixed=TRUE))source_unavailable <<- TRUE
      paste0("Response ",count,if(grepl("CONSOLE_BROWSER_VALUE_42",prompt,fixed=TRUE))" Console evidence received."else"",
             "\n\n```r\nx <- c(\"<tag>\", \"&lt;\", \"\\\\(x\\\\)\", \"$$\")\nkeep <- x > \"a\" & x < \"z\"\n```\n\n",
             "Math: \\(\\alpha < 1\\).\n\n[[ACADEMIC_COMMENT]]Verified result.[[/ACADEMIC_COMMENT]]")
    }
    previous_limit <- getOption("shiny.maxRequestSize")
    handle <- shiny::startApp(shiny::shinyApp(app$ui,app$server,onStart=app$on_start),
      host="127.0.0.1",port=port,launch.browser=FALSE,quiet=TRUE)
    on.exit(handle$stop(),add=TRUE)
    launch_env <- new.env(parent=globalenv())
    sys.source("R/launch.R",launch_env)
    launch_env$.in_rstudio <- function()TRUE
    launch_env$.show_viewer <- function(url)writeLines(url,reopen_url)
    launch_env$.resolve_api_key <- function()stop("Reopening must not read credentials")
    launch_env$.app_state$handle <- handle
    stopifnot(identical(getOption("shiny.maxRequestSize"),52*1024^2))
    while(identical(handle$status(),"running")){
      later::run_now(.05)
      if(file.exists(reopen_marker)){
        unlink(reopen_marker)
        stopifnot(identical(launch_env$launch(),handle))
      }
    }
    stopifnot(identical(getOption("shiny.maxRequestSize"),previous_limit))
    stopifnot(!console_enabled)
  }
  start_worker <- function(port,standalone=FALSE)callr::r_bg(run_app,args=list(root,port,update_marker,history_root,available_marker,reopen_marker,reopen_url,standalone),
    stdout="|",stderr="|",supervise=TRUE)
  worker <- start_worker(port)
  on.exit(worker$kill(),add=TRUE)
  browser <- chromote::ChromoteSession$new()
  on.exit(browser$parent$close(),add=TRUE)
  js <- function(expression) {
    result <- browser$Runtime$evaluate(expression,awaitPromise=TRUE,returnByValue=TRUE)
    if(!is.null(result$exceptionDetails))stop(result$exceptionDetails$text," ",result$exceptionDetails$exception$description)
    result$result$value
  }
  wait_js <- function(expression,timeout=10) {
    deadline <- Sys.time()+timeout
    repeat {
      if(isTRUE(js(expression)))return(invisible(TRUE))
      if(Sys.time()>deadline) {
        cat("Page tail:\n",tail(strsplit(js('document.body && document.body.innerText'),"\n")[[1]],40),sep="\n")
        cat("\nServer:\n",paste(worker$read_error_lines(),collapse="\n"),"\n")
        cat("\nMessages:\n",js('JSON.stringify(window.receivedMessages||[])'),"\n")
        cat("\nControls:\n",js('JSON.stringify({send:document.getElementById("send")&&document.getElementById("send").disabled,prompt:document.getElementById("prompt")&&document.getElementById("prompt").value,users:document.querySelectorAll(".msg.user").length,errors:window.aisourceErrors})'),"\n")
        stop("Timed out: ",expression)
      }
      Sys.sleep(.05)
    }
  }
  checks <- 0L
  check <- function(ok,description) {
    if(!isTRUE(ok))stop(description,call.=FALSE)
    checks <<- checks+1L;cat("PASS",description,"\n")
  }
  url <- paste0("http://127.0.0.1:",port)
  for(i in 1:100) {
    ready <- suppressWarnings(tryCatch({con<-url(paste0(url,"/"),open="rb");close(con);TRUE},error=function(e)FALSE))
    if(ready)break
    Sys.sleep(.05)
  }
  browser$Page$enable()
  page_setup <- 'window.openAction=function(id){var menu=document.getElementById("app_menu");if(menu&&!menu.open)menu.querySelector("summary").click();document.getElementById(id).click();};window.aisourceErrors=[];window.addEventListener("error",function(e){aisourceErrors.push(String(e.message));});window.addEventListener("unhandledrejection",function(e){aisourceErrors.push(String(e.reason));});'
  browser$Page$addScriptToEvaluateOnNewDocument(source=page_setup)
  browser$Page$navigate(url)
  wait_js('!!document.getElementById("send") && !document.getElementById("send").disabled')
  wait_js('document.querySelector(".empty")!==null')
  js('window.alerts=[];window.alert=function(x){alerts.push(x);};window.receivedMessages=[];$(document).on("shiny:message",function(e){if(e.message.custom)receivedMessages.push(e.message.custom);});')
  check(isTRUE(js('document.querySelector(".empty")!==null')), "existing Shiny UI connects and renders")
  check(isTRUE(js('!document.getElementById("app_menu").open && document.querySelector("#app_menu #history")!==null && document.querySelector("#app_menu #knowledge")!==null && document.querySelector("#app_menu #help")!==null && document.querySelector("#app_menu #close")!==null && document.getElementById("check_updates").closest("#app_menu")===null')),
    "secondary actions live in More actions while update checks remain directly available")
  js('document.querySelector("#app_menu>summary").click();document.dispatchEvent(new KeyboardEvent("keydown",{key:"Escape",bubbles:true}));')
  check(isTRUE(js('!document.getElementById("app_menu").open && document.activeElement===document.querySelector("#app_menu>summary")')),
    "Escape closes the actions menu and restores keyboard focus")
  js('window.initialComposerHeight=document.getElementById("prompt").getBoundingClientRect().height;var p=document.getElementById("prompt");p.value=Array(15).fill("A longer draft line").join("\\n");p.dispatchEvent(new Event("input",{bubbles:true}));')
  check(isTRUE(js('document.getElementById("prompt").getBoundingClientRect().height>initialComposerHeight && document.getElementById("prompt").getBoundingClientRect().height<=141')),
    "the compact composer grows with the draft up to a bounded height")
  js('var p=document.getElementById("prompt");p.value="";p.dispatchEvent(new Event("input",{bubbles:true}));')
  check(isTRUE(js('document.getElementById("prompt").getBoundingClientRect().height<=56 && document.getElementById("chat_stream").getBoundingClientRect().height>innerHeight*.6')),
    "clearing a draft restores the small composer and gives most space to chat")
  check(isTRUE(js('document.getElementById("update_now")===null')),
    "no banner appears when the installed version is current")
  js('document.getElementById("prompt").value="Keep this draft";')
  writeLines("new version published",available_marker)
  wait_js('document.getElementById("update_now")!==null')
  check(isTRUE(js('document.querySelector(".app-version").textContent==="v0.1.1" && document.querySelector(".update-notice").textContent.includes("0.1.2") && document.getElementById("prompt").value==="Keep this draft"')) && !file.exists(update_marker),
    "a newly published version automatically displays its banner without a manual check, lost draft or installation")
  js('document.getElementById("prompt").value="Keep this draft";openAction("help");')
  wait_js('document.querySelector("#shiny-modal .code-copy")!==null')
  check(isTRUE(js('document.getElementById("shiny-modal").textContent.includes("RAssistant::launch()") && document.getElementById("shiny-modal").textContent.includes("RAssistant::set_api_key()") && document.getElementById("shiny-modal").textContent.includes("hidden password prompt") && !document.getElementById("shiny-modal").textContent.includes("Restart R") && document.querySelector(".toolbar-actions #check_updates")!==null && document.querySelector("#shiny-modal #check_updates")===null')),
    "Help explains launching, hidden key replacement and updates")
  browser$Emulation$setDeviceMetricsOverride(width=380L,height=700L,deviceScaleFactor=1,mobile=FALSE)
  wait_js('getComputedStyle(document.getElementById("shiny-modal")).opacity==="1" && document.querySelector("#shiny-modal .modal-dialog").getBoundingClientRect().width>0')
  check(isTRUE(js('document.querySelector("#shiny-modal .modal-dialog").getBoundingClientRect().right<=innerWidth && document.querySelector("#shiny-modal .modal-dialog").getBoundingClientRect().left>=0 && document.getElementById("help").getBoundingClientRect().right<=innerWidth')),
    "Help and the toolbar fit a narrow RStudio-sized Viewer")
  screenshot <- Sys.getenv("RA_BROWSER_HELP_SCREENSHOT")
  if(nzchar(screenshot)){
    shot<-browser$Page$captureScreenshot(format="png")
    writeBin(jsonlite::base64_dec(shot$data),screenshot)
  }
  browser$Emulation$clearDeviceMetricsOverride()
  js('window.copied=null;Object.defineProperty(navigator,"clipboard",{configurable:true,value:{writeText:function(t){window.copied=t;return Promise.resolve();}}});document.querySelector("#shiny-modal .code-copy").click();')
  wait_js('window.copied!==null')
  check(identical(js('window.copied'),"RAssistant::launch()\n"),"Help command Copy preserves the exact Console command")
  js('document.querySelectorAll("#shiny-modal .code-copy")[1].click();')
  check(identical(js('window.copied'),"RAssistant::set_api_key()\nRAssistant::launch()\n"),
    "the key-replacement instructions copy correctly")
  js('document.querySelector("#shiny-modal .modal-footer [data-dismiss=modal]").click();')
  wait_js('document.getElementById("shiny-modal")===null')
  js('document.getElementById("check_updates").click();')
  wait_js('document.getElementById("update_now")===null && document.getElementById("update_check_status").textContent.includes("up to date")')
  check(TRUE,"toolbar update checks show results without opening Help and remove an obsolete banner")
  js('openAction("help");')
  wait_js('document.querySelector("#shiny-modal .code-copy")!==null')
  check(isTRUE(js('document.querySelector("#shiny-modal #update_help_status")===null && !document.getElementById("shiny-modal").textContent.includes("up to date")')),
    "Help omits the up-to-date result while a manual toolbar result is active")
  js('document.querySelector("#shiny-modal .modal-footer [data-dismiss=modal]").click();')
  wait_js('document.getElementById("shiny-modal")===null')
  wait_js('document.getElementById("update_check_status").textContent===""',timeout=8)
  check(isTRUE(js('getComputedStyle(document.getElementById("update_check_status")).display==="none"')),
    "an up-to-date result expires and releases its banner space")
  js('document.getElementById("check_updates").click();')
  wait_js('document.getElementById("update_now")!==null')
  check(isTRUE(js('document.getElementById("prompt").value==="Keep this draft"')),
    "opening Help and checking updates preserves the chat draft")
  js('openAction("knowledge");')
  wait_js('document.getElementById("knowledge_files")!==null')
  js('var dt=new DataTransfer();dt.items.add(new File(["Lecture material about reproducible statistical analysis and interpreting model output.\\n"," ".repeat(6*1024*1024)],"large-notes.txt",{type:"text/plain"}));var input=document.getElementById("knowledge_files");input.files=dt.files;input.dispatchEvent(new Event("change",{bubbles:true}));')
  wait_js('document.getElementById("knowledge_selected")!==null && document.getElementById("knowledge_selected").textContent.includes("large-notes.txt")',timeout=30)
  check(!isTRUE(js('document.body.textContent.includes("Maximum upload size exceeded")')),
    "a course document larger than 5 MB uploads after nonblocking startup returns")
  js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
  wait_js('document.getElementById("shiny-modal")===null')
  js('document.getElementById("update_now").click();')
  wait_js('document.getElementById("confirm_update")!==null')
  check(isTRUE(js('var notes=document.querySelectorAll("#shiny-modal .release-notes li"),text=document.getElementById("shiny-modal").textContent;notes.length===2 && notes[0].textContent==="Recover a disconnected Viewer." && notes[1].textContent==="Keep <draft> text & images." && notes[1].querySelector("draft")===null && text.includes("v0.1.1") && text.includes("v0.1.2") && text.includes("Chat saved. Reopens after updating.") && !text.includes("same R library")')),
    "Update now lists the target release changes as escaped text with a short restart note")
  js('document.querySelector("#shiny-modal .modal-footer [data-dismiss=modal]").click();')
  wait_js('document.getElementById("shiny-modal")===null')
  check(!file.exists(update_marker),"cancelling an update leaves the app running without installing")
  js('document.getElementById("attach_current_plot").click();document.getElementById("attach_current_plot").click();')
  check(isTRUE(js('document.getElementById("send").disabled && document.getElementById("attach_current_plot").disabled')),
    "plot capture disables Send and repeat clicks immediately")
  wait_js('document.querySelectorAll(".attachment-item").length===1 && !document.getElementById("send").disabled')
  check(isTRUE(js('document.querySelector(".attachment-item img").naturalWidth>0 && document.getElementById("prompt").value==="Keep this draft" && document.querySelectorAll(".msg.user").length===0')),
    "Attach current plot previews one image and preserves the draft without an AI request")
  browser$Emulation$setDeviceMetricsOverride(width=380L,height=700L,deviceScaleFactor=1,mobile=FALSE)
  check(isTRUE(js('document.getElementById("attach_current_plot").getBoundingClientRect().right<=innerWidth && document.getElementById("send").getBoundingClientRect().right<=innerWidth && document.getElementById("source_context").closest(".toolbar")!==null && document.querySelector(".toolbar").getBoundingClientRect().height<=40')),
    "Source shares a single header row and attachment controls fit a narrow Viewer")
  screenshot<-Sys.getenv("RA_BROWSER_LAYOUT_SCREENSHOT")
  if(nzchar(screenshot)){
    js('document.querySelectorAll(".shiny-notification-close").forEach(function(b){b.click();});')
    writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
  }
  screenshot<-Sys.getenv("RA_BROWSER_PLOT_SCREENSHOT")
  if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
  browser$Emulation$clearDeviceMetricsOverride()
  js('document.querySelector(".remove-attachment").click();')
  wait_js('document.querySelectorAll(".attachment-item").length===0')
  check(isTRUE(js('!document.getElementById("console_context").checked')),
    "Console context is an opt-in control")
  js('document.getElementById("console_context").click();')
  wait_js('document.getElementById("console_context_status").textContent.includes("On: 1 line")')
  js('window.thinkingShown=false;window.notificationObserver=new MutationObserver(function(){document.querySelectorAll(".shiny-notification").forEach(function(n){if(n.textContent.includes("Thinking"))window.thinkingShown=true;});});notificationObserver.observe(document.body,{childList:true,subtree:true});document.getElementById("prompt").value="Explain this";document.getElementById("send").click();document.getElementById("send").click();')
  check(isTRUE(js('var s=document.getElementById("send");s.disabled && s.classList.contains("is-loading") && s.getAttribute("aria-busy")==="true" && getComputedStyle(s,"::before").animationName==="send-spin" && document.getElementById("compose_status").hidden')),
    "Send shows loading immediately without a separate status row and blocks double clicks")
  wait_js('document.querySelector(".code-copy")!==null && !document.getElementById("send").disabled')
  check(isTRUE(js('document.querySelectorAll(".msg.user").length===1')), "double clicking produces one user turn")
  check(isTRUE(js('document.querySelector(".msg.assistant").textContent.includes("Console evidence received.")')),
    "Send automatically supplies Console context without clipboard interaction")
  check(isTRUE(js('notificationObserver.disconnect();var s=document.getElementById("send");document.getElementById("prompt").value==="" && !s.classList.contains("is-loading") && s.getAttribute("aria-busy")==="false" && !window.thinkingShown && document.getElementById("request_indicator").textContent===""')),
    "success clears composer and button loading without a Thinking notification")
  expected <- 'x <- c("<tag>", "&lt;", "\\(x\\)", "$$")\nkeep <- x > "a" & x < "z"\n'
  # The response uses escaped backslashes in a literal R string.
  expected <- sub('"\\(x\\)"','"\\\\(x\\\\)"',expected,fixed=TRUE)
  check(identical(js('document.querySelector("pre code").textContent'),expected), "code rendering preserves operators, literal entities, and math-like strings")
  js('window.copied=null;Object.defineProperty(navigator,"clipboard",{configurable:true,value:{writeText:function(t){window.copied=t;return Promise.resolve();}}});document.querySelector(".code-copy").click();')
  wait_js('window.copied!==null')
  check(identical(js('window.copied'),expected), "code Copy writes the exact displayed code")
  js('document.querySelector(".comment-copy").click();')
  wait_js('window.copied==="Verified result."')
  check(TRUE, "academic comment Copy works")
  js('navigator.clipboard.writeText=function(){return Promise.reject(new Error("denied"));};document.execCommand=function(){window.copied=document.querySelector("textarea[readonly]").value;return true;};var p=document.getElementById("prompt");p.value="draft";p.focus();p.setSelectionRange(1,3);document.querySelector(".code-copy").click();')
  wait_js('window.copied!=="Verified result."')
  check(identical(js('window.copied'),expected), "clipboard denial uses the Viewer fallback")
  check(isTRUE(js('document.activeElement.id==="prompt" && document.activeElement.selectionStart===1 && document.activeElement.selectionEnd===3')), "fallback Copy restores composer focus and selection")
  js('openAction("history");')
  wait_js('document.getElementById("history_selected")!==null')
  check(isTRUE(js('document.getElementById("history_selected").options.length===1 && document.querySelectorAll("#shiny-modal .modal-footer .ai-btn").length===4')),
    "History lists the current auto-saved chat with consistent controls")
  js('window.firstChatId=document.getElementById("history_selected").value;document.getElementById("history_rename").click();')
  wait_js('document.getElementById("history_name")!==null')
  js('$("#history_name").val("Saved screenshot assignment").trigger("change");document.getElementById("history_save_name").click();')
  wait_js('document.getElementById("history_selected")!==null && document.getElementById("history_selected").textContent.includes("Saved screenshot assignment")')
  check(TRUE,"Rename persists the chosen chat title")
  js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
  wait_js('document.getElementById("shiny-modal")===null')
  js('document.getElementById("new_chat").click();')
  wait_js('document.querySelector(".empty")!==null')
  js('document.getElementById("prompt").value="A separate history conversation";document.getElementById("send").click();')
  wait_js('!document.getElementById("send").disabled && document.querySelectorAll(".msg.user").length===1')
  js('openAction("history");')
  wait_js('document.getElementById("history_selected")!==null && document.getElementById("history_selected").options.length===2')
  js('window.secondChatId=document.getElementById("history_selected").value;$("#history_selected").val(firstChatId).trigger("change");document.getElementById("history_restore").click();')
  wait_js('document.getElementById("shiny-modal")===null && document.querySelectorAll(".msg.user").length===1 && document.querySelector(".msg.user").textContent.includes("Explain this")')
  check(isTRUE(js('!document.getElementById("chat_stream").textContent.includes("A separate history conversation")')),
    "Restore switches conversations without mixing their visible history")
  js('openAction("history");')
  wait_js('document.getElementById("history_selected")!==null')
  js('$("#history_selected").val(secondChatId).trigger("change");document.getElementById("history_delete").click();')
  wait_js('document.getElementById("history_confirm_delete")!==null')
  js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
  wait_js('document.getElementById("shiny-modal")===null')
  js('openAction("history");')
  wait_js('document.getElementById("history_selected")!==null')
  check(isTRUE(js('document.getElementById("history_selected").options.length===2')),
    "cancelling Delete preserves the saved conversation")
  js('$("#history_selected").val(secondChatId).trigger("change");document.getElementById("history_delete").click();')
  wait_js('document.getElementById("history_confirm_delete")!==null')
  js('document.getElementById("history_confirm_delete").click();')
  wait_js('document.getElementById("history_selected")!==null && document.getElementById("history_selected").options.length===1')
  check(TRUE,"Delete removes the selected saved chat while retaining the active chat")
  js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
  wait_js('document.getElementById("shiny-modal")===null')
  js('window.canvas=document.createElement("canvas");canvas.width=12;canvas.height=12;canvas.getContext("2d").fillRect(0,0,12,12);window.imageData=canvas.toDataURL("image/png");window.imageBytes=Uint8Array.from(atob(imageData.split(",")[1]),function(c){return c.charCodeAt(0);});window.stage=function(files){var dt=new DataTransfer();files.forEach(function(f){dt.items.add(f);});var input=document.getElementById("plot_upload_native");input.files=dt.files;input.dispatchEvent(new Event("change",{bubbles:true}));};stage([new File([imageBytes],"plot.PNG",{type:""})]);')
  wait_js('document.querySelectorAll(".attachment-item").length===1 && !document.getElementById("send").disabled')
  check(isTRUE(js('document.querySelector(".attachment-item img").complete && document.querySelector(".attachment-item img").naturalWidth>0')), "an image with missing MIME type uploads and previews")
  check(isTRUE(js('document.getElementById("plot_upload_native").value===""')), "file picker resets after selection")
  js('var p=document.getElementById("prompt");p.focus();var dt=new DataTransfer();dt.items.add(new File([imageBytes],"pasted.png",{type:"image/png"}));p.dispatchEvent(new ClipboardEvent("paste",{clipboardData:dt,bubbles:true,cancelable:true}));')
  wait_js('document.querySelectorAll(".attachment-item").length===2 && !document.getElementById("send").disabled')
  check(TRUE, "pasted screenshots use the same upload queue")
  js('document.getElementById("prompt").value="SIMULATED_FAILURE";document.getElementById("send").click();')
  wait_js('!document.getElementById("send").disabled && document.querySelector(".content").parentElement.parentElement.textContent.includes("Simulated API failure")')
  check(isTRUE(js('document.getElementById("prompt").value==="SIMULATED_FAILURE" && document.querySelectorAll(".attachment-item").length===2 && !document.getElementById("send").classList.contains("is-loading") && document.getElementById("send").getAttribute("aria-busy")==="false"')),
    "API failure clears button loading and keeps the prompt and image previews ready to retry")
  js('document.getElementById("prompt").value="Interpret images";document.getElementById("send").click();')
  wait_js('!document.getElementById("send").disabled && document.querySelectorAll(".attachment-item").length===0')
  check(isTRUE(js('document.querySelectorAll(".sent-attachment img").length===4 && Array.from(document.querySelectorAll(".sent-attachment img")).every(function(i){return i.complete && i.naturalWidth>0;})')), "sent images stay visible after temporary files are removed")
  js('stage([new File(["not an image, at all"],"bad.png",{type:"image/png"})]);')
  wait_js('alerts.length>0 && !document.getElementById("send").disabled')
  check(isTRUE(js('document.querySelectorAll(".attachment-item").length===0')), "unreadable images are rejected and loading ends")
  js('window.encodedFile=function(mime,name){var data=canvas.toDataURL(mime).split(",")[1];return new File([Uint8Array.from(atob(data),function(c){return c.charCodeAt(0);})],name,{type:mime});};stage([encodedFile("image/jpeg","photo.jpg"),encodedFile("image/webp","plot.webp")]);')
  wait_js('document.querySelectorAll(".attachment-item").length===2 && !document.getElementById("send").disabled')
  check(isTRUE(js('Array.from(document.querySelectorAll(".attachment-item img")).every(function(i){return i.complete && i.naturalWidth>0;})')), "JPEG and WebP uploads both preview correctly")
  js('window.remainingPreview=document.querySelectorAll(".attachment-item img")[1].src;document.querySelector(".remove-attachment").click();')
  wait_js('document.querySelectorAll(".attachment-item").length===1')
  check(isTRUE(js('document.querySelector(".attachment-item img").src===remainingPreview')),
    "removing one preview retains the other image")
  js('document.querySelector(".remove-attachment").click();')
  wait_js('document.querySelectorAll(".attachment-item").length===0')
  js('stage([new File([imageBytes],"one.png",{type:"image/png"}),new File([imageBytes],"two.png",{type:"image/png"}),new File([imageBytes],"three.png",{type:"image/png"}),new File([imageBytes],"four.png",{type:"image/png"})]);')
  wait_js('document.querySelectorAll(".attachment-item").length===3 && !document.getElementById("send").disabled')
  check(TRUE, "sequential uploads enforce the three-image limit")
  js('document.getElementById("clear_images").click();')
  wait_js('document.querySelectorAll(".attachment-item").length===0')
  js('document.getElementById("prompt").value="";stage([new File([imageBytes],"only.png",{type:"image/png"})]);')
  wait_js('document.querySelectorAll(".attachment-item").length===1 && !document.getElementById("send").disabled')
  js('document.getElementById("send").click();')
  wait_js('document.querySelectorAll(".attachment-item").length===0 && !document.getElementById("send").disabled')
  check(isTRUE(js('Array.from(document.querySelectorAll(".msg.user")).pop().textContent.includes("Uploaded image.")')), "image-only messages can be sent")
  js('document.getElementById("prompt").value="Please run the script";document.getElementById("send").click();')
  wait_js('document.getElementById("yes")!==null')
  check(isTRUE(js('document.getElementById("send").disabled && !document.getElementById("new_chat").disabled')), "approval state blocks sends and allows New")
  check(isTRUE(js('document.getElementById("update_now").disabled')),
    "an update cannot interrupt a pending script approval")
  check(isTRUE(js('document.getElementById("history").disabled')),
    "history switching cannot interrupt a pending script approval")
  js('document.getElementById("no").click();')
  wait_js('!document.getElementById("send").disabled')
  check(isTRUE(js('document.querySelector(".run-status").textContent==="Not run"')), "No dismisses validation and restores controls")
  js('document.getElementById("prompt").value="Please run the script";document.getElementById("send").click();')
  wait_js('document.getElementById("yes")!==null')
  js('document.getElementById("yes").click();')
  check(isTRUE(js('document.getElementById("send").disabled')), "Yes immediately enters the running state")
  wait_js('!document.getElementById("send").disabled && document.querySelector(".log-details")!==null')
  check(isTRUE(js('Array.from(document.querySelectorAll(".run-status")).pop().textContent==="Execution completed"')), "completed validation replaces the Running status")
  js('document.querySelector(".log-details").open=true;')
  wait_js('document.querySelector(".log-details").open')
  js('var h=document.getElementById("chat_stream");h.dispatchEvent(new WheelEvent("wheel",{deltaY:-100,bubbles:true}));h.scrollTop=100;h.dispatchEvent(new Event("scroll"));window.previousTop=h.scrollTop;var last=h.querySelectorAll(".msg");var extra=document.createElement("div");extra.style.height="250px";last[last.length-1].appendChild(extra);')
  js('new Promise(function(resolve){setTimeout(resolve,200);})')
  check(isTRUE(js('Math.abs(document.getElementById("chat_stream").scrollTop-window.previousTop)<3')),
    "layout changes preserve intentional reading of earlier messages between replies")
  js('document.getElementById("prompt").value="Another question";document.getElementById("send").click();')
  wait_js('!document.getElementById("send").disabled')
  check(isTRUE(js('document.querySelector(".log-details").open')), "expanded execution details survive a new response")
  wait_js('var h=document.getElementById("chat_stream");h.scrollHeight-h.scrollTop-h.clientHeight<3')
  check(TRUE,"a new reply returns to the latest response even after reading older messages")
  js('var h=document.getElementById("chat_stream"),last=h.querySelectorAll(".msg"),img=document.createElement("img");img.style.width="120px";last[last.length-1].appendChild(img);setTimeout(function(){img.style.height="280px";img.src=imageData;},100);')
  js('new Promise(function(resolve){setTimeout(resolve,250);})')
  wait_js('var h=document.getElementById("chat_stream");h.scrollHeight-h.scrollTop-h.clientHeight<3')
  check(TRUE,"late image layout keeps the latest response in view")
  browser$Emulation$setDeviceMetricsOverride(width=380L,height=480L,deviceScaleFactor=1,mobile=FALSE)
  wait_js('var h=document.getElementById("chat_stream");h.scrollHeight-h.scrollTop-h.clientHeight<3')
  check(TRUE,"resizing the Viewer keeps the latest reply anchored")
  browser$Emulation$clearDeviceMetricsOverride()
  js('window.OriginalReader=window.FileReader;window.FileReader=function(){this.readyState=0;this.readAsDataURL=function(){this.result=imageData;this.readyState=1;window.delayedReader=this;};this.abort=function(){this.readyState=2;};};stage([encodedFile("image/png","late.png")]);document.getElementById("new_chat").click();')
  wait_js('document.querySelector(".empty")!==null && !document.getElementById("send").disabled')
  js('delayedReader.onload();window.FileReader=OriginalReader;')
  check(isTRUE(js('document.querySelectorAll(".attachment-item").length===0 && document.getElementById("uploading_indicator").textContent===""')), "reset cancels an in-progress read and ignores its late callback")
  js('document.getElementById("new_chat").click();')
  wait_js('document.querySelector(".empty")!==null && !document.getElementById("send").disabled')
  js('document.getElementById("prompt").value="SIMULATE_SOURCE_LOSS";document.getElementById("send").click();')
  wait_js('!document.getElementById("send").disabled && document.querySelectorAll(".msg.user").length===1')
  js('stage([encodedFile("image/png","no-source.png")]);')
  wait_js('document.querySelectorAll(".attachment-item").length===1 && !document.getElementById("send").disabled')
  js('document.getElementById("send").click();')
  wait_js('document.querySelectorAll(".attachment-item").length===0 && !document.getElementById("send").disabled && document.querySelectorAll(".msg.assistant").length===2')
  check(isTRUE(js('document.getElementById("filename").textContent==="No Source document" && document.querySelectorAll(".msg.assistant").length===2')), "image-only chat succeeds after Source editor access fails")
  check(isTRUE(js('document.querySelector(".sent-attachment img").naturalWidth>0 && !document.body.textContent.includes("Cannot read the open script")')), "Source lookup failure does not block or discard uploaded images")
  check(isTRUE(js('window.aisourceErrors && window.aisourceErrors.length===0')), "browser completed without JavaScript errors")
  js('$("#prompt").val("Unsent draft that survives relaunch").trigger("change");')
  js('document.getElementById("update_now").click();')
  wait_js('document.getElementById("confirm_update")!==null')
  js('document.getElementById("confirm_update").click();document.getElementById("confirm_update").click();')
  for(i in 1:100){
    if(file.exists(update_marker))break
    Sys.sleep(.05)
  }
  check(file.exists(update_marker) && identical(readLines(update_marker),"requested"),
    "confirming Update now requests one installation even after repeated clicks")
  worker$wait(timeout=10000)
  check(identical(worker$get_exit_status(),0L),"updating stops the app and restores the previous upload option")
  port <- httpuv::randomPort();worker <- start_worker(port)
  url <- paste0("http://127.0.0.1:",port)
  for(i in 1:100) {
    ready <- suppressWarnings(tryCatch({con<-url(paste0(url,"/"),open="rb");close(con);TRUE},error=function(e)FALSE))
    if(ready)break
    Sys.sleep(.05)
  }
  browser$Page$navigate(url)
  wait_js('!!document.getElementById("send") && !document.getElementById("send").disabled && document.querySelectorAll(".msg.user").length===2 && document.getElementById("prompt").value==="Unsent draft that survives relaunch"')
  check(isTRUE(js('document.querySelector(".sent-attachment img").complete && document.querySelector(".sent-attachment img").naturalWidth>0')),
    "a new R server process restores the latest chat, draft and image preview")
  js('window.copied=null;Object.defineProperty(navigator,"clipboard",{configurable:true,value:{writeText:function(t){window.copied=t;return Promise.resolve();}}});document.querySelector(".code-copy").click();')
  wait_js('window.copied!==null')
  check(identical(js('window.copied'),expected),"restored code Copy buttons preserve the exact code")
  check(isTRUE(js('window.aisourceErrors.length===0')),"restoring history has no JavaScript errors")
  js('$("#prompt").val("Draft before Viewer disappears").trigger("change");')
  js('new Promise(function(resolve){setTimeout(resolve,200);})')
  browser$Page$navigate("about:blank")
  unlink(reopen_url);file.create(reopen_marker)
  for(i in 1:100){if(file.exists(reopen_url))break;Sys.sleep(.05)}
  check(worker$is_alive() && file.exists(reopen_url) && identical(readLines(reopen_url),paste0(url,"?ra_reopen=1")),
    "launch reopens the same running server after the Viewer page disappears")
  browser$Page$navigate(readLines(reopen_url))
  wait_js('!!document.getElementById("send") && !document.getElementById("send").disabled && document.querySelectorAll(".msg.user").length===2 && document.getElementById("prompt").value==="Draft before Viewer disappears"')
  check(isTRUE(js('document.querySelector(".sent-attachment img").naturalWidth>0')),
    "reopening a disconnected Viewer restores chat, draft and image pixels without restarting R")
  js('$("#prompt").val("Draft from the still-connected Viewer").trigger("change");')
  js('new Promise(function(resolve){setTimeout(resolve,200);})')
  old_browser <- browser
  browser <- chromote::ChromoteSession$new(parent=old_browser$parent)
  browser$Page$enable();browser$Page$addScriptToEvaluateOnNewDocument(source=page_setup)
  browser$Page$navigate(url)
  wait_js('!!document.getElementById("send") && !document.getElementById("send").disabled && document.getElementById("prompt").value==="Draft from the still-connected Viewer"')
  check(TRUE,"a replacement Viewer saves the still-connected session before restoring its draft")
  old_state <- old_browser$Runtime$evaluate('document.getElementById("send").disabled',returnByValue=TRUE)
  check(isTRUE(old_state$result$value),"the superseded Viewer disconnects and cannot send a duplicate request")
  js('document.getElementById("console_context").click();$("#prompt").val("Newest Viewer draft").trigger("change");')
  wait_js('document.getElementById("console_context_status").textContent.includes("On: 1 line")')
  js('new Promise(function(resolve){setTimeout(resolve,1200);})')
  check(isTRUE(js('document.getElementById("console_context").checked && document.getElementById("console_context_status").textContent.includes("On: 1 line")')),
    "late disconnect cleanup does not stop the replacement Viewer's Console capture")
  old_browser$close()
  browser$Page$navigate(url)
  wait_js('!!document.getElementById("send") && !document.getElementById("send").disabled && document.getElementById("prompt").value==="Newest Viewer draft"')
  check(isTRUE(js('document.querySelectorAll(".msg.user").length===2 && !document.getElementById("console_context").checked')),
    "late old-session saves cannot overwrite the replacement chat or newest draft")
  js('openAction("close");')
  worker$wait(timeout=10000)
  check(identical(worker$get_exit_status(),0L),"Close restores the upload option after a relaunched session")
  port <- httpuv::randomPort();worker <- start_worker(port,standalone=TRUE)
  url <- paste0("http://127.0.0.1:",port)
  for(i in 1:100) {
    ready <- suppressWarnings(tryCatch({con<-url(paste0(url,"/"),open="rb");close(con);TRUE},error=function(e)FALSE))
    if(ready)break
    Sys.sleep(.05)
  }
  browser$Page$navigate(url)
  wait_js('!!document.getElementById("send") && !document.getElementById("send").disabled')
  js('openAction("help");')
  wait_js('document.querySelector("#shiny-modal .code-copy")!==null')
  check(isTRUE(js('document.getElementById("shiny-modal").textContent.includes("OPENAI_API_KEY") && document.getElementById("shiny-modal").textContent.includes("replace AISource.R") && !document.getElementById("shiny-modal").textContent.includes("RAssistant::")')),
    "standalone Help shows environment and script-update instructions without package commands")
  js('window.copied=null;Object.defineProperty(navigator,"clipboard",{configurable:true,value:{writeText:function(t){window.copied=t;return Promise.resolve();}}});document.querySelector("#shiny-modal .code-copy").click();')
  wait_js('window.copied!==null')
  check(identical(js('window.copied'),'source("AISource.R")\n'),"standalone Help copies the exact script launch command")
  js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
  wait_js('document.getElementById("shiny-modal")===null')
  js('openAction("close");')
  worker$wait(timeout=10000)
  check(identical(worker$get_exit_status(),0L),"standalone Close restores the upload option")
  cat(checks,"browser checks passed\n")
}
main()
