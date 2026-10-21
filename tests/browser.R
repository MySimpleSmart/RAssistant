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
  edit_marker <- tempfile()
  on.exit(unlink(edit_marker),add=TRUE)
  source_marker <- tempfile()
  on.exit(unlink(source_marker),add=TRUE)
  navigation_marker <- tempfile()
  on.exit(unlink(navigation_marker),add=TRUE)
  history_root <- tempfile("ra-browser-history-");dir.create(history_root)
  on.exit(unlink(history_root,recursive=TRUE),add=TRUE)
  run_app <- function(root,port,update_marker,history_root,available_marker,reopen_marker,reopen_url,edit_marker,navigation_marker,source_marker,standalone=FALSE) {
    setwd(root)
    source("tests/helpers.R")
    store_env <- new.env(parent=globalenv())
    for(file in c("R/history.R","R/knowledge.R"))sys.source(file,store_env)
    history <- store_env$.conversation_store(project=root,directory=history_root)
    knowledge <- store_env$.knowledge_store(project=root,directory=history_root)
    function_source <- c("f <- function(x) {","","  result <- x + 1","","  result","}")
    if(identical(Sys.getenv("RA_BROWSER_SOURCE_INTENT_ONLY"),"true")) {
      original <- tempfile(fileext=".R");writeLines(function_source,original)
      knowledge$add(original,"test.R");unlink(original)
    }
    if(identical(Sys.getenv("RA_BROWSER_KNOWLEDGE_STRUCTURE_ONLY"),"true")) {
      body <- paste0("# Formatting\n\n```r\n  structure_sentinel <- \"",strrep("k",2400L),
        "\"\n\n  print(structure_sentinel)\n```\n")
      original <- tempfile(fileext=".md");writeChar(body,original,eos=NULL,useBytes=TRUE)
      doc <- knowledge$add(original,"legacy.md");unlink(original)
      path <- file.path(knowledge$path,"documents",paste0(doc$id,".json"))
      saved <- jsonlite::fromJSON(path,simplifyVector=FALSE);saved$format_version <- NULL
      saved$chunks <- list(list(label="lines 1-9",text="structure_sentinel was flattened by an earlier format version."))
      jsonlite::write_json(saved,path,auto_unbox=TRUE)
      knowledge <- store_env$.knowledge_store(project=root,directory=history_root)
    }
    available <- list(status="available",version="0.1.2",current="0.1.1",
      notes=c("Recover a disconnected Viewer.","Keep <draft> text & images."))
    update_result <- list(status="current",version="0.1.1",current="0.1.1")
    published <- FALSE
    console_enabled <- FALSE
    console <- list(start=function(){console_enabled<<-TRUE},stop=function(){console_enabled<<-FALSE;TRUE},
      dispose=function(){console_enabled<<-FALSE;TRUE},snapshot=function(redact){
        fixture <- if(file.exists(source_marker))readRDS(source_marker)else list()
        if(is.character(fixture$console))fixture$console else "CONSOLE_BROWSER_VALUE_42"
      },
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
    if(identical(Sys.getenv("RA_BROWSER_SOURCE_INTENT_ONLY"),"true"))
      app$initial <- list(id="fixture",path=file.path(tempdir(),"test.R"),contents=function_source)
    if(standalone)app$initial <- list(id="fixture",path=file.path(tempdir(),"v2.R"),contents=c(
      "library(tidyverse)","","# 1. Read data",'birth_data <- read_csv("BIRTH_reduced.csv")',"",
      "# 2. Select four numeric variables","X_df <- birth_data %>%", "  select(matage, gestage, birthwt, head)","",
      "# 3. Convert to a matrix","X <- as.matrix(X_df)","","# 4. Check matrix dimensions","rtrtryydim(X)",""))
    edits <- list()
    app$write_source <- function(id,text){
      stopifnot(identical(id,app$initial$id))
      app$initial$contents <- strsplit(paste0(text,"\n"),"\n",fixed=TRUE)[[1L]]
      edits[[length(edits)+1L]] <<- list(operation="replace",text=text,id=id)
      saveRDS(edits,edit_marker)
    }
    app$new_source <- function(text,kind){
      edits[[length(edits)+1L]] <<- list(operation="new",text=text,kind=kind)
      saveRDS(edits,edit_marker)
    }
    app$focus_source <- function(id,row,column,end_column)saveRDS(list(id=id,row=row,column=column,end_column=end_column),navigation_marker)
    data_env <- new.env(parent=emptyenv())
    data_env$birth_data <- data.frame(weight=c(3100,2800),group=c("A","B"))
    data_env$extra_data <- data.frame(value=c(5432,9876))
    data_env$X <- matrix(1:12,3,4,dimnames=list(NULL,c("a","b","c","d")))
    catalog_data <- app$workspace_catalog;dataset_data <- app$workspace_dataset;context_data <- app$workspace_data_context
    app$workspace_catalog <- function(env=data_env)catalog_data(env)
    app$workspace_dataset <- function(name,env=data_env,...)dataset_data(name,env,...)
    app$workspace_data_context <- function(request,source="",selected=character(),env=data_env)context_data(request,source,selected,env)
    work_data <- app$knowledge_work_context
    app$knowledge_work_context <- function(request,...,env=data_env)work_data(request,...,env=env)
    app$workspace_summary <- function()"X: matrix; dimensions=3 x 4; columns=a, b, c, d"
    app$capture_current_plot <- function(directory,width=1200L,height=900L){
      if(file.exists(source_marker) && isTRUE(readRDS(source_marker)$no_plot))stop("No plot selected")
      Sys.sleep(.15)
      path<-tempfile("browser-current-plot-",tmpdir=directory,fileext=".png")
      grDevices::png(path,width=400,height=240)
      par(mar=c(3,3,2,1));hist(1:40,main="Current RStudio plot",col="darkorange")
      invisible(grDevices::dev.off());path
    }
    source_unavailable <- FALSE
    app$active_doc <- function(id=NULL) {
      if(source_unavailable)stop("Simulated RStudio Source lookup failure")
      app$initial$selection <- NULL
      context <- app$initial
      if(file.exists(source_marker)) {
        selected <- readRDS(source_marker);row <- selected$row
        if(!is.null(selected$path))context$path <- selected$path
        if(is.character(selected$contents))context$contents <- selected$contents
        if(is.numeric(row) && row>=1L && row<=length(app$initial$contents)) {
          text <- if(selected$mode=="selection")app$initial$contents[[row]]else""
          context$selection <- list(list(text=text,range=list(start=c(row,1L),end=c(row,nchar(text)+1L))))
        }
      }
      context
    }
    count <- 0L
    app$ask_ai <- function(system_prompt,prompt,images=character()) {
      count <<- count+1L
      Sys.sleep(.2)
      if(grepl("SIMULATED_FAILURE",prompt,fixed=TRUE))stop("Simulated API failure")
      if(grepl("SIMULATE_SOURCE_LOSS",prompt,fixed=TRUE))source_unavailable <<- TRUE
      if(identical(Sys.getenv("RA_BROWSER_SOURCE_INTENT_ONLY"),"true")) {
        if(grepl("LATEST USER REQUEST:\nFIX_FUNCTION",prompt,fixed=TRUE))return(paste0("Correction:\n\n```r-replace\n",
          paste(sub("x + 1","x + 2",function_source,fixed=TRUE),collapse="\n"),"\n```\n\nUsage:\n\n```r\nf(5)\n```"))
        stopifnot(grepl(paste(function_source,collapse="\n"),prompt,fixed=TRUE))
        return("f adds one to each supplied value.\n\n```r\nf(5)\nf(c(1, 2, 3))\n```")
      }
      if(standalone) {
        if(grepl("LATEST USER REQUEST:\nSWITCH_TO_RMD",prompt,fixed=TRUE)) {
          app$initial <- list(id="chunk-fixture",path=file.path(tempdir(),"report.qmd"),contents=c("---","title: Keep","---","Keep this prose.",
            "```{r first}","x <- bad(X)","```","More prose.","~~~{r other}","y <- 1","~~~"))
          return("Rmd fixture opened for the next request.")
        }
        if(grepl("CURRENT R CHUNK: first",prompt,fixed=TRUE))return("```r-replace\nx <- dim(X)\n```")
        if(grepl("LATEST USER REQUEST:\nBLANK_TEST",prompt,fixed=TRUE))return(paste0("Only spacing proposed.\n\n```r-replace\n",paste(app$initial$contents,collapse="\n"),"\n\n```"))
        if(grepl("; action=explain",prompt,fixed=TRUE))return("Selected function call explained. No code changes proposed.")
        if(grepl("; action=fix",prompt,fixed=TRUE))return("```r-replace\ndim(X)\n```")
        if(grepl("; action=improve",prompt,fixed=TRUE))return("```r-replace\nprint(dim(X))\n```")
        code <- sub("rtrtryydim(X)","dim(X)",paste(app$initial$contents,collapse="\n"),fixed=TRUE)
        if(grepl("GRANULAR_TEST",prompt,fixed=TRUE)) {
          code <- sub("library(tidyverse)","suppressPackageStartupMessages(library(tidyverse))",code,fixed=TRUE)
          code <- sub("dim(X)","print(dim(X))",code,fixed=TRUE)
        }
        return(paste0(if(grepl("WORKSPACE METADATA (enabled for this turn)",prompt,fixed=TRUE))"Workspace evidence received: 3 x 4.\n\n"else"",
          if(grepl("5432",prompt,fixed=TRUE))"Dataset cell evidence received: 5432.\n\n"else"",
          "v2.R, line 14: rtrtryydim is a misspelled function name. Use dim(X). Static review; not run.\n\n```r-replace\n",code,"```"))
      }
      paste0("Response ",count,if(grepl("CONSOLE_BROWSER_VALUE_42",prompt,fixed=TRUE))" Console evidence received."else"",
             "\n\n```r-replace\nx <- c(\"<tag>\", \"&lt;\", \"\\\\(x\\\\)\", \"$$\")\nkeep <- x > \"a\" & x < \"z\"\n```\n\n",
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
  start_worker <- function(port,standalone=FALSE)callr::r_bg(run_app,args=list(root,port,update_marker,history_root,available_marker,reopen_marker,reopen_url,edit_marker,navigation_marker,source_marker,standalone),
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
        cat("\nWorker:\n",worker$is_alive(),worker$get_exit_status(),"\n",
          paste(worker$read_output_lines(),collapse="\n"),"\n")
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
  page_setup <- 'Object.defineProperty(document,"hasFocus",{configurable:true,value:function(){return false;}});window.openAction=function(id){var menu=document.getElementById("app_menu");if(menu&&!menu.open)menu.querySelector("summary").click();document.getElementById(id).click();};window.aisourceErrors=[];window.addEventListener("error",function(e){aisourceErrors.push(String(e.message));});window.addEventListener("unhandledrejection",function(e){aisourceErrors.push(String(e.reason));});'
  browser$Page$addScriptToEvaluateOnNewDocument(source=page_setup)
  browser$Page$navigate(url)
  wait_js('!!document.getElementById("send") && !document.getElementById("send").disabled')
  wait_js('document.querySelector(".empty")!==null')
  js('window.alerts=[];window.alert=function(x){alerts.push(x);};window.receivedMessages=[];$(document).on("shiny:message",function(e){if(e.message.custom)receivedMessages.push(e.message.custom);});')
  check(isTRUE(js('document.querySelector(".empty")!==null')), "existing Shiny UI connects and renders")
  check(isTRUE(js('!document.getElementById("app_menu").open && document.querySelector("#app_menu #history")!==null && document.querySelector("#app_menu #knowledge")!==null && document.getElementById("knowledge").textContent==="Knowledge Base" && document.querySelector("#app_menu #help")!==null && document.querySelector("#app_menu #close")!==null && document.getElementById("check_updates").closest("#app_menu")!==null && document.getElementById("console_context").closest(".toolbar-actions")!==null && document.querySelector(".upload-row #console_context")===null && document.getElementById("workspace_context").closest(".toolbar-actions")!==null && document.querySelector("#app_menu #workspace_context")===null && document.getElementById("workspace_context").checked && document.getElementById("workspace_context_state").textContent==="On"')),
    "Check updates lives in More actions while Console and default-on Workspace share the header")
  check(isTRUE(js('document.getElementById("preview_undo").disabled && document.getElementById("preview_redo").disabled')),
    "Undo and Redo are disabled when this Source has no AI change")
  wait_js('!document.getElementById("plot_hover_thumbnail").hidden && document.getElementById("plot_hover_thumbnail").naturalWidth>0')
  check(isTRUE(js('var dot=document.getElementById("plot_available_indicator"),button=document.getElementById("attach_current_plot");button.querySelectorAll("img").length===0 && !dot.hidden && button.firstElementChild===dot && getComputedStyle(dot).backgroundColor==="rgb(47, 128, 237)" && document.querySelectorAll(".attachment-item").length===0 && document.querySelectorAll(".msg.user").length===0')),
    "a blue dot appears before Plot as soon as a plot exists, before any chat attachment")
  saveRDS(list(no_plot=TRUE),source_marker)
  wait_js('document.getElementById("plot_available_indicator").hidden',timeout=6)
  check(TRUE,"clearing RStudio plots automatically hides the dot without hover or focus")
  unlink(source_marker)
  wait_js('!document.getElementById("plot_available_indicator").hidden',timeout=6)
  check(TRUE,"a new RStudio plot automatically restores the dot without hover, focus or attachment")
  # Fast check for the passive indicator; the default still runs the full suite.
  if(identical(Sys.getenv("RA_BROWSER_PLOT_ONLY"),"true")) {
    check(isTRUE(js('document.querySelectorAll(".attachment-item").length===0 && document.querySelectorAll(".msg.user").length===0 && window.aisourceErrors.length===0')),
      "automatic availability checks leave attachments and chat empty without JavaScript errors")
    cat(checks,"Plot browser checks passed\n");return(invisible(NULL))
  }
  if(identical(Sys.getenv("RA_BROWSER_SOURCE_INTENT_ONLY"),"true")) {
    js('openAction("knowledge");')
    wait_js('document.querySelector("#knowledge_selected input")!==null')
    js('document.querySelector("#knowledge_selected input").click();')
    wait_js('document.querySelector(".knowledge-unpin")!==null')
    js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
    wait_js('document.getElementById("shiny-modal")===null')
    js('var p=document.getElementById("prompt");p.value="Энэ файл дахь f() функцийг тайлбарла";p.dispatchEvent(new Event("input",{bubbles:true}));document.getElementById("send").click();')
    wait_js('document.querySelector(".msg.assistant pre code")!==null && !document.getElementById("send").disabled')
    check(isTRUE(js('document.getElementById("source-review-card")===null && document.querySelector(".source-diff")===null && document.querySelector(".source-apply")===null')) && !file.exists(edit_marker),
      "the reported f() explanation displays example calls without a replacement diff or an editor write")
    js('window.copied=null;Object.defineProperty(navigator,"clipboard",{configurable:true,value:{writeText:function(t){window.copied=t;return Promise.resolve();}}});document.querySelector(".code-copy").click();')
    wait_js('window.copied!==null')
    check(identical(js('window.copied'),"f(5)\nf(c(1, 2, 3))\n"),"example Copy preserves the two function calls exactly")
    js('document.querySelector(".knowledge-read>summary").click();')
    check(isTRUE(js('document.querySelector(".knowledge-read summary").textContent.includes("Sent") && document.querySelector(".knowledge-excerpts").textContent.includes("f <- function(x) {\\n\\n  result <- x + 1\\n\\n  result\\n}")')),
      "the same explanation receives the complete selected Knowledge Base function with indentation and blank lines")
    screenshot <- Sys.getenv("RA_BROWSER_SOURCE_INTENT_SCREENSHOT")
    if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
    js('var p=document.getElementById("prompt");p.value="FIX_FUNCTION";p.dispatchEvent(new Event("input",{bubbles:true}));document.getElementById("send").click();')
    wait_js('document.querySelector(".source-apply")!==null && !document.getElementById("send").disabled')
    check(isTRUE(js('var wraps=Array.from(document.querySelectorAll(".msg.assistant .code-wrap"));wraps[1].classList.contains("review-open") && !wraps[2].classList.contains("review-open") && wraps[1].querySelector(".code-type").textContent==="R" && document.querySelector(".source-apply").textContent==="Replace"')) && !file.exists(edit_marker),
      "a marked correction opens its own diff while its separate usage example remains plain code")
    js('document.querySelector(".source-apply").click();')
    wait_js('document.getElementById("source-review-card")===null && !document.getElementById("send").disabled')
    writes <- readRDS(edit_marker)
    check(length(writes)==1L && grepl("f <- function(x)",writes[[1L]]$text,fixed=TRUE) &&
      grepl("  result <- x + 2",writes[[1L]]$text,fixed=TRUE) && !grepl("f(5)",writes[[1L]]$text,fixed=TRUE),
      "Replace applies the complete correction once and keeps the example calls out of Source")
    check(isTRUE(js('window.aisourceErrors.length===0')),"example and correction rendering completes without JavaScript errors")
    cat(checks,"Source intent browser checks passed\n");return(invisible(NULL))
  }
  if(identical(Sys.getenv("RA_BROWSER_KNOWLEDGE_STRUCTURE_ONLY"),"true")) {
    js('openAction("knowledge");')
    wait_js('document.querySelector("#knowledge_selected input")!==null')
    check(isTRUE(js('document.getElementById("shiny-modal").textContent.includes("legacy.md (older format)") && document.getElementById("shiny-modal").textContent.includes("Re-add files marked older format")')),
      "legacy references identify the formatting limitation and the re-add action")
    js('window.legacyId=document.querySelector("#knowledge_selected input").value;document.querySelector("#knowledge_selected input").click();')
    wait_js('document.querySelector(".knowledge-unpin")!==null')
    body <- paste0("# Formatting\n\n```r\n  structure_sentinel <- \"",strrep("k",2400L),
      "\"\n\n  print(structure_sentinel)\n```\n")
    js(paste0('window.structureBody=',jsonlite::toJSON(body,auto_unbox=TRUE),';var dt=new DataTransfer();dt.items.add(new File([structureBody],"legacy.md",{type:"text/markdown"}));var input=document.getElementById("knowledge_files");input.files=dt.files;input.dispatchEvent(new Event("change",{bubbles:true}));'))
    wait_js('document.querySelector("#knowledge_selected input")!==null && !document.getElementById("shiny-modal").textContent.includes("older format")',timeout=30)
    check(isTRUE(js('document.querySelector("#knowledge_selected input").value===legacyId && document.querySelector("#knowledge_selected input").checked && document.querySelector(".knowledge-unpin").getAttribute("data-id")===legacyId')),
      "re-adding identical bytes upgrades formatting while keeping the same file id and selected scope")
    js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
    wait_js('document.getElementById("shiny-modal")===null')
    js('var p=document.getElementById("prompt");p.value="Explain this";p.dispatchEvent(new Event("input",{bubbles:true}));document.getElementById("send").click();')
    wait_js('document.querySelectorAll(".msg.user").length===1 && !document.getElementById("send").disabled && document.querySelector(".knowledge-read summary").textContent.includes("Sent")')
    js('document.querySelector(".knowledge-read>summary").click();')
    check(isTRUE(js('document.querySelector(".knowledge-excerpts").textContent.includes(structureBody.trimEnd()) && document.querySelector(".knowledge-excerpts").textContent.includes("Section: Formatting") && !document.querySelector(".knowledge-excerpts").textContent.includes("[excerpt truncated]")')),
      "the sent receipt shows the entire long code fence, section, indentation and blank lines")
    screenshot <- Sys.getenv("RA_BROWSER_KNOWLEDGE_STRUCTURE_SCREENSHOT")
    if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
    js('openAction("knowledge");')
    wait_js('document.getElementById("knowledge_files")!==null')
    js('var content="```r\\nfirst_block <- \\\""+"z".repeat(7000)+"\\\"\\n```\\n\\n```r\\nsecond_block <- \\\""+"w".repeat(7000)+"\\\"\\n```\\n";var dt=new DataTransfer();dt.items.add(new File([content],"oversized.md",{type:"text/markdown"}));var input=document.getElementById("knowledge_files");input.files=dt.files;input.dispatchEvent(new Event("change",{bubbles:true}));')
    wait_js('document.querySelectorAll("#knowledge_selected input").length===2',timeout=30)
    js('document.querySelector("#knowledge_selected input:checked").click();Array.from(document.querySelectorAll("#knowledge_selected label")).find(function(x){return x.textContent.includes("oversized.md");}).querySelector("input").click();')
    wait_js('document.querySelector(".knowledge-unpin").textContent.includes("oversized.md")')
    js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
    wait_js('document.getElementById("shiny-modal")===null')
    js('var p=document.getElementById("prompt");p.value="Explain this";p.dispatchEvent(new Event("input",{bubbles:true}));document.getElementById("send").click();')
    wait_js('document.querySelectorAll(".msg.user").length===2 && !document.getElementById("send").disabled')
    check(isTRUE(js('var r=Array.from(document.querySelectorAll(".knowledge-read")).pop();r.querySelector("summary").textContent.includes("Matching content exceeds excerpt limit") && r.querySelector(".knowledge-excerpts")===null && r.querySelectorAll("li").length===2')),
      "oversized complete blocks receive omission notices instead of partial code in the browser")
    browser$Page$reload()
    wait_js('document.querySelectorAll(".knowledge-read").length===2 && !document.getElementById("send").disabled',timeout=30)
    check(isTRUE(js('var r=Array.from(document.querySelectorAll(".knowledge-read")).pop();r.querySelectorAll("li").length===2 && document.querySelector(".knowledge-excerpts").textContent.includes("k".repeat(2400)) && document.querySelector(".knowledge-unpin").textContent.includes("oversized.md") && window.aisourceErrors.length===0')),
      "reopening preserves complete old receipts, multiple limit notices and the current file selection")
    cat(checks,"structural Knowledge Base browser checks passed\n");return(invisible(NULL))
  }
  if(identical(Sys.getenv("RA_BROWSER_KNOWLEDGE_WORK_ONLY"),"true")) {
    js('openAction("knowledge");')
    wait_js('document.getElementById("knowledge_files")!==null')
    js('var dt=new DataTransfer();[["graphics.md","The ggplot2 package uses layers to draw statistical graphics with geom_smooth for fitted trends."],["birth.md","The weight column is birth weight in grams; group records the observed category. Check weight units and group definitions."],["arithmetic.md","A non-numeric argument to binary operator means arithmetic received character input. Inspect typeof and convert with as.numeric."],["regression.md","Linear regression estimates coefficients with lm. Examine linear regression residual diagnostics."]].forEach(function(x){dt.items.add(new File([x[1]],x[0],{type:"text/markdown"}));});var input=document.getElementById("knowledge_files");input.files=dt.files;input.dispatchEvent(new Event("change",{bubbles:true}));')
    wait_js('document.querySelectorAll("#knowledge_selected input").length===4',timeout=30)
    js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
    wait_js('document.getElementById("shiny-modal")===null')
    submit <- function(text,turn){
      js(paste0('var p=document.getElementById("prompt");p.value=',jsonlite::toJSON(text,auto_unbox=TRUE),';p.dispatchEvent(new Event("input",{bubbles:true}));document.getElementById("send").click();'))
      wait_js(paste0('document.querySelectorAll(".msg.user").length===',turn,' && !document.getElementById("send").disabled && Array.from(document.querySelectorAll(".knowledge-read>summary")).pop().textContent.includes("Sent")'))
    }
    last_receipt <- function()js('Array.from(document.querySelectorAll(".knowledge-read")).pop().textContent')
    saveRDS(list(contents="library(ggplot2)"),source_marker)
    js('document.getElementById("console_context").click();document.getElementById("workspace_context").click();')
    wait_js('document.getElementById("console_context_state").textContent==="Off" && document.getElementById("workspace_context_state").textContent==="Off"')
    submit("Explain my code",1L)
    check(grepl("graphics.md",last_receipt(),fixed=TRUE) && !grepl("birth.md",last_receipt(),fixed=TRUE),
      "a generic browser request retrieves a guide from the current Source package alone")
    saveRDS(list(contents="identity(birth_data)"),source_marker)
    js('document.getElementById("workspace_context").click();')
    wait_js('document.getElementById("workspace_context_state").textContent==="On"')
    submit("Explain these columns",2L)
    check(grepl("birth.md",last_receipt(),fixed=TRUE),"Workspace column names connect the browser request to its data dictionary")
    diagnostic <- "Error in arithmetic(): non-numeric argument to binary operator\n[End failed Console task; full command may be unavailable]"
    saveRDS(list(contents="",console=diagnostic),source_marker)
    js('document.getElementById("console_context").click();')
    wait_js('document.getElementById("console_context_state").textContent==="On"')
    submit("Энэ алдааг тайлбарла",3L)
    check(grepl("arithmetic.md",last_receipt(),fixed=TRUE),"the enabled Console error supplies the topic for a short Mongolian browser question")
    js('document.getElementById("console_context").click();')
    wait_js('document.getElementById("console_context_state").textContent==="Off"')
    submit("Fix this error",4L)
    check(grepl("No matching excerpts",last_receipt(),fixed=TRUE),"Console Off removes the diagnostic from browser retrieval")
    saveRDS(list(contents=c("library(ggplot2)","identity(birth_data)"),console=diagnostic),source_marker)
    js('document.getElementById("console_context").click();')
    wait_js('document.getElementById("console_context_state").textContent==="On"')
    submit("Explain linear regression",5L)
    check(grepl("regression.md",last_receipt(),fixed=TRUE) && !grepl("graphics.md|birth.md|arithmetic.md",last_receipt()),
      "a specifically requested new topic overrides unrelated Source, Console and Workspace hints in the browser")
    check(isTRUE(js('Array.from(document.querySelectorAll(".msg.assistant .content")).pop().textContent.includes("Response 5") && window.aisourceErrors.length===0')),
      "five sends make five model requests with no extra translation call or JavaScript error")
    cat(checks,"work-aware Knowledge Base browser checks passed\n");return(invisible(NULL))
  }
  if(identical(Sys.getenv("RA_BROWSER_KNOWLEDGE_ONLY"),"true")) {
    js('openAction("knowledge");')
    wait_js('document.getElementById("knowledge_files")!==null')
    js('var dt=new DataTransfer();dt.items.add(new File(["Kmeans clustering uses the elbow method and fviz_nbclust. <script>window.kbInjected=true</script>"],"clustering.md",{type:"text/markdown"}));dt.items.add(new File(["Linear regression coefficients are estimated with lm and examined with summary."],"regression.md",{type:"text/markdown"}));var input=document.getElementById("knowledge_files");input.files=dt.files;input.dispatchEvent(new Event("change",{bubbles:true}));')
    wait_js('document.querySelectorAll("#knowledge_selected input").length===2',timeout=30)
    check(isTRUE(js('document.querySelectorAll("#knowledge_selected input:checked").length===0 && document.getElementById("shiny-modal").textContent.includes("Leave unchecked for automatic search")')),
      "Knowledge Base starts with automatic search and a short explanation")
    js('document.querySelector("#knowledge_selected input").click();$("#knowledge_instructions").val("Prefer <ggplot2> & explain briefly.").trigger("change");document.getElementById("knowledge_save").click();')
    wait_js('document.querySelector(".knowledge-unpin")!==null')
    js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
    wait_js('document.getElementById("shiny-modal")===null')
    js('openAction("knowledge");')
    wait_js('document.querySelectorAll("#knowledge_selected input:checked").length===1')
    check(TRUE,"closing and reopening Knowledge Base keeps this chat's file selection")
    js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
    wait_js('document.getElementById("shiny-modal")===null')
    js('var p=document.getElementById("prompt");p.value="Explain this";p.dispatchEvent(new Event("input",{bubbles:true}));document.getElementById("send").click();')
    wait_js('document.querySelectorAll(".msg.user").length===1 && !document.getElementById("send").disabled && document.querySelector(".knowledge-read summary").textContent.includes("Sent")')
    check(isTRUE(js('!document.querySelector(".knowledge-read").open && document.querySelector(".knowledge-read summary").textContent.includes("1 file / 1 excerpt") && getComputedStyle(document.querySelector(".knowledge-read summary")).display==="list-item"')),
      "a short question gets a compact collapsed receipt from the selected file")
    js('document.querySelector(".knowledge-read>summary").click();')
    check(isTRUE(js('document.querySelector(".knowledge-excerpts").textContent.includes("[K1] clustering.md, line 1") && document.querySelector(".knowledge-excerpts").textContent.includes("<script>") && document.querySelector(".knowledge-read script")===null && !window.kbInjected && document.querySelector(".knowledge-read").textContent.includes("Prefer <ggplot2> & explain briefly.")')),
      "expanded receipts show exact text and instructions with inert HTML")
    screenshot <- Sys.getenv("RA_BROWSER_KNOWLEDGE_SCREENSHOT")
    if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
    browser$Page$reload()
    wait_js('document.querySelector(".knowledge-unpin")!==null && document.querySelector(".knowledge-read summary").textContent.includes("Sent") && !document.getElementById("send").disabled',timeout=30)
    check(isTRUE(js('document.querySelector(".knowledge-excerpts").textContent.includes("fviz_nbclust") && document.querySelectorAll(".msg.user").length===1')),
      "reopening the Viewer restores selected files and exact historical receipts")
    js('document.querySelector(".knowledge-unpin").click();')
    wait_js('document.querySelector(".knowledge-unpin")===null')
    js('var p=document.getElementById("prompt");p.value="Translate hello into French";p.dispatchEvent(new Event("input",{bubbles:true}));document.getElementById("send").click();')
    wait_js('document.querySelectorAll(".knowledge-read").length===2 && !document.getElementById("send").disabled')
    check(isTRUE(js('Array.from(document.querySelectorAll(".knowledge-read")).pop().textContent.includes("Automatic search") && document.querySelector(".knowledge-unpin")===null')),
      "unpinning a document returns to automatic search")
    check(isTRUE(js('Array.from(document.querySelectorAll(".knowledge-read>summary")).pop().textContent.includes("No matching excerpts")')),
      "an unrelated question explicitly reports no matching material")
    js('openAction("knowledge");')
    wait_js('document.querySelectorAll("#knowledge_selected input").length===2')
    js('document.querySelector("#knowledge_selected input").click();')
    wait_js('document.querySelector(".knowledge-unpin")!==null')
    js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
    wait_js('document.getElementById("shiny-modal")===null')
    js('document.getElementById("new_chat").click();')
    wait_js('document.querySelectorAll(".msg.user").length===0 && document.querySelector(".knowledge-unpin")===null')
    js('openAction("history");')
    wait_js('document.getElementById("history_selected")!==null')
    check(isTRUE(js('document.getElementById("history_selected").options.length===1')),
      "a new chat resets selection without adding a blank History entry")
    js('$("#history_selected").val(document.getElementById("history_selected").options[0].value).trigger("change");document.getElementById("history_restore").click();')
    wait_js('document.getElementById("shiny-modal")===null && document.querySelector(".knowledge-unpin")!==null')
    check(isTRUE(js('document.querySelectorAll(".msg.user").length===2 && document.querySelectorAll(".knowledge-read").length===2')),
      "History restores the chat's selection and receipts together")
    js('openAction("knowledge");')
    wait_js('document.querySelectorAll("#knowledge_selected input:checked").length===1')
    js('document.getElementById("knowledge_remove").click();')
    wait_js('document.getElementById("knowledge_confirm_remove")!==null')
    js('document.getElementById("knowledge_confirm_remove").click();')
    wait_js('document.querySelectorAll("#knowledge_selected input").length===1 && document.querySelector(".knowledge-unpin")===null')
    js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
    wait_js('document.getElementById("shiny-modal")===null')
    check(isTRUE(js('document.querySelector(".knowledge-excerpts").textContent.includes("fviz_nbclust") && window.aisourceErrors.length===0')),
      "deleting selected material clears its scope chip and preserves earlier receipts without JavaScript errors")
    cat(checks,"Knowledge Base browser checks passed\n");return(invisible(NULL))
  }
  js('document.querySelector("#workspace_menu>summary").click();')
  wait_js('document.querySelector("#workspace_datasets_selected input[value=birth_data]")!==null')
  js('document.querySelector("#workspace_datasets_selected input[value=birth_data]").click();')
  wait_js('document.querySelector(".dataset-preview")!==null && document.querySelector(".dataset-preview").textContent.includes("3100")')
  check(isTRUE(js('document.querySelector(".workspace-panel").getBoundingClientRect().left>=0 && document.querySelector(".workspace-panel").getBoundingClientRect().right<=innerWidth && document.querySelectorAll(".msg.user").length===0')),
    "Workspace picker previews actual local cells without a chat request and stays inside the Viewer")
  js('document.querySelector("#workspace_datasets_selected input[value=birth_data]").click();document.querySelector("#workspace_menu>summary").click();')
  check(isTRUE(js('document.getElementById("preview_undo").closest(".upload-row")!==null && document.getElementById("preview_redo").closest(".upload-row")!==null && document.querySelector("#app_menu #preview_undo")===null && document.getElementById("preview_undo").getClientRects().length>0 && document.getElementById("preview_redo").getClientRects().length>0')),
    "Undo and Redo are visible beside Image and Plot without opening a menu")
  js('document.querySelector("#app_menu>summary").click();')
  check(isTRUE(js('Array.from(document.querySelectorAll(".menu-heading")).map(function(e){return e.textContent;}).join("|")==="Code|Chat|App" && document.getElementById("history").textContent==="Chat history" && document.getElementById("edit_history").textContent==="Code edit history" && document.querySelectorAll(".selection-menu").length===0 && document.getElementById("code_actions")===null')),
    "Code tools no longer require a Code actions entry or either old scope menu")
  check(isTRUE(js('var update=document.getElementById("check_updates").getBoundingClientRect(),help=document.getElementById("help").getBoundingClientRect(),close=document.getElementById("close").getBoundingClientRect();Math.abs(update.top-help.top)<2 && close.top>=help.bottom && close.width>=document.querySelector("#close").parentElement.getBoundingClientRect().width-2 && document.getElementById("close").closest(".menu-close")!==null && document.getElementById("close").closest(".menu-group")===null && getComputedStyle(document.getElementById("close")).textAlign==="center" && getComputedStyle(document.getElementById("close")).color==="rgb(255, 255, 255)" && getComputedStyle(document.getElementById("close")).backgroundColor!=="rgb(255, 255, 255)"')),
    "Check updates and Help share a row; Close is isolated, centered and red")
  js('document.dispatchEvent(new KeyboardEvent("keydown",{key:"Escape",bubbles:true}));')
  check(isTRUE(js('!document.getElementById("app_menu").open && document.activeElement===document.querySelector("#app_menu>summary")')),
    "Escape closes the actions menu and restores keyboard focus")
  saveRDS(list(mode="cursor",row=1L,path="/tmp/other-workspace.R"),source_marker)
  wait_js('document.getElementById("filename").textContent==="other-workspace.R"',timeout=2)
  check(isTRUE(js('document.querySelector(".code-target-toolbar")===null && document.querySelectorAll(".msg").length===0')),
    "the header quickly follows an unselected Source tab without a toolbar or chat request")
  saveRDS(list(mode="cursor",row=1L,path="/tmp/renamed-workspace.R"),source_marker)
  wait_js('document.getElementById("filename").textContent==="renamed-workspace.R"',timeout=2)
  unlink(source_marker)
  wait_js('document.getElementById("filename").textContent==="example.R"',timeout=2)
  check(TRUE,"Source renames and tab switches update the rendered label within two seconds")
  js('document.getElementById("new_chat").click();')
  wait_js('document.querySelector(".empty")!==null')
  js('openAction("history");')
  wait_js('document.getElementById("shiny-modal")!==null')
  check(isTRUE(js('document.getElementById("history_selected")===null && document.getElementById("shiny-modal").textContent.includes("No saved chats yet.")')),
    "opening History after New chat does not list an empty conversation")
  js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
  wait_js('document.getElementById("shiny-modal")===null')
  saveRDS(list(mode="selection",row=1L),source_marker)
  wait_js('document.querySelector(".code-target-toolbar")!==null')
  check(isTRUE(js('document.querySelector(".code-target-label").textContent.includes("example.R") && document.querySelector(".code-target-label").textContent.includes("Selected line 1") && document.getElementById("smart_explain").getClientRects().length>0 && document.getElementById("smart_fix").getClientRects().length>0 && document.getElementById("smart_improve").getClientRects().length>0 && document.querySelectorAll(".msg.user").length===0 && !document.getElementById("app_menu").open')),
    "selecting Source code automatically reveals Explain, Fix and Improve without a menu or AI request")
  check(isTRUE(js('document.querySelectorAll("#preview_undo svg,#preview_redo svg").length===2 && document.getElementById("preview_undo").textContent.trim()==="" && document.getElementById("preview_redo").textContent.trim()==="" && document.getElementById("preview_undo").title.includes("Undo") && document.getElementById("preview_redo").getAttribute("aria-label").includes("Redo")')),
    "Undo and Redo are accessible SVG icon buttons with descriptive tooltips")
  js('document.querySelector(".code-target-close").click();')
  wait_js('document.querySelector(".code-target-toolbar")===null')
  Sys.sleep(1.4)
  check(isTRUE(js('document.querySelector(".code-target-toolbar")===null')),"dismissed actions stay hidden while the same Source selection remains")
  unlink(source_marker)
  Sys.sleep(.8)
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
  check(isTRUE(js('document.getElementById("shiny-modal").textContent.includes("RAssistant::launch()") && document.getElementById("shiny-modal").textContent.includes("RAssistant::set_api_key()") && document.getElementById("shiny-modal").textContent.includes("hidden password prompt") && !document.getElementById("shiny-modal").textContent.includes("Restart R") && document.querySelector("#app_menu #check_updates")!==null && document.querySelector("#shiny-modal #check_updates")===null')),
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
  js('openAction("check_updates");')
  wait_js('document.getElementById("update_now")===null && document.getElementById("update_check_status").textContent.includes("up to date")')
  check(isTRUE(js('!document.getElementById("app_menu").open')),"menu update checks close the menu and show results without opening Help")
  js('openAction("help");')
  wait_js('document.querySelector("#shiny-modal .code-copy")!==null')
  check(isTRUE(js('document.querySelector("#shiny-modal #update_help_status")===null && !document.getElementById("shiny-modal").textContent.includes("up to date")')),
    "Help omits the up-to-date result while a manual check result is active")
  js('document.querySelector("#shiny-modal .modal-footer [data-dismiss=modal]").click();')
  wait_js('document.getElementById("shiny-modal")===null')
  wait_js('document.getElementById("update_check_status").textContent===""',timeout=8)
  check(isTRUE(js('getComputedStyle(document.getElementById("update_check_status")).display==="none"')),
    "an up-to-date result expires and releases its banner space")
  js('openAction("check_updates");')
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
  check(isTRUE(js('!document.getElementById("plot_available_indicator").hidden && document.querySelectorAll("#attach_current_plot img").length===0')),
    "the blue availability dot remains when a plot is attached")
  check(isTRUE(js('document.querySelector(".attachment-item img").naturalWidth>0 && document.getElementById("prompt").value==="Keep this draft" && document.querySelectorAll(".msg.user").length===0')),
    "Attach current plot previews one image and preserves the draft without an AI request")
  browser$Emulation$setDeviceMetricsOverride(width=380L,height=700L,deviceScaleFactor=1,mobile=FALSE)
  saveRDS(list(mode="selection",row=1L),source_marker)
  wait_js('document.querySelector(".code-target-toolbar")!==null')
  check(isTRUE(js('var toolbar=document.querySelector(".code-target-toolbar").getBoundingClientRect(),last=document.querySelector(".code-target-close").getBoundingClientRect();toolbar.left>=0 && toolbar.right<=innerWidth && last.right<=innerWidth')),
    "the automatic code toolbar fits a narrow Viewer without clipping its buttons")
  js('document.querySelector("#app_menu>summary").click();')
  screenshot<-Sys.getenv("RA_BROWSER_MENU_SCREENSHOT")
  if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
  js('document.dispatchEvent(new KeyboardEvent("keydown",{key:"Escape",bubbles:true}));document.querySelector(".code-target-close").click();')
  wait_js('document.querySelector(".code-target-toolbar")===null')
  unlink(source_marker)
  check(isTRUE(js('document.getElementById("attach_current_plot").getBoundingClientRect().right<=innerWidth && document.getElementById("send").getBoundingClientRect().right<=innerWidth && document.getElementById("source_context").closest(".toolbar")!==null && document.getElementById("source_context").textContent.trim()==="example.R" && document.getElementById("console_context").closest(".console-context").getBoundingClientRect().right<=innerWidth && document.querySelector(".toolbar").getBoundingClientRect().height<=40 && Math.abs(document.querySelector(".workspace-context").getBoundingClientRect().top-document.querySelector(".console-context").getBoundingClientRect().top)<2 && document.querySelector(".workspace-context").getBoundingClientRect().right<=innerWidth && document.getElementById("preview_redo").getBoundingClientRect().right<=innerWidth && Math.abs(document.getElementById("preview_undo").getBoundingClientRect().top-document.getElementById("send").getBoundingClientRect().top)<2')),
    "the file name, Console and Workspace share one compact header in a narrow Viewer")
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
  wait_js('!document.getElementById("plot_available_indicator").hidden')
  check(TRUE,"removing a chat attachment does not hide the dot while RStudio still has a plot")
  check(isTRUE(js('document.getElementById("console_context").checked')),
    "Console context is checked by default")
  wait_js('document.getElementById("console_context_status").textContent.includes("On: 1 line")')
  js('document.getElementById("console_context").click();')
  wait_js('document.getElementById("console_context_state").textContent==="Off"')
  check(isTRUE(js('!document.getElementById("console_context").checked')),"the default Console capture can still be switched off")
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
  check(isTRUE(js('document.querySelector(".source-read").textContent.includes("example.R") && document.getElementById("source_context").title.includes("sent to AI")')),
    "chat and file tooltip identify the Source snapshot actually sent")
  wait_js('document.querySelector(".source-diff")!==null && document.querySelector(".source-apply")!==null && !document.querySelector(".source-apply").disabled')
  check(isTRUE(js('document.getElementById("shiny-modal")===null && document.querySelector("#chat_stream .code-wrap .source-edit-card")!==null && getComputedStyle(document.querySelector(".review-open > pre")).display==="none" && document.querySelector(".source-apply").textContent==="Replace"')),
    "the answer automatically shows its comparison, Replace and Copy inside the code block without Review or a modal")
  check(isTRUE(js('document.querySelector(".diff-remove").getAttribute("data-old-line")==="1" && document.querySelector(".diff-add").getAttribute("data-new-line")==="1" && document.querySelector(".diff-word")!==null && document.querySelector(".diff-location").textContent.includes("example.R:1")')),
    "the comparison numbers the actual Source and proposed lines and highlights the changed characters")
  js('window.copied=null;document.querySelector(".source-proposal-copy").click();')
  wait_js('window.copied!==null')
  check(identical(js('window.copied'),expected) && !file.exists(edit_marker),
    "Copy beside Replace copies the exact proposed code without changing Source")
  check(!file.exists(edit_marker) && isTRUE(js('document.querySelector(".source-diff").textContent.includes("-plot(1:3)") && document.querySelector(".source-diff").textContent.includes("+x <-") && document.querySelector(".edit-check").textContent.includes("Not run")')),
    "Review shows syntax checking and a real comparison before any editor write")
  browser$Emulation$setDeviceMetricsOverride(width=380L,height=480L,deviceScaleFactor=1,mobile=FALSE)
  check(isTRUE(js('document.querySelector(".source-edit-card").getBoundingClientRect().right<=innerWidth && document.querySelector(".source-diff").clientHeight<=280 && document.getElementById("cancel_source_edit").closest("#chat_stream")!==null && document.documentElement.scrollWidth<=innerWidth')),
    "the inline comparison fits a small Viewer with bounded diff scrolling")
  browser$Emulation$clearDeviceMetricsOverride()
  screenshot<-Sys.getenv("RA_BROWSER_EDIT_SCREENSHOT")
  if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
  js('document.getElementById("cancel_source_edit").click();')
  wait_js('document.getElementById("source-review-card")===null && document.querySelector(".review-open")===null')
  check(!file.exists(edit_marker) && isTRUE(js('getComputedStyle(document.querySelector(".code-wrap > pre")).display!=="none"')),"Cancel leaves Source untouched and restores the code")
  js('document.querySelector(".source-review").click();')
  wait_js('document.querySelector(".source-apply")!==null && !document.querySelector(".source-apply").disabled')
  js('document.querySelector(".source-apply").click();document.querySelector(".source-apply").click();')
  wait_js('document.getElementById("source-review-card")===null && !document.getElementById("send").disabled')
  check(file.exists(edit_marker) && length(readRDS(edit_marker))==1L && identical(readRDS(edit_marker)[[1L]]$text,expected),
    "Apply writes the exact reviewed bytes once after explicit approval")
  js('document.querySelector(".source-review").click();')
  wait_js('document.querySelector(".source-apply")!==null && !document.querySelector(".source-apply").disabled')
  check(isTRUE(js('document.querySelector(".source-edit-mode select").value==="new_r"')),
    "a stale code answer cannot replace a changed Source snapshot")
  js('document.querySelector(".source-apply").click();')
  wait_js('document.getElementById("source-review-card")===null && !document.getElementById("send").disabled')
  check(length(readRDS(edit_marker))==2L && readRDS(edit_marker)[[2L]]$operation=="new",
    "a reviewed new-document proposal needs its own explicit approval")
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
  # A fresh server must initialize its bindings and restore image history.
  # Allow startup time separately from the short checks on an existing page.
  wait_js('!!document.getElementById("send") && !document.getElementById("send").disabled && document.querySelectorAll(".msg.user").length===2 && document.getElementById("prompt").value==="Unsent draft that survives relaunch"',timeout=30)
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
  js('$("#prompt").val("Newest Viewer draft").trigger("change");')
  wait_js('document.getElementById("console_context_status").textContent.includes("On: 1 line")')
  js('new Promise(function(resolve){setTimeout(resolve,1200);})')
  check(isTRUE(js('document.getElementById("console_context").checked && document.getElementById("console_context_status").textContent.includes("On: 1 line")')),
    "late disconnect cleanup does not stop the replacement Viewer's Console capture")
  old_browser$close()
  browser$Page$navigate(url)
  wait_js('!!document.getElementById("send") && !document.getElementById("send").disabled && document.getElementById("prompt").value==="Newest Viewer draft"')
  check(isTRUE(js('document.querySelectorAll(".msg.user").length===2 && document.getElementById("console_context").checked')),
    "late old-session saves cannot overwrite the replacement chat or newest draft")
  js('openAction("close");')
  worker$wait(timeout=10000)
  check(identical(worker$get_exit_status(),0L),"Close restores the upload option after a relaunched session")
  unlink(edit_marker)
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
  js('document.getElementById("prompt").value="Кодонд ямар алдаа байна?";document.getElementById("send").click();')
  wait_js('document.querySelector(".source-proposal-copy")!==null && !document.getElementById("send").disabled')
  check(isTRUE(js('document.querySelector(".source-read").textContent.includes("15 lines") && document.querySelector(".diff-location").textContent.includes("v2.R:14") && document.querySelector(".diff-remove").textContent.includes("rtrtryydim(X)") && document.querySelector(".diff-add").textContent.includes("+dim(X)") && document.querySelector(".diff-remove .diff-word").textContent==="rtrtryy" && document.getElementById("shiny-modal")===null')),
    "the reported v2.R example automatically marks the actual faulty line 14 in a 15-line file and its exact correction")
  js('window.copied=null;document.querySelector(".source-proposal-copy").click();')
  wait_js('window.copied!==null')
  check(isTRUE(js('window.copied.startsWith("library(tidyverse)") && window.copied.includes("BIRTH_reduced.csv") && window.copied.includes("dim(X)") && !window.copied.includes("rtrtryydim")')),
    "the reported example copies the complete corrected file beside Replace")
  if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),sub("\\.png$","-v2.png",screenshot))
  js('document.querySelector(".source-jump.diff-location").click();')
  for(i in 1:100){if(file.exists(navigation_marker))break;Sys.sleep(.05)}
  check(file.exists(navigation_marker) && readRDS(navigation_marker)$row==14L && readRDS(navigation_marker)$id=="fixture",
    "the filename and line link focuses the exact verified Source document without a write")
  open_code <- function(action){
    saveRDS(list(mode="selection",row=14L),source_marker)
    wait_js('document.getElementById("smart_explain")!==null && !document.getElementById("smart_explain").disabled')
    js(paste0('document.getElementById("smart_',action,'").click();'))
  }
  open_code("explain")
  wait_js('!document.getElementById("send").disabled && document.querySelector("#messages").textContent.includes("Selected function call explained")')
  check(isTRUE(js('document.getElementById("source-review-card")===null && document.querySelector("#messages").textContent.includes("selected 14-14")')),
    "Selected code Explain stays read-only and labels the selected Source lines")
  open_code("fix")
  wait_js('document.querySelector(".source-apply")!==null && !document.querySelector(".source-apply").disabled')
  check(isTRUE(js('document.querySelector(".source-apply").textContent==="Replace selection" && document.querySelector(".source-diff").textContent.includes("rtrtryydim")')),
    "Selected code Fix previews a bounded replacement instead of replacing the whole file")
  js('window.copied=null;document.querySelector(".source-proposal-copy").click();')
  wait_js('window.copied!==null')
  check(identical(js('window.copied'),"dim(X)"),"Copy for a selected-code correction copies only its replacement fragment")
  before_edits <- if(file.exists(edit_marker))length(readRDS(edit_marker))else 0L
  js('document.querySelector(".source-apply").click();')
  wait_js('document.getElementById("source-review-card")===null && !document.getElementById("send").disabled')
  check(length(readRDS(edit_marker))==before_edits+1L && startsWith(tail(readRDS(edit_marker),1L)[[1L]]$text,"library(tidyverse)") &&
    endsWith(tail(readRDS(edit_marker),1L)[[1L]]$text,"dim(X)\n"),"selected-code approval keeps the surrounding file intact")
  open_code("improve")
  wait_js('document.querySelector(".source-apply")!==null && !document.querySelector(".source-apply").disabled')
  check(isTRUE(js('document.querySelector(".source-apply").textContent==="Replace selection" && document.querySelector(".source-diff").textContent.includes("+print(dim(X))")')),
    "Selected code Improve uses the fresh current selection and the same bounded approval")
  js('document.querySelector(".source-skip").click();')
  wait_js('document.getElementById("source-review-card")===null && !document.getElementById("send").disabled')
  js('document.getElementById("prompt").value="GRANULAR_TEST";document.getElementById("send").click();')
  wait_js('document.querySelectorAll(".source-hunk").length===2 && !document.getElementById("send").disabled')
  check(isTRUE(js('document.querySelectorAll(".source-skip").length===2 && document.querySelectorAll(".source-apply").length===3')),
    "independent changes each have Apply and Skip plus an explicit Apply all")
  js('document.querySelector(".source-hunk .source-apply").click();')
  wait_js('document.querySelectorAll(".source-hunk").length===1 && !document.getElementById("send").disabled')
  check(startsWith(tail(readRDS(edit_marker),1L)[[1L]]$text,"suppressPackageStartupMessages") &&
    endsWith(tail(readRDS(edit_marker),1L)[[1L]]$text,"dim(X)\n"),"Apply changes one part and refreshes the remaining comparison against the new Source")
  before_edits <- length(readRDS(edit_marker))
  js('document.querySelector(".source-skip").click();')
  wait_js('document.getElementById("source-review-card")===null && !document.getElementById("send").disabled')
  check(length(readRDS(edit_marker))==before_edits,"Skip dismisses the remaining proposal without modifying Source")
  js('document.getElementById("prompt").value="Draft kept while viewing code history";window.historyMessageCount=document.querySelectorAll(".msg").length;openAction("edit_history");')
  wait_js('document.querySelector(".edit-history-panel")!==null')
  wait_js('getComputedStyle(document.getElementById("edit_history_page")).display!=="none"')
  check(isTRUE(js('document.querySelectorAll(".edit-log-view").length===2 && document.querySelector(".edit-history-panel").textContent.includes("applied")')),
    "Edit history shows each applied replacement and its actual code comparison")
  check(isTRUE(js('document.querySelector("#chat_stream .edit-history-panel")===null && getComputedStyle(document.getElementById("chat_stream")).display==="none" && getComputedStyle(document.querySelector(".compose")).display==="none" && document.querySelectorAll(".msg").length===historyMessageCount')),
    "Code edit history opens a separate view and keeps history out of the chat stream")
  browser$Emulation$setDeviceMetricsOverride(width=380L,height=700L,deviceScaleFactor=1,mobile=FALSE)
  check(isTRUE(js('document.querySelector(".edit-page-actions").getBoundingClientRect().right<=innerWidth && document.getElementById("edit_history_page").getBoundingClientRect().bottom<=innerHeight')),
    "the history page and its Undo, Redo and Back controls fit a narrow Viewer")
  js('document.querySelector("#edit_history_page .undo-preview").click();')
  wait_js('document.querySelector(".source-undo")!==null')
  before_edits <- length(readRDS(edit_marker))
  check(isTRUE(js('document.querySelector(".undo-comparison").textContent.includes("Undo preview")')),
    "Undo previews its comparison on the history page before any editor write")
  js('document.querySelector(".source-undo").click();')
  wait_js('document.querySelector(".source-undo")===null && !document.getElementById("send").disabled')
  check(length(readRDS(edit_marker))==before_edits+1L && startsWith(tail(readRDS(edit_marker),1L)[[1L]]$text,"library(tidyverse)") &&
    endsWith(tail(readRDS(edit_marker),1L)[[1L]]$text,"dim(X)\n"),"confirming Undo restores only the last AI change and logs the restoration")
  check(isTRUE(js('!document.querySelector("#edit_history_page .redo-preview").disabled')),
    "an approved Undo immediately enables the Redo icon")
  js('document.querySelector("#edit_history_page .redo-preview").click();')
  wait_js('document.querySelector(".source-redo")!==null')
  check(isTRUE(js('document.querySelector(".undo-comparison").textContent.includes("Redo preview") && document.querySelector("#chat_stream .source-redo")===null')),
    "Redo previews the undone change on the separate history page")
  screenshot<-Sys.getenv("RA_BROWSER_EDIT_HISTORY_SCREENSHOT")
  if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
  before_edits <- length(readRDS(edit_marker))
  js('document.querySelector(".source-redo").click();')
  wait_js('document.querySelector(".source-redo")===null && !document.getElementById("send").disabled')
  check(length(readRDS(edit_marker))==before_edits+1L && startsWith(tail(readRDS(edit_marker),1L)[[1L]]$text,"suppressPackageStartupMessages"),
    "Confirm Redo reapplies the exact last undone code once")
  check(isTRUE(js('document.querySelector("#edit_history_page .redo-preview").disabled && !document.querySelector("#edit_history_page .undo-preview").disabled')),
    "Redo becomes disabled again when there is nothing left to redo")
  js('document.querySelector(".edit-history-close").click();')
  wait_js('getComputedStyle(document.getElementById("chat_stream")).display!=="none" && getComputedStyle(document.querySelector(".compose")).display!=="none"')
  check(isTRUE(js('document.getElementById("prompt").value==="Draft kept while viewing code history" && document.querySelectorAll(".msg").length===historyMessageCount')),
    "Back to chat restores the existing conversation and unchanged draft")
  browser$Emulation$clearDeviceMetricsOverride()
  js('document.querySelector("#workspace_menu>summary").click();')
  wait_js('document.querySelector("#workspace_datasets_selected input[value=extra_data]")!==null')
  js('document.querySelector("#workspace_datasets_selected input[value=extra_data]").click();document.querySelector("#workspace_menu>summary").click();')
  js('document.getElementById("prompt").value="Workspace evidence please";document.getElementById("send").click();')
  wait_js('!document.getElementById("send").disabled && document.querySelector("#messages").textContent.includes("Workspace evidence received: 3 x 4")')
  check(isTRUE(js('document.querySelector("#messages").textContent.includes("Dataset cell evidence received: 5432.")')),
    "a manually selected Global Environment dataset supplies real cell values to chat")
  check(isTRUE(js('document.getElementById("workspace_context").checked')),"Workspace starts on and supplies metadata without opening More actions")
  js('document.getElementById("workspace_context").click();')
  wait_js('document.getElementById("workspace_context_state").textContent==="Off"')
  check(isTRUE(js('!document.getElementById("workspace_context").checked && !document.getElementById("app_menu").open')),"Workspace can be switched off directly in the header")
  js('document.getElementById("prompt").value="BLANK_TEST";document.getElementById("send").click();')
  wait_js('!document.getElementById("send").disabled && document.querySelector("#messages").textContent.includes("Only spacing proposed")')
  check(isTRUE(js('document.getElementById("source-review-card")===null')),"extra blank lines do not automatically appear as code errors")
  js('var reviews=document.querySelectorAll(".source-review");reviews[reviews.length-1].click();')
  wait_js('document.querySelector(".formatting-changes")!==null')
  check(isTRUE(js('document.querySelector(".formatting-changes").textContent.includes("not code errors") && document.querySelector("#source-review-card .source-apply")===null')),
    "manual Review separates spacing changes and requires explicit inclusion")
  js('document.getElementById("cancel_source_edit").click();document.getElementById("prompt").value="SWITCH_TO_RMD";document.getElementById("send").click();')
  wait_js('!document.getElementById("send").disabled && document.querySelector("#messages").textContent.includes("Rmd fixture opened")')
  saveRDS(list(mode="cursor",row=6L),source_marker)
  wait_js('document.querySelector(".code-target-label")!==null && document.querySelector(".code-target-label").textContent.includes("R chunk first")')
  check(isTRUE(js('document.querySelector(".code-target-label").textContent.includes("line 6")')),"the same toolbar automatically detects the current R chunk when nothing is selected")
  js('document.getElementById("smart_fix").click();')
  wait_js('document.querySelector(".source-apply")!==null && !document.querySelector(".source-apply").disabled')
  check(isTRUE(js('Array.from(document.querySelectorAll(".source-read")).pop().textContent.includes("chunk first") && document.querySelector("#source-review-card .edit-check").textContent.includes("R chunk first syntax checked")')),
    "Current R chunk Fix checks only the identified chunk and previews an inline correction")
  js('document.querySelector(".source-apply").click();')
  wait_js('document.getElementById("source-review-card")===null && !document.getElementById("send").disabled')
  actual_chunk <- tail(readRDS(edit_marker),1L)[[1L]]$text
  check(startsWith(actual_chunk,"---\ntitle: Keep\n---\nKeep this prose.\n```{r first}\nx <- dim(X)\n```") &&
    endsWith(actual_chunk,"More prose.\n~~~{r other}\ny <- 1\n~~~"),"chunk approval preserves document YAML, fences, prose and the other chunk")
  js('openAction("close");')
  worker$wait(timeout=10000)
  check(identical(worker$get_exit_status(),0L),"standalone Close restores the upload option")
  cat(checks,"browser checks passed\n")
}
main()
