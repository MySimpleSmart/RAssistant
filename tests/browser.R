# Browser integration checks against the existing UI/server with a fake API.
# Requires the optional chromote package and a local Chrome installation.
# Run from the project directory: Rscript tests/browser.R
main <- function() {
  stopifnot(requireNamespace("chromote",quietly=TRUE))
  root <- normalizePath(".")
  port <- if(identical(Sys.getenv("RA_BROWSER_VIEWER_ONLY"),"true"))httpuv::randomPort(20000L,29999L)else httpuv::randomPort()
  update_marker <- tempfile()
  on.exit(unlink(update_marker),add=TRUE)
  available_marker <- tempfile()
  on.exit(unlink(available_marker),add=TRUE)
  reopen_marker <- tempfile();reopen_url <- tempfile()
  on.exit(unlink(c(reopen_marker,reopen_url)),add=TRUE)
  edit_marker <- tempfile()
  on.exit(unlink(edit_marker),add=TRUE)
  on.exit(unlink(paste0(edit_marker,"-scoped-run")),add=TRUE)
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
    patch_source <- c("---","title: Keep","---","# Results","```{r analysis}","#| echo: true",
      "x <- bad(X)","keep <- 99","# Keep this comment","y <- bad(Y)","print(x)","```",
      "Keep this paragraph.","```{r other}",'stop("Untouched chunk")',"```")
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
      diagnostics=function(redact){
        fixture <- if(file.exists(source_marker))readRDS(source_marker)else list()
        fixture$diagnostics
      },
      status=function()list(enabled=console_enabled,ready=TRUE,pending=FALSE,notice="",lines=if(console_enabled)1L else 0L))
    api <- NULL
    if(identical(Sys.getenv("RA_BROWSER_API_ONLY"),"true")) {
      for(file in c("R/credentials.R","R/providers.R"))sys.source(file,store_env)
      Sys.unsetenv(store_env$.api_key_envvars())
      keys <- new.env(parent=emptyenv())
      store_env$.credential_backend <- function()list(name="macos",
        list=function(service=NULL) {
    services <- ls(keys,all.names=TRUE)
    if(!is.null(service))services <- services[services==service]
    data.frame(service=services,username=rep("default",length(services)))
  },
        get=function(service,username)get(service,keys,inherits=FALSE),
        set_with_value=function(service,username,password)assign(service,password,keys))
      store_env$.provider_preferences_path <- function()file.path(history_root,"api-settings.rds")
      store_env$.api_models_factory <- function(name)function(credentials,base_url=NULL) {
        stopifnot(grepl("browser-key$",credentials()))
        if(grepl("catalog-failure",credentials(),fixed=TRUE))stop(paste("Mock discovery failure",credentials()))
        data.frame(id=c("catalog-model-alpha","catalog-model-beta",paste0(sub("-browser-key$","",credentials()),"-only-model")))
      }
      api <- store_env$.new_api_settings(store_env$.api_profile())
    }
    app <- load_assistant(package_info=if(standalone)NULL else list(version="0.1.1",history=history,knowledge=knowledge,
      api=api,
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
    if(!standalone) {
      app$package_info$viewer_ready <- function(token) {
        result <- launch_env$.viewer_ready(token)
        if(identical(Sys.getenv("RA_BROWSER_VIEWER_ONLY"),"true"))cat("Viewer acknowledgement:",token,"accepted:",result,
          "expected:",launch_env$.app_state$viewer_request$token,"running:",launch_env$.app_running(),"time:",as.numeric(Sys.time()),"\n")
        result
      }
      app$package_info$viewer_disconnected <- function(token)launch_env$.viewer_disconnected(token)
      app$package_info$viewer_loading <- function(token)launch_env$.viewer_loading(token)
      app$package_info$viewer_closed <- function()launch_env$.cancel_viewer_open()
    }
    app$initial <- list(id="fixture",path=file.path(tempdir(),"example.R"),contents="plot(1:3)")
    if(identical(Sys.getenv("RA_BROWSER_POST_EDIT_ONLY"),"true")) {
      app$initial <- list(id="fixture",path=file.path(tempdir(),"scoped.qmd"),contents=c(
        "# Results","```{r changed}","x <- bad(X)","print(x)","```","```{r other}",'stop("UNTOUCHED")',"```"))
      app$scope_workspace_inputs <- function(code,definition_only=FALSE,copy=FALSE,...)
        list(names="X",missing=character(),bindings=if(copy)list(X=matrix(1:6,2,3))else list(),packages=character())
      app$validate_script <- function(code,...){
        saveRDS(list(code=code,args=list(...)),paste0(edit_marker,"-scoped-run"))
        list(ok=TRUE,logs="SCOPED_BROWSER_OUTPUT: 2 3",plots=character(),plot_dir=NULL)
      }
    }
    if(identical(Sys.getenv("RA_BROWSER_SOURCE_INTENT_ONLY"),"true"))
      app$initial <- list(id="fixture",path=file.path(tempdir(),"test.R"),contents=function_source)
    if(identical(Sys.getenv("RA_BROWSER_SOURCE_PATCH_ONLY"),"true"))
      app$initial <- list(id="fixture",path=file.path(tempdir(),"analysis.qmd"),contents=patch_source)
    if(standalone)app$initial <- list(id="fixture",path=file.path(tempdir(),"v2.R"),contents=c(
      "library(tidyverse)","","# 1. Read data",'birth_data <- read_csv("BIRTH_reduced.csv")',"",
      "# 2. Select four numeric variables","X_df <- birth_data %>%", "  select(matage, gestage, birthwt, head)","",
      "# 3. Convert to a matrix","X <- as.matrix(X_df)","","# 4. Check matrix dimensions","rtrtryydim(X)",""))
    if(identical(Sys.getenv("RA_BROWSER_EDIT_PREVIEW_ONLY"),"true")) {
      app$edit_store <- app$source_edit_store()
      text <- function(i){lines<-c("# PREFIX_START",sprintf("value_%02d <- %d",1:90,1:90),"# SUFFIX_END","")
        lines[[45L]]<-paste0('long_value <- "',strrep("x",1400L),'" # END_LONG_LINE')
        lines[[70L]]<-paste("changed <-",i);paste(lines,collapse="\n")}
      for(i in 1:8){id<-app$edit_store$begin(list(before=text(i-1L),after=text(i),path=app$initial$path,
        name=basename(app$initial$path),document_id=app$initial$id,session=app$source_session_id,
        operation="replace",summary="Preserve the complete script and its formatting."));app$edit_store$finish(id,"applied")}
      app$initial$contents <- strsplit(text(8L),"\n",fixed=TRUE)[[1L]]
    }
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
      if(identical(Sys.getenv("RA_BROWSER_POST_EDIT_ONLY"),"true"))return("```r-edit-3-3\nx <- dim(X)\n```")
      if(identical(Sys.getenv("RA_BROWSER_CONSOLE_SOURCE_ONLY"),"true"))return("Only the failing Source line needs review.")
      if(identical(Sys.getenv("RA_BROWSER_API_ONLY"),"true"))
        return(paste("Selected connection:",api$get()$provider,api$get()$model))
      if(identical(Sys.getenv("RA_BROWSER_HISTORY_ONLY"),"true")) {
        rich <- paste("Response 1","","```r",paste0("# ",strrep("x",1800L)),
          "answer <- 42 # END_CODE_SENTINEL","```","","| x | y |","|---|---|","| 1 | 3 |","| 2 | 5 |","",
          "Math: \\(y = 2x + 1\\).",sep="\n")
        for(i in seq_len(12L))history$save(list(id=history$new_id(),title=paste("Saved conversation",i),
          title_custom=TRUE,created_at="2026-10-26T00:00:00Z",draft="",assignment="",attachments=list(),knowledge_files=list(),
          messages=lapply(seq_len(45L),function(n)list(role=if(n%%2L)"AI"else"You",type="text",sequence=n,
            text=if(n==45L)rich else paste("Historical message",n)))))
        return(rich)
      }
      if(grepl("SIMULATED_FAILURE",prompt,fixed=TRUE))stop("Simulated API failure")
      if(grepl("SIMULATE_SOURCE_LOSS",prompt,fixed=TRUE))source_unavailable <<- TRUE
      if(identical(Sys.getenv("RA_BROWSER_SOURCE_INTENT_ONLY"),"true")) {
        if(grepl("LATEST USER REQUEST:\nFIX_FUNCTION",prompt,fixed=TRUE))return(paste0("Correction:\n\n```r-replace\n",
          paste(sub("x + 1","x + 2",function_source,fixed=TRUE),collapse="\n"),"\n```\n\nUsage:\n\n```r\nf(5)\n```"))
        stopifnot(grepl(paste(function_source,collapse="\n"),prompt,fixed=TRUE))
        return("f adds one to each supplied value.\n\n```r\nf(5)\nf(c(1, 2, 3))\n```")
      }
      if(identical(Sys.getenv("RA_BROWSER_SOURCE_PATCH_ONLY"),"true")) {
        stopifnot(grepl("7 | x <-",prompt,fixed=TRUE),grepl("R chunk analysis: body lines 6-11",prompt,fixed=TRUE),
          grepl("Dataset: extra_data",prompt,fixed=TRUE),grepl("5432",prompt,fixed=TRUE))
        return("Correct the undefined calls on lines 7 and 10.\n\n```r-edit-7-7\nx <- dim(X)\n```\n\n```r-edit-10-10\ny <- dim(Y)\n```")
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
    handle <- shiny::startApp(shiny::shinyApp(app$viewer_ui,app$server,onStart=app$on_start),
      host="127.0.0.1",port=port,launch.browser=FALSE,quiet=TRUE)
    on.exit(handle$stop(),add=TRUE)
    launch_env <- new.env(parent=globalenv())
    sys.source("R/launch.R",launch_env)
    launch_env$.in_rstudio <- function()TRUE
    launch_env$.show_viewer <- function(url) {
      if(identical(Sys.getenv("RA_BROWSER_VIEWER_ONLY"),"true"))cat("Viewer opening:",url,"time:",as.numeric(Sys.time()),"\n")
      writeLines(url,reopen_url)
    }
    launch_env$.resolve_api_key <- function()stop("Reopening must not read credentials")
    launch_env$.app_state$handle <- handle
    launch_env$.app_state$last_port <- port
    stopifnot(identical(getOption("shiny.maxRequestSize"),52*1024^2))
    while(identical(handle$status(),"running")){
      later::run_now(.05)
      if(file.exists(reopen_marker)){
        action <- if(identical(Sys.getenv("RA_BROWSER_VIEWER_ONLY"),"true"))readRDS(reopen_marker)else"reopen"
        unlink(reopen_marker)
        if(identical(action,"restart")) {
          handle$stop()
          fresh_port <- launch_env$.new_app_port()
          handle <- shiny::startApp(shiny::shinyApp(app$viewer_ui,app$server,onStart=app$on_start),
            host="127.0.0.1",port=fresh_port,launch.browser=FALSE,quiet=TRUE)
          launch_env$.app_state$handle <- handle
          launch_env$.app_state$last_port <- fresh_port
          launch_env$.open_viewer(handle)
        } else stopifnot(identical(launch_env$launch(),handle))
      }
    }
    stopifnot(identical(getOption("shiny.maxRequestSize"),previous_limit))
    stopifnot(!console_enabled)
  }
  start_worker <- function(port,standalone=FALSE)callr::r_bg(run_app,args=list(root,port,update_marker,history_root,available_marker,reopen_marker,reopen_url,edit_marker,navigation_marker,source_marker,standalone),
    stdout="|",stderr="|",supervise=TRUE)
  worker <- start_worker(port)
  on.exit(worker$kill(),add=TRUE)
  chrome_profile <- tempfile("ra-browser-profile-")
  previous_args <- chromote::get_chrome_args()
  chromote::set_chrome_args(c(previous_args,paste0("--user-data-dir=",chrome_profile)))
  on.exit({chromote::set_chrome_args(previous_args);unlink(chrome_profile,recursive=TRUE)},add=TRUE)
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
      value <- tryCatch(js(expression),error=function(e) {
        if(grepl("Inspected target navigated or closed|Cannot find context",conditionMessage(e)))FALSE else stop(e)
      })
      if(isTRUE(value))return(invisible(TRUE))
      if(Sys.time()>deadline) {
        cat("Page tail:\n",tail(strsplit(js('document.body && document.body.innerText'),"\n")[[1]],40),sep="\n")
        cat("\nServer:\n",paste(worker$read_error_lines(),collapse="\n"),"\n")
        cat("\nWorker:\n",worker$is_alive(),worker$get_exit_status(),"\n",
          paste(worker$read_output_lines(),collapse="\n"),"\n")
        cat("\nMessage types:\n",js('JSON.stringify((window.receivedMessages||[]).map(function(x){return Object.keys(x)}))'),"\n")
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
  if(identical(Sys.getenv("RA_BROWSER_VIEWER_ONLY"),"true"))page_setup <- paste0(page_setup,
    'window.addEventListener("message",function(event){if(event.data==="disconnected" && window.location.href.indexOf(event.origin)===0)window.location.replace("about:blank");},true);')
  browser$Page$addScriptToEvaluateOnNewDocument(source=page_setup)
  browser$Page$navigate(url)
  wait_js('!!document.getElementById("send") && !document.getElementById("send").disabled',timeout=30)
  wait_js('document.querySelector(".empty")!==null')
  js('window.alerts=[];window.alert=function(x){alerts.push(x);};window.receivedMessages=[];$(document).on("shiny:message",function(e){if(e.message.custom)receivedMessages.push(e.message.custom);});')
  check(isTRUE(js('document.querySelector(".empty")!==null')), "existing Shiny UI connects and renders")
  if(identical(Sys.getenv("RA_BROWSER_POST_EDIT_ONLY"),"true")) {
    marker <- paste0(edit_marker,"-scoped-run")
    js('var p=document.getElementById("prompt");p.value="Fix the code";p.dispatchEvent(new Event("input",{bubbles:true}));document.getElementById("send").click()')
    wait_js('document.querySelector(".source-apply")!==null && !document.querySelector(".source-apply").disabled')
    js('document.querySelector(".source-apply").click()')
    wait_js('document.querySelector(".post-edit-preview")!==null && !document.querySelector(".post-edit-preview").disabled')
    check(file.exists(edit_marker) && length(readRDS(edit_marker))==1L && !file.exists(marker),
      "Replace offers a scoped check without running anything")
    js('document.querySelector(".post-edit-preview").click()')
    wait_js('document.querySelector(".scoped-run-approval")!==null && !document.querySelector(".scope-run").disabled')
    check(isTRUE(js('var e=document.querySelector(".scoped-run-approval");e.querySelector("details").open && e.textContent.includes("x <- dim(X)") && e.textContent.includes("print(x)") && !e.textContent.includes("UNTOUCHED") && e.textContent.includes("Workspace copies: X")')) && !file.exists(marker),
      "inline approval shows the changed chunk and input names without another chunk or execution")
    check(isTRUE(js('var a=getComputedStyle(document.querySelector(".scope-cancel")),b=getComputedStyle(document.getElementById("new_chat"));["borderRadius","backgroundColor","borderColor","paddingTop","paddingLeft"].every(function(k){return a[k]===b[k]})')),
      "Cancel follows the existing button design system")
    js('window.oldScopedToken=document.querySelector(".scope-run").dataset.token;document.querySelector(".scope-cancel").click()')
    wait_js('document.querySelector(".scope-run")===null && !document.getElementById("send").disabled')
    check(!file.exists(marker),"Cancel restores chat without execution")
    js('document.querySelector(".post-edit-preview").click()')
    wait_js('document.querySelector(".scope-run")!==null && !document.querySelector(".scope-run").disabled')
    js('Shiny.setInputValue("run_post_edit_check",{token:oldScopedToken,nonce:5000},{priority:"event"})')
    wait_js('!document.querySelector(".scope-run").disabled')
    check(!file.exists(marker),"a cancelled browser approval cannot approve a fresh preview")
    changed <- c("# Results","```{r changed}","x <- dim(X)","print(x)","```","```{r other}","manual <- 42","```")
    saveRDS(list(contents=changed),source_marker)
    js('document.querySelector(".scope-run").click()')
    wait_js('document.querySelector(".scope-run")===null && document.querySelector("#messages").textContent.includes("Not run: Source changed")')
    check(!file.exists(marker),"the browser refuses execution after a manual Source edit")
    unlink(source_marker)
    js('document.querySelector(".post-edit-preview").click()')
    wait_js('document.querySelector(".scope-run")!==null && !document.querySelector(".scope-run").disabled')
    browser$Emulation$setDeviceMetricsOverride(width=380,height=780,deviceScaleFactor=1,mobile=FALSE)
    check(isTRUE(js('var e=document.querySelector(".scoped-run-approval");e.getBoundingClientRect().right<=innerWidth+1 && e.querySelector("pre").scrollWidth<=e.querySelector("pre").clientWidth+1')),
      "the approval and code preview fit a narrow Viewer")
    screenshot <- Sys.getenv("RA_BROWSER_POST_EDIT_SCREENSHOT")
    if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
    js('document.querySelector(".scope-run").click()')
    wait_js('document.querySelector(".scope-run")===null && !document.getElementById("send").disabled && document.querySelector("#messages").textContent.includes("Scoped check completed")')
    run <- readRDS(marker)
    check(identical(run$code,"x <- dim(X)\nprint(x)\n") && isTRUE(run$args$scoped) && length(readRDS(edit_marker))==1L,
      "Run changed code submits only the approved chunk and performs no further Source writes")
    check(isTRUE(js('document.querySelector("#messages").textContent.includes("SCOPED_BROWSER_OUTPUT") && window.aisourceErrors.length===0')),
      "scoped results appear in chat with no browser errors")
    cat(checks,"Post-edit browser checks passed\n");return(invisible(NULL))
  }
  if(identical(Sys.getenv("RA_BROWSER_CONSOLE_SOURCE_ONLY"),"true")) {
    path <- file.path(tempdir(),"console-analysis.qmd")
    contents <- c("# Results","```{r analysis}","x <- missing_fn(3)","print(x)","```","Keep this paragraph.")
    failed <- paste("Error: could not find function missing_fn","[Console error 1]",
      "[End failed Console task; full command may be unavailable]",sep="\n")
    metadata <- list(marker="[Console error 1]",calls="missing_fn(3)",locations=list())
    fixture <- list(path=path,contents=contents,console=failed,diagnostics=metadata)
    submit <- function(text) {
      old <- js('document.querySelectorAll("#messages .msg.user").length')
      js(paste0('var p=document.getElementById("prompt");p.value=',jsonlite::toJSON(text,auto_unbox=TRUE),
        ';p.dispatchEvent(new Event("input",{bubbles:true}));document.getElementById("send").click();'))
      wait_js(paste0('!document.getElementById("send").disabled && document.querySelectorAll("#messages .msg.user").length===',old+1L))
    }
    saveRDS(fixture,source_marker)
    submit("Explain this Console error")
    wait_js('document.querySelector(".console-source-jump")!==null')
    check(isTRUE(js('var e=document.querySelector(".console-error-location");e.textContent.includes("Matching Source") && e.textContent.includes("console-analysis.qmd:3") && e.textContent.includes("analysis")')),
      "a Console error displays its matching global Source row and chunk, with unverified origin labelled")
    check(isTRUE(js('var b=document.querySelector(".console-source-jump"),s=getComputedStyle(b),base=getComputedStyle(document.getElementById("new_chat"));b.classList.contains("btn") && b.classList.contains("ai-btn") && s.backgroundColor===base.backgroundColor && s.borderRadius===base.borderRadius && s.borderColor===base.borderColor')),
      "Console navigation uses the existing button design system")
    check(isTRUE(js('var e=document.querySelector(".console-read");e.querySelector("summary").textContent.includes("Sent") && e.querySelector("pre").textContent.includes("could not find function missing_fn") && !e.open')),
      "a collapsed Console receipt reports delivery and keeps the exact captured text inspectable")
    js('document.querySelector(".console-read summary").click()')
    check(isTRUE(js('document.querySelector(".console-read").open && getComputedStyle(document.querySelector(".console-excerpt")).overflowY==="auto"')),
      "the Console receipt expands with bounded scrolling in the existing disclosure style")
    js('document.querySelector(".console-read summary").click()')
    js('document.querySelector(".console-source-jump").click()')
    for(i in 1:100){if(file.exists(navigation_marker))break;Sys.sleep(.05)}
    check(file.exists(navigation_marker) && readRDS(navigation_marker)$row==3L && !file.exists(edit_marker),
      "the real browser click focuses the error line without writing Source")
    unlink(navigation_marker)
    fixture$contents <- c("# Inserted",contents);saveRDS(fixture,source_marker)
    js('document.querySelector(".console-source-jump").click()')
    wait_js('document.querySelector(".shiny-notification")!==null && document.querySelector(".shiny-notification").textContent.includes("Source changed")')
    check(!file.exists(navigation_marker) && !file.exists(edit_marker),"a stale browser location refuses to focus a different line")
    fixture$contents <- contents
    fixture$diagnostics$locations <- list(list(path=path,row=3L,end_row=3L,text=contents[[3L]]))
    saveRDS(fixture,source_marker);submit("Read the recorded runtime location")
    check(isTRUE(js('var e=Array.from(document.querySelectorAll(".console-error-location")).pop();!e.textContent.includes("Matching Source") && e.querySelector("button").title.includes("failing R call stack")')),
      "recorded runtime locations are distinct from expression matches")
    fixture$diagnostics$locations <- list()
    fixture$contents <- c(contents,"```{r duplicate}","x <- missing_fn(3)","```")
    saveRDS(fixture,source_marker);submit("Locate this duplicate expression")
    check(isTRUE(js('var e=Array.from(document.querySelectorAll(".console-error-location")).pop();e.textContent.includes("Multiple matching") && e.querySelector("button")===null')),
      "ambiguous code has a clear status and no fabricated navigation link")
    fixture$console <- paste(failed,"[Completed Console command; no text output captured]","> x <- 1","[End Console task]",sep="\n")
    saveRDS(fixture,source_marker);submit("Explain the latest result")
    check(isTRUE(js('Array.from(document.querySelectorAll("#messages .msg.user")).pop().querySelector(".console-error-location")===null')),
      "a newer successful Console task does not acquire an earlier error link")
    fixture$console <- "";saveRDS(fixture,source_marker)
    submit("Explain the Console output")
    check(isTRUE(js('var user=Array.from(document.querySelectorAll("#messages .msg.user")).pop(),answer=Array.from(document.querySelectorAll("#messages .msg.assistant")).pop();user.textContent.includes("Console · No new output") && answer.textContent.includes("Run your code with Console On") && answer.textContent.length<220 && answer.querySelector("table")===null')),
      "an empty capture produces a short actionable response instead of hypothetical Source results")
    browser$Emulation$setDeviceMetricsOverride(width=380,height=780,deviceScaleFactor=1,mobile=FALSE)
    check(isTRUE(js('Array.from(document.querySelectorAll(".console-error-location")).every(function(e){return e.getBoundingClientRect().right<=innerWidth+1})')),
      "error locations fit a narrow Viewer using the existing button style")
    screenshot <- Sys.getenv("RA_BROWSER_CONSOLE_SOURCE_SCREENSHOT")
    if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
    check(isTRUE(js('window.aisourceErrors.length===0')),"Console navigation causes no browser errors")
    cat(checks,"Console Source browser checks passed\n");return(invisible(NULL))
  }
  if(identical(Sys.getenv("RA_BROWSER_API_ONLY"),"true")) {
    wait_js('document.getElementById("api_provider")!==null && document.getElementById("api_key_entry")!==null')
    check(isTRUE(js('document.querySelectorAll("#api_provider option").length===14 && document.getElementById("api_key_entry").type==="password"')),
      "first launch offers all providers with hidden key input and no OpenAI key requirement")
    wait_js('document.querySelector(".api-overview .api-active")!==null')
    check(isTRUE(js('var cancel=document.querySelector("#shiny-modal .modal-footer [data-dismiss=modal]"),base=document.getElementById("new_chat");var a=getComputedStyle(cancel),b=getComputedStyle(base);cancel.classList.contains("ai-btn") && ["fontSize","paddingTop","paddingBottom","paddingLeft","paddingRight","borderRadius","lineHeight"].every(function(k){return a[k]===b[k]}) && document.getElementById("api_save").classList.contains("ai-send")')),
      "Cancel uses the same button component and computed style as the app toolbar")
    check(isTRUE(js('document.querySelector(".api-active").textContent.includes("No active connection") && document.querySelector(".api-key-in-use").textContent.includes("Not configured")')),
      "the overview distinguishes an unconfigured selection from an active connection")
    js('document.querySelector("#shiny-modal .modal-footer [data-dismiss=modal]").click()')
    wait_js('document.getElementById("shiny-modal")===null && document.getElementById("change_model")!==null')
    js('document.getElementById("change_model").click()')
    wait_js('document.getElementById("api_current_connection")!==null')
    check(isTRUE(js('document.getElementById("api_current_connection").textContent==="No active connection"')),
      "the provider picker never claims an unconfigured model is already in use")
    js('document.getElementById("api_more_settings").click()')
    wait_js('document.getElementById("api_key_entry")!==null && document.querySelector(".modal-title").textContent==="API settings"')
    choose <- function(id) {
      js(paste0('var id=',jsonlite::toJSON(id,auto_unbox=TRUE),';Array.from(document.querySelectorAll(".api-provider-choice")).find(function(b){return b.dataset.provider===id;}).click();'))
      wait_js(paste0('document.getElementById("api_form_provider")!==null && document.getElementById("api_form_provider").value===',jsonlite::toJSON(id,auto_unbox=TRUE)))
    }
    fill <- function(id,value) {
      if(id=="api_model")wait_js('document.getElementById("api_model")!==null && !!document.getElementById("api_model").selectize')
      if(id=="api_model") {
        js(paste0('var value=',jsonlite::toJSON(value,auto_unbox=TRUE),';var row=Array.from(document.querySelectorAll(".api-model-choice")).find(function(b){return b.dataset.model===value;});if(row){row.click();}else{document.querySelector(".api-manual-model").open=true;var field=document.getElementById("api_manual_model");field.value=value;field.dispatchEvent(new Event("input",{bubbles:true}));field.dispatchEvent(new Event("change",{bubbles:true}));document.querySelector(".api-manual-use").click();}'))
      }else js(paste0('var field=document.getElementById(',jsonlite::toJSON(id,auto_unbox=TRUE),');field.value=',jsonlite::toJSON(value,auto_unbox=TRUE),';field.dispatchEvent(new Event("input",{bubbles:true}));field.dispatchEvent(new Event("change",{bubbles:true}));'))
      input_id <- if(id=="api_key_entry")paste0(id,":shiny.password")else id
      wait_js(paste0('Shiny.shinyapp.$inputValues[',jsonlite::toJSON(input_id,auto_unbox=TRUE),']===',jsonlite::toJSON(value,auto_unbox=TRUE)))
    }
    choose("gemini")
    wait_js('!!document.getElementById("api_model").selectize')
    check(isTRUE(js('document.getElementById("api_model").value==="" && Object.keys(document.getElementById("api_model").selectize.options).length===0')),
      "an unconfigured provider does not present an invented available model")
    fill("api_key_entry","gemini-browser-key")
    wait_js('document.getElementById("api_model").selectize.options["gemini-only-model"]!==undefined && !document.getElementById("api_load_models").disabled')
    check(TRUE,"entering a key automatically loads its provider-reported models without pressing Refresh")
    js('var filter=document.getElementById("api_model_filter");filter.value="gemini-only";filter.dispatchEvent(new Event("input",{bubbles:true}));')
    check(isTRUE(js('document.querySelectorAll(".api-model-choice:not([hidden])").length===1 && document.querySelector(".api-model-choice:not([hidden])").textContent.includes("gemini-only-model")')),
      "the model button list filters locally without making another API request")
    js('document.querySelector(".api-model-choice:not([hidden])").click();var filter=document.getElementById("api_model_filter");filter.value="";filter.dispatchEvent(new Event("input",{bubbles:true}));')
    wait_js('Shiny.shinyapp.$inputValues.api_model==="gemini-only-model"')
    check(isTRUE(js('document.querySelector(".api-model-choice[aria-pressed=true]").dataset.model==="gemini-only-model" && document.querySelector(".api-model-choice[aria-pressed=true]").classList.contains("ai-send")')),
      "clicking a model row selects it with the same pressed and highlight state as Conversation")
    check(isTRUE(js('var width=document.querySelector(".api-settings").getBoundingClientRect().width;["api_provider_buttons","api_model_buttons","api_key_entry"].every(function(id){return Math.abs(document.getElementById(id).getBoundingClientRect().width-width)<2;})')),
      "Provider, Model and API key controls fill the settings page width")
    fill("api_model","gemini-test")
    js('document.getElementById("api_save").click()')
    wait_js('document.getElementById("shiny-modal")===null')
    check(TRUE,"Gemini saves and activates directly from the first-launch form")
    check(isTRUE(js('getComputedStyle(document.querySelector("#send .send-plane")).display!=="none" && getComputedStyle(document.querySelector("#send .send-spinner")).display==="none"')),
      "Send shows its send icon while idle")
    check(isTRUE(js('var icon=document.querySelector("#send .send-icon").getBoundingClientRect(),label=document.querySelector("#send .send-label").getBoundingClientRect(),plane=document.querySelector("#send .send-plane").getBBox();Math.abs((icon.top+icon.bottom-label.top-label.bottom)/2)<0.5 && Math.abs(plane.y+plane.height/2-12)<0.1')),
      "the send icon is vertically centered with its text and its drawing is balanced around the middle")
    fill("prompt","Keep this draft while switching providers")
    js('openAction("api_settings")')
    wait_js('document.getElementById("api_key_entry")!==null && document.getElementById("api_model").value==="gemini-test"')
    check(isTRUE(js('document.getElementById("api_key_entry").value==="" && document.getElementById("api_model").value==="gemini-test" && document.getElementById("api_provider").value==="gemini"')),
      "reopening settings remembers the model without returning the saved key to the browser")
    check(isTRUE(js('var row=document.querySelector(".api-connections tr[data-provider=gemini]");row.dataset.active==="true" && row.children[1].textContent.includes("gemini-test") && row.querySelector(".api-connection-state").dataset.state==="using" && row.querySelector(".api-connection-state").textContent==="Using now" && row.querySelector(".api-key-source").textContent==="Saved on this computer" && document.querySelector(".api-key-in-use").textContent.includes("Keychain (loaded)")')),
      "the overview clearly identifies the active model, saved key and actual loaded key source")
    choose("custom");fill("api_model","custom-test");fill("api_base_url","https://mock.example/v1")
    js('document.getElementById("api_save").click()')
    wait_js('document.getElementById("api_notice").textContent.includes("Enter an API key")')
    check(isTRUE(js('document.getElementById("shiny-modal").textContent.includes("Google Gemini") && document.getElementById("prompt").value==="Keep this draft while switching providers"')),
      "missing Custom credentials preserve the active Gemini connection and draft")
    fill("api_key_entry","custom-browser-key");js('document.getElementById("api_save").click()')
    wait_js('document.getElementById("shiny-modal")===null')
    js('document.getElementById("send").click()')
    wait_js('document.querySelector(".msg.assistant")!==null && !document.getElementById("send").disabled')
    check(isTRUE(js('document.querySelector(".msg.assistant").textContent.includes("Selected connection: custom custom-test")')),
      "the next Send uses the newly selected provider and model")
    js('openAction("api_settings")')
    wait_js('document.getElementById("api_provider")!==null');choose("gemini")
    wait_js('document.getElementById("api_model").value==="gemini-test"')
    js('document.getElementById("api_save").click()')
    wait_js('document.getElementById("shiny-modal")===null')
    fill("prompt","Second provider request");js('document.getElementById("send").click()')
    wait_js('document.querySelectorAll(".msg.assistant").length===2 && !document.getElementById("send").disabled')
    check(isTRUE(js('Array.from(document.querySelectorAll(".msg.assistant")).pop().textContent.includes("Selected connection: gemini gemini-test") && document.querySelectorAll(".msg.user").length===2')),
      "switching back reuses Gemini's saved key and retains the previous chat")
    check(isTRUE(js('!document.getElementById("messages").textContent.includes("browser-key") && window.aisourceErrors.length===0')),
      "provider settings do not leak keys into chat or cause JavaScript errors")
    js('openAction("api_settings")');wait_js('document.getElementById("api_model")!==null')
    js('document.getElementById("api_load_models").click()')
    wait_js('document.getElementById("api_model").selectize.options["catalog-model-alpha"]!==undefined && !document.getElementById("api_load_models").disabled')
    check(isTRUE(js('document.getElementById("api_model").value==="gemini-test" && document.querySelector(".api-active").textContent.includes("gemini-test")')),
      "loading models adds real choices while preserving the selected model and active connection")
    default_model <- "catalog-model-beta"
    fill("api_model",default_model);js('document.getElementById("api_save").click()')
    wait_js('document.getElementById("shiny-modal")===null')
    js('openAction("api_settings")')
    wait_js('document.querySelector(".api-active code")!==null && document.getElementById("api_model")!==null && document.getElementById("api_model").value!==""')
    check(isTRUE(js('var code=document.querySelector(".api-active code").textContent;code.length>0 && code!=="Default unavailable" && document.getElementById("api_model").value===code')),
      "selecting a provider-reported model uses its exact runtime model ID")
    choose("openai")
    check(isTRUE(js('document.querySelector(".api-active").textContent.includes("Google Gemini") && document.querySelector("tr[data-provider=gemini]").dataset.active==="true"')),
      "editing another provider does not falsely mark it as active before Save and use")
    browser$Emulation$setDeviceMetricsOverride(width=380L,height=700L,deviceScaleFactor=1,mobile=FALSE)
    check(isTRUE(js('var width=document.querySelector(".api-settings").getBoundingClientRect().width;["api_provider_buttons","api_model_buttons","api_key_entry"].every(function(id){return Math.abs(document.getElementById(id).getBoundingClientRect().width-width)<2;})')),
      "settings controls remain full width in a narrow Viewer")
    check(isTRUE(js('["api_provider_buttons","api_model_buttons"].every(function(id){var list=document.getElementById(id);return list.classList.contains("edit-log-list") && getComputedStyle(list).maxHeight==="150px" && getComputedStyle(list).overflowY==="auto";}) && document.getElementById("api_provider").getClientRects().length===0 && document.getElementById("api_model").getClientRects().length===0')),
      "provider and model choices use the same scrollable button-list style as Conversation without visible dropdowns")
    check(isTRUE(js('var table=document.querySelector(".api-connections").getBoundingClientRect(),dialog=document.querySelector(".modal-dialog").getBoundingClientRect();table.right<=innerWidth && table.left>=0 && dialog.right<=innerWidth && dialog.left>=0')),
      "provider, model and key status columns fit a narrow Viewer without horizontal clipping")
    screenshot <- Sys.getenv("RA_BROWSER_API_SCREENSHOT")
    if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
    browser$Emulation$clearDeviceMetricsOverride()
    js('document.querySelector("#shiny-modal .modal-footer [data-dismiss=modal]").click()')
    wait_js('document.getElementById("shiny-modal")===null')
    js('openAction("api_settings")');wait_js('document.getElementById("api_provider")!==null')
    check(isTRUE(js('document.getElementById("api_provider").value==="gemini" && !document.getElementById("shiny-modal").textContent.includes("browser-key")')),
      "Cancel preserves the active provider and key values never enter the overview")
    # An environment-configured OpenAI connection can be used without entering a key.
    # Synthetic fixture credentials only; no inference request is made by Use.
    choose("openai");fill("api_key_entry","openai-browser-key");js('document.getElementById("api_save").click()')
    wait_js('document.getElementById("shiny-modal")===null')
    js('openAction("api_settings")');wait_js('document.querySelector("tr[data-provider=gemini] .api-use-connection")!==null')
    js('document.querySelector("tr[data-provider=gemini] .api-use-connection").click()')
    wait_js('document.getElementById("shiny-modal")===null')
    fill("prompt","Keep this draft for OpenAI")
    js('openAction("api_settings")');wait_js('document.querySelector("tr[data-provider=openai] .api-use-connection")!==null')
    check(isTRUE(js('document.querySelector("tr[data-provider=openai] .api-use-connection").textContent==="Use OpenAI"')),
      "saved providers expose a clearly named one-click Use button")
    check(isTRUE(js('document.querySelector("tr[data-provider=openai] .api-connection-state").textContent==="Key available" && document.querySelector("tr[data-provider=openai] .api-connection-state").dataset.state==="ready" && document.querySelector("tr[data-provider=gemini] .api-connection-state").textContent==="Using now"')),
      "ready providers and the connection currently in use have distinct text and visual states")
    js('document.querySelector("tr[data-provider=openai] .api-use-connection").click()')
    wait_js('document.getElementById("shiny-modal")===null')
    js('openAction("api_settings")');wait_js('document.querySelector("tr[data-provider=openai][data-active=true]")!==null')
    check(isTRUE(js('document.querySelector(".api-active").textContent.includes("OpenAI") && document.getElementById("prompt").value==="Keep this draft for OpenAI" && document.querySelectorAll(".msg.user").length===2')),
      "Use OpenAI switches the actual connection in one click without a repeated key, chat request or lost draft")
    js('document.querySelector("#shiny-modal .modal-footer [data-dismiss=modal]").click()')
    wait_js('document.getElementById("shiny-modal")===null && document.getElementById("change_model")!==null')
    check(isTRUE(js('var button=document.getElementById("change_model");button.title.includes("OpenAI") && button.querySelector(".model-name").textContent===button.title.split(" · ")[1] && button.closest(".send-controls")===document.getElementById("send").parentElement')),
      "the composer shows only the model name and keeps the full provider and model in its tooltip")
    js('document.getElementById("change_model").click()')
    wait_js('document.getElementById("api_model")!==null && document.getElementById("api_provider").value==="openai"')
    check(isTRUE(js('document.querySelector(".modal-title").textContent==="Provider & model" && document.querySelector(".api-overview")===null && document.getElementById("api_key_entry")===null && document.getElementById("api_more_settings")!==null')),
      "the composer opens a compact picker with an API settings link and no repeated key entry")
    check(isTRUE(js('document.getElementById("api_current_connection").textContent.includes("OpenAI") && document.getElementById("api_save").textContent==="Apply" && Array.from(document.getElementById("api_provider").options).find(function(o){return o.value==="gemini";}).textContent.includes("key available") && Array.from(document.getElementById("api_provider").options).find(function(o){return o.value==="xai";}).textContent.includes("add key")')),
      "the picker explicitly supports provider changes, shows the current connection and marks available keys")
    wait_js('document.getElementById("api_model").selectize.options["openai-only-model"]!==undefined && !document.getElementById("api_load_models").disabled')
    check(isTRUE(js('var p=document.getElementById("api_model").selectize;p.options["openai-only-model"].group==="reported" && p.options["gemini-only-model"]===undefined && document.getElementById("api_model_notice").textContent.includes("models returned")')),
      "opening the composer picker automatically shows only the selected provider's freshly reported catalog")
    check(isTRUE(js('document.getElementById("api_model").selectize.options[document.getElementById("api_model").value].label.includes("not in provider list")')),
      "a configured model absent from the catalog is marked separately from available models")

    fill("api_model","catalog-model-alpha");js('document.getElementById("api_save").click()')
    wait_js('document.getElementById("shiny-modal")===null && document.getElementById("change_model").textContent.includes("catalog-model-alpha")')
    check(isTRUE(js('document.getElementById("prompt").value==="Keep this draft for OpenAI" && document.querySelectorAll(".msg.user").length===2')),
      "choosing a composer model updates its label without a request or lost draft")
    js('window.idleSendWidth=document.getElementById("send").getBoundingClientRect().width;document.getElementById("send").click();window.modelLockedDuringSend=document.getElementById("change_model").disabled;window.sendIconWasLoading=getComputedStyle(document.querySelector("#send .send-plane")).display==="none" && getComputedStyle(document.querySelector("#send .send-spinner")).display!=="none" && getComputedStyle(document.querySelector("#send .send-spinner")).animationName==="send-spin";window.loadingSendWidth=document.getElementById("send").getBoundingClientRect().width;')
    wait_js('document.querySelectorAll(".msg.assistant").length===3 && !document.getElementById("send").disabled')
    check(isTRUE(js('modelLockedDuringSend && Array.from(document.querySelectorAll(".msg.assistant")).pop().textContent.includes("Selected connection: openai catalog-model-alpha")')),
      "Send uses the composer-selected model and prevents switching during the request")
    check(isTRUE(js('sendIconWasLoading && Math.abs(idleSendWidth-loadingSendWidth)<1 && getComputedStyle(document.querySelector("#send .send-plane")).display!=="none" && getComputedStyle(document.querySelector("#send .send-spinner")).display==="none" && document.getElementById("send").getAttribute("aria-busy")==="false"')),
      "the send icon becomes a spinner during loading and returns without changing button width")
    fill("prompt","Draft kept while selecting Gemini")
    js('document.getElementById("change_model").click()');wait_js('document.getElementById("api_model")!==null')
    choose("gemini")
    wait_js('document.getElementById("api_model").selectize.options["gemini-only-model"]!==undefined && !document.getElementById("api_load_models").disabled')
    check(isTRUE(js('document.getElementById("api_model").selectize.options["openai-only-model"]===undefined')),
      "changing providers refreshes availability automatically without keeping another provider's models")
    js('document.getElementById("api_save").click()')
    wait_js('document.getElementById("shiny-modal")===null && document.getElementById("change_model").title.includes("Google Gemini")')
    js('document.getElementById("change_model").click()');wait_js('document.getElementById("api_model")!==null')
    choose("xai");fill("api_model","missing-key-model");js('document.getElementById("api_save").click()')
    wait_js('document.getElementById("api_notice").textContent.includes("Open API settings")')
    check(isTRUE(js('document.getElementById("change_model").title.includes("Google Gemini") && document.getElementById("prompt").value==="Draft kept while selecting Gemini"')),
      "a missing provider key preserves the active model and explains how to add its key")
    js('document.getElementById("api_more_settings").click()')
    wait_js('document.querySelector(".modal-title").textContent==="API settings" && document.getElementById("api_key_entry")!==null')
    check(isTRUE(js('document.getElementById("api_provider").value==="xai" && document.getElementById("api_model").value==="missing-key-model"')),
      "adding a provider key keeps the selected provider and model in full API settings")
    fill("api_key_entry","catalog-failure-browser-key")
    wait_js('document.getElementById("api_model_notice").textContent.includes("Could not load available models") && !document.getElementById("api_load_models").disabled')
    check(isTRUE(js('Object.values(document.getElementById("api_model").selectize.options).every(function(x){return x.group!=="reported";}) && !document.getElementById("api_model_notice").textContent.includes("catalog-failure-browser-key") && document.getElementById("change_model").title.includes("Google Gemini")')),
      "failed discovery removes earlier available options, redacts the key and leaves the active connection unchanged")
    js('document.querySelector("#shiny-modal .modal-footer [data-dismiss=modal]").click()')
    wait_js('document.getElementById("shiny-modal")===null')
    js('document.getElementById("change_model").click()');wait_js('document.getElementById("api_model")!==null')
    long_model <- paste0("model-",strrep("x",190L));fill("api_model",long_model)
    js('document.getElementById("api_save").click()')
    wait_js('document.getElementById("shiny-modal")===null && document.getElementById("change_model").textContent.includes("model-xxxxx")')
    browser$Emulation$setDeviceMetricsOverride(width=380L,height=700L,deviceScaleFactor=1,mobile=FALSE)
    check(isTRUE(js('var model=document.getElementById("change_model").getBoundingClientRect(),send=document.getElementById("send").getBoundingClientRect();model.left>=0 && model.width<=110 && model.height<=send.height+1 && send.right<=innerWidth && model.right<=send.left && Math.abs((model.top+model.bottom)/2-(send.top+send.bottom)/2)<2 && document.documentElement.scrollWidth<=innerWidth && getComputedStyle(document.querySelector("#change_model .model-name")).textOverflow==="ellipsis"')),
      "a long model ID uses one compact line beside Send, with an ellipsis instead of extra rows")
    check(isTRUE(js('document.getElementById("change_model").title.includes("Google Gemini") && document.getElementById("change_model").title.length>190 && Math.abs(document.getElementById("image_picker").getBoundingClientRect().top-document.getElementById("send").getBoundingClientRect().top)<2')),
      "the complete connection remains accessible while Image, Plot, undo, redo and Send fit one row")
    screenshot <- Sys.getenv("RA_BROWSER_COMPOSER_SCREENSHOT")
    if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
    browser$Emulation$clearDeviceMetricsOverride()
    check(isTRUE(js('!document.getElementById("change_model").textContent.includes("browser-key") && window.aisourceErrors.length===0')),
      "the composer never exposes API keys and model switching has no JavaScript errors")
    cat(checks,"API settings browser checks passed\n");return(invisible(NULL))
  }
  check(isTRUE(js('!document.getElementById("app_menu").open && document.querySelector("#app_menu #history")!==null && document.querySelector("#app_menu #knowledge")!==null && document.getElementById("knowledge").textContent==="Knowledge Base" && document.querySelector("#app_menu #help")!==null && document.querySelector("#app_menu #close")!==null && document.getElementById("check_updates").closest("#app_menu")!==null && document.getElementById("console_context").closest(".toolbar-actions")!==null && document.querySelector(".upload-row #console_context")===null && document.getElementById("workspace_context").closest(".toolbar-actions")!==null && document.querySelector("#app_menu #workspace_context")===null && document.getElementById("workspace_context").checked && document.getElementById("workspace_context_state").textContent==="On"')),
    "Check updates lives in More actions while Console and default-on Workspace share the header")
  if(!identical(Sys.getenv("RA_BROWSER_EDIT_PREVIEW_ONLY"),"true"))
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
  if(identical(Sys.getenv("RA_BROWSER_EDIT_PREVIEW_ONLY"),"true")) {
    js('document.getElementById("prompt").value="Draft kept while reading code history";openAction("edit_history");')
    wait_js('document.querySelectorAll(".edit-log-view").length===8 && document.querySelector(".edit-history-preview pre")!==null')
    check(isTRUE(js('var text=document.querySelector(".edit-history-preview pre").textContent;text.includes("PREFIX_START") && text.includes("SUFFIX_END") && text.includes("END_LONG_LINE")')),
      "code history includes the entire before/after script instead of only nearby diff context")
    browser$Emulation$setDeviceMetricsOverride(width=380,height=700,deviceScaleFactor=1,mobile=FALSE)
    check(isTRUE(js('var pre=document.querySelector(".edit-history-preview pre");pre.scrollWidth>pre.clientWidth && pre.scrollHeight>pre.clientHeight && document.documentElement.scrollWidth<=innerWidth && pre.getBoundingClientRect().right<=innerWidth')),
      "long and tall code stays inside a narrow preview with both scroll directions available")
    check(isTRUE(js('var row=Array.from(document.querySelectorAll(".edit-history-preview .diff-row")).find(function(x){return x.textContent.includes("END_LONG_LINE");});row.getBoundingClientRect().height<25 && getComputedStyle(document.querySelector(".diff-canvas")).whiteSpace==="pre"')),
      "long code lines preserve indentation and stay on one line without broken wrapping")
    js('var pre=document.querySelector(".edit-history-preview pre");pre.scrollLeft=pre.scrollWidth;pre.scrollTop=pre.scrollHeight;')
    check(isTRUE(js('var pre=document.querySelector(".edit-history-preview pre");pre.scrollLeft>0 && pre.scrollTop>0 && pre.textContent.includes("SUFFIX_END")')),
      "the right edge and end of the complete preview remain reachable by scrolling")
    js('var list=document.querySelector("#edit_history_page .edit-log-list");list.scrollTop=list.scrollHeight;window.listTop=list.scrollTop;list.querySelector(".edit-log-view:last-child").click();')
    wait_js('document.querySelector(".edit-log-view:last-child").getAttribute("aria-pressed")==="true"')
    check(isTRUE(js('Math.abs(document.querySelector("#edit_history_page .edit-log-list").scrollTop-listTop)<2 && document.querySelectorAll(".edit-log-view[aria-pressed=true]").length===1 && document.querySelector(".edit-history-preview").textContent.includes("changed <- 1")')),
      "selecting another edit updates the complete preview and keeps its list position")
    js('var pre=document.querySelector(".edit-history-preview pre");pre.scrollLeft=0;pre.scrollTop=pre.scrollHeight;document.getElementById("edit_history_page").scrollTop=document.getElementById("edit_history_page").scrollHeight;')
    screenshot <- Sys.getenv("RA_BROWSER_EDIT_HISTORY_SCREENSHOT")
    if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
    check(!file.exists(edit_marker) && isTRUE(js('document.querySelectorAll("#messages .msg").length===0')),
      "reading and scrolling edit previews does not call the model, write Source or insert chat messages")
    js('document.querySelector(".edit-history-close").click();')
    wait_js('getComputedStyle(document.getElementById("chat_stream")).display!=="none"')
    check(isTRUE(js('document.getElementById("prompt").value==="Draft kept while reading code history" && window.aisourceErrors.length===0')),
      "Back to chat preserves the draft without JavaScript errors")
    cat(checks,"complete code history preview browser checks passed\n");return(invisible(NULL))
  }
  if(identical(Sys.getenv("RA_BROWSER_HISTORY_ONLY"),"true")) {
    js('var p=document.getElementById("prompt");p.value="History rendering test";document.getElementById("send").click();')
    wait_js('!document.getElementById("send").disabled && document.querySelector("#messages .content table")!==null')
    js('window.originalCode=document.querySelector("#messages pre code").textContent;window.originalStyle={};["code","td","th"].forEach(function(k){var style=getComputedStyle(document.querySelector("#messages .content "+k));originalStyle[k]={fontSize:style.fontSize,fontFamily:style.fontFamily,padding:style.padding,border:style.border};});var p=document.getElementById("prompt");p.value="Draft survives history";p.dispatchEvent(new Event("input",{bubbles:true}));openAction("history");')
    wait_js('document.querySelectorAll(".chat-log-view").length===13 && document.querySelector(".chat-history-preview table")!==null')
    check(isTRUE(js('document.querySelector("#chat_history_page select")===null && document.getElementById("chat_history_list").classList.contains("edit-log-list") && getComputedStyle(document.getElementById("chat_history_list")).maxHeight==="150px" && getComputedStyle(document.getElementById("chat_history_list")).overflowY==="auto"')),
      "Conversation uses the same scroll-list component and height as Code edit history")
    check(isTRUE(js('["code","td","th"].every(function(k){var s=getComputedStyle(document.querySelector(".chat-history-preview .content "+k)),base=originalStyle[k];return Object.keys(base).every(function(p){return s[p]===base[p];});})')),
      "history code and table cells have the same computed styles as the active chat")
    check(isTRUE(js('document.querySelector(".chat-history-preview pre code").textContent===originalCode && document.querySelector(".chat-history-preview").textContent.includes("END_CODE_SENTINEL") && document.querySelectorAll(".chat-history-preview table tr").length===3')),
      "long code blocks and complete Markdown tables render without character truncation")
    wait_js('document.querySelector(".chat-history-preview .MathJax")!==null')
    check(TRUE,"history preview typesets formulas with the same MathJax renderer")
    js('var list=document.getElementById("chat_history_list");list.scrollTop=list.scrollHeight;window.savedListTop=list.scrollTop;list.querySelector(".chat-log-view:last-child").click();')
    wait_js('document.querySelector(".chat-history-preview").textContent.includes("latest 20 of 45")')
    check(isTRUE(js('Math.abs(document.getElementById("chat_history_list").scrollTop-savedListTop)<2 && document.querySelector(".chat-log-view:last-child").getAttribute("aria-pressed")==="true"')),
      "choosing a scrolled conversation preserves the list position and marks the selected row")
    check(isTRUE(js('document.querySelectorAll(".chat-history-preview .history-preview-message").length===20 && document.querySelectorAll(".chat-history-preview .source-review,.chat-history-preview .source-apply").length===0 && document.querySelectorAll("#messages .msg.user").length===1')),
      "preview shows twenty messages without live editor controls or duplicate chat turns")
    js('document.getElementById("history_more").click();')
    wait_js('document.querySelectorAll(".history-preview-message").length===40')
    js('document.getElementById("history_more").click();')
    wait_js('document.querySelectorAll(".history-preview-message").length===45 && document.getElementById("history_more")===null')
    check(isTRUE(js('document.querySelector(".chat-history-preview").textContent.includes("Historical message 1") && document.querySelector(".chat-history-preview pre code").textContent===originalCode')),
      "Load earlier reveals every earlier message without cutting or changing the code")
    js('window.lastCopied=null;Object.defineProperty(navigator,"clipboard",{configurable:true,value:{writeText:function(t){lastCopied=t;return Promise.resolve();}}});document.querySelector(".chat-history-preview .code-copy").click();')
    wait_js('lastCopied!==null')
    check(isTRUE(js('lastCopied===originalCode')) && !file.exists(edit_marker),"preview Copy preserves the complete code without writing to Source")
    browser$Emulation$setDeviceMetricsOverride(width=380,height=700,deviceScaleFactor=1,mobile=FALSE)
    check(isTRUE(js('document.getElementById("chat_history_page").getBoundingClientRect().bottom<=innerHeight && document.getElementById("history_close").getBoundingClientRect().right<=innerWidth && document.querySelector(".chat-history-preview table").getBoundingClientRect().right<=innerWidth')),
      "conversation buttons and rich previews fit a narrow Viewer")
    screenshot <- Sys.getenv("RA_BROWSER_HISTORY_SCREENSHOT")
    if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
    browser$Emulation$clearDeviceMetricsOverride()
    js('document.getElementById("history_close").click();')
    wait_js('getComputedStyle(document.getElementById("chat_stream")).display!=="none"')
    check(isTRUE(js('document.getElementById("prompt").value==="Draft survives history" && document.querySelectorAll("#messages .msg.user").length===1 && window.aisourceErrors.length===0')),
      "Back to chat preserves the draft, original conversation and clean JavaScript state")
    cat(checks,"rich Chat history browser checks passed\n");return(invisible(NULL))
  }
  # Fast check for the passive indicator; the default still runs the full suite.
  if(identical(Sys.getenv("RA_BROWSER_PLOT_ONLY"),"true")) {
    check(isTRUE(js('document.querySelectorAll(".attachment-item").length===0 && document.querySelectorAll(".msg.user").length===0 && window.aisourceErrors.length===0')),
      "automatic availability checks leave attachments and chat empty without JavaScript errors")
    cat(checks,"Plot browser checks passed\n");return(invisible(NULL))
  }
  if(identical(Sys.getenv("RA_BROWSER_VIEWER_ONLY"),"true")) {
    next_url <- function(previous=NULL) {
      deadline<-Sys.time()+6
      repeat {
        value<-if(file.exists(reopen_url))readLines(reopen_url,warn=FALSE)else character()
        if(length(value)==1L && !identical(value,previous))return(value)
        if(Sys.time()>deadline)stop("Viewer did not provide a new opening URL")
        Sys.sleep(.05)
      }
    }
    js('var p=document.getElementById("prompt");p.value="Keep this relaunch draft";p.dispatchEvent(new Event("input",{bubbles:true}));')
    js('new Promise(function(resolve){setTimeout(resolve,1300);})')
    js('setTimeout(function(){window.dispatchEvent(new MessageEvent("message",{data:"disconnected",origin:window.location.origin}));},50);true;')
    wait_js('window.location.href==="about:blank"')
    check(worker$is_alive(),"RStudio's origin-matching disconnect behavior reproduces an empty Viewer while its server remains alive")
    saveRDS("restart",reopen_marker)
    first<-next_url()
    fresh_origin<-strsplit(first,"?",fixed=TRUE)[[1L]][[1L]]
    check(!identical(fresh_origin,url),"relaunch starts the replacement server on a different origin")
    retry<-next_url(first)
    check(!identical(first,retry) && startsWith(retry,paste0(fresh_origin,"?ra_view=")),
      "a deliberately dropped first Viewer opening retries with a fresh page token")
    browser$Page$navigate(retry)
    wait_js('document.getElementById("prompt")!==null && document.getElementById("prompt").value==="Keep this relaunch draft" && !document.getElementById("send").disabled',timeout=20)
    check(TRUE,"the real reconnected Shiny page restores the unsent draft from before the empty Viewer")
    js('new Promise(function(resolve){setTimeout(resolve,2300);})')
    if(!identical(readLines(reopen_url),retry)) {
      cat("Client state:",js('JSON.stringify({url:location.href,token:Shiny.shinyapp.$inputValues.ai_viewer_ready,errors:window.aisourceErrors})'),"\n",
        "Worker output:",paste(worker$read_output_lines(),collapse="\n"),"\n",
        "Worker errors:",paste(worker$read_error_lines(),collapse="\n"),"\n")
    }
    check(identical(readLines(reopen_url),retry),"a real client acknowledgement ends startup retries")
    js(paste0('window.dispatchEvent(new MessageEvent("message",{data:"disconnected",origin:',jsonlite::toJSON(url,auto_unbox=TRUE),'}));'))
    js('new Promise(function(resolve){setTimeout(resolve,250);})')
    check(isTRUE(js('window.location.href!=="about:blank" && document.getElementById("prompt").value==="Keep this relaunch draft"')),
      "a late disconnect from the previous server cannot clear the replacement Viewer")
    saveRDS("reopen",reopen_marker)
    reopened<-next_url(retry);browser$Page$navigate(reopened)
    wait_js('document.getElementById("send")!==null && !document.getElementById("send").disabled')
    js('setTimeout(function(){window.dispatchEvent(new MessageEvent("message",{data:"disconnected",origin:window.location.origin}));},50);true;')
    wait_js('window.location.href==="about:blank"')
    recovered<-next_url(reopened);browser$Page$navigate(recovered)
    wait_js('document.getElementById("prompt")!==null && document.getElementById("prompt").value==="Keep this relaunch draft" && !document.getElementById("send").disabled')
    check(startsWith(recovered,paste0(fresh_origin,"?ra_reopen=1&ra_view=")),
      "manual reopening also recovers when the new page is cleared just after its initial handshake")
    js('new Promise(function(resolve){setTimeout(resolve,2300);})')
    check(identical(readLines(reopen_url),recovered) && isTRUE(js('window.aisourceErrors.length===0')),
      "the recovered page stays connected without further reopening or JavaScript errors")
    js('setTimeout(function(){openAction("close");},50);true;')
    worker$wait(timeout=10000L)
    check(!worker$is_alive(),"Close ends the app without a pending retry bringing it back")
    cat(checks,"Viewer recovery browser checks passed\n");return(invisible(NULL))
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
  if(identical(Sys.getenv("RA_BROWSER_SOURCE_PATCH_ONLY"),"true")) {
    console <- paste("Error: OLD_ERROR_SENTINEL","[End failed Console task; full command may be unavailable]",
      '[Error condition call] bad(X)',"> {", "+ extra_data", "+ x <- bad(X)", "+ }",
      "Error: could not find function bad","[End failed Console task; full command may be unavailable]",sep="\n")
    saveRDS(list(console=console),source_marker)
    js('var p=document.getElementById("prompt");p.value="Fix these chunk errors";p.dispatchEvent(new Event("input",{bubbles:true}));document.getElementById("send").click();')
    wait_js('document.querySelectorAll(".source-hunk").length===2 && !document.getElementById("send").disabled')
    check(!file.exists(edit_marker) && isTRUE(js('document.getElementById("shiny-modal")===null && Array.from(document.querySelectorAll(".code-type")).every(function(x){return x.textContent==="R";})')),
      "two minimal Rmd fixes appear inline with unchanged button styling and no Source write")
    check(isTRUE(js('document.querySelectorAll(".source-hunk .source-proposal-copy").length===2 && document.querySelectorAll(".source-hunk .source-apply").length===2 && document.querySelectorAll(".source-hunk .source-skip").length===2')),
      "each changed part has its own Copy, Replace and Skip")
    js('window.copied=null;Object.defineProperty(navigator,"clipboard",{configurable:true,value:{writeText:function(t){window.copied=t;return Promise.resolve();}}});document.querySelector(".source-hunk .source-proposal-copy").click();')
    wait_js('window.copied!==null')
    check(identical(js('window.copied'),"x <- dim(X)\n"),"Copy contains only the corrected line, without chunk fences or the rest of Source")
    screenshot <- Sys.getenv("RA_BROWSER_SOURCE_PATCH_SCREENSHOT")
    if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
    js('document.querySelector(".source-hunk .source-apply").click();')
    wait_js('document.querySelectorAll(".source-hunk").length===1 && !document.getElementById("send").disabled')
    writes <- readRDS(edit_marker);after <- writes[[1L]]$text
    check(length(writes)==1L && grepl("x <- dim(X)",after,fixed=TRUE) && grepl("y <- bad(Y)",after,fixed=TRUE) &&
      grepl('stop("Untouched chunk")',after,fixed=TRUE) && grepl("Keep this paragraph.",after,fixed=TRUE),
      "Replace changes one faulty line while retaining another pending error, other chunks and Markdown")
    js('window.copied=null;document.querySelector(".source-hunk .source-proposal-copy").click();')
    wait_js('window.copied!==null')
    check(identical(js('window.copied'),"y <- dim(Y)\n"),"the remaining Copy contains only its own rebased correction")
    js('document.querySelector(".source-skip").click();')
    wait_js('document.getElementById("source-review-card")===null')
    check(length(readRDS(edit_marker))==1L,"Skip leaves the second faulty line unchanged")
    js('var p=document.getElementById("prompt");p.value="Fix the remaining error";p.dispatchEvent(new Event("input",{bubbles:true}));document.getElementById("send").click();')
    wait_js('document.querySelectorAll(".source-hunk").length===1 && !document.getElementById("send").disabled')
    manual <- strsplit(after,"\n",fixed=TRUE)[[1L]];manual[[8L]]<-"manual <- 123"
    saveRDS(list(console=console,contents=manual),source_marker)
    js('document.querySelector(".source-hunk .source-apply").click();')
    wait_js('document.getElementById("source-review-card")===null')
    check(length(readRDS(edit_marker))==1L,"manual edits after review revoke the range proposal before an editor write")
    check(isTRUE(js('window.aisourceErrors.length===0')),"minimal range edits complete without browser errors")
    cat(checks,"Source patch browser checks passed\n");return(invisible(NULL))
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
      wait_js(paste0('document.querySelectorAll("#messages .msg.user").length===',turn,' && !document.getElementById("send").disabled'))
    }
    saveRDS(list(contents="library(ggplot2)"),source_marker)
    submit("Explain my code",1L)
    check(isTRUE(js('document.querySelectorAll(".knowledge-read,.knowledge-sources").length===0')),
      "matching Source package names do not search unselected library files")
    saveRDS(list(contents="identity(birth_data)"),source_marker)
    submit("Explain these columns",2L)
    check(isTRUE(js('document.querySelectorAll(".knowledge-read,.knowledge-sources").length===0')),
      "matching Workspace columns do not silently enable Knowledge Base")
    diagnostic <- "Error in arithmetic(): non-numeric argument to binary operator\n[End failed Console task; full command may be unavailable]"
    saveRDS(list(contents="",console=diagnostic),source_marker)
    submit("Энэ алдааг тайлбарла",3L)
    check(isTRUE(js('document.querySelectorAll(".knowledge-read,.knowledge-sources").length===0')),
      "Console errors remain independent of unselected reference files")
    submit("Explain linear regression",4L)
    check(isTRUE(js('document.querySelectorAll(".knowledge-read,.knowledge-sources").length===0')),
      "even an exact topic match does not use an unselected reference")
    js('openAction("knowledge");')
    wait_js('document.querySelectorAll("#knowledge_selected input").length===4')
    js('Array.from(document.querySelectorAll("#knowledge_selected label")).find(function(x){return x.textContent.includes("graphics.md");}).querySelector("input").click();')
    wait_js('document.querySelector(".knowledge-unpin")!==null')
    js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
    wait_js('document.getElementById("shiny-modal")===null')
    submit("Explain this",5L)
    wait_js('document.querySelector(".knowledge-read summary")!==null')
    check(isTRUE(js('document.querySelector(".knowledge-read").textContent.includes("graphics.md") && document.querySelector(".knowledge-sources").textContent.includes("graphics.md")')),
      "explicit selection activates only the chosen reference file")
    check(isTRUE(js('Array.from(document.querySelectorAll(".msg.assistant .content")).pop().textContent.includes("Response 5") && window.aisourceErrors.length===0')),
      "five sends make five model requests without extra search requests or JavaScript errors")
    cat(checks,"work-aware Knowledge Base browser checks passed\n");return(invisible(NULL))
  }
  if(identical(Sys.getenv("RA_BROWSER_KNOWLEDGE_ONLY"),"true")) {
    js('openAction("knowledge");')
    wait_js('document.getElementById("knowledge_files")!==null')
    js('var dt=new DataTransfer();dt.items.add(new File(["Kmeans clustering uses the elbow method and fviz_nbclust. <script>window.kbInjected=true</script>"],"clustering.md",{type:"text/markdown"}));dt.items.add(new File(["Linear regression coefficients are estimated with lm and examined with summary."],"regression.md",{type:"text/markdown"}));var input=document.getElementById("knowledge_files");input.files=dt.files;input.dispatchEvent(new Event("change",{bubbles:true}));')
    wait_js('document.querySelectorAll("#knowledge_selected input").length===2',timeout=30)
    check(isTRUE(js('document.querySelectorAll("#knowledge_selected input:checked").length===0 && document.getElementById("shiny-modal").textContent.includes("No selection means no files are sent.")')),
      "Knowledge Base starts unselected and explains that no files will be sent")
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
    js('var p=document.getElementById("prompt");p.value="Explain linear regression";p.dispatchEvent(new Event("input",{bubbles:true}));document.getElementById("send").click();')
    wait_js('document.querySelectorAll(".knowledge-read").length===2 && !document.getElementById("send").disabled')
    check(isTRUE(js('Array.from(document.querySelectorAll(".knowledge-read")).pop().textContent.includes("No files selected") && document.querySelector(".knowledge-unpin")===null && Array.from(document.querySelectorAll("#messages .msg.assistant")).pop().querySelector(".knowledge-sources")===null')),
      "unpinning the last document disables file retrieval")
    check(isTRUE(js('Array.from(document.querySelectorAll(".knowledge-read>summary")).pop().textContent.includes("No files selected")')),
      "an unselected matching document does not create a new source label")
    js('openAction("knowledge");')
    wait_js('document.querySelectorAll("#knowledge_selected input").length===2')
    js('document.querySelector("#knowledge_selected input").click();')
    wait_js('document.querySelector(".knowledge-unpin")!==null')
    js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
    wait_js('document.getElementById("shiny-modal")===null')
    js('document.getElementById("new_chat").click();')
    wait_js('document.querySelectorAll(".msg.user").length===0 && document.querySelector(".knowledge-unpin")===null')
    js('openAction("history");')
    wait_js('document.querySelector(".chat-log-view")!==null')
    check(isTRUE(js('document.querySelectorAll(".chat-log-view").length===1')),
      "a new chat resets selection without adding a blank History entry")
    js('document.querySelector(".chat-log-view").click();document.getElementById("history_restore").click();')
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
  check(isTRUE(js('var update=document.getElementById("check_updates").getBoundingClientRect(),help=document.getElementById("help").getBoundingClientRect(),close=document.getElementById("close").getBoundingClientRect();help.top>=update.bottom && Math.abs(update.width-help.width)<2 && close.top>=help.bottom && close.width>=document.querySelector("#close").parentElement.getBoundingClientRect().width-2 && document.getElementById("close").closest(".menu-close")!==null && document.getElementById("close").closest(".menu-group")===null && getComputedStyle(document.getElementById("close")).textAlign==="center" && getComputedStyle(document.getElementById("close")).color==="rgb(255, 255, 255)" && getComputedStyle(document.getElementById("close")).backgroundColor!=="rgb(255, 255, 255)"')),
    "Check updates and Help each fill their own row; Close stays isolated, centered and red")
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
  wait_js('document.querySelector(".chat-history-panel")!==null && getComputedStyle(document.getElementById("chat_history_page")).display!=="none"')
  check(isTRUE(js('document.querySelector(".chat-log-view")===null && document.getElementById("chat_history_page").textContent.includes("No saved chats yet.") && document.getElementById("shiny-modal")===null')),
    "opening History after New chat uses a separate page without listing an empty conversation")
  js('document.getElementById("history_close").click();')
  wait_js('getComputedStyle(document.getElementById("chat_history_page")).display==="none"')
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
  check(isTRUE(js('document.getElementById("shiny-modal").textContent.includes("RAssistant::launch()") && document.getElementById("shiny-modal").textContent.includes("API settings") && document.getElementById("shiny-modal").textContent.includes("Each provider") && !document.getElementById("shiny-modal").textContent.includes("Restart R") && document.querySelector("#app_menu #check_updates")!==null && document.querySelector("#shiny-modal #check_updates")===null')),
    "Help explains launching, separate provider settings and updates")
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
  check(identical(js('window.copied'),"RAssistant::update_assistant()\n"),
    "the update instructions copy correctly")
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
  check(isTRUE(js('var s=document.getElementById("send");s.disabled && s.classList.contains("is-loading") && s.getAttribute("aria-busy")==="true" && getComputedStyle(s.querySelector(".send-spinner")).animationName==="send-spin" && getComputedStyle(s.querySelector(".send-plane")).display==="none" && document.getElementById("compose_status").hidden')),
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
  check(isTRUE(js('document.querySelector(".source-edit-mode select")===null && document.querySelector(".source-apply").textContent==="Create document"')),
    "a stale code answer cannot replace a changed Source snapshot")
  js('document.querySelector(".source-apply").click();')
  wait_js('document.getElementById("source-review-card")===null && !document.getElementById("send").disabled')
  check(length(readRDS(edit_marker))==2L && readRDS(edit_marker)[[2L]]$operation=="new",
    "a reviewed new-document proposal needs its own explicit approval")
  js('openAction("history");')
  wait_js('document.querySelector(".chat-log-view")!==null')
  check(isTRUE(js('document.querySelectorAll(".chat-log-view").length===1 && document.querySelectorAll("#chat_history_page .history-actions .ai-btn").length===3 && document.getElementById("shiny-modal")===null')),
    "History lists the current auto-saved chat with consistent controls")
  wait_js('document.querySelector(".chat-history-preview")!==null && getComputedStyle(document.getElementById("chat_stream")).display==="none"')
  check(isTRUE(js('getComputedStyle(document.querySelector(".compose")).display==="none" && document.querySelector("#chat_stream .chat-history-panel")===null && document.querySelector(".chat-history-preview .source-review")===null')),
    "Chat history uses a separate read-only page without mixing preview messages into chat")
  browser$Emulation$setDeviceMetricsOverride(width=380,height=700,deviceScaleFactor=1,mobile=FALSE)
  check(isTRUE(js('document.getElementById("chat_history_page").getBoundingClientRect().bottom<=innerHeight && document.getElementById("history_close").getBoundingClientRect().right<=innerWidth')),
    "Chat history and Back to chat fit a narrow Viewer")
  screenshot <- Sys.getenv("RA_BROWSER_HISTORY_SCREENSHOT")
  if(nzchar(screenshot))writeBin(jsonlite::base64_dec(browser$Page$captureScreenshot(format="png")$data),screenshot)
  browser$Emulation$clearDeviceMetricsOverride()
  js('window.firstChatId=document.querySelector(".chat-log-view[aria-pressed=true]").getAttribute("data-id");document.getElementById("history_rename").click();')
  wait_js('document.getElementById("history_name")!==null')
  js('$("#history_name").val("Saved screenshot assignment").trigger("change");document.getElementById("history_save_name").click();')
  wait_js('document.querySelector(".chat-log-view")!==null && document.getElementById("chat_history_list").textContent.includes("Saved screenshot assignment")')
  check(TRUE,"Rename persists the chosen chat title")
  js('document.getElementById("history_close").click();')
  wait_js('getComputedStyle(document.getElementById("chat_history_page")).display==="none"')
  js('document.getElementById("new_chat").click();')
  wait_js('document.querySelector(".empty")!==null')
  js('document.getElementById("prompt").value="A separate history conversation";document.getElementById("send").click();')
  wait_js('!document.getElementById("send").disabled && document.querySelectorAll(".msg.user").length===1')
  js('openAction("history");')
  wait_js('document.querySelector(".chat-log-view")!==null && document.querySelectorAll(".chat-log-view").length===2')
  js('window.secondChatId=document.querySelector(".chat-log-view[aria-pressed=true]").getAttribute("data-id");Array.from(document.querySelectorAll(".chat-log-view")).find(function(b){return b.dataset.id===firstChatId;}).click();')
  wait_js('document.querySelector(".chat-history-preview").textContent.includes("Explain this")')
  js('document.getElementById("history_restore").click();')
  wait_js('document.getElementById("shiny-modal")===null && document.querySelectorAll(".msg.user").length===1 && document.querySelector(".msg.user").textContent.includes("Explain this")')
  check(isTRUE(js('!document.getElementById("chat_stream").textContent.includes("A separate history conversation")')),
    "Restore switches conversations without mixing their visible history")
  js('openAction("history");')
  wait_js('document.querySelector(".chat-log-view")!==null')
  js('Array.from(document.querySelectorAll(".chat-log-view")).find(function(b){return b.dataset.id===secondChatId;}).click();')
  wait_js('document.querySelector(".chat-history-preview").textContent.includes("A separate history conversation")')
  js('document.getElementById("history_delete").click();')
  wait_js('document.getElementById("history_confirm_delete")!==null')
  js('document.querySelector("#shiny-modal [data-dismiss=modal]").click();')
  wait_js('document.getElementById("shiny-modal")===null')
  js('openAction("history");')
  wait_js('document.querySelector(".chat-log-view")!==null')
  check(isTRUE(js('document.querySelectorAll(".chat-log-view").length===2')),
    "cancelling Delete preserves the saved conversation")
  js('Array.from(document.querySelectorAll(".chat-log-view")).find(function(b){return b.dataset.id===secondChatId;}).click();')
  wait_js('document.querySelector(".chat-history-preview").textContent.includes("A separate history conversation")')
  js('document.getElementById("history_delete").click();')
  wait_js('document.getElementById("history_confirm_delete")!==null')
  js('document.getElementById("history_confirm_delete").click();')
  wait_js('document.querySelector(".chat-log-view")!==null && document.querySelectorAll(".chat-log-view").length===1')
  check(TRUE,"Delete removes the selected saved chat while retaining the active chat")
  js('document.getElementById("history_close").click();')
  wait_js('getComputedStyle(document.getElementById("chat_history_page")).display==="none"')
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
  check(worker$is_alive() && file.exists(reopen_url) && startsWith(readLines(reopen_url),paste0(url,"?ra_reopen=1&ra_view=")),
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
  check(identical(js('window.copied'),"dim(X)\n"),
    "the reported example copies only the corrected line beside Replace")
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
  js('document.querySelector("#workspace_datasets_selected input[value=extra_data]").click();')
  wait_js('document.getElementById("workspace_preview").textContent.includes("5432")')
  js('document.querySelector("#workspace_menu>summary").click();')
  js('document.getElementById("prompt").value="Workspace evidence please";document.getElementById("send").click();')
  wait_js('!document.getElementById("send").disabled && document.querySelector("#messages").textContent.includes("Dataset cell evidence received: 5432.")')
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
