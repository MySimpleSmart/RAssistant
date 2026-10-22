# Viewer lifecycle uses a deterministic event queue and synthetic app handles.
# No RStudio, network, credentials or package installations are needed.
library(RAssistant)
main <- function() {
  ns <- asNamespace("RAssistant");env <- new.env(parent=ns)
  for(name in ls(ns,all.names=TRUE)) {
    object <- get(name,ns)
    if(is.function(object))environment(object)<-env
    assign(name,object,env)
  }
  env$.app_state <- new.env(parent=emptyenv())
  env$.app_state$handle <- NULL;env$.app_state$updated <- FALSE
  env$.app_state$update_queued <- FALSE;env$.app_state$viewer_generation <- 0L
  env$.app_state$viewer_request <- NULL;env$.app_state$last_port <- NULL
  checks <- 0L
  check <- function(ok,label){if(!isTRUE(ok))stop(label,call.=FALSE);checks<<-checks+1L;cat("PASS",label,"\n")}
  queue <- list();viewed <- character();events <- character();running <- TRUE
  env$.viewer_later <- function(callback,delay){stopifnot(delay==2);queue[[length(queue)+1L]]<<-callback}
  tick <- function(){callback<-queue[[1L]];queue<<-queue[-1L];callback();invisible(NULL)}
  drain <- function(){while(length(queue))tick()}
  env$.show_viewer <- function(url){events<<-c(events,"viewer");viewed<<-c(viewed,url)}
  handle <- list(status=function()if(running)"running"else"success",url=function()"http://127.0.0.1:21001")
  env$.app_state$handle <- handle
  env$.open_viewer(handle)
  first <- env$.app_state$viewer_request$token
  check(length(viewed)==1L && length(queue)==1L && grepl("?ra_view=viewer-page-",viewed[[1L]],fixed=TRUE),
    "the first Viewer opening has a unique page token and a bounded connection check")
  check(!env$.viewer_ready("old-page") && !env$.viewer_ready(NA_character_) && !env$.viewer_ready(list(first)),
    "missing, malformed and stale acknowledgements cannot confirm the current page")
  check(env$.viewer_ready(first) && env$.app_state$viewer_request$connected,
    "the matching connected page acknowledges its retained running handle")
  tick()
  check(!is.null(env$.app_state$viewer_request),"a first handshake has a short settling check for late Viewer clearing")
  tick()
  check(is.null(env$.app_state$viewer_request) && !length(queue) && length(viewed)==1L,
    "a stable connection ends all startup checks without reopening the Viewer")
  env$.open_viewer(handle,reopen=TRUE);old<-env$.app_state$viewer_request$token
  tick();new<-env$.app_state$viewer_request$token
  check(!identical(old,new) && env$.app_state$viewer_request$attempts==2L &&
    startsWith(tail(viewed,1L),"http://127.0.0.1:21001?ra_reopen=1&ra_view="),
    "an unconfirmed opening retries with a different page token and cache-busting URL")
  check(!env$.viewer_ready(old) && env$.viewer_ready(new),"a delayed old page cannot acknowledge the retry")
  env$.viewer_disconnected(old)
  check(env$.app_state$viewer_request$connected,"retiring an older page cannot mark its replacement disconnected")
  env$.viewer_disconnected(new);tick()
  check(env$.app_state$viewer_request$attempts==3L && !env$.app_state$viewer_request$connected,
    "a page cleared after its first handshake is retried during startup confirmation")
  token<-env$.app_state$viewer_request$token;env$.viewer_ready(token);drain()
  check(is.null(env$.app_state$viewer_request),"a successful retry cancels checks after its connection settles")
  env$.open_viewer(handle);before<-length(viewed);token<-env$.app_state$viewer_request$token
  check(!env$.viewer_loading("old-page") && env$.viewer_loading(token),"only the requested page can report that its HTTP document is loading")
  for(i in seq_len(4L))tick()
  check(length(viewed)==before && env$.app_state$viewer_request$loading_checks==4L,
    "a slow document load is given time to connect without destructive reloads")
  env$.viewer_ready(token);drain()
  check(is.null(env$.app_state$viewer_request),"a slow page's eventual handshake ends its startup checks")
  env$.open_viewer(handle);before<-length(viewed);env$.viewer_loading(env$.app_state$viewer_request$token)
  for(i in seq_len(11L))tick()
  check(length(viewed)==before+1L && env$.app_state$viewer_request$attempts==2L,
    "a document that never finishes loading still has a bounded retry")
  env$.cancel_viewer_open();drain()
  before<-length(viewed);messages<-character()
  env$.open_viewer(handle)
  withCallingHandlers(drain(),message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart("muffleMessage")})
  check(length(viewed)==before+3L && !length(queue) && is.null(env$.app_state$viewer_request) &&
    any(grepl("Viewer connection was not confirmed",messages,fixed=TRUE)) &&
    any(grepl("RAssistant::launch()",messages,fixed=TRUE)),
    "three unsuccessful attempts report recovery without looping or misreporting installation failure")
  env$.open_viewer(handle);before<-length(viewed);env$.cancel_viewer_open();drain()
  check(length(viewed)==before,"Close cancels outstanding retries")
  env$.open_viewer(handle);before<-length(viewed);running<-FALSE;drain()
  check(length(viewed)==before,"a stopped server is never reopened by an old callback")
  running<-TRUE;env$.open_viewer(handle);old<-env$.app_state$viewer_request$token
  env$.open_viewer(handle);new<-env$.app_state$viewer_request$token
  before<-length(viewed);env$.viewer_ready(new);drain()
  check(length(viewed)==before && !identical(old,new),"a newer launch supersedes the previous request and its queued callback")
  env$.open_viewer(handle);before<-length(viewed)
  env$.app_state$handle <- list(status=function()"running",url=function()"http://127.0.0.1:21002")
  drain()
  check(length(viewed)==before,"a replaced app handle invalidates checks owned by the old server")
  env$.app_state$handle <- handle
  env$.show_viewer <- function(url)stop("Synthetic Viewer failure")
  failure <- tryCatch(env$.open_viewer(handle),error=identity)
  check(inherits(failure,"error") && env$.app_running() && is.null(env$.app_state$viewer_request),
    "a Viewer API error preserves the running app and revokes the opening request")

  # Launch must retain the server before any Viewer call, and never reread the
  # API key when reopening that same server.
  queue<-list();viewed<-events<-character();key_reads<-0L
  env$.app_state$handle<-NULL
  env$.in_rstudio <- function()TRUE
  env$.resolve_api_key <- function(){key_reads<<-key_reads+1L;"synthetic-key"}
  env$.new_app_port <- function()21001L
  env$.package_ui_info <- function()list()
  env$.load_launcher <- function(path)function(api_key,package_info){
    stopifnot(package_info$port==21001L,is.function(package_info$viewer_ready),is.function(package_info$viewer_disconnected),
      is.function(package_info$viewer_closed));events<<-c(events,"server");handle
  }
  env$.show_viewer <- function(url){stopifnot(identical(env$.app_state$handle,handle));events<<-c(events,"viewer");viewed<<-c(viewed,url)}
  env$launch()
  check(identical(events,c("server","viewer")) && key_reads==1L,
    "launch retains the started server before asking RStudio to display its page")
  env$launch()
  check(key_reads==1L && length(viewed)==2L,"manual reopening reuses the existing server and saved credentials")
  env$.cancel_viewer_open();drain()

  # Port selection deterministically rejects both the previous app's port and
  # a global shiny.port setting that would otherwise force the same origin.
  env$.new_app_port <- get(".new_app_port",ns);environment(env$.new_app_port)<-env
  candidates<-c(21001L,21002L,21003L);selected<-0L
  env$`::` <- function(pkg,name) {
    p<-as.character(substitute(pkg));n<-as.character(substitute(name))
    if(p=="httpuv" && n=="randomPort")return(function(min,max){
      stopifnot(min==20000L,max==29999L);selected<<-selected+1L;candidates[[min(selected,length(candidates))]]
    })
    base::getExportedValue(p,n)
  }
  old_options<-options(shiny.port=21001L);on.exit(options(old_options),add=TRUE)
  env$.app_state$last_port<-21002L
  check(env$.new_app_port()==21003L && selected==3L,"fresh port selection excludes the last app and a fixed Shiny port")
  candidates<-21002L;selected<-0L
  failure<-tryCatch(env$.new_app_port(),error=identity)
  check(inherits(failure,"error") && selected==20L,"unavailable fresh-port selection fails after a bounded number of attempts")
  cat(checks,"Viewer lifecycle checks passed\n")
}
main()
