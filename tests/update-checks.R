# Package update discovery/lifecycle tests. No real network, credentials or installs.
library(RAssistant)
main <- function() {
  ns <- asNamespace("RAssistant")
  env <- new.env(parent=ns)
  for (name in ls(ns,all.names=TRUE)) {
    value <- get(name,ns)
    if(is.function(value))environment(value)<-env
    assign(name,value,env)
  }
  env$.update_state <- new.env(parent=emptyenv())
  env$.update_state$worker <- NULL
  env$.update_state$result <- list(status="disabled")
  env$.update_state$checked_at <- NULL
  env$.app_state <- new.env(parent=emptyenv())
  env$.app_state$handle <- NULL
  env$.app_state$updated <- FALSE
  env$.app_state$update_queued <- FALSE
  options_old <- options(RAssistant.check_updates=TRUE)
  on.exit(options(options_old),add=TRUE)
  checks <- 0L
  check <- function(ok,description){
    if(!isTRUE(ok))stop(description,call.=FALSE)
    checks<<-checks+1L;cat("PASS",description,"\n")
  }
  env$.installed_info <- function()list(version="0.1.1",sha="original-sha")
  starts <- 0L; alive <- TRUE; killed <- 0L; latest <- "0.1.2"; failure <- FALSE
  env$.new_update_worker <- function(){
    starts<<-starts+1L
    list(is_alive=function()alive,get_result=function(){
      if(failure)stop("Synthetic offline failure")
      latest
    },kill=function(){killed<<-killed+1L;alive<<-FALSE})
  }
  env$.start_update_check()
  check(starts==1L && identical(env$.poll_update_check()$status,"checking"),
    "a background check returns immediately while work is running")
  env$.start_update_check(force=TRUE)
  check(starts==1L,"an in-flight check is reused")
  alive<-FALSE
  result<-env$.poll_update_check()
  check(identical(result$status,"available") && identical(result$version,"0.1.2"),
    "a newer package version produces an update notice")
  env$.start_update_check()
  check(starts==1L,"a completed check is cached between automatic checks")
  latest<-"0.1.1";env$.start_update_check(force=TRUE)
  check(starts==2L && identical(env$.poll_update_check()$status,"current"),
    "manual checking bypasses the cache and recognizes the installed version")
  latest<-"0.1.0";env$.start_update_check(force=TRUE)
  check(identical(env$.poll_update_check()$status,"current"),"an older remote version never prompts a downgrade")
  failure<-TRUE;env$.start_update_check(force=TRUE)
  check(identical(env$.poll_update_check()$status,"unavailable"),"offline errors do not interrupt chat startup")
  failure<-FALSE;latest<-"not a version";env$.start_update_check(force=TRUE)
  check(identical(env$.poll_update_check()$status,"unavailable"),"invalid metadata cannot produce an update action")
  alive<-TRUE;env$.start_update_check(force=TRUE)
  env$.update_state$started_at<-Sys.time()-11
  check(identical(env$.poll_update_check()$status,"unavailable") && killed==1L,
    "a stuck update worker is stopped after its deadline")
  alive<-TRUE;env$.start_update_check(force=TRUE)
  options(RAssistant.check_updates=FALSE)
  env$.start_update_check()
  check(identical(env$.poll_update_check()$status,"disabled") && killed==2L,
    "disabling checks cancels an existing worker")
  before<-starts;env$.start_update_check()
  check(starts==before,"disabled automatic checks never start network work")
  env$.start_update_check(force=TRUE)
  check(starts==before+1L,"an explicit manual check works even when automatic checks are disabled")
  alive<-FALSE;env$.poll_update_check()
  options(RAssistant.check_updates=TRUE)
  env$.update_state$checked_at<-Sys.time()-1801
  alive<-FALSE;latest<-"0.1.2"
  result<-env$.poll_update_status()
  check(starts==before+2L && identical(result$status,"available"),
    "an open session automatically discovers a new version after 30 minutes")
  before<-starts
  env$.update_state$checked_at<-Sys.time()-60
  env$.poll_update_status()
  check(starts==before && env$.update_poll_delay()>1700000,
    "one minute does not trigger another network check or frequent idle polling")
  env$.update_state$checked_at<-Sys.time()-1790
  env$.poll_update_status()
  check(starts==before && env$.update_poll_delay()>5000 && env$.update_poll_delay()<=10000,
    "the next automatic check waits for the remaining 30-minute interval")
  env$.update_state$checked_at<-Sys.time()-1801;alive<-TRUE
  result<-env$.poll_update_status()
  check(starts==before+1L && identical(result$status,"available") && env$.update_poll_delay()==250,
    "a known banner stays visible while the next background check runs")
  env$.poll_update_status()
  check(starts==before+1L,"repeated polling never starts a second in-flight worker")
  alive<-FALSE;failure<-TRUE
  result<-env$.poll_update_status()
  check(identical(result$status,"available") && env$.update_poll_delay()>1790000,
    "a temporary connection failure keeps the known banner and schedules a later retry")
  factory<-env$.new_update_worker
  env$.new_update_worker<-function()stop("Synthetic worker startup failure")
  env$.update_state$checked_at<-Sys.time()-1801
  result<-env$.poll_update_status()
  check(identical(result$status,"available") && env$.update_poll_delay()>1790000,
    "a worker startup failure preserves the banner without a rapid retry loop")
  env$.new_update_worker<-factory
  env$.update_state$result<-list(status="current")
  env$.update_state$checked_at<-Sys.time()-1801
  check(identical(env$.poll_update_status()$status,"unavailable"),
    "a failed check without a known update does not invent a banner")
  failure<-FALSE;latest<-"0.1.3"
  env$.update_state$checked_at<-Sys.time()-1801
  check(identical(env$.poll_update_status()$version,"0.1.3"),
    "an offline session resumes automatic discovery when connectivity returns")

  env$.conversation_store<-env$.knowledge_store<-function()NULL
  before<-starts
  info<-env$.package_ui_info()
  check(starts==before+1L && identical(info$poll_update()$version,"0.1.3"),
    "a new launch checks immediately despite a recent cached result")
  before<-starts
  latest<-"0.1.4";info$refresh_update()
  check(starts==before+1L && identical(info$poll_update()$version,"0.1.4"),
    "Check for updates remains available and bypasses the automatic interval")
  options(RAssistant.check_updates=FALSE);before<-starts
  info<-env$.package_ui_info()
  check(starts==before && identical(info$poll_update()$status,"disabled") &&
    info$update_poll_delay()==1800000,"disabled discovery starts no work on launch or idle polling")
  alive<-TRUE;info$refresh_update()
  check(starts==before+1L && identical(info$poll_update()$status,"checking"),
    "automatic polling does not cancel an explicit check when automatic discovery is disabled")
  alive<-FALSE
  check(identical(info$poll_update()$version,"0.1.4"),
    "manual results still display when automatic discovery is disabled")
  before<-starts;env$.update_state$checked_at<-Sys.time()-1801
  check(identical(info$poll_update()$status,"available") && starts==before,
    "disabled automatic checks preserve manual results without repeating network requests")
  options(RAssistant.check_updates=TRUE)

  # Synthetic Git HTTP packets exercise exact-commit discovery without network,
  # an installed Git executable, REST API access or stale branch-file metadata.
  path <- tempfile(fileext=".dcf")
  on.exit(unlink(path),add=TRUE)
  sha_a <- strrep("a",40L);sha_b <- strrep("b",40L);sha_other <- strrep("c",40L)
  packet <- function(payload) {
    if(is.character(payload))payload <- charToRaw(payload)
    c(charToRaw(sprintf("%04x",length(payload)+4L)),payload)
  }
  advertisement <- function(main,branch="main",service="# service=git-upload-pack\n") c(
    packet(service),charToRaw("0000"),
    packet(c(charToRaw(paste0(sha_other," HEAD")),as.raw(0),charToRaw("symref=HEAD:refs/heads/other\n"))),
    packet(paste0(main," refs/heads/",branch,"\n")),charToRaw("0000"))
  refs <- advertisement(sha_a)
  requests <- list()
  transport <- function(url,destfile,...) {
    requests[[length(requests)+1L]] <<- c(list(url=url,timeout=getOption("timeout"),destfile=destfile),list(...))
    if(grepl("info/refs?service=git-upload-pack",url,fixed=TRUE))writeBin(refs,destfile)
    else writeLines(c("Package: RAssistant","Version: 0.1.2"),destfile)
    0L
  }
  old_timeout <- getOption("timeout")
  downloader <- get(".download_version_file",ns)
  downloader(path,download=transport)
  refs <- advertisement(sha_b);downloader(path,download=transport)
  first <- requests[[1L]];second <- requests[[3L]]
  check(length(requests)==4L && identical(first$url,
    "https://github.com/MySimpleSmart/RAssistant.git/info/refs?service=git-upload-pack") &&
    !any(vapply(requests,function(x)grepl("api.github.com",x$url,fixed=TRUE),logical(1))),
    "public version discovery does not consume the IP-shared GitHub REST API quota")
  check(identical(requests[[2L]]$url,paste0("https://raw.githubusercontent.com/MySimpleSmart/RAssistant/",sha_a,"/DESCRIPTION")) &&
    identical(requests[[4L]]$url,paste0("https://raw.githubusercontent.com/MySimpleSmart/RAssistant/",sha_b,"/DESCRIPTION")) &&
    identical(first$cacheOK,FALSE) &&
    identical(unname(first$headers[["Cache-Control"]]),"no-cache, max-age=0") &&
    identical(unname(first$headers[["Pragma"]]),"no-cache"),
    "moving main resolves to immutable version files instead of cached branch URLs or a different HEAD")
  check(identical(first$timeout,4L) && identical(requests[[2L]]$timeout,4L) && identical(first$method,"libcurl") &&
    identical(first$mode,"wb") && identical(getOption("timeout"),old_timeout),
    "both metadata downloads fit the worker deadline and restore timeout settings")
  download_failure <- tryCatch(downloader(path,download=function(...)stop("Synthetic offline failure")),error=identity)
  check(inherits(download_failure,"error") && identical(getOption("timeout"),old_timeout),
    "failed metadata downloads also restore timeout settings")
  reader <- get(".github_main_commit",ns)
  rejected_refs <- function(value) {
    refs <<- value
    inherits(tryCatch(reader(download=transport),error=identity),"error")
  }
  check(rejected_refs(advertisement(sha_a,branch="develop")),"a missing main branch cannot select another branch's version")
  duplicate <- advertisement(sha_a)
  duplicate <- c(utils::head(duplicate,-4L),packet(paste0(sha_b," refs/heads/main\n")),charToRaw("0000"))
  check(rejected_refs(duplicate),"ambiguous main references are rejected")
  check(rejected_refs(utils::head(advertisement(sha_a),-2L)) &&
    rejected_refs(utils::head(advertisement(sha_a),-4L)),"truncated packets and missing terminal flush markers are rejected")
  check(rejected_refs(advertisement(sha_a,service="# service=git-receive-pack\n")),
    "reference discovery verifies the requested Git service")
  check(rejected_refs(raw(1024^2+1L)),"oversized reference responses are rejected before parsing")
  check(inherits(tryCatch(reader(download=function(...)1L),error=identity),"error"),
    "failed reference downloads cannot produce an update notice")
  refs <- c(packet("# service=git-upload-pack\n"),charToRaw("0000"),
    packet(c(charToRaw(paste0(sha_a," refs/heads/main")),as.raw(0),charToRaw("multi_ack\n"))),charToRaw("0000"))
  check(identical(reader(download=transport),sha_a),"main references with NUL-separated capabilities are decoded correctly")

  metadata <- c("Package: RAssistant","Version: 0.1.2")
  env$.download_version_file <- function(path){writeLines(metadata,path);0L}
  check(identical(env$.fetch_latest_version(),"0.1.2"),"discovery reads only the package identity and version")
  metadata<-c("Package: another-package","Version: 99.0")
  check(inherits(tryCatch(env$.fetch_latest_version(),error=identity),"error"),
    "metadata from another package is rejected")
  metadata<-c("Package: RAssistant","Version: system('invalid')")
  check(inherits(tryCatch(env$.fetch_latest_version(),error=identity),"error"),
    "remote metadata is parsed as data and cannot execute R code")

  # Run the real deferred callback against a fake handle and installer.
  running<-TRUE;events<-character()
  env$.app_state$handle<-list(status=function()if(running)"running" else "success",
    stop=function(){running<<-FALSE;events<<-c(events,"stop")})
  env$.in_rstudio<-function()TRUE
  env$launch<-function(){
    stopifnot(!env$.app_state$update_queued)
    events<<-c(events,"launch")
  }
  env$update_assistant<-function(relaunch){
    stopifnot(identical(relaunch,FALSE))
    events<<-c(events,"install")
  }
  check(isTRUE(env$.schedule_update()) && !isTRUE(env$.schedule_update()),
    "repeated Update now clicks queue only one installation")
  later::run_now(timeoutSecs=.3)
  check(identical(events,c("stop","install","launch")) && !env$.app_state$update_queued,
    "Update now stops the old app, installs and reopens exactly once after releasing the lock")
  events<-character()
  env$update_assistant<-function(relaunch)stop("Synthetic install failure")
  env$.schedule_update();later::run_now(timeoutSecs=.3)
  check(!env$.app_state$update_queued && !length(events),
    "failed deferred installations release their lock without relaunching")

  # Exercise the Console updater, including unchanged installations and failures.
  env$update_assistant<-get("update_assistant",ns);environment(env$update_assistant)<-env
  env$.package_path<-function()system.file(package="RAssistant")
  env$.installed_info<-function()list(version="0.1.2",sha="same-sha")
  installs<-0L;sha<-"same-sha"
  env$.install_from_github<-function(lib,ref){
    installs<<-installs+1L
    list(path=normalizePath(file.path(lib,"RAssistant")),version="0.1.2",sha=sha)
  }
  result<-withVisible(env$update_assistant())
  check(!result$visible && !result$value$updated && result$value$relaunched &&
    !env$.app_state$updated && identical(events,"launch"),
    "an unchanged update reopens the app without requesting a namespace reload")
  events<-character()
  result<-env$update_assistant(relaunch=FALSE)
  check(!result$relaunched && !length(events),
    "relaunch FALSE leaves a successful installation closed")
  before<-installs
  invalid<-list(NA,logical(),c(TRUE,FALSE),"yes",1L,NULL)
  check(all(vapply(invalid,function(value)inherits(
    tryCatch(env$update_assistant(relaunch=value),error=identity),"error"),logical(1))) &&
    installs==before,"invalid relaunch arguments are rejected before installation")
  sha<-"new-sha"
  result<-env$update_assistant()
  check(result$updated && result$relaunched && env$.app_state$updated &&
    identical(events,"launch"),"a changed Console update requests a reload and reopens by default")
  events<-character()
  env$.in_rstudio<-function()FALSE
  result<-env$update_assistant()
  check(result$updated && !result$relaunched && !length(events),
    "updates outside RStudio install successfully without attempting to open an app")
  env$.in_rstudio<-function()TRUE
  env$launch<-function(){
    stopifnot(!env$.app_state$update_queued)
    events<<-c(events,"launch")
    stop("Synthetic launch failure")
  }
  messages<-character()
  capture_messages<-function(expr)withCallingHandlers(expr,message=function(m){
    messages<<-c(messages,conditionMessage(m));invokeRestart("muffleMessage")
  })
  result<-capture_messages(env$update_assistant())
  check(result$updated && !result$relaunched && identical(events,"launch") &&
    any(grepl("update succeeded, but",messages,fixed=TRUE)) &&
    any(grepl("RAssistant::launch()",messages,fixed=TRUE)) &&
    !any(grepl("Update failed",messages,fixed=TRUE)),
    "a relaunch failure preserves successful installation and explains how to retry")
  events<-messages<-character();before<-installs
  env$.schedule_update();capture_messages(later::run_now(timeoutSecs=.3))
  check(installs==before+1L && identical(events,"launch") && !env$.app_state$update_queued &&
    any(grepl("update succeeded, but",messages,fixed=TRUE)) &&
    !any(grepl("Update failed",messages,fixed=TRUE)),
    "a deferred relaunch failure neither repeats nor misreports the installation")
  events<-character();env$.app_state$updated<-FALSE
  env$.install_from_github<-function(lib,ref)stop("Synthetic install failure")
  result<-tryCatch(env$update_assistant(),error=identity)
  check(inherits(result,"error") && !env$.app_state$updated && !length(events),
    "a failed Console installation never launches or marks the namespace updated")
  fixture_env <- new.env(parent=baseenv())
  fixture_env$fixture_version <- function()"0.1.2"
  environment(fixture_env$fixture_version)<-baseenv()
  fixture_env$fetch <- function(){
    if(nzchar(Sys.getenv("OPENAI_API_KEY")))stop("Unexpected API credential in metadata worker.")
    fixture_version()
  }
  environment(fixture_env$fetch)<-fixture_env
  env$.fetch_latest_version<-fixture_env$fetch
  env$.new_update_worker<-get(".new_update_worker",ns)
  environment(env$.new_update_worker)<-env
  worker<-env$.new_update_worker()
  on.exit(try(worker$kill(),silent=TRUE),add=TRUE)
  worker$wait(timeout=10000)
  check(identical(worker$get_result(),"0.1.2"),
    "a real metadata subprocess retains its helper environment without inheriting the API key")
  cat(checks,"update checks passed\n")
}
main()
