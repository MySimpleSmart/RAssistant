# R CMD check runs this against the installed package. No real store, prompt,
# browser or network calls are used. Private functions are cloned for isolation.
library(RAssistant)
main <- function() {
  ns <- asNamespace("RAssistant")
  env <- new.env(parent = ns)
  for (name in ls(ns, all.names = TRUE)) {
    object <- get(name, envir = ns)
    if (is.function(object)) environment(object) <- env
    assign(name, object, envir = env)
  }
  env$.app_state <- new.env(parent = emptyenv())
  env$.app_state$handle <- NULL
  env$.app_state$updated <- FALSE
  env$.app_state$update_queued <- FALSE
  old_key <- Sys.getenv("OPENAI_API_KEY", unset = NA_character_)
  on.exit(if (is.na(old_key)) Sys.unsetenv("OPENAI_API_KEY") else Sys.setenv(OPENAI_API_KEY = old_key), add = TRUE)
  Sys.unsetenv("OPENAI_API_KEY")
  checks <- 0L
  check <- function(ok, description) {
    if (!isTRUE(ok)) stop(description, call. = FALSE)
    checks <<- checks + 1L
    cat("PASS", description, "\n")
  }
  failed <- function(expr, pattern) {
    result <- tryCatch(force(expr), error = identity)
    inherits(result, "error") && grepl(pattern, conditionMessage(result), fixed = TRUE)
  }
  store <- new.env(parent = emptyenv())
  store$key <- NULL; store$reads <- 0L; store$writes <- 0L; store$deletes <- 0L
  store$fail_list <- FALSE; store$fail_read <- FALSE; store$fail_write <- FALSE
  backend <- list(name = "macos", list = function(service) {
    if (store$fail_list) stop("Synthetic store failure")
    stopifnot(identical(service, "RAssistant/OpenAI"))
    data.frame(username = if (is.null(store$key)) character() else "default")
  }, get = function(service, username) {
    store$reads <- store$reads + 1L
    if (store$fail_read) stop("Do not expose this backend error")
    store$key
  }, set_with_value = function(service, username, password) {
    if (store$fail_write) stop("Do not expose the supplied password")
    store$key <- password; store$writes <- store$writes + 1L
  }, delete = function(service, username) {
    store$key <- NULL; store$deletes <- store$deletes + 1L
  })
  env$.credential_backend <- function() backend
  env$.interactive_session <- function() TRUE
  prompts <- 0L
  env$.prompt_key <- function() { prompts <<- prompts + 1L; "synthetic-key" }
  check(identical(env$.resolve_api_key(), "synthetic-key") && store$writes == 1L && prompts == 1L,
    "first launch prompts once and stores the key")
  check(identical(Sys.getenv("OPENAI_API_KEY"), ""), "a saved key does not modify the process environment")
  check(identical(env$.resolve_api_key(), "synthetic-key") && prompts == 1L,
    "later launches reuse the saved key")
  Sys.setenv(OPENAI_API_KEY = "synthetic-env-key")
  before <- store$reads
  check(identical(env$.resolve_api_key(), "synthetic-env-key") && store$reads == before,
    "an existing environment key takes precedence without accessing the store")
  Sys.setenv(OPENAI_API_KEY = "invalid key")
  check(failed(env$.resolve_api_key(), "whitespace"), "an invalid environment key fails clearly")
  Sys.unsetenv("OPENAI_API_KEY")
  env$.prompt_key <- function() NULL
  check(failed(env$set_api_key(), "cancelled") && identical(store$key, "synthetic-key"),
    "cancelling replacement preserves the existing key")
  env$.prompt_key <- function() "invalid key"
  check(failed(env$set_api_key(), "whitespace") && store$writes == 1L,
    "invalid input is rejected before it is stored")
  env$.prompt_key <- function() "  replacement-key  "
  result <- withVisible(env$set_api_key())
  check(identical(store$key, "replacement-key") && is.null(result$value) && !result$visible,
    "replacement trims surrounding whitespace without returning the secret")
  store$fail_read <- TRUE
  check(failed(env$.resolve_api_key(), "Unlock the OS credential store") && store$writes == 2L,
    "a locked or unreadable existing key does not trigger replacement")
  store$fail_read <- FALSE; store$fail_list <- TRUE
  check(failed(env$.resolve_api_key(), "saved key was not changed"), "listing failure does not masquerade as a missing key")
  store$fail_list <- FALSE; store$fail_write <- TRUE
  check(failed(env$set_api_key(), "Could not save"), "store write errors give a recovery message")
  store$fail_write <- FALSE
  env$.interactive_session <- function() FALSE
  check(failed(env$set_api_key(), "interactively"), "noninteractive setup fails without prompting")
  env$.interactive_session <- function() TRUE
  env$forget_api_key()
  env$forget_api_key()
  check(is.null(store$key) && store$deletes == 1L, "forget removes only the saved key and handles an absent key")

  # Exercise production backend selection with a deliberate env-backend option.
  real_backend <- get(".credential_backend", ns)
  opts <- options(keyring_backend = "env")
  on.exit(options(opts), add = TRUE)
  check(failed(real_backend(), "secure OS credential store is unavailable"),
    "setup rejects keyring's nonpersistent environment fallback")
  options(opts)

  script <- system.file("app", "AISource.R", package = "RAssistant")
  check(nzchar(script) && file.exists(script), "the original assistant script is shipped in the package")
  opts <- options(RAssistant.load_only = "previous-value")
  on.exit(options(opts), add = TRUE)
  launcher <- env$.load_launcher(script)
  check(is.function(launcher) && "api_key" %in% names(formals(launcher)),
    "the bundled script loads its launcher without starting Shiny or reading a key")
  check(identical(getOption("RAssistant.load_only"), "previous-value"), "loading restores the prior option")
  bad_script <- tempfile(fileext = ".R")
  writeLines("stop('Synthetic source failure')", bad_script)
  check(failed(env$.load_launcher(bad_script), "Synthetic source failure") &&
    identical(getOption("RAssistant.load_only"), "previous-value"), "loading also restores options after an error")
  unlink(bad_script); options(opts)
  check(failed(env$.load_launcher(paste0(script, ".missing")), "script is missing"),
    "missing package assets report reinstallation guidance")

  key_reads <- 0L
  env$.resolve_api_key <- function() { key_reads <<- key_reads + 1L; "launch-key" }
  env$.in_rstudio <- function() FALSE
  check(failed(env$launch(), "Open RStudio") && key_reads == 0L,
    "launch outside RStudio does not read credentials")
  env$.in_rstudio <- function() TRUE
  env$.package_ui_info <- function() list(version="0.1.2")
  passed_key <- NULL
  running <- TRUE
  viewed <- character()
  env$.new_app_port <- function()20001L
  env$.viewer_later <- function(callback,delay)invisible(NULL)
  env$.show_viewer <- function(url) { viewed <<- c(viewed,url);invisible(NULL) }
  handle <- list(status = function() if (running) "running" else "success",
    url = function() "http://127.0.0.1:4321")
  env$.load_launcher <- function(path) function(api_key, package_info) { passed_key <<- api_key; handle }
  check(identical(env$launch(), handle) && identical(passed_key, "launch-key") &&
    identical(Sys.getenv("OPENAI_API_KEY"), ""), "launch supplies the key directly without setting a global environment variable")
  env$launch()
  check(key_reads == 1L && length(viewed)==2L && startsWith(tail(viewed,1L),"http://127.0.0.1:4321?ra_reopen=1&ra_view="),
    "repeated launch reopens the running server in Viewer without reading another key")
  env$launch()
  check(startsWith(tail(viewed,1L),"http://127.0.0.1:4321?ra_reopen=2&ra_view="),
    "each reopen uses a fresh page URL instead of a cached disconnected Viewer")
  env$.show_viewer <- function(url)stop("Synthetic Viewer failure")
  check(failed(env$launch(),"Synthetic Viewer failure") && identical(env$.app_state$handle,handle) && running,
    "a Viewer opening error preserves the running app so opening can be retried")
  env$.show_viewer <- function(url) { viewed <<- c(viewed,url);invisible(NULL) }
  check(failed(env$update_assistant(), "Close RAssistant"), "updating a running app is blocked")
  running <- FALSE

  install_root <- tempfile(); dir.create(install_root)
  lib <- file.path(install_root, "library with spaces")
  dir.create(lib); dir.create(file.path(lib, "RAssistant"))
  writeLines(c("Package: RAssistant","Version: 0.1.0","RemoteSha: original-sha"),
    file.path(lib,"RAssistant","DESCRIPTION"))
  on.exit(unlink(install_root, recursive = TRUE), add = TRUE)
  env$.package_path <- function() file.path(lib, "RAssistant")
  installed_lib <- NULL; installed_ref <- NULL
  env$.install_from_github <- function(lib, ref) {
    installed_lib <<- lib; installed_ref <<- ref
    list(path = normalizePath(file.path(lib, "RAssistant")), version = "0.1.1")
  }
  check(failed(env$update_assistant(NA_character_), "ref must be"), "invalid update references fail before installation")
  check(identical(env$update_assistant(ref = "v0.1.1", relaunch = FALSE)$version, "0.1.1") &&
    identical(normalizePath(installed_lib), normalizePath(lib)) && identical(installed_ref, "v0.1.1"),
    "updates target the loaded package library, including paths with spaces")
  check(key_reads == 1L && store$writes == 2L && store$deletes == 1L,
    "installation without relaunch never reads, writes or deletes credentials")
  fresh <- new.env(parent = emptyenv())
  fresh$launch <- function() "fresh-namespace-handle"
  reloads <- 0L
  env$.reload_namespace <- function() { reloads <<- reloads + 1L; fresh }
  result <- withVisible(env$launch())
  check(identical(result$value, "fresh-namespace-handle") && !result$visible &&
    reloads == 1L && key_reads == 1L,
    "launch after update delegates to the new namespace without using the old credentials or launcher")
  env$.app_state$update_queued <- TRUE
  check(failed(env$launch(), "update is in progress") && reloads == 1L,
    "launch cannot race with a queued installation")
  env$.app_state$update_queued <- FALSE
  running <- TRUE
  check(identical(env$launch(), handle) && reloads == 1L,
    "an app that is still running is never unloaded for a pending reload")
  running <- FALSE
  reload_env <- new.env(parent = ns)
  reload_env$.reload_namespace <- get(".reload_namespace", ns)
  environment(reload_env$.reload_namespace) <- reload_env
  reload_env$asNamespace <- function(name) reload_env
  reload_env$getNamespaceUsers <- function(ns) "DependentFixture"
  check(failed(reload_env$.reload_namespace(), "imported by: DependentFixture"),
    "an importing package prevents namespace unloading with a specific recovery message")
  reload_env$getNamespaceUsers <- function(ns) character()
  reload_env$.package_path <- env$.package_path
  reload_env$.stop_update_worker <- function() invisible(NULL)
  reload_env$unloadNamespace <- function(ns) invisible(NULL)
  reload_env$loadNamespace <- function(...) stop("Synthetic reload failure")
  check(failed(reload_env$.reload_namespace(), "Synthetic reload failure") &&
    failed(reload_env$.reload_namespace(), "Restart R"),
    "a genuine namespace-loading failure retains its cause and restart recovery guidance")
  env$.app_state$updated <- FALSE
  env$.install_from_github <- function(lib, ref) stop("Synthetic installation failure")
  check(failed(env$update_assistant(), "Synthetic installation failure") && !env$.app_state$updated,
    "an installation error does not report successful update")
  env$.install_from_github <- function(lib, ref) list(path = "wrong-place", version = "0.1.1")
  check(failed(env$update_assistant(), "verify the updated package location"), "an unexpected install location is rejected")

  # Activation queues only our fixed helper and refuses to overwrite typed code.
  control <- new.env(parent=ns)
  for(name in c(".prepare_console_capture",".activate_console_capture",".stop_console_capture")) {
    value<-get(name,ns);environment(value)<-control;assign(name,value,control)
  }
  control$.app_state<-new.env(parent=emptyenv())
  prepared<-0L;complete<-TRUE;commands<-list();context<-list(contents="unfinished <- ")
  controller<-list(id="fixture-token",status=function()list(ready=FALSE),
    prepare=function(){prepared<<-prepared+1L;TRUE},dispose=function()complete)
  control$.app_state$console<-controller
  control$`::`<-function(pkg,name){
    p<-as.character(substitute(pkg));n<-as.character(substitute(name))
    if(p=="rstudioapi" && n=="getConsoleEditorContext")return(function()context)
    if(p=="rstudioapi" && n=="sendToConsole")return(function(...){commands[[length(commands)+1L]]<<-list(...)})
    base::getExportedValue(p,n)
  }
  check(failed(control$.prepare_console_capture(),"Finish or clear the command") && !length(commands),
    "Console context activation never overwrites an unfinished Console command")
  context<-list(contents="")
  control$.prepare_console_capture()
  check(length(commands)==1L && identical(commands[[1L]][[1L]],
    'RAssistant:::.activate_console_capture("fixture-token")') &&
    isTRUE(commands[[1L]]$execute) && identical(commands[[1L]]$echo,FALSE) &&
    identical(commands[[1L]]$focus,FALSE),
    "activation runs only the token-bound helper without echoing code or changing focus")
  control$.activate_console_capture("old-token")
  check(prepared==0L,"stale activation requests cannot enable another app generation")
  control$.activate_console_capture("fixture-token")
  check(prepared==1L,"the current activation request prepares the capture controller")
  complete<-FALSE
  check(failed(control$.stop_console_capture(),"Finish the other output capture") &&
    identical(control$.app_state$console,controller),
    "pending sink cleanup blocks updates rather than popping another package's sink")
  complete<-TRUE;control$.stop_console_capture()
  check(is.null(control$.app_state$console),"completed capture cleanup releases the old app controller")
  cat(checks, "package checks passed\n")
}
main()
