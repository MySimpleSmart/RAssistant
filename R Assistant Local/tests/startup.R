# Standalone first-launch checks. No real passwords, model calls or dialogs.
source("tests/helpers.R")
main <- function() {
  previous_key <- Sys.getenv("OPENAI_API_KEY",unset=NA_character_)
  on.exit(if(is.na(previous_key))Sys.unsetenv("OPENAI_API_KEY") else
    Sys.setenv(OPENAI_API_KEY=previous_key),add=TRUE)
  Sys.unsetenv("OPENAI_API_KEY")
  old_options <- options(AISource.load_only=TRUE)
  on.exit(options(old_options),add=TRUE)
  changes <- list()
  patch <- function(pkg,name,value) {
    ns <- asNamespace(pkg)
    changes[[length(changes)+1L]] <<- list(ns=ns,name=name,value=get(name,ns))
    unlockBinding(name,ns);assign(name,value,ns);lockBinding(name,ns)
  }
  restore <- function() {
    for(item in rev(changes)) {
      unlockBinding(item$name,item$ns);assign(item$name,item$value,item$ns);lockBinding(item$name,item$ns)
    }
    changes <<- list()
  }
  on.exit(restore(),add=TRUE)
  checks <- 0L
  check <- function(ok,description) {
    if(!isTRUE(ok))stop(description,call.=FALSE)
    checks <<- checks+1L;cat("PASS",description,"\n")
  }
  failed <- function(expr,text) {
    result <- tryCatch(force(expr),error=identity)
    inherits(result,"error") && grepl(text,conditionMessage(result),fixed=TRUE)
  }
  saved <- NULL;prompts <- 0L;reads <- 0L;writes <- 0L;starts <- 0L;opens <- 0L
  fail_list <- FALSE;fail_get <- FALSE;fail_set <- FALSE;fail_prompt <- FALSE
  available <- TRUE;backend_name <- "wincred";answer <- "synthetic-first-key"
  private <- NULL;factory_fail <- FALSE
  backend <- list(name="wincred",list=function(service) {
    stopifnot(identical(service,"RAssistant/OpenAI"))
    if(fail_list)stop("Synthetic list failure")
    data.frame(username=if(is.null(saved))character()else "default")
  },get=function(service,username) {
    reads <<- reads+1L
    if(fail_get)stop("Synthetic read failure")
    saved
  },set_with_value=function(service,username,password) {
    stopifnot(identical(service,"RAssistant/OpenAI"),identical(username,"default"))
    if(fail_set)stop("Do not expose the supplied password")
    saved <<- password;writes <<- writes+1L
  })
  patch("rstudioapi","isAvailable",function(...)available)
  patch("rstudioapi","getSourceEditorContext",function(...)NULL)
  patch("rstudioapi","askForPassword",function(prompt) {
    prompts <<- prompts+1L
    stopifnot(grepl("OpenAI API key",prompt,fixed=TRUE))
    if(fail_prompt)stop("Synthetic dialog failure")
    answer
  })
  patch("keyring","default_backend",function(...) {
    opens <<- opens+1L
    if(factory_fail)stop("Synthetic unavailable backend")
    backend$name <- backend_name
    backend
  })
  patch("shiny","startApp",function(appDir,launch.browser,host,quiet,...) {
    starts <<- starts+1L
    stopifnot(identical(host,"127.0.0.1"),is.function(launch.browser),quiet)
    private <<- environment(appDir$serverFuncSource())
    list(status=function()"running",stop=function()invisible(NULL))
  })
  globals <- ls(globalenv(),all.names=TRUE)
  launcher <- source("AISource.R",local=new.env(parent=globalenv()),encoding="UTF-8")$value
  check(is.function(launcher) && prompts==0L && opens==0L,
    "loading definitions neither reads credentials nor opens a dialog")
  available <- FALSE
  check(failed(launcher(),"inside RStudio") && opens==0L,
    "launch outside RStudio fails before reading credentials")
  available <- TRUE
  launcher()
  check(prompts==1L && writes==1L && starts==1L && identical(saved,answer),
    "the actual first launcher prompts once, securely saves the key and starts the app")
  check(identical(Sys.getenv("OPENAI_API_KEY"),"") &&
    !grepl(answer,as.character(private$ui),fixed=TRUE),
    "the key is absent from the environment and rendered UI")
  check(identical(private$api_key,saved),"the private launcher retains the key for API credentials")
  opts <- options(lifecycle_verbosity="error")
  client <- private$create_ai_client("Synthetic instructions")
  options(opts)
  check(inherits(client,"Chat") && identical(client$get_model(),"gpt-6.1-sol"),
    "the saved key creates a current ellmer client without deprecated arguments or a network call")
  launcher()
  check(prompts==1L && writes==1L && reads==1L && starts==2L,
    "a later launch reads the saved key without prompting again")
  fresh <- source("AISource.R",local=new.env(parent=globalenv()),encoding="UTF-8")$value
  fresh()
  check(prompts==1L && identical(private$api_key,saved),
    "sourcing a fresh copy reuses the OS credential rather than transient launcher state")
  before <- opens
  Sys.setenv(OPENAI_API_KEY="synthetic-environment-key");launcher()
  check(opens==before && identical(private$api_key,"synthetic-environment-key"),
    "a configured environment key takes precedence without opening the store")
  Sys.setenv(OPENAI_API_KEY="invalid key")
  check(failed(launcher(),"whitespace") && opens==before,
    "invalid environment credentials fail before store access")
  Sys.unsetenv("OPENAI_API_KEY")
  saved <- NULL;answer <- NULL;before <- starts
  check(failed(launcher(),"cancelled") && is.null(saved) && starts==before,
    "cancelling first-time setup saves nothing and starts no app")
  answer <- "   "
  check(failed(launcher(),"nonempty") && is.null(saved),"blank input is rejected before storage")
  answer <- "invalid key"
  check(failed(launcher(),"without whitespace") && is.null(saved),"internal whitespace is rejected before storage")
  answer <- "  synthetic-trimmed-key  ";launcher()
  check(identical(saved,"synthetic-trimmed-key"),"surrounding whitespace is trimmed without exposing the key")
  before <- prompts;fail_list <- TRUE
  check(failed(launcher(),"saved key was not changed") && prompts==before,
    "store-list failures do not masquerade as an absent key")
  fail_list <- FALSE;fail_get <- TRUE
  check(failed(launcher(),"Cannot retrieve") && prompts==before,
    "an unreadable existing credential does not trigger replacement")
  fail_get <- FALSE;saved <- ""
  check(failed(launcher(),"saved API key is empty") && prompts==before,
    "an invalid saved credential reports its own recovery error")
  saved <- NULL;fail_set <- TRUE
  check(failed(launcher(),"Could not save") && is.null(saved),
    "a failed secure write does not fall back to a plaintext file")
  fail_set <- FALSE;fail_prompt <- TRUE
  check(failed(launcher(),"hidden password prompt") && is.null(saved),
    "password-dialog errors report that no key was saved")
  fail_prompt <- FALSE;backend_name <- "env"
  check(failed(launcher(),"secure OS credential store is unavailable"),
    "nonpersistent environment backends are rejected")
  backend_name <- "wincred";factory_fail <- TRUE
  check(failed(launcher(),"Cannot open the OS credential store"),
    "credential backend initialization errors have actionable diagnostics")
  check(identical(globals,ls(globalenv(),all.names=TRUE)),
    "sourcing and launching keep helper functions and credentials out of the global workspace")
  restore()

  # A real, disposable Windows Credential Manager record, with synthetic data.
  if(.Platform$OS.type=="windows") {
    opts <- options(keyring_backend="wincred")
    on.exit(options(opts),add=TRUE)
    native <- load_assistant()
    native$key_service <- paste0("RAssistant-local-test-",Sys.getpid())
    native$prompt_api_key <- function()"synthetic-windows-test-key"
    kb <- keyring::backend_wincred$new()
    on.exit(try(kb$delete(native$key_service,username="default"),silent=TRUE),add=TRUE)
    check(identical(native$resolve_api_key(),"synthetic-windows-test-key"),
      "first-time local setup writes a synthetic key to native Windows Credential Manager")
    another <- load_assistant();another$key_service <- native$key_service
    another$prompt_api_key <- function()stop("Unexpected repeated prompt")
    check(identical(another$resolve_api_key(),"synthetic-windows-test-key"),
      "a fresh local launcher reads the native Windows credential without another prompt")
    restored <- callr::r(function(service) {
      keyring::backend_wincred$new()$get(service,username="default")
    },args=list(native$key_service),libpath=.libPaths(),user_profile=FALSE,system_profile=FALSE,
      env=c(callr::rcmd_safe_env(),OPENAI_API_KEY=""))
    check(identical(restored,"synthetic-windows-test-key"),
      "the native saved key survives a completely new Windows R process")
  }
  cat(checks,"standalone startup checks passed\n")
}
main()
