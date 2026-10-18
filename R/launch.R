.app_state <- new.env(parent = emptyenv())
.app_state$handle <- NULL
.app_state$updated <- FALSE
.app_state$update_queued <- FALSE

.package_path <- function() getNamespaceInfo(asNamespace("RAssistant"), "path")
.in_rstudio <- function() rstudioapi::isAvailable()
.app_running <- function() {
  !is.null(.app_state$handle) && identical(.app_state$handle$status(), "running")
}

.load_launcher <- function(path) {
  if (!file.exists(path)) stop("The packaged assistant script is missing. Reinstall RAssistant.", call. = FALSE)
  old <- options(RAssistant.load_only = TRUE)
  on.exit(options(old), add = TRUE)
  launcher <- source(path, local = new.env(parent = globalenv()), keep.source = FALSE)$value
  if (!is.function(launcher)) stop("The packaged assistant launcher is invalid. Reinstall RAssistant.", call. = FALSE)
  launcher
}

.reload_namespace <- function() {
  current <- asNamespace("RAssistant")
  # A previously captured launch() may outlive its namespace. Delegate to the
  # registered namespace without unloading an app started by the new version.
  if (!identical(current, environment(.reload_namespace))) return(current)
  users <- getNamespaceUsers(current)
  if (length(users)) {
    stop("Cannot reload RAssistant while it is imported by: ", paste(users, collapse = ", "),
      ". Restart R, then run RAssistant::launch().", call. = FALSE)
  }
  lib <- dirname(.package_path())
  position <- match("package:RAssistant", search())
  .stop_update_worker()
  tryCatch({
    unloadNamespace(current)
    fresh <- loadNamespace("RAssistant", lib.loc = lib)
    if (!is.na(position)) attachNamespace(fresh, pos = position)
    fresh
  }, error = function(e) {
    stop("Could not reload the updated RAssistant: ", conditionMessage(e),
      "\nRestart R, then run RAssistant::launch().", call. = FALSE)
  })
}

launch <- function() {
  if (!.in_rstudio()) stop("Open RStudio and run RAssistant::launch() in its Console.", call. = FALSE)
  if (isTRUE(.app_state$update_queued)) {
    stop("An update is in progress. Wait for it to finish, then run RAssistant::launch().", call. = FALSE)
  }
  if (.app_running()) {
    message("RAssistant is already running. Use its Viewer tab, or Close it before reopening.")
    return(invisible(.app_state$handle))
  }
  if (isTRUE(.app_state$updated)) {
    fresh <- .reload_namespace()
    return(invisible(get("launch", envir = fresh, inherits = FALSE)()))
  }
  launcher <- .load_launcher(file.path(.package_path(), "app", "AISource.R"))
  key <- .resolve_api_key()
  .app_state$handle <- launcher(api_key = key, package_info = .package_ui_info())
  invisible(.app_state$handle)
}
