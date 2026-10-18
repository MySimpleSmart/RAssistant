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

launch <- function() {
  if (!.in_rstudio()) stop("Open RStudio and run RAssistant::launch() in its Console.", call. = FALSE)
  if (isTRUE(.app_state$updated)) stop("RAssistant was updated. Restart R before launching the new version.", call. = FALSE)
  if (.app_running()) {
    message("RAssistant is already running. Use its Viewer tab, or Close it before reopening.")
    return(invisible(.app_state$handle))
  }
  launcher <- .load_launcher(file.path(.package_path(), "app", "AISource.R"))
  key <- .resolve_api_key()
  .app_state$handle <- launcher(api_key = key, package_info = .package_ui_info())
  invisible(.app_state$handle)
}
