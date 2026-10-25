.app_state <- new.env(parent = emptyenv())
.app_state$handle <- NULL
.app_state$updated <- FALSE
.app_state$update_queued <- FALSE
.app_state$console <- NULL
.app_state$viewer_generation <- 0L
.app_state$viewer_request <- NULL
.app_state$last_port <- NULL

.package_path <- function() getNamespaceInfo(asNamespace("RAssistant"), "path")
.in_rstudio <- function() rstudioapi::isAvailable()
.show_viewer <- function(url) rstudioapi::viewer(url)
.viewer_later <- function(callback, delay) later::later(callback, delay)
.app_running <- function() {
  !is.null(.app_state$handle) && isTRUE(tryCatch(identical(.app_state$handle$status(), "running"), error = function(e) FALSE))
}

.new_app_port <- function() {
  # Shiny otherwise reuses its last port. RStudio matches disconnect messages
  # by origin, so an old page can clear a new app on that same origin.
  # This range also differs from the default 3000-8000 used by older releases.
  previous <- unique(c(.app_state$last_port, getOption("shiny.port")))
  for (attempt in seq_len(20L)) {
    port <- httpuv::randomPort(min = 20000L, max = 29999L)
    if (!port %in% previous) return(port)
  }
  stop("Could not select a fresh Viewer port. Run RAssistant::launch() again.", call. = FALSE)
}

.cancel_viewer_open <- function() {
  .app_state$viewer_request <- NULL
  invisible(NULL)
}

.viewer_ready <- function(token) {
  request <- .app_state$viewer_request
  if (is.null(request) || !is.character(token) || length(token) != 1L || is.na(token) ||
      !identical(token, request$token) || !identical(request$handle, .app_state$handle) || !.app_running()) {
    return(invisible(FALSE))
  }
  if(!isTRUE(request$connected))request$settled <- FALSE
  request$connected <- TRUE
  .app_state$viewer_request <- request
  invisible(TRUE)
}

.viewer_disconnected <- function(token) {
  request <- .app_state$viewer_request
  if (!is.null(request) && identical(token, request$token)) {
    request$connected <- FALSE
    request$loading <- FALSE
    request$settled <- FALSE
    .app_state$viewer_request <- request
  }
  invisible(NULL)
}

.viewer_loading <- function(token) {
  request <- .app_state$viewer_request
  if (is.null(request) || !is.character(token) || length(token) != 1L || is.na(token) ||
      !identical(token, request$token) || !identical(request$handle, .app_state$handle) || !.app_running()) {
    return(invisible(FALSE))
  }
  request$loading <- TRUE
  .app_state$viewer_request <- request
  invisible(TRUE)
}

.open_viewer <- function(handle, reopen = FALSE) {
  .cancel_viewer_open()
  generation <- .app_state$viewer_generation
  if (reopen) .app_state$viewer_generation <- if (is.null(generation)) 1L else generation + 1L
  id <- basename(tempfile("viewer-request-"))
  .app_state$viewer_request <- list(id = id, handle = handle, attempts = 0L, token = NULL,
    connected = FALSE, settled = FALSE, loading = FALSE, loading_checks = 0L)
  current <- function() {
    request <- .app_state$viewer_request
    !is.null(request) && identical(request$id, id) && identical(request$handle, .app_state$handle) && .app_running()
  }
  failed <- function(detail = NULL) {
    .cancel_viewer_open()
    message("RAssistant is running, but its Viewer connection was not confirmed.",
      if (!is.null(detail)) paste0(" ", detail) else "",
      "\nRun RAssistant::launch() in the Console to reopen it.")
  }
  show <- function() {
    if (!current()) return(invisible(NULL))
    request <- .app_state$viewer_request
    request$attempts <- request$attempts + 1L
    request$token <- basename(tempfile("viewer-page-"))
    request$connected <- FALSE
    request$settled <- FALSE
    request$loading <- FALSE
    request$loading_checks <- 0L
    .app_state$viewer_request <- request
    url <- handle$url()
    query <- paste0(if (reopen) paste0("ra_reopen=", .app_state$viewer_generation, "&") else "",
      "ra_view=", request$token)
    tryCatch(.show_viewer(paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?", query)),
      error = function(e) {.cancel_viewer_open(); stop(e)})
    .viewer_later(check, 2)
    invisible(NULL)
  }
  check <- function() {
    if (!current()) return(invisible(NULL))
    if (isTRUE(.app_state$viewer_request$connected)) {
      if(isTRUE(.app_state$viewer_request$settled)) .cancel_viewer_open() else {
        request <- .app_state$viewer_request
        request$settled <- TRUE
        .app_state$viewer_request <- request
        .viewer_later(check, 2)
      }
      return(invisible(NULL))
    }
    # The page has reached the server but may still be loading dependencies
    # (e.g. MathJax). Do not repeatedly tear down a legitimate slow load.
    request <- .app_state$viewer_request
    if (isTRUE(request$loading) && request$loading_checks < 10L) {
      request$loading_checks <- request$loading_checks + 1L
      .app_state$viewer_request <- request
      .viewer_later(check, 2)
      return(invisible(NULL))
    }
    if (.app_state$viewer_request$attempts >= 3L) {
      failed()
      return(invisible(NULL))
    }
    tryCatch(show(), error = function(e) failed(conditionMessage(e)))
    invisible(NULL)
  }
  show()
  invisible(handle)
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
  last_port <- .app_state$last_port
  position <- match("package:RAssistant", search())
  .stop_update_worker()
  .stop_console_capture()
  .cancel_viewer_open()
  tryCatch({
    unloadNamespace(current)
    fresh <- loadNamespace("RAssistant", lib.loc = lib)
    fresh_state <- get(".app_state", envir = fresh)
    fresh_state$last_port <- last_port
    if (!is.na(position)) attachNamespace(fresh, pos = position)
    fresh
  }, error = function(e) {
    stop("Could not reload the updated RAssistant: ", conditionMessage(e),
      "\nRestart R, then run RAssistant::launch().", call. = FALSE)
  })
}

launch <- function(provider = NULL, model = NULL, base_url = NULL) {
  if (!.in_rstudio()) stop("Open RStudio and run RAssistant::launch() in its Console.", call. = FALSE)
  if (isTRUE(.app_state$update_queued)) {
    stop("An update is in progress. The assistant will reopen when it finishes.", call. = FALSE)
  }
  if (.app_running()) {
    if (!is.null(provider) || !is.null(model) || !is.null(base_url))
      stop("Use More actions (...) > API settings to change the running connection.", call. = FALSE)
    .open_viewer(.app_state$handle, reopen = TRUE)
    return(invisible(.app_state$handle))
  }
  if (isTRUE(.app_state$updated)) {
    fresh <- .reload_namespace()
    return(invisible(get("launch", envir = fresh, inherits = FALSE)(provider=provider,model=model,base_url=base_url)))
  }
  launcher <- .load_launcher(file.path(.package_path(), "app", "AISource.R"))
  profile <- .selected_api_profile(provider, model, base_url)
  key <- tryCatch(.resolve_api_key(profile$provider, base_url = profile$base_url, prompt = FALSE),
    error = function(e) {message(conditionMessage(e)); NULL})
  port <- .new_app_port()
  info <- .package_ui_info()
  info$api <- .new_api_settings(profile, key)
  info$port <- port
  info$viewer_ready <- .viewer_ready
  info$viewer_loading <- .viewer_loading
  info$viewer_disconnected <- .viewer_disconnected
  info$viewer_closed <- .cancel_viewer_open
  .app_state$console <- info$console
  .app_state$handle <- tryCatch(launcher(api_key = key, package_info = info), error = function(e) {
    .stop_console_capture()
    stop(e)
  })
  .app_state$last_port <- info$port
  if (!.app_running()) stop("RAssistant stopped during startup. Run RAssistant::launch() again.", call. = FALSE)
  # Open only after startApp has returned, its handle is retained and RStudio's
  # app-start hooks have finished. Confirm the real page's Shiny connection.
  .open_viewer(.app_state$handle)
  invisible(.app_state$handle)
}
