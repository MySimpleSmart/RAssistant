.install_from_github <- function(lib, ref) {
  # A clean process avoids installing over a namespace in use on Windows.
  # R's standard installation lock/rollback remains enabled.
  callr::r(function(lib, ref) {
    old <- options(warn = 2)
    on.exit(options(old), add = TRUE)
    remotes::install_github("MySimpleSmart/RAssistant", ref = ref,
      lib = lib, dependencies = NA, upgrade = "never", build_vignettes = FALSE)
    info <- utils::packageDescription("RAssistant", lib.loc = lib)
    list(path = normalizePath(file.path(lib, "RAssistant"), mustWork = TRUE),
      version = info$Version, sha = if (is.null(info$RemoteSha)) "" else info$RemoteSha)
  }, args = list(lib = lib, ref = ref),
  libpath = unique(c(lib, .libPaths())), show = TRUE)
}

update_assistant <- function(ref = "main") {
  if (!is.character(ref) || length(ref) != 1L || is.na(ref) || !nzchar(trimws(ref))) {
    stop("ref must be a branch, tag or commit reference.", call. = FALSE)
  }
  if (.app_running()) stop("Close RAssistant using its Close button before updating.", call. = FALSE)
  before <- .installed_info()
  package_path <- normalizePath(.package_path(), mustWork = TRUE)
  lib <- dirname(package_path)
  if (file.access(lib, 2L) != 0L) {
    stop("The installed R library is not writable: ", lib,
      ". Install RAssistant into your personal R library first.", call. = FALSE)
  }
  # Resolve lazy bindings before replacing their on-disk R database. In
  # particular, launch() must remain callable until it reloads the namespace.
  ns <- asNamespace("RAssistant")
  for (name in ls(ns, all.names = TRUE)) get(name, envir = ns, inherits = FALSE)
  .stop_update_worker()
  result <- .install_from_github(lib, ref)
  if (!is.list(result) || !identical(result$path, package_path) ||
      !.valid_version(result$version)) {
    stop("Could not verify the updated package location. Restart R and check the installation.", call. = FALSE)
  }
  result$updated <- !identical(result$version, before$version) ||
    is.null(result$sha) || !nzchar(result$sha) || !identical(result$sha, before$sha)
  if (result$updated) {
    .app_state$updated <- TRUE
    message("Updated RAssistant ", before$version, " -> ", result$version, " in ", lib,
      ". Run RAssistant::launch() to open the new version. Your saved API key is unchanged.")
  } else {
    message("RAssistant is already up to date (", result$version, ").")
  }
  invisible(result)
}

.installed_info <- function() {
  info <- read.dcf(file.path(.package_path(), "DESCRIPTION"))
  list(version = unname(info[1L, "Version"]),
    sha = if ("RemoteSha" %in% colnames(info)) unname(info[1L, "RemoteSha"]) else "")
}

.valid_version <- function(version) {
  is.character(version) && length(version) == 1L && !is.na(version) &&
    nzchar(version) && !is.null(tryCatch(package_version(version), error = function(e) NULL))
}

.update_state <- new.env(parent = emptyenv())
.update_state$worker <- NULL
.update_state$result <- list(status = "disabled")
.update_state$checked_at <- NULL
.update_state$started_at <- NULL

.stop_update_worker <- function() {
  if (!is.null(.update_state$worker)) try(.update_state$worker$kill(), silent = TRUE)
  .update_state$worker <- NULL
  invisible(NULL)
}

.github_main_commit <- function(download) {
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  status <- suppressWarnings(download(
    "https://github.com/MySimpleSmart/RAssistant.git/info/refs?service=git-upload-pack",
    destfile = path, method = "libcurl", quiet = TRUE, mode = "wb", cacheOK = FALSE,
    headers = c("Cache-Control" = "no-cache, max-age=0", Pragma = "no-cache")))
  if (status != 0L) stop("Could not read the current package commit.", call. = FALSE)
  size <- file.info(path)$size
  if (is.na(size) || size < 4L || size > 1024^2) stop("Invalid package references.", call. = FALSE)
  bytes <- readBin(path, "raw", size)
  records <- character(); lengths <- integer(); offset <- 1L
  # Git's documented HTTP advertisement is a sequence of length-prefixed
  # packets, with NUL-separated capabilities on the first reference.
  # https://git-scm.com/docs/http-protocol#_smart_clients
  while (offset <= size) {
    if (offset + 3L > size) stop("Incomplete package references.", call. = FALSE)
    header <- rawToChar(bytes[offset:(offset + 3L)])
    if (!grepl("^[0-9a-f]{4}$", header)) stop("Invalid package references.", call. = FALSE)
    count <- strtoi(header, base = 16L)
    if (count != 0L && (count < 4L || offset + count - 1L > size)) {
      stop("Incomplete package references.", call. = FALSE)
    }
    payload <- if (count > 4L) bytes[(offset + 4L):(offset + count - 1L)] else raw()
    nul <- match(as.raw(0), payload, nomatch = length(payload) + 1L)
    records <- c(records, sub("\n$", "", rawToChar(payload[seq_len(nul - 1L)]), useBytes = TRUE))
    lengths <- c(lengths, count)
    offset <- offset + if (count == 0L) 4L else count
  }
  if (length(records) < 4L || !identical(records[[1L]], "# service=git-upload-pack") ||
      lengths[[2L]] != 0L || utils::tail(lengths, 1L) != 0L) {
    stop("Invalid package references.", call. = FALSE)
  }
  main <- grep("^[a-f0-9]{40} refs/heads/main$", records, value = TRUE, useBytes = TRUE)
  if (length(main) != 1L) stop("Could not identify the current main commit.", call. = FALSE)
  substr(main, 1L, 40L)
}

.download_version_file <- function(path, download = utils::download.file) {
  # Two bounded downloads fit within the existing ten-second worker deadline.
  old <- options(timeout = 4L)
  on.exit(options(old), add = TRUE)
  # Resolve main without REST API quotas or an installed Git executable, then
  # read immutable metadata. Raw branch caches can ignore cache-busting queries.
  commit <- .github_main_commit(download)
  url <- paste0("https://raw.githubusercontent.com/MySimpleSmart/RAssistant/", commit, "/DESCRIPTION")
  suppressWarnings(download(url,
    destfile = path, method = "libcurl", quiet = TRUE, mode = "wb", cacheOK = FALSE,
    headers = c("Cache-Control" = "no-cache, max-age=0", Pragma = "no-cache")))
}

.fetch_latest_version <- function() {
  path <- tempfile(fileext = ".dcf")
  on.exit(unlink(path), add = TRUE)
  if (.download_version_file(path) != 0L) stop("Update check unavailable.", call. = FALSE)
  if (file.info(path)$size > 65536L) stop("Invalid update metadata.", call. = FALSE)
  info <- read.dcf(path)
  if (nrow(info) != 1L || !"Version" %in% colnames(info) ||
      !"Package" %in% colnames(info) || !identical(unname(info[1L, "Package"]), "RAssistant") ||
      !.valid_version(unname(info[1L, "Version"]))) {
    stop("Invalid update metadata.", call. = FALSE)
  }
  unname(info[1L, "Version"])
}

.new_update_worker <- function() {
  # Passing the reader as an argument retains its helper environment. Startup
  # files can restore environment variables, so clear the key after R starts.
  callr::r_bg(function(fetch) {
    Sys.unsetenv("OPENAI_API_KEY")
    fetch()
  }, args = list(fetch = .fetch_latest_version), libpath = .libPaths(),
    user_profile = FALSE, system_profile = FALSE,
    env = c(callr::rcmd_safe_env(), OPENAI_API_KEY = ""),
    stdout = "|", stderr = "|", supervise = TRUE)
}

.start_update_check <- function(force = FALSE) {
  if (!force && !isTRUE(getOption("RAssistant.check_updates", TRUE))) {
    .stop_update_worker()
    .update_state$checked_at <- NULL
    .update_state$result <- list(status = "disabled")
    return(invisible(NULL))
  }
  if (!is.null(.update_state$worker)) {
    .poll_update_check()
    if (!is.null(.update_state$worker)) return(invisible(NULL))
  }
  age <- if (is.null(.update_state$checked_at)) Inf else
    as.numeric(difftime(Sys.time(), .update_state$checked_at, units = "secs"))
  if (!force && is.finite(age) && age >= 0 && age < 3600) return(invisible(NULL))
  .update_state$result <- list(status = "checking")
  .update_state$started_at <- Sys.time()
  .update_state$worker <- tryCatch(.new_update_worker(), error = function(e) NULL)
  if (is.null(.update_state$worker)) {
    .update_state$result <- list(status = "unavailable")
    .update_state$checked_at <- Sys.time()
  }
  invisible(NULL)
}

.poll_update_check <- function() {
  worker <- .update_state$worker
  if (is.null(worker)) return(.update_state$result)
  alive <- tryCatch(worker$is_alive(),error=function(e)FALSE)
  if (alive && as.numeric(difftime(Sys.time(), .update_state$started_at, units = "secs")) < 10) {
    return(list(status = "checking"))
  }
  if (alive) try(worker$kill(), silent = TRUE)
  version <- if (alive) NULL else tryCatch(worker$get_result(), error = function(e) NULL)
  .update_state$worker <- NULL
  .update_state$checked_at <- Sys.time()
  .update_state$result <- if (.valid_version(version)) {
    current <- .installed_info()$version
    list(status = if (package_version(version) > package_version(current)) "available" else "current",
      version = version, current = current)
  } else list(status = "unavailable")
  .update_state$result
}

.schedule_update <- function() {
  if (isTRUE(.app_state$update_queued)) return(invisible(FALSE))
  .app_state$update_queued <- TRUE
  later::later(function() {
    on.exit(.app_state$update_queued <- FALSE, add = TRUE)
    tryCatch({
      if (.app_running()) .app_state$handle$stop()
      update_assistant()
    }, error = function(e) {
      message("Update failed: ", conditionMessage(e),
        "\nRetry RAssistant::update_assistant() in the Console, or reopen with RAssistant::launch().")
    })
  }, delay = 0.05)
  invisible(TRUE)
}

.package_ui_info <- function() {
  .start_update_check()
  list(version = .installed_info()$version, poll_update = .poll_update_check,
    refresh_update = function() .start_update_check(force = TRUE),
    request_update = .schedule_update, history = .conversation_store())
}
