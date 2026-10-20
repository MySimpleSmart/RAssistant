.install_from_github <- function(lib, ref) {
  # Archive downloads avoid the GitHub REST API: its anonymous quota of 60
  # requests per hour is shared by every user behind a campus or office IP.
  sha <- if (grepl("^[a-f0-9]{40}$", ref)) ref else .github_main_commit(utils::download.file, ref)
  # A clean process avoids installing over a namespace in use on Windows.
  # R's standard installation lock/rollback remains enabled.
  callr::r(function(lib, ref, sha) {
    old <- options(warn = 2)
    on.exit(options(old), add = TRUE)
    archive <- tempfile(fileext = ".tar.gz")
    source <- tempfile("RAssistant-source-")
    on.exit(unlink(c(archive, source), recursive = TRUE), add = TRUE)
    utils::download.file(paste0("https://github.com/MySimpleSmart/RAssistant/archive/", sha, ".tar.gz"),
      archive, mode = "wb", quiet = TRUE)
    utils::untar(archive, exdir = source)
    pkg <- list.dirs(source, recursive = FALSE)
    if (length(pkg) != 1L || !file.exists(file.path(pkg, "DESCRIPTION"))) {
      stop("The downloaded package archive is invalid.", call. = FALSE)
    }
    # Record the commit, as install_github() did, so unchanged updates are detected.
    desc <- file.path(pkg, "DESCRIPTION")
    fields <- read.dcf(desc)
    fields <- fields[, !grepl("^Remote", colnames(fields)), drop = FALSE]
    write.dcf(cbind(fields, RemoteType = "github", RemoteUsername = "MySimpleSmart",
      RemoteRepo = "RAssistant", RemoteRef = ref, RemoteSha = sha), desc)
    remotes::install_deps(pkg, dependencies = NA, upgrade = "never", lib = lib)
    utils::install.packages(pkg, lib = lib, repos = NULL, type = "source")
    info <- utils::packageDescription("RAssistant", lib.loc = lib)
    list(path = normalizePath(file.path(lib, "RAssistant"), mustWork = TRUE),
      version = info$Version, sha = if (is.null(info$RemoteSha)) "" else info$RemoteSha)
  }, args = list(lib = lib, ref = ref, sha = sha),
  libpath = unique(c(lib, .libPaths())), show = TRUE)
}

update_assistant <- function(ref = "main", relaunch = TRUE) {
  if (!is.character(ref) || length(ref) != 1L || is.na(ref) || !nzchar(trimws(ref))) {
    stop("ref must be a branch, tag or commit reference.", call. = FALSE)
  }
  if (!is.logical(relaunch) || length(relaunch) != 1L || is.na(relaunch)) {
    stop("relaunch must be TRUE or FALSE.", call. = FALSE)
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
  .stop_console_capture()
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
      ". Your saved API key is unchanged.")
  } else {
    message("RAssistant is already up to date (", result$version, ").")
  }
  result$relaunched <- if (relaunch) .relaunch_after_update() else FALSE
  invisible(result)
}

.relaunch_after_update <- function() {
  if (!.in_rstudio()) return(FALSE)
  tryCatch({
    message("Reopening RAssistant...")
    launch()
    TRUE
  }, error = function(e) {
    message("The update succeeded, but RAssistant could not reopen: ", conditionMessage(e),
      "\nRun RAssistant::launch() in the Console to retry.")
    FALSE
  })
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
.update_check_interval <- 30 * 60

.stop_update_worker <- function() {
  if (!is.null(.update_state$worker)) try(.update_state$worker$kill(), silent = TRUE)
  .update_state$worker <- NULL
  invisible(NULL)
}

.github_main_commit <- function(download, ref = "main") {
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
  refs <- grep("^[a-f0-9]{40} ", records, value = TRUE, useBytes = TRUE)
  names <- substring(refs, 42L)
  # Prefer a branch, then an annotated tag's peeled commit, then a plain tag.
  for (name in paste0("refs/", c("heads/", "tags/", "tags/"), ref, c("", "^{}", ""))) {
    hit <- refs[names == name]
    if (length(hit) == 1L) return(substr(hit, 1L, 40L))
  }
  stop("Could not identify the current ", ref, " commit.", call. = FALSE)
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

.fetch_update_info <- function() {
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
  notes <- if("Config/RAssistant/Changes" %in% colnames(info))
    strsplit(unname(info[1L,"Config/RAssistant/Changes"]),"\n",fixed=TRUE)[[1L]] else character()
  notes <- trimws(notes[!is.na(notes)])
  list(version=unname(info[1L,"Version"]),notes=utils::head(substr(notes[nzchar(notes)],1L,600L),8L))
}

.fetch_latest_version <- function() .fetch_update_info()$version

.new_update_worker <- function() {
  # Passing the reader as an argument retains its helper environment. Startup
  # files can restore environment variables, so clear the key after R starts.
  callr::r_bg(function(fetch) {
    Sys.unsetenv("OPENAI_API_KEY")
    fetch()
  }, args = list(fetch = .fetch_update_info), libpath = .libPaths(),
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
  if (!force && is.finite(age) && age >= 0 && age < .update_check_interval) return(invisible(NULL))
  # Keep a known update visible while refreshing, including an offline retry.
  if (!identical(.update_state$result$status, "available")) {
    .update_state$result <- list(status = "checking")
  }
  .update_state$started_at <- Sys.time()
  .update_state$worker <- tryCatch(.new_update_worker(), error = function(e) NULL)
  if (is.null(.update_state$worker)) {
    if (!identical(.update_state$result$status, "available")) {
      .update_state$result <- list(status = "unavailable")
    }
    .update_state$checked_at <- Sys.time()
  }
  invisible(NULL)
}

.poll_update_check <- function() {
  worker <- .update_state$worker
  if (is.null(worker)) return(.update_state$result)
  alive <- tryCatch(worker$is_alive(),error=function(e)FALSE)
  if (alive && as.numeric(difftime(Sys.time(), .update_state$started_at, units = "secs")) < 10) {
    return(.update_state$result)
  }
  if (alive) try(worker$kill(), silent = TRUE)
  metadata <- if (alive) NULL else tryCatch(worker$get_result(), error = function(e) NULL)
  version <- if(is.list(metadata))metadata$version else metadata
  .update_state$worker <- NULL
  .update_state$checked_at <- Sys.time()
  .update_state$result <- if (.valid_version(version)) {
    current <- .installed_info()$version
    list(status = if (package_version(version) > package_version(current)) "available" else "current",
      version = version, current = current, notes=if(is.list(metadata))metadata$notes else character())
  } else if (identical(.update_state$result$status, "available")) {
    .update_state$result
  } else list(status = "unavailable")
  .update_state$result
}

.poll_update_status <- function() {
  if (isTRUE(getOption("RAssistant.check_updates", TRUE))) .start_update_check()
  .poll_update_check()
}

.update_poll_delay <- function() {
  # Only poll quickly while a background worker is running. Idle sessions wait
  # until the next network check is due; manual checks bypass that wait.
  if (!is.null(.update_state$worker)) return(250)
  if (!isTRUE(getOption("RAssistant.check_updates", TRUE))) {
    return(.update_check_interval * 1000)
  }
  age <- if (is.null(.update_state$checked_at)) .update_check_interval else
    as.numeric(difftime(Sys.time(), .update_state$checked_at, units = "secs"))
  if (!is.finite(age)) return(1000)
  max(1000, min(.update_check_interval, .update_check_interval - age) * 1000)
}

.schedule_update <- function() {
  if (isTRUE(.app_state$update_queued)) return(invisible(FALSE))
  .app_state$update_queued <- TRUE
  later::later(function() {
    on.exit(.app_state$update_queued <- FALSE, add = TRUE)
    installed <- tryCatch({
      if (.app_running()) .app_state$handle$stop()
      update_assistant(relaunch = FALSE)
      TRUE
    }, error = function(e) {
      message("Update failed: ", conditionMessage(e),
        "\nRetry RAssistant::update_assistant() in the Console, or reopen with RAssistant::launch().")
      FALSE
    })
    # launch() rejects a queued installation. Release the old namespace's lock
    # before it reloads the installed package and restores the saved chat.
    .app_state$update_queued <- FALSE
    if (installed) .relaunch_after_update()
  }, delay = 0.05)
  invisible(TRUE)
}

.package_ui_info <- function() {
  # Each new launch checks immediately, even if a previous session checked recently.
  .start_update_check(force = isTRUE(getOption("RAssistant.check_updates", TRUE)))
  list(version = .installed_info()$version, poll_update = .poll_update_status,
    update_poll_delay = .update_poll_delay,
    refresh_update = function() .start_update_check(force = TRUE),
    request_update = .schedule_update, history = .conversation_store(),
    knowledge = .knowledge_store(), console = .new_console_capture(),
    prepare_console = .prepare_console_capture)
}
