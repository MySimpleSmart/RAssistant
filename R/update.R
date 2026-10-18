.install_from_github <- function(lib, ref) {
  # A clean process avoids installing over a namespace in use on Windows.
  # R's standard installation lock/rollback remains enabled.
  callr::r(function(lib, ref) {
    old <- options(warn = 2)
    on.exit(options(old), add = TRUE)
    remotes::install_github("MySimpleSmart/RAssistant", ref = ref,
      lib = lib, dependencies = NA, upgrade = "never", build_vignettes = FALSE)
    list(path = normalizePath(file.path(lib, "RAssistant"), mustWork = TRUE),
      version = as.character(utils::packageVersion("RAssistant", lib.loc = lib)))
  }, args = list(lib = lib, ref = ref),
  libpath = unique(c(lib, .libPaths())), show = TRUE)
}

update_assistant <- function(ref = "main") {
  if (!is.character(ref) || length(ref) != 1L || is.na(ref) || !nzchar(trimws(ref))) {
    stop("ref must be a branch, tag or commit reference.", call. = FALSE)
  }
  if (.app_running()) stop("Close RAssistant using its Close button before updating.", call. = FALSE)
  package_path <- normalizePath(.package_path(), mustWork = TRUE)
  lib <- dirname(package_path)
  if (file.access(lib, 2L) != 0L) {
    stop("The installed R library is not writable: ", lib,
      ". Install RAssistant into your personal R library first.", call. = FALSE)
  }
  result <- .install_from_github(lib, ref)
  if (!is.list(result) || !identical(result$path, package_path) ||
      !is.character(result$version) || length(result$version) != 1L) {
    stop("Could not verify the updated package location. Restart R and check the installation.", call. = FALSE)
  }
  .app_state$updated <- TRUE
  message("RAssistant ", result$version, " is installed in ", lib,
    ". Restart R before launching it. Your saved API key is unchanged.")
  invisible(result)
}
