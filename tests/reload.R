# Exercise actual namespace unloading/loading in isolated R processes. Each
# process uses a disposable package copy, a fake app and synthetic credentials.
# No installs, browser, network or real credential-store calls are made.
main <- function() {
  root <- tempfile("ra-reload-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  lib <- file.path(root, "library with spaces")
  other_lib <- file.path(root, "first library")
  dir.create(lib); dir.create(other_lib)
  installed <- system.file(package = "RAssistant")
  stopifnot(nzchar(installed), file.copy(installed, lib, recursive = TRUE),
    file.copy(installed, other_lib, recursive = TRUE))
  checks <- 0L
  for (attached in c(FALSE, TRUE)) {
    passed <- callr::r(function(lib, other_lib, attached) {
      options(RAssistant.check_updates = FALSE)
      Sys.unsetenv("OPENAI_API_KEY")
      .libPaths(c(other_lib, lib, .libPaths()))
      ns <- loadNamespace("RAssistant", lib.loc = lib)
      if (attached) attachNamespace(ns)
      # Force old lazy bindings before simulating the changed installed app.
      for (name in ls(ns, all.names = TRUE)) get(name, envir = ns, inherits = FALSE)
      old_launch <- get("launch", ns)
      patch <- function(ns, name, value) {
        unlockBinding(name, ns)
        assign(name, value, envir = ns)
        lockBinding(name, ns)
      }
      patch(ns, ".in_rstudio", function() TRUE)
      patch(ns, ".resolve_api_key", function() stop("Stale credential resolver used"))
      patch(ns, ".package_ui_info", function() stop("Stale package UI helper used"))
      key_reads <- 0L
      setHook(packageEvent("RAssistant", "onLoad"), function(...) {
        fresh <- asNamespace("RAssistant")
        patch(fresh, ".in_rstudio", function() TRUE)
        patch(fresh, ".resolve_api_key", function() {
          key_reads <<- key_reads + 1L
          "synthetic-reload-key"
        })
      }, action = "append")
      writeLines(c("function(api_key, package_info) {",
        "  list(status = function() 'running', generation = 'updated',",
        "    version = package_info$version, key = api_key)", "}"),
        file.path(lib, "RAssistant", "app", "AISource.R"))
      killed <- 0L
      old_updates <- get(".update_state", ns)
      old_updates$worker <- list(kill = function() { killed <<- killed + 1L })
      old_state <- get(".app_state", ns)
      old_state$updated <- TRUE
      dependency <- asNamespace("shiny")
      assign("ra_workspace_sentinel", data.frame(value = 42L), envir = globalenv())
      handle <- old_launch()
      fresh <- asNamespace("RAssistant")
      checks <- c(
        namespace = !identical(ns, fresh),
        library = identical(normalizePath(getNamespaceInfo(fresh, "path")),
          normalizePath(file.path(lib, "RAssistant"))),
        new_helpers = identical(handle$generation, "updated") && key_reads == 1L &&
          identical(handle$key, "synthetic-reload-key") &&
          identical(handle$version, get(".installed_info", fresh)()$version),
        fresh_state = !get(".app_state", fresh)$updated &&
          identical(get(".app_state", fresh)$handle, handle),
        old_worker = killed == 1L && is.null(get(".update_state", ns)$worker),
        workspace = identical(get("ra_workspace_sentinel", globalenv()), data.frame(value = 42L)),
        dependency = identical(asNamespace("shiny"), dependency),
        attachment = identical("package:RAssistant" %in% search(), attached),
        stale_reference = identical(old_launch(), handle) &&
          identical(asNamespace("RAssistant"), fresh) && key_reads == 1L)
      if (attached) checks <- c(checks,
        attached_exports = identical(environment(get("launch", as.environment("package:RAssistant"))), fresh))
      if (!all(checks)) stop("Reload failed: ", paste(names(checks)[!checks], collapse = ", "))
      names(checks)
    }, args = list(lib, other_lib, attached),
      libpath = .libPaths(), user_profile = FALSE, system_profile = FALSE)
    for (name in passed) {
      cat("PASS", if (attached) "attached" else "namespace-only", name, "\n")
      checks <- checks + 1L
    }
  }
  cat(checks, "namespace reload checks passed\n")
}
main()
