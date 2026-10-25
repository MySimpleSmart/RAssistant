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
  for (trigger in c("manual_launch", "console_update", "ui_update")) for (attached in c(FALSE, TRUE)) {
    passed <- callr::r(function(lib, other_lib, attached, trigger) {
      data <- tempfile("reload-data-")
      on.exit(unlink(data, recursive = TRUE), add = TRUE)
      options(RAssistant.check_updates = FALSE, RAssistant.save_history = TRUE,
        RAssistant.history_dir = data, RAssistant.knowledge_dir = data)
      Sys.unsetenv("OPENAI_API_KEY")
      .libPaths(c(other_lib, lib, .libPaths()))
      ns <- loadNamespace("RAssistant", lib.loc = lib)
      if (attached) attachNamespace(ns)
      # Force old lazy bindings before simulating the changed installed app.
      for (name in ls(ns, all.names = TRUE)) get(name, envir = ns, inherits = FALSE)
      old_launch <- get("launch", ns)
      old_state <- get(".app_state", ns)
      old_state$last_port <- 22222L
      history <- get(".conversation_store", ns)()
      chat <- list(id = history$new_id(), title = "Before updating",
        messages = list(list(role = "You", type = "text", text = "Keep this chat", sequence = 1L)),
        draft = "Unsent follow-up", attachments = list())
      history$save(chat)
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
        patch(fresh, ".show_viewer", function(url)invisible(NULL))
        patch(fresh, ".resolve_api_key", function(...) {
          stopifnot(!old_state$update_queued)
          key_reads <<- key_reads + 1L
          "synthetic-reload-key"
        })
      }, action = "append")
      writeLines(c("function(api_key, package_info) {",
        "  list(status = function() 'running', url = function() 'http://127.0.0.1:4321', generation = 'updated',",
        "    version = package_info$version, key = api_key, port = package_info$port, restored = package_info$history$load())", "}"),
        file.path(lib, "RAssistant", "app", "AISource.R"))
      killed <- 0L
      old_updates <- get(".update_state", ns)
      old_updates$worker <- list(kill = function() { killed <<- killed + 1L })
      installs <- 0L; stops <- 0L
      patch(ns, ".install_from_github", function(lib, ref) {
        installs <<- installs + 1L
        list(path = normalizePath(file.path(lib, "RAssistant")),
          version = get(".installed_info", ns)()$version, sha = "synthetic-new-commit")
      })
      dependency <- asNamespace("shiny")
      assign("ra_workspace_sentinel", data.frame(value = 42L), envir = globalenv())
      if (trigger == "manual_launch") {
        old_state$updated <- TRUE
        handle <- old_launch()
      } else if (trigger == "console_update") {
        result <- get("update_assistant", ns)()
        stopifnot(result$updated, result$relaunched)
        handle <- get(".app_state", asNamespace("RAssistant"))$handle
      } else {
        running <- TRUE
        old_state$handle <- list(status = function() if (running) "running" else "success",
          stop = function() { running <<- FALSE; stops <<- stops + 1L })
        schedule <- get(".schedule_update", ns)
        stopifnot(schedule(), !schedule())
        later::run_now(timeoutSecs = .3)
        handle <- get(".app_state", asNamespace("RAssistant"))$handle
      }
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
        fresh_origin = handle$port>=20000L && handle$port<=29999L && handle$port!=22222L &&
          identical(get(".app_state",fresh)$last_port,handle$port),
        old_worker = killed == 1L && is.null(get(".update_state", ns)$worker),
        lifecycle = !old_state$update_queued && installs == as.integer(trigger != "manual_launch") &&
          stops == as.integer(trigger == "ui_update"),
        history = identical(handle$restored$id, chat$id) &&
          identical(handle$restored$draft, chat$draft) &&
          identical(handle$restored$messages[[1L]]$text, chat$messages[[1L]]$text),
        workspace = identical(get("ra_workspace_sentinel", globalenv()), data.frame(value = 42L)),
        dependency = identical(asNamespace("shiny"), dependency),
        attachment = identical("package:RAssistant" %in% search(), attached),
        stale_reference = identical(old_launch(), handle) &&
          identical(asNamespace("RAssistant"), fresh) && key_reads == 1L)
      if (attached) checks <- c(checks,
        attached_exports = identical(environment(get("launch", as.environment("package:RAssistant"))), fresh))
      if (!all(checks)) stop("Reload failed: ", paste(names(checks)[!checks], collapse = ", "))
      names(checks)
    }, args = list(lib, other_lib, attached, trigger),
      libpath = .libPaths(), user_profile = FALSE, system_profile = FALSE)
    for (name in passed) {
      cat("PASS", trigger, if (attached) "attached" else "namespace-only", name, "\n")
      checks <- checks + 1L
    }
  }
  cat(checks, "namespace reload checks passed\n")
}
main()
