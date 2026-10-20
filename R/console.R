# Opt-in capture of future R output and commands. The spool is temporary; only a bounded
# tail is read on Send. stderr is never redirected or muffled.
.new_console_capture <- function() {
  state <- new.env(parent = emptyenv())
  state$active <- FALSE
  state$disposed <- FALSE
  state$prepared <- FALSE
  state$busy <- FALSE
  state$path <- NULL
  state$connection <- NULL
  state$depth <- 0L
  state$notice <- ""
  state$started_at <- NULL
  state$boundary <- 0
  limit <- 12000L
  activation_helper <- call(":::", as.name("RAssistant"), as.name(".activate_console_capture"))
  call_text <- function(expr) {
    lines <- deparse(expr, width.cutoff = 100L, nlines = 31L)
    text <- paste(utils::head(lines, 30L), collapse = "\n+ ")
    if (length(lines) > 30L || nchar(text) > 4000L) {
      text <- paste0(substr(text, 1L, 4000L), " [command truncated]")
    }
    text
  }
  append_lines <- function(text) {
    flush(state$connection)
    con <- file(state$path, "a", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(text, con)
    flush(con)
  }
  mark_boundary <- function() state$boundary <- file.info(state$path)$size
  owns_connection <- function() {
    if (is.null(state$connection)) return(FALSE)
    tryCatch(identical(getConnection(as.integer(state$connection)), state$connection) &&
      isOpen(state$connection), error = function(e) FALSE)
  }
  open_sink <- function() {
    before <- getAllConnections()
    old <- options(encoding = "UTF-8")
    on.exit(options(old), add = TRUE)
    # Let sink own the connection, so a user popping our sink also closes it.
    sink(state$path, append = TRUE, split = TRUE)
    state$depth <- sink.number()
    state$connection <- getConnection(setdiff(getAllConnections(), before)[[1L]])
  }
  remove_spool <- function() {
    if (!is.null(state$path)) unlink(state$path)
    state$path <- NULL
    state$connection <- NULL
  }
  release_sink <- function() {
    if (owns_connection()) {
      # Never pop another package's output sink. The task callback retries
      # cleanup when that package releases its sink.
      if (sink.number() != state$depth) return(FALSE)
      sink()
    }
    remove_spool()
    TRUE
  }
  read_tail <- function() {
    if (is.null(state$path) || !file.exists(state$path)) return("")
    if (owns_connection()) flush(state$connection)
    size <- file.info(state$path)$size
    if (is.na(size) || size < 1) return("")
    con <- file(state$path, "rb")
    on.exit(close(con), add = TRUE)
    offset <- max(0, size - 128 * 1024)
    seek(con, offset, origin = "start")
    bytes <- readBin(con, "raw", min(size, 128 * 1024))
    # Drop an incomplete leading UTF-8 line when reading a large spool's tail.
    if (offset > 0) {
      newline <- match(as.raw(10), bytes)
      if (!is.na(newline) && newline < length(bytes)) bytes <- bytes[-seq_len(newline)]
    }
    text <- iconv(rawToChar(bytes[bytes != as.raw(0)]), "UTF-8", "UTF-8", sub = "")
    text <- gsub("\033\\[[0-?]*[ -/]*[@-~]", "", text, perl = TRUE)
    text <- gsub("\r\n?", "\n", text)
    text <- paste(utils::tail(strsplit(text, "\n", fixed = TRUE)[[1L]], 200L), collapse = "\n")
    if (nchar(text) > limit) text <- substr(text, nchar(text) - limit + 1L, nchar(text))
    trimws(text)
  }
  maintain <- function() {
    if (!state$active) return(release_sink())
    if (!owns_connection()) {
      state$active <- FALSE
      state$notice <- "Console capture was stopped by another output capture. Turn it on again to resume."
      remove_spool()
      return(TRUE)
    }
    flush(state$connection)
    size <- file.info(state$path)$size
    if (!is.na(size) && size > 64 * 1024 && sink.number() == state$depth) {
      unfinished <- size > state$boundary
      text <- read_tail()
      sink()
      con <- file(state$path, "w", encoding = "UTF-8")
      tryCatch(writeLines(text, con), finally = close(con))
      open_sink()
      if (unfinished) state$boundary <- 0 else mark_boundary()
    }
    TRUE
  }
  record <- function(kind, condition, calls = list()) {
    if (!state$active || state$busy || !owns_connection()) return(invisible(NULL))
    state$busy <- TRUE
    on.exit(state$busy <- FALSE, add = TRUE)
    # A failed logger must never replace the user's original error/warning.
    tryCatch({
      text <- paste0(kind, ": ", conditionMessage(condition))
      if (kind == "Error") {
        origin <- conditionCall(condition)
        if (!is.null(origin)) text <- c(text, paste0("[Error condition call] ", call_text(origin)))
        # Top-level errors do not trigger addTaskCallback. Preserve the stack
        # entry before unwinding, even when stop() supplies no condition call.
        # This can be an enclosing function, not the full Console expression.
        if (length(calls)) text <- c(text,
          paste0("[Error stack entry; may be an enclosing call] ", call_text(calls[[1L]])))
        text <- c(text, "[End failed Console task; full command may be unavailable]")
      }
      append_lines(text)
      if (kind == "Error") mark_boundary()
    }, error = function(e) NULL)
    invisible(NULL)
  }
  handlers <- list(error = function(e) record("Error", e, sys.calls()),
    warning = function(w) record("Warning", w), message = function(m) record("Message", m))
  # Registration must run as a real top-level Console expression. Registering
  # from a task callback only changes the registry, not the live handler stack.
  prepare <- function() {
    if (state$disposed || state$prepared) return(invisible(FALSE))
    globalCallingHandlers(handlers)
    state$prepared <- TRUE
    invisible(TRUE)
  }
  # On Close, deactivate immediately and remove our registry entries after the
  # Shiny handler returns. Any already-stacked logger is gated off as well.
  task <- function(expr, value, ok, visible) {
    if (state$disposed) {
      if (state$prepared) {
        current <- globalCallingHandlers()
        keep <- !vapply(current, function(fun) any(vapply(handlers,
          function(ours) identical(fun, ours), logical(1))), logical(1))
        globalCallingHandlers(NULL)
        if (any(keep)) globalCallingHandlers(current[keep])
        state$prepared <- FALSE
      }
      return(!isTRUE(tryCatch(maintain(), error = function(e) FALSE)))
    }
    tryCatch({
      internal <- is.call(expr) && identical(expr[[1L]], activation_helper)
      if (state$active && state$prepared && !state$busy && owns_connection() && !internal && isTRUE(ok)) {
        flush(state$connection)
        # R provides the completed expression after printing its value. Record
        # it after its output, with explicit boundaries; never infer it from
        # Source or try to re-evaluate it. Plotting and assignments may emit no
        # text at all; their completed expressions are still useful evidence.
        label <- if (file.info(state$path)$size > state$boundary)
          "[Completed Console command for preceding output]" else
          "[Completed Console command; no text output captured]"
        append_lines(c("", label, paste0("> ", call_text(expr)), "[End Console task]"))
        mark_boundary()
      }
      maintain()
    }, error = function(e) NULL)
    TRUE
  }
  callback <- addTaskCallback(task, name = paste0("RAssistant-console-", basename(tempfile())))
  stop_capture <- function() {
    state$active <- FALSE
    state$notice <- ""
    isTRUE(release_sink())
  }
  start_capture <- function() {
    if (state$disposed) stop("Reopen the assistant before enabling Console context.", call. = FALSE)
    if (state$active && owns_connection()) return(invisible(TRUE))
    if (!release_sink()) stop("Finish the other output capture before enabling Console context again.", call. = FALSE)
    state$path <- tempfile("RAssistant-console-", fileext = ".log")
    file.create(state$path)
    Sys.chmod(state$path, "0600")
    tryCatch(open_sink(), error = function(e) { remove_spool(); stop(e) })
    state$notice <- ""
    state$active <- TRUE
    state$started_at <- Sys.time()
    state$boundary <- 0
    invisible(TRUE)
  }
  snapshot <- function(redact = character()) {
    maintain()
    if (!state$active) return("")
    text <- read_tail()
    for (secret in redact[!is.na(redact) & nzchar(redact)]) {
      text <- gsub(secret, "[API key redacted]", text, fixed = TRUE)
    }
    if (nchar(text) > limit) text <- substr(text, nchar(text) - limit + 1L, nchar(text))
    text
  }
  status <- function() {
    maintain()
    if (state$active && !state$prepared &&
        as.numeric(difftime(Sys.time(), state$started_at, units = "secs")) > 15) {
      stop_capture()
      state$notice <- "Console context could not start. Clear the Console prompt and try enabling it again."
    }
    text <- if (state$active) read_tail() else ""
    list(enabled = state$active, ready = state$prepared,
      pending = !state$active && owns_connection(), notice = state$notice,
      lines = if (nzchar(text)) length(strsplit(text, "\n", fixed = TRUE)[[1L]]) else 0L)
  }
  dispose <- function() {
    state$disposed <- TRUE
    done <- stop_capture()
    if (!state$prepared && done) removeTaskCallback(callback)
    done
  }
  list(id = basename(tempfile("capture-")), prepare = prepare,
    start = start_capture, stop = stop_capture, snapshot = snapshot,
    status = status, dispose = dispose)
}

.activate_console_capture <- function(id) {
  capture <- .app_state$console
  if (is.null(capture) || !identical(capture$id, id)) return(invisible(FALSE))
  capture$prepare()
}

.prepare_console_capture <- function() {
  capture <- .app_state$console
  if (is.null(capture) || capture$status()$ready) return(invisible(NULL))
  context <- rstudioapi::getConsoleEditorContext()
  if (!is.list(context) || !is.character(context$contents) ||
      any(nzchar(trimws(context$contents)))) {
    stop("Finish or clear the command being typed in the Console, then enable Console context again.", call. = FALSE)
  }
  # Only our fixed registration helper is evaluated, with a per-launch token.
  rstudioapi::sendToConsole(paste0("RAssistant:::.activate_console_capture(",
    encodeString(capture$id, quote = '"'), ")"), execute = TRUE, echo = FALSE, focus = FALSE)
  invisible(NULL)
}

.stop_console_capture <- function() {
  capture <- .app_state$console
  if (!is.null(capture) && !isTRUE(capture$dispose())) {
    stop("Finish the other output capture before updating or reloading RAssistant.", call. = FALSE)
  }
  .app_state$console <- NULL
  invisible(NULL)
}
