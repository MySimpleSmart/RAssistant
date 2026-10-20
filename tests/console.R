# Use an actual top-level R prompt, so uncaught errors and global handlers are
# exercised outside testServer/callr's own condition handlers. No API calls.
main <- function() {
  root <- tempfile("ra-console-test-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  script <- file.path(root, "console-input.R")
  result <- file.path(root, "result.rds")
  output <- file.path(root, "stdout.txt")
  errors <- file.path(root, "stderr.txt")
  r <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rterm.exe" else "R")
  q <- function(x) paste(deparse(x), collapse = "")
  lines <- c(
    paste0(".libPaths(", q(.libPaths()), ")"),
    paste0("setwd(", q(root), ")"),
    "checks <- list()",
    "priorWarnings <- 0L",
    "globalCallingHandlers(warning = function(w) priorWarnings <<- priorWarnings + 1L)",
    "baseline <- globalCallingHandlers()",
    "factory <- getFromNamespace('.new_console_capture', 'RAssistant')",
    "cap <- factory()",
    "checks$default_off <- !cap$status()$enabled && !cap$status()$ready && sink.number() == 0L",
    "cat('BEFORE_ENABLE_SENTINEL\\n')",
    "cap$prepare()",
    "cap$start()",
    "cat('VISIBLE_CONSOLE_VALUE_42\\n')",
    "warning('VISIBLE_WARNING_17')",
    "message('VISIBLE_MESSAGE_23')",
    "stop('VISIBLE_ERROR_31')",
    "captured <- cap$snapshot()",
    "checks$output <- grepl('VISIBLE_CONSOLE_VALUE_42', captured, fixed=TRUE)",
    "checks$warning <- grepl('Warning: VISIBLE_WARNING_17', captured, fixed=TRUE) && priorWarnings == 1L",
    "checks$message <- grepl('Message: VISIBLE_MESSAGE_23', captured, fixed=TRUE)",
    "checks$error <- grepl('Error: VISIBLE_ERROR_31', captured, fixed=TRUE)",
    "checks$since_enable <- !grepl('BEFORE_ENABLE_SENTINEL', captured, fixed=TRUE)",
    "cat('synthetic-secret-key\\n')",
    "redacted <- cap$snapshot(redact='synthetic-secret-key')",
    "checks$redaction <- !grepl('synthetic-secret-key', redacted, fixed=TRUE) && grepl('[API key redacted]', redacted, fixed=TRUE)",
    "checks$repeated_start <- {cap$start(); sink.number() == 1L}",
    "path <- environment(cap$start)$state$path",
    "checks$stop <- cap$stop() && sink.number() == 0L && !file.exists(path) && identical(cap$snapshot(), '')",
    "cat('WHILE_OFF_SENTINEL\\n')",
    "cap$start()",
    "checks$fresh_start <- identical(cap$snapshot(), '')",
    "for(i in 1:1000) cat(sprintf('line-%04d %s\\n', i, strrep('x', 60)))",
    "bounded <- cap$snapshot()",
    "checks$bound <- nchar(bounded) <= 12000 && length(strsplit(bounded, '\\n')[[1]]) <= 200 && grepl('line-1000', bounded, fixed=TRUE) && !grepl('line-0001', bounded, fixed=TRUE)",
    "checks$spool_rotation <- file.info(environment(cap$start)$state$path)$size < 65536",
    "cat(strrep('z', 200000), 'LONG_LINE_TAIL', sep='')",
    "checks$long_line <- grepl('LONG_LINE_TAIL', cap$snapshot(), fixed=TRUE) && nchar(cap$snapshot()) <= 12000",
    "cat('\\nUTF8_', intToUtf8(c(0x43C,0x43E,0x43D,0x433,0x43E,0x43B)), '_END\\n', sep='')",
    "checks$utf8 <- grepl(paste0('UTF8_',intToUtf8(c(0x43C,0x43E,0x43D,0x433,0x43E,0x43B)),'_END'), cap$snapshot(), fixed=TRUE)",
    "inner <- tempfile(); sink(inner)",
    "checks$nested_stop <- !cap$stop() && cap$status()$pending && sink.number() == 2L",
    "cat('INNER_LOG_REMAINS\\n')",
    "sink()",
    "checks$nested_cleanup <- sink.number() == 0L && !cap$status()$pending && identical(cap$snapshot(), '') && grepl('INNER_LOG_REMAINS',paste(readLines(inner),collapse=''),fixed=TRUE)",
    "outer <- tempfile(); sink(outer)",
    "cap$start()",
    "cat('OUTER_CAPTURE_PRESERVED\\n')",
    "checks$previous_sink <- cap$stop() && sink.number() == 1L",
    "sink()",
    "checks$outer_output <- grepl('OUTER_CAPTURE_PRESERVED',paste(readLines(outer),collapse=''),fixed=TRUE)",
    "cap$start()",
    "sink()",
    "checks$user_popped <- !cap$status()$enabled && sink.number() == 0L",
    "cap$start()",
    "replacement <- tempfile(); {sink(); sink(replacement)}",
    "checks$reused_connection <- !cap$status()$enabled && cap$stop() && sink.number() == 1L",
    "sink()",
    "extra <- function(m) invisible(NULL)",
    "globalCallingHandlers(message=extra)",
    "cap$start()",
    "extraWarnings <- 0L; extraWarn <- function(w) extraWarnings <<- extraWarnings + 1L",
    "globalCallingHandlers(warning=extraWarn)",
    "warning('AFTER_NEW_HANDLER')",
    "checks$added_handler <- extraWarnings == 1L && priorWarnings == 2L && grepl('AFTER_NEW_HANDLER',cap$snapshot(),fixed=TRUE)",
    "checks$dispose <- cap$dispose() && sink.number() == 0L",
    "checks$handlers_preserved <- identical(globalCallingHandlers(), c(list(warning=extraWarn,message=extra), baseline))",
    "pending <- factory(); pending$start()",
    "environment(pending$status)$state$started_at <- Sys.time()-16",
    "checks$startup_timeout <- !pending$status()$enabled && sink.number() == 0L && grepl('could not start',pending$status()$notice,fixed=TRUE)",
    "pending$dispose()",
    "checks$callback_removed <- !any(grepl('RAssistant-console-', getTaskCallbackNames(), fixed=TRUE))",
    "checks$no_spool_files <- !length(list.files(tempdir(), pattern='^RAssistant-console-.*\\\\.log$'))",
    paste0("saveRDS(checks, ", q(result), ")"))
  writeLines(lines, script)
  code <- system2(r, c("--vanilla", "--slave", "--interactive"), stdin=script,
    stdout=output, stderr=errors)
  if (!identical(code, 0L) || !file.exists(result)) stop("Console subprocess failed; ", paste(readLines(errors),collapse="\n"))
  checks <- readRDS(result)
  stopifnot(length(checks) == 26L)
  if (!all(vapply(checks, isTRUE, logical(1)))) stop("Console checks failed: ",
    paste(names(checks)[!vapply(checks,isTRUE,logical(1))],collapse=", "))
  visible <- paste(readLines(output,warn=FALSE),collapse="\n")
  stderr <- paste(readLines(errors,warn=FALSE),collapse="\n")
  stopifnot(grepl("VISIBLE_CONSOLE_VALUE_42", visible, fixed=TRUE),
    grepl("VISIBLE_WARNING_17", stderr, fixed=TRUE),
    grepl("VISIBLE_MESSAGE_23", stderr, fixed=TRUE),
    grepl("VISIBLE_ERROR_31", stderr, fixed=TRUE))
  for (name in names(checks)) cat("PASS console", name, "\n")
  cat(length(checks)+1L,"Console capture checks passed\n")
}
main()
