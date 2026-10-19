# Load definitions without launching RStudio, reading credentials, or calling an API.
load_assistant <- function(path = "AISource.R") {
  tree <- parse(path)
  launcher <- tree[[1L]][[2L]][[2L]][[3L]]
  env <- new.env(parent = globalenv())
  for (statement in as.list(launcher[[3L]])[-1L]) {
    if (is.call(statement) && identical(statement[[1L]], as.name("<-")) &&
        is.symbol(statement[[2L]])) {
      name <- as.character(statement[[2L]])
      if (!name %in% c("packages", "initial", "app")) eval(statement, env)
    }
  }
  env$api_key <- "synthetic-test-key"
  env
}
