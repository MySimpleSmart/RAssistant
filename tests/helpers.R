# Load definitions without launching RStudio, reading credentials, or calling an API.
load_assistant <- function(path = "AISource.R", package_info = NULL) {
  tree <- parse(path)
  launcher <- tree[[1L]][[2L]][[2L]][[3L]]
  env <- new.env(parent = globalenv())
  # The production launcher receives its key as an argument. Helper extraction
  # does not read secrets; ellmer's NULL default supports the opt-in live test.
  env$api_key <- NULL
  env$package_info <- package_info
  for (statement in as.list(launcher[[3L]])[-1L]) {
    if (is.call(statement) && identical(statement[[1L]], as.name("<-")) &&
        is.symbol(statement[[2L]])) {
      name <- as.character(statement[[2L]])
      if (!name %in% c("packages", "initial", "app", "edit_store")) eval(statement, env)
    }
  }
  env$edit_store <- if(is.null(package_info$edits))env$source_edit_store()else package_info$edits
  env
}
