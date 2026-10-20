# AISource.R is the maintained source. The package ships a byte-identical copy.
# Run after editing: Rscript tools/sync-app.R
# CI verifies it without modifying files: Rscript tools/sync-app.R --check
source_path <- "AISource.R"
target_path <- file.path("inst", "app", "AISource.R")
if (!file.exists(source_path)) stop("Run from the repository root.")
# Include the release bullets in the existing version metadata download. Keep
# NEWS as their single source so a future release cannot show stale changes.
description <- readLines("DESCRIPTION",warn=FALSE)
version <- unname(read.dcf("DESCRIPTION")[1L,"Version"])
news <- readLines("NEWS.md",warn=FALSE)
if(!identical(news[[1L]],paste0("# RAssistant ",version)))stop("NEWS must start with the current package version.")
next_release <- which(seq_along(news)>1L & grepl("^# ",news))
body <- news[seq.int(2L,if(length(next_release))next_release[[1L]]-1L else length(news))]
notes <- character()
for(line in trimws(body)){
  if(!nzchar(line))next
  if(startsWith(line,"- "))notes <- c(notes,substring(line,3L))
  else if(length(notes))notes[[length(notes)]] <- paste(notes[[length(notes)]],line)
}
if(!length(notes) || length(notes)>8L || any(nchar(notes)>600L))stop("Use up to eight short release bullets in NEWS.")
field <- grep("^Config/RAssistant/Changes:",description)
if(length(field)){
  after <- which(seq_along(description)>field[[1L]] & grepl("^[^[:space:]]",description))
  description <- description[-seq.int(field[[1L]],if(length(after))after[[1L]]-1L else length(description))]
}
while(length(description) && !nzchar(trimws(tail(description,1L))))description <- head(description,-1L)
metadata <- c(description,paste0("Config/RAssistant/Changes: ",notes[[1L]]),
  if(length(notes)>1L)paste0("    ",notes[-1L])else character())
if ("--check" %in% commandArgs(trailingOnly = TRUE)) {
  if (!file.exists(target_path) || !identical(unname(tools::md5sum(source_path)), unname(tools::md5sum(target_path)))) {
    stop("The packaged script is out of date. Run Rscript tools/sync-app.R and commit both files.")
  }
  if(!identical(readLines("DESCRIPTION",warn=FALSE),metadata))stop("Release notes metadata is out of date. Run Rscript tools/sync-app.R.")
  cat("Packaged script matches AISource.R.\n")
} else {
  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(source_path, target_path, overwrite = TRUE)) stop("Could not copy the assistant script.")
  writeLines(metadata,"DESCRIPTION")
  cat("Updated", target_path, "\n")
}
