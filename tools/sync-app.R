# AISource.R is the maintained source. The package ships a byte-identical copy.
# Run after editing: Rscript tools/sync-app.R
# CI verifies it without modifying files: Rscript tools/sync-app.R --check
source_path <- "AISource.R"
target_path <- file.path("inst", "app", "AISource.R")
if (!file.exists(source_path)) stop("Run from the repository root.")
if ("--check" %in% commandArgs(trailingOnly = TRUE)) {
  if (!file.exists(target_path) || !identical(unname(tools::md5sum(source_path)), unname(tools::md5sum(target_path)))) {
    stop("The packaged script is out of date. Run Rscript tools/sync-app.R and commit both files.")
  }
  cat("Packaged script matches AISource.R.\n")
} else {
  dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(source_path, target_path, overwrite = TRUE)) stop("Could not copy the assistant script.")
  cat("Updated", target_path, "\n")
}
