# Run once in RStudio. This installs CRAN dependencies for the local script.
local({
  minimum <- c(shiny="1.14.0",ellmer="0.5.0",rstudioapi="0.0.0",
    commonmark="0.0.0",htmltools="0.0.0",jsonlite="0.0.0",
    callr="0.0.0",knitr="0.0.0",keyring="1.3.0")
  needed <- names(minimum)[vapply(names(minimum),function(pkg) {
    version <- tryCatch(utils::packageVersion(pkg),error=function(e)NULL)
    is.null(version) || version < minimum[[pkg]]
  },logical(1))]
  if(length(needed)) {
    utils::install.packages(needed,repos="https://cloud.r-project.org",
      type=if(.Platform$OS.type=="windows")"binary" else getOption("pkgType"))
  }
  for(pkg in names(minimum)) {
    tryCatch(loadNamespace(pkg),error=function(e) {
      stop("Cannot load '",pkg,"': ",conditionMessage(e),call.=FALSE)
    })
    if(getNamespaceVersion(pkg) < minimum[[pkg]]) {
      stop("Restart R after updating '",pkg,"', then run Setup.R again.",call.=FALSE)
    }
  }
  message("Setup complete. Open AISource.R in RStudio and click Source to launch.")
  invisible(TRUE)
})
