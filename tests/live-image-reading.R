# Optional live smoke test. Uses a synthetic image, never project/Console data.
# Makes one API attempt only when explicitly enabled:
# AISOURCE_LIVE_VISION=1 Rscript tests/live-image-reading.R
main <- function() {
  if (!identical(Sys.getenv("AISOURCE_LIVE_VISION"),"1")) {
    cat("Skipped: set AISOURCE_LIVE_VISION=1 to enable one live API attempt.\n")
    return(invisible(NULL))
  }
  if (!nzchar(Sys.getenv("OPENAI_API_KEY")))stop("OPENAI_API_KEY is not configured.")
  source("tests/helpers.R",local=TRUE)
  app <- load_assistant()
  picture <- tempfile(fileext=".png")
  on.exit(unlink(picture),add=TRUE)
  grDevices::png(picture,width=1400,height=620,res=140)
  tryCatch({
    par(mar=rep(0,4),family="mono")
    plot.new();plot.window(xlim=c(0,1),ylim=c(0,1))
    lines <- c("SCREENSHOT: text and console excerpt", "", "Item       Estimate       Status",
      "alpha      -0.0052         measured", "beta        1.20e-03       measured",
      "gamma       NA            missing", "", "keep <- x >= -0.0052 & y != 10L",
      "Error in log(x): non-numeric argument", "Footer: sample Q7; no graph is shown")
    text(.035,seq(.94,.10,length.out=length(lines)),lines,adj=c(0,.5),cex=1.05)
  },finally=grDevices::dev.off())
  old_options <- options(ellmer_max_tries=1L,ellmer_timeout_s=60)
  on.exit(options(old_options),add=TRUE)
  answer <- app$ask_ai(app$image_reading_prompt(""),
    "UPLOADED IMAGES THIS TURN: 1",images=picture)
  critical <- c("-0.0052","1.20e-03","NA","keep <- x >= -0.0052 & y != 10L",
                "Error in log(x): non-numeric argument","Q7")
  ok <- vapply(critical,function(x)grepl(x,answer,fixed=TRUE),logical(1))
  cat("Critical strings read:",sum(ok),"/",length(ok),"\n")
  stopifnot(all(ok),!grepl("ACADEMIC_COMMENT",answer,fixed=TRUE))
  cat("Live image-reading smoke test passed.\n")
}
main()
