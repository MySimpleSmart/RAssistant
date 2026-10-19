# Optional, three-request live check using only two synthetic screenshots.
# AISOURCE_LIVE_CONTEXT=1 Rscript tests/live-conversation-context.R
main <- function() {
  if (!identical(Sys.getenv("AISOURCE_LIVE_CONTEXT"),"1")) {
    cat("Skipped: set AISOURCE_LIVE_CONTEXT=1 for three live API requests.\n")
    return(invisible(NULL))
  }
  stopifnot(nzchar(Sys.getenv("OPENAI_API_KEY")))
  source("tests/helpers.R",local=TRUE)
  app <- load_assistant()
  app$api_key <- Sys.getenv("OPENAI_API_KEY")
  app$initial <- list(id="synthetic",path="",contents="")
  app$active_doc <- function(id=NULL) app$initial
  old <- options(ellmer_max_tries=1L,ellmer_timeout_s=60)
  on.exit(options(old),add=TRUE)
  refs <- c("FIELD-7926","FIELD-4681")
  paths <- vapply(seq_along(refs),function(i) {
    path <- tempfile(fileext=".png")
    grDevices::png(path,width=1000,height=300,res=110)
    tryCatch({
      par(mar=rep(0,4),family="mono");plot.new()
      text(.05,c(.8,.5,.2),c("SAMPLE DOCUMENT",paste("Reference:",refs[[i]]),"Status: complete"),adj=0,cex=1.4)
    },finally=grDevices::dev.off())
    path
  },character(1))
  on.exit(unlink(paths),add=TRUE)
  urls <- vapply(paths,function(p)paste0("data:image/png;base64,",
    jsonlite::base64_enc(readBin(p,"raw",file.info(p)$size))),character(1))
  real_ask <- app$ask_ai
  responses <- character()
  app$ask_ai <- function(...) {
    answer <- real_ask(...)
    responses <<- c(responses,answer)
    answer
  }
  shiny::testServer(app$server, {
    session$flushReact()
    session$setInputs(paste_image=list(image=urls[[1L]],filename="synthetic.png",nonce=1L,epoch=state$upload_epoch))
    session$setInputs(chat_submit=list(text="Read only the document title. I will ask about its details next. Do not transcribe the reference identifier yet.",nonce=1L))
    stopifnot(length(responses)==1L,!grepl(refs[[1L]],responses[[1L]],fixed=TRUE))
    cat("PASS first screenshot read without putting its reference in the answer\n")
    session$setInputs(chat_submit=list(text="What is the exact reference identifier in that screenshot? Read it from the earlier image.",nonce=2L))
    stopifnot(length(responses)==2L,grepl(refs[[1L]],responses[[2L]],fixed=TRUE))
    cat("PASS live text follow-up reads an earlier image without re-upload\n")
    session$setInputs(paste_image=list(image=urls[[2L]],filename="synthetic-next.png",nonce=2L,epoch=state$upload_epoch))
    session$setInputs(chat_submit=list(text="Compare this screenshot with the earlier one. Give both exact reference identifiers, distinguishing the earlier and the new image.",nonce=3L))
    stopifnot(length(responses)==3L,all(vapply(refs,function(ref)grepl(ref,responses[[3L]],fixed=TRUE),logical(1))))
    cat("PASS live new upload compares the two screenshots in the same conversation\n")
  })
  cat("3 live conversation-context checks passed\n")
}
main()
