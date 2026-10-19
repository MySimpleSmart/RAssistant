# Optional: two model requests with synthetic PDF/CSV content, no project files.
# AISOURCE_LIVE_DOCUMENTS=1 Rscript tests/live-document-reading.R
main <- function() {
  if(!identical(Sys.getenv("AISOURCE_LIVE_DOCUMENTS"),"1")) {
    cat("Skipped: set AISOURCE_LIVE_DOCUMENTS=1 for two live API requests.\n");return(invisible(NULL))
  }
  stopifnot(nzchar(Sys.getenv("OPENAI_API_KEY")),requireNamespace("pdftools",quietly=TRUE))
  source("tests/helpers.R",local=TRUE)
  app <- load_assistant();app$api_key <- Sys.getenv("OPENAI_API_KEY")
  app$initial <- app$empty_source_context();app$active_doc <- function(id=NULL)NULL
  old <- options(ellmer_max_tries=1L,ellmer_timeout_s=60);on.exit(options(old),add=TRUE)
  root <- tempfile("live-doc-");dir.create(root);on.exit(unlink(root,recursive=TRUE),add=TRUE)
  pdf <- file.path(root,"source.pdf");grDevices::pdf(pdf,width=7,height=4)
  plot.new();text(.5,.5,"LOCAL DOCUMENT TEST")
  plot.new();text(.5,.5,"Record identifier: ORBIT42");invisible(grDevices::dev.off())
  csv <- file.path(root,"data.csv");writeLines(c("id,score","001,42","002,73"),csv)
  payload <- function(path)paste0("data:application/octet-stream;base64,",jsonlite::base64_enc(readBin(path,"raw",file.info(path)$size)))
  ask <- app$ask_ai;answers <- character()
  app$ask_ai <- function(...) {answer<-ask(...);answers<<-c(answers,answer);answer}
  shiny::testServer(app$server,{
    session$flushReact()
    session$setInputs(document_upload=list(data=payload(pdf),filename="source.pdf",nonce=1L,epoch=state$upload_epoch))
    session$setInputs(document_upload=list(data=payload(csv),filename="data.csv",nonce=2L,epoch=state$upload_epoch))
    session$setInputs(chat_submit=list(text="Acknowledge the two uploaded file names only. I will ask a follow-up next; do not transcribe their contents yet.",nonce=1L))
    stopifnot(length(answers)==1L,!grepl("ORBIT42",answers[[1L]],fixed=TRUE),!length(state$attachments))
    cat("PASS live PDF/CSV upload completes without exposing the hidden follow-up value\n")
    session$setInputs(chat_submit=list(text="From page 2 of source.pdf give the exact record identifier. From data.csv give the score for id 002. Use the original uploaded contents.",nonce=2L))
    stopifnot(length(answers)==2L,grepl("ORBIT42",answers[[2L]],fixed=TRUE),grepl("73",answers[[2L]],fixed=TRUE))
    cat("PASS live follow-up reads the retained second PDF page and CSV row correctly\n")
  })
  cat("2 live document-reading checks passed\n")
}
main()
