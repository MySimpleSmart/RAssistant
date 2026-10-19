# Inspect the actual image-request payload without calling an external API.
# Run from the project directory: Rscript tests/image-reading.R
source("tests/helpers.R")
app <- load_assistant()
checks <- 0L
check <- function(ok,description) {
  if(!isTRUE(ok))stop(description,call.=FALSE)
  checks <<- checks+1L;cat("PASS",description,"\n")
}
app$initial <- list(id="image-doc",path=file.path(tempdir(),"example.R"),
                    contents='background_only <- "unrelated clustering code"')
app$active_doc <- function(id=NULL)app$initial
calls <- new.env();calls$items <- list()
app$ask_ai <- function(system_prompt,prompt,images=character()) {
  calls$items <- append(calls$items,list(list(system=system_prompt,prompt=prompt,
    paths=images,bytes=lapply(images,function(p)readBin(p,"raw",file.info(p)$size)))))
  "Received."
}
fixtures <- vapply(c("Visible text: x <- 0.001; y != 1", "Visible text: Error in mean(x)"),function(label) {
  path <- tempfile(fileext=".png")
  grDevices::png(path,width=800,height=120)
  par(mar=rep(0,4));plot.new();text(.5,.5,label)
  invisible(grDevices::dev.off())
  path
},character(1))
bytes <- lapply(fixtures,function(p)readBin(p,"raw",file.info(p)$size))
urls <- vapply(bytes,function(raw)paste0("data:image/png;base64,",jsonlite::base64_enc(raw)),character(1))
last_call <- function()tail(calls$items,1L)[[1L]]
shiny::testServer(app$server, {
  session$flushReact()
  state$assignment <- "Exercise 1: An earlier unrelated statistics exercise."
  session$setInputs(paste_image=list(image=urls[[1L]],filename="plot.png",nonce=1L,epoch=state$upload_epoch))
  session$setInputs(chat_submit=list(text="",nonce=2L))
  request <- last_call()
  check(!grepl("Please analyze",request$prompt,fixed=TRUE),"an image-only upload does not invent a user request")
  check(!grepl("statistics tutor",request$system,fixed=TRUE) &&
        !grepl("ACADEMIC_COMMENT",request$system,fixed=TRUE),"ordinary image reading has no statistics or academic-output template")
  check(grepl("unreadable",request$system,fixed=TRUE) && grepl("Do not guess",request$system,fixed=TRUE),
        "image-reading instructions require uncertainty instead of guessed text")
  check(identical(request$bytes[[1L]],bytes[[1L]]),"the full original image bytes reach the request")
  check(grepl("OPTIONAL BACKGROUND",request$prompt,fixed=TRUE) &&
        grepl("unrelated clustering code",request$prompt,fixed=TRUE),"editor and prior assignment context are retained as optional background")
  check(!grepl("plot.png",request$prompt,fixed=TRUE),"a filename does not impose an image type on the prompt")
  request_text <- "Read only the visible code. Keep x <- 0.001 and != exactly.\nDo not fix it or translate its comments."
  session$setInputs(paste_image=list(image=urls[[1L]],filename="code.png",nonce=3L,epoch=state$upload_epoch))
  session$setInputs(chat_submit=list(text=request_text,nonce=4L))
  request <- last_call()
  check(startsWith(request$prompt,paste0("USER MESSAGE:\n",request_text,"\n\n")),"the user's exact image request leads the payload")
  check(!grepl("ACADEMIC_COMMENT",request$system,fixed=TRUE),"a code-transcription request does not acquire a statistical-comment format")
  session$setInputs(paste_image=list(image=urls[[1L]],filename="read-only.png",nonce=30L,epoch=state$upload_epoch))
  session$setInputs(chat_submit=list(text="Do not write an Academic English Comment. Just read the screenshot.",nonce=31L))
  check(!grepl("ACADEMIC_COMMENT",last_call()$system,fixed=TRUE),"a negated academic-comment request does not add a formatting instruction")
  session$setInputs(paste_image=list(image=urls[[1L]],filename="heading.png",nonce=32L,epoch=state$upload_epoch))
  session$setInputs(chat_submit=list(text="What does the heading Academic English Comment say?",nonce=33L))
  check(!grepl("ACADEMIC_COMMENT",last_call()$system,fixed=TRUE),"mentioning an academic-comment heading does not request that output format")
  session$setInputs(paste_image=list(image=urls[[1L]],filename="first.png",nonce=5L,epoch=state$upload_epoch))
  session$setInputs(paste_image=list(image=urls[[2L]],filename="second.png",nonce=6L,epoch=state$upload_epoch))
  session$setInputs(chat_submit=list(text="Compare the readable text in these two screenshots.",nonce=7L))
  request <- last_call()
  check(length(request$paths)>=2L && identical(request$bytes[1:2],unname(bytes)),"multiple screenshots preserve their upload order and original contents")
  check(grepl("UPLOADED IMAGES THIS TURN: 2",request$prompt,fixed=TRUE),"multiple images are described without a preset image category")
  session$setInputs(paste_image=list(image=urls[[1L]],filename="requested-comment.png",nonce=8L,epoch=state$upload_epoch))
  session$setInputs(chat_submit=list(text="Please write an Academic English Comment using only the visible results.",nonce=9L))
  check(grepl("[[ACADEMIC_COMMENT]]",last_call()$system,fixed=TRUE),"an explicitly requested academic comment keeps the existing copyable-card format")
  session$setInputs(chat_submit=list(text="Explain this R code without running it.",nonce=10L))
  check(grepl("RStudio coding and statistics tutor",last_call()$system,fixed=TRUE),"text-only R help retains its existing instructions")
})
job <- list(assignment="",request="Validate my script",csv_headers="",history="",code="plot(1:3)")
run <- list(ok=TRUE,logs="Script completed",plots=fixtures[[1L]],total_plots=1L)
invisible(app$analysis_after_run(job,run))
check(grepl("VERIFIED logs and attached plots",last_call()$system,fixed=TRUE),
      "approved script execution retains its separate verified-result analysis")
unlink(fixtures)
cat(checks,"image-reading checks passed\n")
