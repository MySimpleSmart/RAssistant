# Exercise real Shiny turns and inspect the outgoing text and original pixels.
# No real credentials, network requests, or user files are used.
source("tests/helpers.R")
app <- load_assistant()
checks <- 0L
check <- function(ok, description) {
  if (!isTRUE(ok)) stop(description, call. = FALSE)
  checks <<- checks + 1L; cat("PASS", description, "\n")
}
app$initial <- list(id="first-doc", path=file.path(tempdir(), "first.R"),
                    contents="first_editor_marker <- 1")
control <- new.env(); control$available <- TRUE; control$fail <- FALSE
app$active_doc <- function(id=NULL) if (control$available) app$initial else NULL
calls <- list()
app$ask_ai <- function(system_prompt, prompt, images=character()) {
  calls[[length(calls)+1L]] <<- list(system=system_prompt, prompt=prompt, paths=images,
    bytes=lapply(images, function(p) readBin(p, "raw", file.info(p)$size)))
  if (control$fail) stop("Synthetic API failure")
  "Previous answer: use the requested method for part (a)."
}
fixtures <- vapply(1:4, function(i) {
  path <- tempfile(fileext=".png")
  grDevices::png(path, width=120, height=120)
  par(mar=rep(0,4)); plot.new(); text(.5,.5, paste("Screenshot",i))
  invisible(grDevices::dev.off()); path
}, character(1))
bytes <- lapply(fixtures, function(p) readBin(p, "raw", file.info(p)$size))
urls <- vapply(bytes, function(b) paste0("data:image/png;base64,", jsonlite::base64_enc(b)), character(1))
last_call <- function() tail(calls,1L)[[1L]]
shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(paste_image=list(image=urls[[1L]], filename="first.png", nonce=1L, epoch=state$upload_epoch))
  session$setInputs(chat_submit=list(text="Read this exercise; work through each part as I ask. Use GGally if the task needs it.", nonce=1L))
  session$setInputs(chat_submit=list(text="Continue with part (b) of that screenshot.", nonce=2L))
  check(length(last_call()$bytes)==1L && identical(last_call()$bytes[[1L]],bytes[[1L]]),
    "a text follow-up resends the original screenshot pixels without another upload")
  check(grepl("Use GGally if the task needs it",last_call()$prompt,fixed=TRUE) &&
        grepl("Previous answer",last_call()$prompt,fixed=TRUE),
    "follow-ups carry both the prior request and the previous answer")
  check(all(!file.exists(last_call()$paths)), "restored image files are removed after the request")
  control$available <- FALSE
  before <- length(calls)
  session$setInputs(chat_submit=list(text="Explain the next part of the screenshot.", nonce=3L))
  check(length(calls)==before+1L && identical(last_call()$bytes[[1L]],bytes[[1L]]) &&
        !grepl("first_editor_marker",last_call()$prompt,fixed=TRUE),
    "image follow-ups work without Source access and exclude stale editor code")
  session$setInputs(paste_image=list(image=urls[[2L]], filename="next.png", nonce=2L, epoch=state$upload_epoch))
  session$setInputs(chat_submit=list(text="Compare this screenshot with the previous one for the same exercise.", nonce=4L))
  check(identical(last_call()$bytes,unname(bytes[c(2L,1L)])),
    "a new screenshot can be compared with the previous original image")
  check(grepl("Continue",last_call()$system,fixed=TRUE) &&
        grepl("UPLOADED IMAGES THIS TURN: 1",last_call()$prompt,fixed=TRUE),
    "image requests distinguish new uploads and retained context while allowing continuation")
  control$available <- TRUE
  app$initial <- list(id="second-doc",path=file.path(tempdir(),"second.R"),contents="second_editor_marker <- 2")
  session$setInputs(chat_submit=list(text="Continue the same exercise with this script.", nonce=5L))
  check(length(state$messages)==10L && grepl("first.R",last_call()$prompt,fixed=TRUE) &&
        grepl("second_editor_marker",last_call()$prompt,fixed=TRUE) &&
        !grepl("first_editor_marker",last_call()$prompt,fixed=TRUE),
    "switching Source tabs preserves the conversation and uses only the fresh current editor")
  session$setInputs(chat_submit=list(text="Read image #1 again.", nonce=6L))
  check(identical(last_call()$bytes,list(bytes[[1L]])),
    "an explicit image number selects an earlier screenshot instead of the newest one")
  session$setInputs(chat_submit=list(text="Compare images 1 and 2.", nonce=61L))
  check(identical(last_call()$bytes,unname(bytes[1:2])),
    "plural image references select both requested original screenshots")
  session$setInputs(chat_submit=list(text="Read image #90 again.", nonce=7L))
  check(!length(last_call()$bytes) && grepl("unavailable",last_call()$prompt,ignore.case=TRUE),
    "a missing image reference is reported without attaching an unrelated image")
  control$fail <- TRUE
  session$setInputs(paste_image=list(image=urls[[3L]], filename="failed.png", nonce=3L, epoch=state$upload_epoch))
  session$setInputs(chat_submit=list(text="Failed screenshot request", nonce=8L))
  check(length(state$attachments)==1L && !state$busy, "an API failure preserves the draft image and releases busy state")
  session$setInputs(clear_images=1L)
  control$fail <- FALSE
  session$setInputs(chat_submit=list(text="Explain the previous successful screenshot.", nonce=9L))
  check(identical(last_call()$bytes,list(bytes[[2L]])) &&
        !grepl("Failed screenshot request",last_call()$prompt,fixed=TRUE),
    "failed and removed uploads never become successful conversation evidence")
  session$setInputs(new_chat=1L)
  session$setInputs(chat_submit=list(text="A new text topic.", nonce=10L))
  check(!length(last_call()$bytes) && !grepl("Use GGally",last_call()$prompt,fixed=TRUE),
    "New clears prior image pixels and conversation instructions")
  for (i in 1:3) session$setInputs(paste_image=list(image=urls[[i]], filename=paste0(i,".png"), nonce=10L+i, epoch=state$upload_epoch))
  session$setInputs(chat_submit=list(text="Read these three images.", nonce=11L))
  session$setInputs(paste_image=list(image=urls[[4L]], filename="fourth.png", nonce=14L, epoch=state$upload_epoch))
  session$setInputs(chat_submit=list(text="Compare the new image with the earlier images.", nonce=12L))
  check(identical(last_call()$bytes,unname(bytes[c(4L,1L,2L,3L)])),
    "up to three retained images keep their order alongside the new upload")
  session$setInputs(chat_submit=list(text="Read the previous screenshot again.", nonce=13L))
  check(identical(last_call()$bytes,list(bytes[[4L]])),
    "ordinary follow-ups reuse only the latest image turn rather than all old images")
  msg <- state$messages
  index <- which(vapply(msg,function(m) length(m$attachments)>0L,logical(1)))
  for (i in index) for (j in seq_along(msg[[i]]$attachments)) msg[[i]]$attachments[[j]]$preview <- "data:image/png;base64,AAAA"
  state$messages <- msg
  session$setInputs(chat_submit=list(text="Read the previous screenshot again.", nonce=14L))
  check(!length(last_call()$bytes) && !state$busy && grepl("unavailable",last_call()$prompt,ignore.case=TRUE),
    "an unreadable retained image is reported and does not crash the conversation")
})
long_history <- c(list(list(role="You",type="text",text="For dendrograms in this exercise use single linkage and Euclidean distance.")),
  lapply(1:40,function(i)list(role="AI",type="text",text=paste("Recent discussion",i,strrep("x",1500)))))
context <- app$summarize_history(long_history,max_chars=32000L,request="Continue the dendrogram using single linkage.")
check(nchar(context)<=32000L && grepl("single linkage and Euclidean",context,fixed=TRUE) &&
      grepl("Recent discussion 40",context,fixed=TRUE),
  "bounded history retrieves an earlier relevant method as well as the latest turns")
unlink(fixtures)
cat(checks,"conversation-context checks passed\n")
