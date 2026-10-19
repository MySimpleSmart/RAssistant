# Follow-up payloads must retain visual evidence and conversation continuity.
source("tests/helpers.R")
app <- load_assistant()
checks <- 0L
check <- function(ok,description) {
  if(!isTRUE(ok))stop(description,call.=FALSE)
  checks <<- checks+1L;cat("PASS",description,"\n")
}
fixture <- tempfile(fileext=".png")
grDevices::png(fixture,width=80,height=80)
par(mar=rep(0,4));plot.new();points(.5,.5)
invisible(grDevices::dev.off())
bytes <- readBin(fixture,"raw",file.info(fixture)$size)
data_url <- paste0("data:image/png;base64,",jsonlite::base64_enc(bytes))
calls <- list()
fail <- FALSE
app$initial <- list(id="source1",path="original.R",contents="original_source <- 1")
app$active_doc <- function(id=NULL)app$initial
app$ask_ai <- function(system_prompt,prompt,images=character()) {
  calls[[length(calls)+1L]] <<- list(system=system_prompt,prompt=prompt,paths=images,
    bytes=lapply(images,function(p)readBin(p,"raw",file.info(p)$size)))
  if(fail)stop("Synthetic failure")
  paste("Screenshot contains the value 42.",strrep("Detailed transcription. ",180L),"TAIL_OF_FULL_READOUT")
}
shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(paste_image=list(image=data_url,filename="screenshot.png",nonce=1L,epoch=state$upload_epoch))
  staged <- state$attachments[[1L]]$path
  session$setInputs(chat_submit=list(text="Read the screenshot",nonce=1L))
  check(!file.exists(staged) && !length(state$attachments),"successful uploads still release staging files")
  session$setInputs(chat_submit=list(text="Explain why that value is 42",nonce=2L))
  request <- tail(calls,1L)[[1L]]
  check(length(request$paths)==1L && identical(request$bytes[[1L]],bytes),
    "a text follow-up automatically receives the original screenshot pixels")
  check(grepl("TAIL_OF_FULL_READOUT",request$prompt,fixed=TRUE),
    "long screenshot readouts remain available beyond the old per-message excerpt")
  check(grepl("RETAINED IMAGE CONTEXT",request$prompt,fixed=TRUE) &&
    grepl("Do not request another upload",request$system,fixed=TRUE),
    "the payload identifies earlier images and requests a continuing answer")
  check(all(!file.exists(request$paths)),"follow-up image materialization is cleaned up after the request")
  check(length(Filter(function(m)length(m$attachments)>0L,state$messages))==1L,
    "reusing image context does not duplicate visible uploads")
  app$initial <- list(id="source2",path="different.R",contents="new_source <- 2")
  session$setInputs(chat_submit=list(text="Continue the earlier screenshot explanation",nonce=3L))
  request <- tail(calls,1L)[[1L]]
  check(grepl("Explain why that value",request$prompt,fixed=TRUE) &&
    grepl("Source at that turn: original.R",request$prompt,fixed=TRUE) &&
    grepl("new_source <- 2",request$prompt,fixed=TRUE),
    "changing Source preserves dialogue while distinguishing historical and current code")
  app$active_doc <- function(id=NULL)NULL
  session$setInputs(chat_submit=list(text="What about the next step?",nonce=4L))
  check(length(calls)==4L && length(tail(calls,1L)[[1L]]$paths)==1L,
    "a follow-up does not require an open Source document")
  fail <- TRUE
  session$setInputs(chat_submit=list(text="Explain it again",nonce=5L))
  fail <- FALSE
  session$setInputs(chat_submit=list(text="Explain it again",nonce=6L))
  check(identical(tail(calls,1L)[[1L]]$bytes[[1L]],bytes) && !state$busy,
    "failed follow-ups retain image evidence for a retry")
  session$setInputs(paste_image=list(image=data_url,filename="next.png",nonce=2L,epoch=state$upload_epoch))
  session$setInputs(chat_submit=list(text="Read this new screenshot",nonce=7L))
  check(length(tail(calls,1L)[[1L]]$paths)==1L &&
    grepl("UPLOADED IMAGES THIS TURN: 1",tail(calls,1L)[[1L]]$prompt,fixed=TRUE),
    "new uploads replace the visual batch rather than resending the entire image history")
  session$setInputs(chat_submit=list(text="Compare Image 1 and Image 2",nonce=9L))
  check(length(tail(calls,1L)[[1L]]$paths)==2L &&
    grepl("Image order: Image 1, Image 2",tail(calls,1L)[[1L]]$prompt,fixed=TRUE),
    "numbered references can retrieve older images from separate upload turns")
  session$setInputs(chat_submit=list(text="Explain Image 99",nonce=10L))
  check(!length(tail(calls,1L)[[1L]]$paths),"an unavailable image reference never substitutes unrelated pixels")
  session$setInputs(new_chat=1L)
  session$setInputs(chat_submit=list(text="A new unrelated question",nonce=8L))
  check(!length(tail(calls,1L)[[1L]]$paths) &&
    !grepl("value 42",tail(calls,1L)[[1L]]$prompt,fixed=TRUE),
    "New clears prior text and visual context even without an open Source document")
})
messages <- lapply(1:70,function(i)list(role=if(i%%2L)"You"else"AI",type="text",
  text=if(i==11L)"old_topic_marker <- 42"else paste("Message",i,strrep("x",300))))
context <- app$summarize_history(messages,max_chars=12000L,request="Explain old_topic_marker")
check(nchar(context)<=12000L && grepl("old_topic_marker",context,fixed=TRUE) &&
  grepl("Message 70",context,fixed=TRUE),"bounded context retrieves a relevant older topic while retaining recent turns")
unlink(fixture)
cat(checks,"conversation-context checks passed\n")
