# Existing Shiny server, actual temporary storage and a fake model.
source("tests/helpers.R")
store_env <- new.env(parent=globalenv());sys.source("R/history.R",store_env)
main <- function() {
  root <- tempfile("ra-chat-lifecycle-");dir.create(root)
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  history <- store_env$.conversation_store(project=root,directory=file.path(root,"data"))
  app <- load_assistant(package_info=list(version="0.2.0",history=history))
  app$initial <- list(id="doc",path="example.R",contents="x <- 1")
  app$active_doc <- function(id=NULL)app$initial
  fixture <- file.path(root,"screenshot.png")
  grDevices::png(fixture,width=80,height=80);par(mar=rep(0,4));plot.new();points(.5,.5)
  invisible(grDevices::dev.off())
  bytes <- readBin(fixture,"raw",file.info(fixture)$size)
  image <- paste0("data:image/png;base64,",jsonlite::base64_enc(bytes))
  calls <- 0L;last <- NULL
  app$ask_ai <- function(system_prompt,prompt,images=character()) {
    calls <<- calls+1L;last <<- list(prompt=prompt,images=images,
      bytes=lapply(images,function(path)readBin(path,"raw",file.info(path)$size)))
    "```r\nx <- 42\n```\n[[ACADEMIC_COMMENT]]Verified result.[[/ACADEMIC_COMMENT]]"
  }
  checks <- 0L
  check <- function(ok,description){
    if(!isTRUE(ok))stop(description,call.=FALSE)
    checks <<- checks+1L;cat("PASS",description,"\n")
  }
  first <- NULL;second <- NULL
  shiny::testServer(app$server,{
    session$flushReact()
    session$setInputs(paste_image=list(image=image,filename="Screenshot.png",nonce=1L,epoch=state$upload_epoch))
    session$setInputs(chat_submit=list(text="Remember the value 42",nonce=1L))
    first <<- state$chat$id
    check(identical(history$load(first)$messages[[1L]]$text,"Remember the value 42"),
      "successful turns auto-save the current chat")
    session$setInputs(new_chat=1L)
    second <<- state$chat$id
    check(!identical(first,second) && length(history$list())==2L && !length(state$messages),
      "New chat saves the previous conversation and opens a separate chat")
    session$setInputs(chat_submit=list(text="A separate topic",nonce=2L))
    session$setInputs(history_selected=first,history_rename=1L)
    session$setInputs(history_name="Important assignment",history_save_name=1L)
    check(identical(history$load(first)$title,"Important assignment"),"History can rename a saved conversation")
    session$setInputs(history_selected=first,history_restore=1L)
    check(identical(state$chat$id,first) && length(state$messages)==2L && is.null(state$pending),
      "Restore selects the saved conversation without calling the model or restoring approval")
    session$setInputs(chat_submit=list(text="Explain the value again",nonce=3L))
    check(grepl("Remember the value 42",last$prompt,fixed=TRUE) &&
      !grepl("A separate topic",last$prompt,fixed=TRUE),
      "restored chat context stays isolated from other conversations")
    session$setInputs(history_selected=second,history_delete=1L,history_confirm_delete=1L)
    check(is.null(history$load(second)) && identical(state$chat$id,first),
      "Delete removes only the selected chat and preserves the active conversation")
    session$setInputs(chat_submit=list(text="Please run the script",nonce=4L))
    check(!is.null(state$pending),"pending validation is saved as a message rather than executable approval")
  })
  before <- calls
  app$initial$id <- "new-rstudio-document-id"
  shiny::testServer(app$server,{
    session$flushReact()
    check(identical(state$chat$id,first) && length(state$messages)>0L && calls==before,
      "relaunch automatically restores the latest chat without an API request")
    check(is.null(state$pending) && !state$busy && any(vapply(state$messages,function(m)
      identical(m$outcome,"Interrupted; request validation again"),logical(1))),
      "relaunch never resumes a saved run approval")
    check(any(vapply(state$messages,function(m)grepl('class="code-copy"',as.character(m$html),fixed=TRUE),logical(1))),
      "restored code regenerates working Copy markup")
    session$setInputs(chat_submit=list(text="Continue the earlier screenshot explanation",nonce=5L))
    check(length(last$bytes)==1L && identical(last$bytes[[1L]],bytes),
      "a screenshot follow-up after relaunch reuses the original locally saved pixels")
    session$setInputs(history_selected=first,history_delete=2L,history_confirm_delete=2L)
    check(!identical(state$chat$id,first) && !length(state$messages) && is.null(history$load(first)),
      "deleting the active chat opens a new empty conversation")
  })
  unavailable <- history
  unavailable$save <- function(...)stop("Synthetic disk full")
  broken <- load_assistant(package_info=list(version="0.2.0",history=unavailable))
  broken$initial <- app$initial;broken$active_doc <- app$active_doc;broken$ask_ai <- app$ask_ai
  shiny::testServer(broken$server,{
    session$flushReact()
    session$setInputs(chat_submit=list(text="Keep this unsaved conversation",nonce=20L))
    id <- state$chat$id;count <- length(state$messages)
    session$setInputs(new_chat=1L)
    check(identical(state$chat$id,id) && length(state$messages)==count && count==2L,
      "a failed auto-save does not discard the live conversation when New chat is requested")
  })
  snapshot <- app$snapshot_messages(list(list(role="AI",type="text",text="Safe text",sequence=1L,
    html="<script>not saved</script>",api_key="synthetic-private-sentinel")))
  check(is.null(snapshot[[1L]]$html) && is.null(snapshot[[1L]]$api_key),
    "saved messages exclude cached HTML and unrecognized private fields")
  cat(checks,"conversation-history checks passed\n")
}
main()
