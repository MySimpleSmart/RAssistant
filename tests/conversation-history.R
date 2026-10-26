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
    choose_history <- function(id,nonce) {
      if(!state$history_panel)show_history()
      session$setInputs(history_view=list(id=id,revision=as.character(state$history_revision),nonce=nonce))
    }
    check(!length(history$list()),"opening a blank chat does not create a History entry")
    session$setInputs(new_chat=1L);session$setInputs(new_chat=2L)
    check(!length(history$list()) && is.null(history$load()),"repeated New chat clicks do not accumulate empty conversations")
    session$setInputs(prompt=" \n\t ");persist_chat()
    check(!length(history$list()),"whitespace-only drafts are not saved")
    session$setInputs(prompt="Unsent draft");persist_chat()
    check(identical(history$load(state$chat$id)$draft,"Unsent draft"),"unsent draft text is saved before its first message")
    session$setInputs(prompt="");persist_chat()
    check(!length(history$list()),"clearing an unsent draft removes its now-empty history entry")
    session$setInputs(paste_image=list(image=image,filename="Screenshot.png",nonce=1L,epoch=state$upload_epoch))
    check(length(history$load(state$chat$id)$attachments)==1L,"an image-only draft is saved before its first message")
    session$setInputs(chat_submit=list(text="Remember the value 42",nonce=1L))
    first <<- state$chat$id
    check(identical(history$load(first)$messages[[1L]]$text,"Remember the value 42"),
      "successful turns auto-save the current chat")
    before_calls <- calls
    session$setInputs(prompt="Draft kept while browsing",history=1L)
    check(state$history_panel && !state$edit_panel && identical(state$chat$id,first) &&
      identical(state$draft,"Draft kept while browsing") && calls==before_calls &&
      grepl("Back to chat",output$chat_history_panel$html,fixed=TRUE),
      "Chat history opens its own page without changing the chat, draft or model calls")
    choose_history(first,1L)
    check(grepl("Remember the value 42",output$chat_history_preview$html,fixed=TRUE) &&
      !grepl("source-review",output$chat_history_preview$html,fixed=TRUE),
      "history previews stored messages without Source editing controls")
    session$setInputs(edit_history=1L)
    check(state$edit_panel && !state$history_panel,"Code edit history and Chat history never overlap")
    session$setInputs(history=2L,history_close=1L)
    check(!state$history_panel && !state$edit_panel && identical(state$draft,"Draft kept while browsing"),
      "Back to chat retains the existing draft")
    session$setInputs(prompt="")
    session$setInputs(new_chat=3L)
    second <<- state$chat$id
    check(!identical(first,second) && length(history$list())==1L && !length(state$messages) && is.null(history$load(second)),
      "New chat preserves the previous conversation without saving the new empty chat")
    session$setInputs(chat_submit=list(text="A separate topic",nonce=2L))
    check(length(history$list())==2L,"sending the first message adds the new conversation to History")
    choose_history(first,2L);session$setInputs(history_rename=1L)
    session$setInputs(history_name="Important assignment",history_save_name=1L)
    check(identical(history$load(first)$title,"Important assignment"),"History can rename a saved conversation")
    choose_history(first,3L);session$setInputs(history_restore=1L)
    check(identical(state$chat$id,first) && length(state$messages)==2L && is.null(state$pending),
      "Restore selects the saved conversation without calling the model or restoring approval")
    session$setInputs(chat_submit=list(text="Explain the value again",nonce=3L))
    check(grepl("Remember the value 42",last$prompt,fixed=TRUE) &&
      !grepl("A separate topic",last$prompt,fixed=TRUE),
      "restored chat context stays isolated from other conversations")
    choose_history(second,4L);session$setInputs(history_delete=1L,history_confirm_delete=1L)
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
    show_history(first);session$setInputs(history_delete=2L,history_confirm_delete=2L)
    check(!identical(state$chat$id,first) && !length(state$messages) && is.null(history$load(first)) && !length(history$list()),
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
  rich <- paste("```r",paste0("# ",strrep("x",1800L)),"answer <- 42 # END_CODE_SENTINEL","```","",
    "| x | y |","|---|---|","| 1 | 3 |","| 2 | 5 |","",'\\[y = 2x + 1\\]',
    "<script>window.previewInjected=true</script>",sep="\n")
  record <- list(id=history$new_id(),title="Rich preview",title_custom=TRUE,created_at="2026-10-26T00:00:00Z",
    draft="",assignment="",attachments=list(),knowledge_files=list(),
    messages=lapply(seq_len(45L),function(i)list(role=if(i%%2L)"AI"else"You",type="text",sequence=i,
      text=if(i==45L)rich else paste("Historical message",i))))
  history$save(record)
  shiny::testServer(app$server,{
    session$flushReact();session$setInputs(prompt="Keep my draft",history=1L)
    before_calls <- calls;active <- state$chat$id
    html <- output$chat_history_preview$html
    check(grepl("latest 20 of 45",html,fixed=TRUE) && grepl("END_CODE_SENTINEL",html,fixed=TRUE) &&
      grepl("<table>",html,fixed=TRUE) && grepl("code-copy",html,fixed=TRUE),
      "preview renders the latest twenty complete messages, including long code and Markdown tables")
    check(!grepl('class="source-review"',html,fixed=TRUE) && !grepl("<script>window.previewInjected",html,fixed=TRUE) &&
      grepl("&lt;script&gt;",html,fixed=TRUE),
      "preview rebuilds safe Markdown without live Source editing controls or raw HTML execution")
    check(grepl('class="btn ai-btn chat-log-view',output$chat_history_list$html,fixed=TRUE) &&
      !grepl("<select",output$chat_history_panel$html,fixed=TRUE),
      "conversation selection uses scrollable buttons instead of a native select")
    session$setInputs(history_more=1L)
    check(grepl("latest 40 of 45",output$chat_history_preview$html,fixed=TRUE),"Load earlier adds twenty complete messages")
    session$setInputs(history_more=2L)
    check(grepl("latest 45 of 45",output$chat_history_preview$html,fixed=TRUE) &&
      !grepl('id="history_more"',output$chat_history_preview$html,fixed=TRUE),
      "all earlier messages remain accessible and the paging button ends at the first message")
    session$setInputs(history_view=list(id=record$id,revision=as.character(state$history_revision-1L),nonce=1L))
    check(state$history_limit==60L,"stale conversation clicks cannot reset or switch a newer history page")
    session$setInputs(history_view=list(id=record$id,revision=as.character(state$history_revision),nonce=2L))
    check(state$history_limit==20L && identical(selected_chat(),record$id),"choosing a conversation starts its preview at the latest twenty messages")
    check(calls==before_calls && identical(state$chat$id,active) && identical(state$draft,"Keep my draft"),
      "preview paging and selection never call the model, replace the active chat or lose the draft")
  })
  cat(checks,"conversation-history checks passed\n")
}
main()
