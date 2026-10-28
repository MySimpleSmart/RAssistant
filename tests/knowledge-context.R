# Course knowledge reaches requests, cites its sources and stays apart from chats.
source("tests/helpers.R")
store_env <- new.env(parent=globalenv())
for (file in c("R/history.R","R/knowledge.R")) sys.source(file,store_env)
checks <- 0L
check <- function(ok,description) {
  if(!isTRUE(ok))stop(description,call.=FALSE)
  checks <<- checks+1L;cat("PASS",description,"\n")
}
root <- tempfile("ra-knowledge-app-");dir.create(root)
on.exit(unlink(root,recursive=TRUE),add=TRUE)
data <- file.path(root,"data")
history <- store_env$.conversation_store(project=root,directory=data)
knowledge <- store_env$.knowledge_store(project=root,directory=data)
notes <- file.path(root,"upload-1")
writeLines(c("# Week 4: Clustering","","Run k-means with kmeans(x, centers = k, nstart = 25) after set.seed(123).",
  "Choose the number of clusters with the elbow method: fviz_nbclust(x, kmeans, method = \"wss\")."),notes)
calls <- list()
fake_ai <- function(system_prompt,prompt,images=character()) {
  calls[[length(calls)+1L]] <<- list(system=system_prompt,prompt=prompt)
  "Use the elbow method, as in week4.md (lines 1-4)."
}
app <- load_assistant(package_info=list(version="0.3.0",history=history,knowledge=knowledge))
app$initial <- list(id="source1",path=file.path(root,"analysis.R"),contents="km <- kmeans(iris[, 1:4], 3)")
app$active_doc <- function(id=NULL)app$initial
app$ask_ai <- fake_ai
app$validate_script <- function(...)list(ok=TRUE,logs="Within-cluster sum of squares: 78.9",
  plots=character(0),plot_dir=NULL,total_plots=0L,kind="R")

instructions <- "STAT1170: follow the lecture method. Prefer factoextra and use <- for assignment."
shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(chat_submit=list(text="How should I choose k?",nonce=1L))
  first <- tail(calls,1L)[[1L]]
  check(!grepl("KNOWLEDGE BASE",first$system,fixed=TRUE) && !grepl("KNOWLEDGE BASE EXCERPTS",first$prompt,fixed=TRUE) &&
    !length(state$messages[[2L]]$references),"without course knowledge, requests are unchanged")

  session$setInputs(knowledge=1L)
  session$setInputs(knowledge_files=data.frame(name="week4.md",size=file.info(notes)$size,
    type="text/markdown",datapath=notes,stringsAsFactors=FALSE))
  check(identical(vapply(knowledge$list(),function(x)x$name,character(1)),"week4.md") && !file.exists(notes),
    "the Knowledge dialog adds an uploaded file and releases the upload")
  session$setInputs(knowledge_instructions=instructions,knowledge_save=1L)
  check(identical(knowledge$profile()$instructions,instructions),"the Knowledge dialog saves custom instructions")

  session$setInputs(chat_submit=list(text="How should I choose k for kmeans clustering?",nonce=20L))
  unselected <- tail(calls,1L)[[1L]]
  check(!grepl("KNOWLEDGE BASE EXCERPTS",unselected$prompt,fixed=TRUE) &&
    !length(tail(state$messages,1L)[[1L]]$references) &&
    identical(tail(state$messages,2L)[[1L]]$knowledge_read$mode,"instructions"),
    "matching library files stay unused with no selection; only saved instructions are supplied")
  doc <- knowledge$list()[[1L]]$id
  session$setInputs(knowledge_pin_change=list(ids=list(doc),epoch=state$upload_epoch,nonce=21L))
  session$setInputs(chat_submit=list(text="How should I choose k for kmeans clustering?",nonce=2L))
  request <- tail(calls,1L)[[1L]]
  check(grepl("CUSTOM INSTRUCTIONS",request$system,fixed=TRUE) &&
    grepl(instructions,request$system,fixed=TRUE) &&
    grepl("in preference to generic alternatives",request$system,fixed=TRUE),
    "requests carry custom instructions and the lecture-first instruction")
  check(grepl("KNOWLEDGE BASE EXCERPTS (retrieved locally for this request):\n[K1] week4.md, lines 1-4",request$prompt,fixed=TRUE) &&
    grepl("fviz_nbclust",request$prompt,fixed=TRUE),"relevant course excerpts are sent with their source label")
  answer <- tail(state$messages,1L)[[1L]]
  check(identical(answer$references[[1L]]$name,"week4.md") && identical(answer$references[[1L]]$label,"lines 1-4"),
    "the answer records which course material was supplied")
  receipt <- tail(state$messages,2L)[[1L]]$knowledge_read
  sent_excerpt <- strsplit(request$prompt,"KNOWLEDGE BASE EXCERPTS (retrieved locally for this request):\n",fixed=TRUE)[[1L]][[2L]]
  sent_excerpt <- strsplit(sent_excerpt,"\n\nRECENT CHAT:",fixed=TRUE)[[1L]][[1L]]
  check(identical(receipt$text,sent_excerpt) &&
    identical(receipt$instructions,instructions) && identical(receipt$delivery,"sent"),
    "each user message records the exact excerpts and instructions delivered to the model")
  markup <- as.character(app$knowledge_receipt_ui(receipt))
  check(grepl("<details",markup,fixed=TRUE) && grepl("Knowledge Base",markup,fixed=TRUE) &&
    grepl("fviz_nbclust",markup,fixed=TRUE),"the exact receipt is collapsed and expandable in chat")

  session$setInputs(knowledge_unpin=list(id=doc,epoch=state$upload_epoch,nonce=22L))
  session$setInputs(chat_submit=list(text="Translate hello into French",nonce=3L))
  unrelated <- tail(calls,1L)[[1L]]
  check(grepl("CUSTOM INSTRUCTIONS",unrelated$system,fixed=TRUE) &&
    !grepl("KNOWLEDGE BASE EXCERPTS",unrelated$prompt,fixed=TRUE) && !length(tail(state$messages,1L)[[1L]]$references),
    "unrelated requests send custom instructions but no course excerpts")
  check(identical(tail(state$messages,2L)[[1L]]$knowledge_read$status,"disabled") &&
    !nzchar(tail(state$messages,2L)[[1L]]$knowledge_read$text),"no-match receipts do not imply a document was sent")

  session$setInputs(knowledge_pin_change=list(ids=list(doc),epoch=state$upload_epoch,nonce=23L))
  session$setInputs(chat_submit=list(text="Run my kmeans clustering script and interpret it",nonce=4L))
  check(is.list(state$pending$course) && grepl("fviz_nbclust",state$pending$course$text,fixed=TRUE),
    "a run request keeps the course context for the later analysis")
  check(identical(tail(state$messages,2L)[[1L]]$knowledge_read$delivery,"prepared"),
    "validation excerpts are only prepared until the user approves execution")
  session$setInputs(yes=1L)
  analysis <- tail(calls,1L)[[1L]]
  check(grepl("KNOWLEDGE BASE EXCERPTS",analysis$prompt,fixed=TRUE) && grepl("CUSTOM INSTRUCTIONS",analysis$system,fixed=TRUE) &&
    identical(tail(state$messages,1L)[[1L]]$references[[1L]]$name,"week4.md"),
    "execution analysis uses course material and records its sources")
  check(identical(tail(Filter(function(m)identical(m$role,"You"),state$messages),1L)[[1L]]$knowledge_read$delivery,"sent"),
    "approved validation marks its original user receipt as delivered")

  saved <- history$load(state$chat$id)
  restored <- Filter(function(m)length(m$references),saved$messages)
  check(length(restored)>=2L && identical(restored[[1L]]$references[[1L]]$name,"week4.md") &&
    !any(grepl("fviz_nbclust",unlist(lapply(saved$messages,function(m)m$text)),fixed=TRUE)),
    "retrieved material stays outside conversational message text")
  exact <- Filter(function(m)!is.null(m$knowledge_read) && nzchar(m$knowledge_read$text),saved$messages)
  check(length(exact)>=2L && identical(exact[[1L]]$knowledge_read$text,receipt$text) &&
    identical(exact[[1L]]$knowledge_read$instructions,instructions),"saved receipts preserve the supplied text independently of the Knowledge Base")

  doc <- knowledge$list()[[1L]]$id
  session$setInputs(knowledge_pin_change=list(ids=list(doc),epoch=state$upload_epoch,nonce=10L))
  check(identical(state$knowledge_files[[1L]]$id,doc) &&
    identical(history$load(state$chat$id)$knowledge_files[[1L]]$id,doc),"file selection is saved for this chat")
  session$setInputs(chat_submit=list(text="Explain this",nonce=11L))
  check(grepl("fviz_nbclust",tail(calls,1L)[[1L]]$prompt,fixed=TRUE) &&
    identical(tail(state$messages,2L)[[1L]]$knowledge_read$mode,"selected"),
    "a selected file supplies an excerpt even for a short follow-up without matching words")

  other <- file.path(root,"other.md");writeLines("Use lm for regression and report regression coefficients.",other)
  other_id <- knowledge$add(other)$id
  session$setInputs(chat_submit=list(text="Explain regression coefficients",nonce=12L))
  check(!grepl("[K1] other.md",tail(calls,1L)[[1L]]$prompt,fixed=TRUE) &&
    identical(tail(state$messages,1L)[[1L]]$references[[1L]]$name,"week4.md"),
    "explicit selection excludes other matching documents")

  session$setInputs(knowledge_pin_change=list(ids=list(other_id),epoch=state$upload_epoch-1L,nonce=13L))
  check(identical(state$knowledge_files[[1L]]$id,doc),"stale page events cannot change this chat's document selection")
  session$setInputs(knowledge_unpin=list(id=doc,epoch=state$upload_epoch,nonce=14L))
  check(!length(state$knowledge_files),"removing the last scope chip stops document retrieval")
  session$setInputs(chat_submit=list(text="Explain regression coefficients",nonce=15L))
  check(!grepl("KNOWLEDGE BASE EXCERPTS",tail(calls,1L)[[1L]]$prompt,fixed=TRUE) &&
    !length(tail(state$messages,1L)[[1L]]$references) &&
    grepl("Do not attribute this answer",tail(calls,1L)[[1L]]$system,fixed=TRUE),
    "unpinning does not fall back to another matching file or reuse earlier citations")

  session$setInputs(knowledge_pin_change=list(ids=list(doc),epoch=state$upload_epoch,nonce=16L))
  old_receipt <- receipt$text
  replacement <- file.path(root,"replacement.md");writeLines("Hierarchical clustering uses hclust and cutree.",replacement)
  session$setInputs(knowledge_files=data.frame(name="week4.md",size=file.info(replacement)$size,
    type="text/markdown",datapath=replacement,stringsAsFactors=FALSE))
  doc <- Filter(function(x)identical(x$name,"week4.md"),knowledge$list())[[1L]]$id
  check(identical(state$knowledge_files[[1L]]$id,doc) &&
    identical(exact[[1L]]$knowledge_read$text,old_receipt),"replacement updates the selected file without changing earlier receipts")

  app$ask_ai <- function(...)stop("Simulated offline")
  session$setInputs(chat_submit=list(text="Explain hierarchical clustering",nonce=17L))
  failed <- Filter(function(m)identical(m$role,"You"),state$messages)
  check(identical(tail(failed,1L)[[1L]]$knowledge_read$delivery,"failed"),"a failed request never claims the receipt was delivered")
  app$ask_ai <- fake_ai

  session$setInputs(chat_submit=list(text="Run my clustering script and interpret it",nonce=18L))
  session$setInputs(no=1L)
  declined <- tail(Filter(function(m)identical(m$role,"You"),state$messages),1L)[[1L]]
  check(identical(declined$knowledge_read$delivery,"not_sent"),"declining validation marks its prepared material as not sent")

  saved_selection <- history$load(state$chat$id)
  count <- length(history$list())
  session$setInputs(new_chat=1L)
  check(!length(state$knowledge_files) && length(history$list())==count,
    "a new chat resets document selection and does not save a blank chat")
  adopt_chat(saved_selection)
  check(identical(state$knowledge_files[[1L]]$id,doc) &&
    any(vapply(state$messages,function(m)!is.null(m$knowledge_read) && identical(m$knowledge_read$text,old_receipt),logical(1))),
    "restoring the chat recovers its selection and historical excerpt text")

  knowledge$remove(doc)
  session$setInputs(chat_submit=list(text="Explain regression coefficients",nonce=19L))
  missing <- tail(Filter(function(m)identical(m$role,"You"),state$messages),1L)[[1L]]$knowledge_read
  check(identical(missing$status,"missing") && !nzchar(missing$text) &&
    !grepl("KNOWLEDGE BASE EXCERPTS",tail(calls,1L)[[1L]]$prompt,fixed=TRUE),
    "a missing selected file is reported without silently substituting another document")

  session$setInputs(knowledge_pin_change=list(ids=list(),epoch=state$upload_epoch,nonce=24L))
  check(!length(state$knowledge_files) && length(knowledge$list())==1L,
    "unchecking every file removes chat context without deleting stored documents")
  session$setInputs(knowledge_delete=list(id=other_id,epoch=state$upload_epoch-1L,nonce=25L),knowledge_confirm_remove=1L)
  check(length(knowledge$list())==1L,"a Delete request from a previous chat cannot remove a document")
  session$setInputs(knowledge_delete=list(id=other_id,epoch=state$upload_epoch,nonce=26L))
  check(length(knowledge$list())==1L && !length(state$knowledge_files),
    "an unchecked file can be chosen for deletion without checking it or deleting it before confirmation")
  session$setInputs(knowledge_cancel_remove=1L)
  session$setInputs(knowledge_confirm_remove=2L)
  check(length(knowledge$list())==1L,"Cancel clears the deletion target and a later confirmation does nothing")
  session$setInputs(knowledge_delete=list(id=other_id,epoch=state$upload_epoch,nonce=27L))
  session$setInputs(knowledge_confirm_remove=3L)
  check(!length(knowledge$list()) && length(history$list())==1L && file.exists(other) &&
    identical(readLines(other),"Use lm for regression and report regression coefficients."),
    "per-file Delete removes only the stored document and preserves chats and the original file")
})

fresh <- load_assistant(package_info=list(version="0.3.0",history=history,knowledge=knowledge))
fresh$initial <- app$initial;fresh$active_doc <- app$active_doc;fresh$ask_ai <- fake_ai
shiny::testServer(fresh$server, {
  session$flushReact()
  check(any(vapply(state$messages,function(m)length(m$references)>0L,logical(1))),
    "restored chats keep the course sources shown under answers")
  check(identical(knowledge$profile()$instructions,instructions),
    "custom instructions persist after documents are removed and the app restarts")
  large <- file.path(root,"large-blocks.md")
  writeLines(c("```r",paste0('first_block <- "',strrep("x",7000L),'"'),"```","",
    "```r",paste0('second_block <- "',strrep("y",7000L),'"'),"```"),large)
  session$setInputs(knowledge_files=data.frame(name="large-blocks.md",size=file.info(large)$size,
    type="text/markdown",datapath=large,stringsAsFactors=FALSE))
  id <- knowledge$list()[[1L]]$id
  session$setInputs(knowledge_pin_change=list(ids=list(id),epoch=state$upload_epoch,nonce=900L))
  session$setInputs(chat_submit=list(text="Explain this",nonce=901L))
  receipt <- tail(Filter(function(x)identical(x$role,"You"),state$messages),1L)[[1L]]$knowledge_read
  check(identical(receipt$status,"limit") && length(receipt$notices)==2L && !nzchar(receipt$text) &&
    !grepl("KNOWLEDGE BASE EXCERPTS",tail(calls,1L)[[1L]]$prompt,fixed=TRUE),
    "Send reports both oversized complete blocks without silently sending their prefixes")
  restored <- fresh$restore_chat_data(history$load(state$chat$id))
  restored_receipt <- tail(Filter(function(x)identical(x$role,"You"),restored$messages),1L)[[1L]]$knowledge_read
  check(identical(restored_receipt$notices,receipt$notices) && identical(restored_receipt$status,"limit"),
    "multiple structural-limit notices survive JSON history storage and restoration")
  restored_receipt$notices <- c(restored_receipt$notices,"<script>inert_reference_notice</script>")
  markup <- as.character(fresh$knowledge_receipt_ui(restored_receipt))
  check(grepl("Matching content exceeds excerpt limit",markup,fixed=TRUE) &&
    grepl("&lt;script&gt;inert_reference_notice",markup,fixed=TRUE) && !grepl("<script>",markup,fixed=TRUE),
    "limit receipts use a specific summary and render all omission notices as inert text")
})
cat("Completed",checks,"knowledge context checks\n")
