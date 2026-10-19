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
  check(!grepl("COURSE",first$system,fixed=TRUE) && !grepl("COURSE MATERIAL",first$prompt,fixed=TRUE) &&
    !length(state$messages[[2L]]$references),"without course knowledge, requests are unchanged")

  session$setInputs(knowledge=1L)
  session$setInputs(knowledge_files=data.frame(name="week4.md",size=file.info(notes)$size,
    type="text/markdown",datapath=notes,stringsAsFactors=FALSE))
  check(identical(vapply(knowledge$list(),function(x)x$name,character(1)),"week4.md") && !file.exists(notes),
    "the Knowledge dialog adds an uploaded file and releases the upload")
  session$setInputs(knowledge_instructions=instructions,knowledge_save=1L)
  check(identical(knowledge$profile()$instructions,instructions),"the Knowledge dialog saves custom instructions")

  session$setInputs(chat_submit=list(text="How should I choose k for kmeans clustering?",nonce=2L))
  request <- tail(calls,1L)[[1L]]
  check(grepl("CUSTOM INSTRUCTIONS",request$system,fixed=TRUE) &&
    grepl(instructions,request$system,fixed=TRUE) &&
    grepl("in preference to generic alternatives",request$system,fixed=TRUE),
    "requests carry custom instructions and the lecture-first instruction")
  check(grepl("COURSE MATERIAL (retrieved locally for this request):\n[K1] week4.md, lines 1-4",request$prompt,fixed=TRUE) &&
    grepl("fviz_nbclust",request$prompt,fixed=TRUE),"relevant course excerpts are sent with their source label")
  answer <- tail(state$messages,1L)[[1L]]
  check(identical(answer$references[[1L]]$name,"week4.md") && identical(answer$references[[1L]]$label,"lines 1-4"),
    "the answer records which course material was supplied")

  session$setInputs(chat_submit=list(text="Translate hello into French",nonce=3L))
  unrelated <- tail(calls,1L)[[1L]]
  check(grepl("CUSTOM INSTRUCTIONS",unrelated$system,fixed=TRUE) &&
    !grepl("COURSE MATERIAL",unrelated$prompt,fixed=TRUE) && !length(tail(state$messages,1L)[[1L]]$references),
    "unrelated requests send custom instructions but no course excerpts")

  session$setInputs(chat_submit=list(text="Run my kmeans clustering script and interpret it",nonce=4L))
  check(is.list(state$pending$course) && grepl("fviz_nbclust",state$pending$course$text,fixed=TRUE),
    "a run request keeps the course context for the later analysis")
  session$setInputs(yes=1L)
  analysis <- tail(calls,1L)[[1L]]
  check(grepl("COURSE MATERIAL",analysis$prompt,fixed=TRUE) && grepl("CUSTOM INSTRUCTIONS",analysis$system,fixed=TRUE) &&
    identical(tail(state$messages,1L)[[1L]]$references[[1L]]$name,"week4.md"),
    "execution analysis uses course material and records its sources")

  saved <- history$load(state$chat$id)
  restored <- Filter(function(m)length(m$references),saved$messages)
  check(length(restored)>=2L && identical(restored[[1L]]$references[[1L]]$name,"week4.md") &&
    !any(grepl("fviz_nbclust",unlist(lapply(saved$messages,function(m)m$text)),fixed=TRUE)),
    "chat history saves source labels but not the course excerpts themselves")

  doc <- knowledge$list()[[1L]]$id
  session$setInputs(knowledge_selected=doc,knowledge_remove=1L,knowledge_confirm_remove=1L)
  check(!length(knowledge$list()) && length(history$list())==1L,"removing course material leaves chats intact")
})

fresh <- load_assistant(package_info=list(version="0.3.0",history=history,knowledge=knowledge))
fresh$initial <- app$initial;fresh$active_doc <- app$active_doc;fresh$ask_ai <- fake_ai
shiny::testServer(fresh$server, {
  session$flushReact()
  check(any(vapply(state$messages,function(m)length(m$references)>0L,logical(1))),
    "restored chats keep the course sources shown under answers")
  check(identical(knowledge$profile()$instructions,instructions),
    "custom instructions persist after documents are removed and the app restarts")
})
cat("Completed",checks,"knowledge context checks\n")
