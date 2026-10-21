# Smart scope detection and toolbar activation must never broaden stale edits.
source("tests/helpers.R")
app <- load_assistant();checks <- 0L
check <- function(ok,label){if(!isTRUE(ok))stop(label,call.=FALSE);checks<<-checks+1L;cat("PASS",label,"\n")}
script <- list(id="script",path="/tmp/smart.R",contents=c("x <- bad(X)","y <- 1"),
  selection=list(list(text="bad(X)",range=list(start=c(1,6),end=c(1,12)))))
target <- app$smart_source_target(script)
check(target$mode=="selection" && target$scope$text=="bad(X)","a verified selection takes priority over the whole R file")
rmd <- list(id="rmd",path="/tmp/smart.qmd",contents=c("---","title: Keep","---","Keep prose.","```{r first}","x <- bad(X)","```","Other prose.","```{r second}","y <- 1","```"),
  selection=list(list(text="",range=list(start=c(6,1),end=c(6,1)))))
check(app$smart_source_target(rmd)$mode=="chunk" && app$smart_source_target(rmd)$scope$chunk=="first","a cursor inside Rmd/Quarto automatically targets that R chunk")
rmd$selection <- list(list(text="bad(X)",range=list(start=c(6,6),end=c(6,12))))
check(app$smart_source_target(rmd)$mode=="selection","an explicit selection inside a chunk remains more specific than its chunk")
rmd$selection <- list(list(text="",range=list(start=c(4,1),end=c(4,1))))
check(app$smart_source_target(rmd)$mode=="file","a cursor in document prose targets the clearly labelled whole file")
plain <- script;plain$selection <- NULL
check(app$smart_source_target(plain)$mode=="file","an R script without a selection targets the whole file")
invalid <- plain;invalid$contents <- "x <-"
check(app$smart_source_target(invalid)$recommended=="fix","a real syntax failure recommends Fix without running code")
check(app$smart_source_target(plain)$recommended=="explain","valid syntax recommends Explain without claiming the code has no runtime bugs")
bad <- script;bad$selection[[1L]]$text <- "wrong"
check(inherits(tryCatch(app$smart_source_target(bad),error=identity),"error"),"an invalid selection is rejected instead of silently widening to the file")
bad <- script;bad$selection <- rep(bad$selection,2L)
check(inherits(tryCatch(app$smart_source_target(bad),error=identity),"error"),"multiple selections are rejected rather than guessed")
bad <- plain;bad$path <- "/tmp/readme.txt"
check(inherits(tryCatch(app$smart_source_target(bad),error=identity),"error"),"unsupported documents do not activate code editing")
editor <- new.env();editor$ctx <- script;editor$calls<-list();editor$writes<-list();editor$reply<-"Explained."
app$initial <- editor$ctx;app$active_doc <- function(id=NULL)editor$ctx
app$ask_ai <- function(system_prompt,prompt,images=character()){
  editor$calls[[length(editor$calls)+1L]]<-list(system=system_prompt,prompt=prompt);editor$reply
}
app$write_source <- function(id,text){editor$writes[[length(editor$writes)+1L]]<-text;editor$ctx$contents<-strsplit(paste0(text,"\n"),"\n",fixed=TRUE)[[1L]]}
shiny::testServer(app$server,{
  nonce <- 0L
  activate <- function(){nonce<<-nonce+1L;session$setInputs(code_actions=nonce);session$flushReact();code_target()}
  action <- function(name,token=code_target()$token){nonce<<-nonce+1L;session$setInputs(chat_submit=list(text="",selection_action=name,scope="smart",target_token=token,nonce=nonce));session$flushReact();if(!is.null(review_context()))invisible(output$source_change_preview)}
  approve <- function(){job<-get("preview_job",envir=environment(clear_source_review),inherits=FALSE);nonce<<-nonce+1L;session$setInputs(apply_source_change=list(token=job$token,nonce=nonce));session$flushReact()}
  session$flushReact();activate()
  check(length(editor$calls)==0L && length(editor$writes)==0L && grepl("Selected lines 1",output$code_actions_toolbar$html,fixed=TRUE),"activation displays the exact target and makes no AI request or Source write")
  action("explain")
  check(length(editor$calls)==1L && is.null(tail(state$messages,1L)[[1L]]$source_snapshot) && length(editor$writes)==0L,
    "smart Explain stays read-only and carries no editable Source snapshot")
  editor$reply <- "```r\ndim(X)\n```";action("fix")
  job <- get("preview_job",envir=environment(clear_source_review),inherits=FALSE)
  check(job$mode=="selection" && job$after=="x <- dim(X)\ny <- 1" && length(editor$writes)==0L,"smart Fix proposes only the activated selection and waits for approval")
  approve();check(identical(editor$writes[[1L]],"x <- dim(X)\ny <- 1") && is.null(code_target()),"approval preserves surrounding code and retires the old toolbar target")
  editor$ctx <- script;old <- activate();count <- length(editor$calls)
  editor$ctx$selection <- NULL;action("fix",old$token)
  check(length(editor$calls)==count && code_target()$mode=="file" && !identical(code_target()$token,old$token),"changing the selection refreshes the toolbar and requires choosing the action again")
  action("fix",old$token);check(length(editor$calls)==count,"a stale toolbar token cannot send an AI request")
  editor$reply <- "Whole file explained.";action("explain")
  check(is.null(tail(state$messages,1L)[[1L]]$source_snapshot) && grepl("CURRENT FILE CODE ACTION",tail(editor$calls,1L)[[1L]]$system,fixed=TRUE),
    "whole-file Explain remains read-only and uses the explicit file action contract")
  editor$reply <- "```r\nx <- dim(X)\ny <- 1\n```";action("fix")
  job <- get("preview_job",envir=environment(clear_source_review),inherits=FALSE)
  check(job$mode=="replace" && is.null(review_context()$snapshot$scope) && grepl("CURRENT FILE CODE ACTION",tail(editor$calls,1L)[[1L]]$system,fixed=TRUE),"a whole-file toolbar request uses a complete-file proposal rather than a fictitious selection")
  editor$ctx$selection <- script$selection;count<-length(editor$calls);action("fix")
  check(is.null(review_context()) && length(editor$calls)==count && code_target()$mode=="selection",
    "refreshing a changed target also cancels approval for the earlier target's proposal")
  clear_source_review();editor$ctx <- script;old <- activate();count<-length(editor$calls)
  editor$ctx$contents[[2L]] <- "y <- 99";action("improve",old$token)
  check(length(editor$calls)==count && code_target()$snapshot$text=="x <- bad(X)\ny <- 99","changed Source text refreshes the exact snapshot without calling the model")
  old <- code_target();editor$ctx$path <- "/tmp/other.R";action("fix",old$token)
  check(length(editor$calls)==count && code_target()$snapshot$path=="/tmp/other.R" && state$current$path=="/tmp/other.R","switching files refreshes the target and filename instead of redirecting an old action")
  editor$ctx <- rmd;editor$ctx$selection <- list(list(text="",range=list(start=c(6,1),end=c(6,1))))
  activate();editor$reply <- "```r\nx <- dim(X)\n```";action("improve")
  job <- get("preview_job",envir=environment(clear_source_review),inherits=FALSE)
  check(job$mode=="selection" && grepl("CURRENT R CHUNK: first",tail(editor$calls,1L)[[1L]]$prompt,fixed=TRUE),"smart Improve inside an R chunk keeps the chunk-only contract")
  before <- app$full_text(editor$ctx);approve()
  check(identical(app$full_text(editor$ctx),sub("bad(X)","dim(X)",before,fixed=TRUE)),"a smart chunk approval preserves YAML, prose, fences and every other chunk")
  activate();old<-code_target();session$setInputs(new_chat=1L);session$flushReact();count<-length(editor$calls);action("fix",old$token)
  check(is.null(code_target()) && length(editor$calls)==count,"New chat invalidates the previous toolbar and token")
  activate();session$setInputs(clear_code_target=1L);session$flushReact()
  check(is.null(code_target()),"closing the toolbar removes its active target")
})
cat(checks,"smart Source checks passed\n")
