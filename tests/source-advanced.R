# Spacing, durable edit history, conflict-safe Undo, Workspace and R chunks.
source("tests/helpers.R")
app <- load_assistant()
checks <- 0L
check <- function(ok,label){if(!isTRUE(ok))stop(label,call.=FALSE);checks<<-checks+1L;cat("PASS",label,"\n")}
check(app$source_blank_change("x <- 1\n","\nx <- 1\n\n"),"extra Enter presses are spacing, not code errors")
check(!app$source_blank_change('x <- "a\n\nb"','x <- "a\nb"'),"blank lines inside multiline strings remain meaningful changes")
check(!app$source_blank_change("x <- 1\n","x <- 2\n"),"functional corrections are not hidden as spacing")
check(identical(app$without_source_spacing("x <- bad(X)\n","x <- dim(X)\n\n\n","R"),"x <- dim(X)\n"),
  "extra Enter lines adjacent to a real fix are separated rather than bundled into that fix")
check(identical(app$without_source_spacing('x <- "a\nb"\n','x <- "a\n\nb"\n',"R"),'x <- "a\n\nb"\n'),
  "spacing normalization cannot alter the proposed multiline string value")
check(!app$source_blank_change("---\ndescription: |\n  first\n\n  second\n---","---\ndescription: |\n  first\n  second\n---","Rmd"),
  "Markdown and YAML blank lines are not silently discarded as R spacing")
ctx <- list(id="rmd",path="/tmp/test.qmd",contents=c("---","title: Keep","---","Text.","```{r setup}",
  "#| label: clean-data","x <- bad(X)","```","More text.","~~~{python}","not R","~~~","~~~{r}","y <- 1","~~~"),
  selection=list(list(text="",range=list(start=c(7,2),end=c(7,2)))))
chunks <- app$source_chunks(ctx);scope <- app$current_source_chunk(ctx)
check(length(chunks)==2L && scope$chunk=="clean-data" && scope$text=="#| label: clean-data\nx <- bad(X)\n", "Quarto labels and R chunks are found while Python and prose are excluded")
snapshot <- app$source_snapshot(ctx);snapshot$scope <- scope
changed <- app$prepare_source_change(snapshot,list(code="#| label: clean-data\nx <- dim(X)\n",kind="R",reviewable=TRUE),"selection")
check(identical(changed$after,sub("bad(X)","dim(X)",app$full_text(ctx),fixed=TRUE)) && changed$syntax$ok,
  "a chunk-body correction preserves YAML, fences, other chunks and prose exactly")
empty <- list(id="empty",path="/tmp/test.Rmd",contents=c("```{r}","```"),selection=list(list(range=list(start=c(1,1),end=c(1,1)),text="")))
snap <- app$source_snapshot(empty);snap$scope <- app$current_source_chunk(empty)
check(identical(app$prepare_source_change(snap,list(code="x <- 1\n",kind="R",reviewable=TRUE),"selection")$after,"```{r}\nx <- 1\n```"),
  "an empty chunk accepts body code without joining or overwriting the closing fence")
env <- new.env(parent=emptyenv());forced <- FALSE;active <- FALSE;method_called <- FALSE
env$birth_data <- data.frame(age=1:3,weight=c("ROW_SECRET","ROW_SECRET","ROW_SECRET"));class(env$birth_data)<-c("noexec","data.frame")
env$X <- matrix(1:12,nrow=3,dimnames=list(NULL,c("a","b","c","d")));env$api_key <- "KEY_SECRET"
delayedAssign("lazy",{forced<<-TRUE;stop("must not force")},assign.env=env)
makeActiveBinding("active",function(){active<<-TRUE;stop("must not execute")},env)
dim.noexec <- function(x){method_called<<-TRUE;stop("must not dispatch")}
report <- app$workspace_summary(env)
check(!forced && !active && !method_called && grepl("dimensions=3 x 4",report,fixed=TRUE) && grepl("columns=age, weight",report,fixed=TRUE),
  "Workspace dimensions and names are read without forcing promises, active bindings or dim methods")
check(!grepl("ROW_SECRET|KEY_SECRET|api_key|lazy|active",report),"Workspace context excludes values and credential-like object names")
directory <- tempfile("edit-store-");dir.create(directory)
record <- list(before="x <- 1\n",after="x <- 2\n",path="/tmp/x.R",name="x.R",document_id="doc",session="session",operation="replace",summary="Correct x")
store <- app$source_edit_store(directory,project=directory);id <- store$begin(record);store$finish(id,"applied")
restored <- app$source_edit_store(directory,project=directory)
check(identical(restored$get(id)$before,record$before) && restored$get(id)$status=="applied" && length(restored$list())==1L,
  "edit history and exact Undo backups survive a new store instance")
check(inherits(tryCatch(store$get("../../outside"),error=identity),"error"),"history identifiers cannot escape the project directory")
unlink(directory,recursive=TRUE)

editor <- new.env();editor$ctx <- list(id="doc",path="/tmp/x.R",contents=c("x <- 1","","","y <- 1"));editor$writes<-list();editor$calls<-list()
app$initial <- editor$ctx;app$active_doc <- function(id=NULL)editor$ctx
app$write_source <- function(id,text){editor$writes[[length(editor$writes)+1L]]<-text;editor$ctx$contents<-strsplit(paste0(text,"\n"),"\n",fixed=TRUE)[[1L]]}
reply <- "```r-replace\nx <- 2\n\n\ny <- 1\n```"
app$ask_ai <- function(system_prompt,prompt,images=character()){
  editor$calls[[length(editor$calls)+1L]]<-list(system=system_prompt,prompt=prompt);reply
}
shiny::testServer(app$server,{
  nonce <- 0L
  job <- function()get("preview_job",envir=environment(clear_source_review),inherits=FALSE)
  send <- function(action=NULL,scope=NULL){nonce<<-nonce+1L;session$setInputs(chat_submit=list(text="Review code",selection_action=action,scope=scope,nonce=nonce));session$flushReact();if(!is.null(review_context()))invisible(output$source_change_preview)}
  approve <- function(){nonce<<-nonce+1L;session$setInputs(apply_source_change=list(token=job()$token,nonce=nonce));session$flushReact()}
  session$flushReact();before <- app$full_text(editor$ctx);send();approve()
  entries <- app$edit_store$list()
  check(length(entries)==1L && entries[[1L]]$status=="applied" && identical(entries[[1L]]$before,before) &&
    identical(entries[[1L]]$after,app$full_text(editor$ctx)),"approved replacements record the exact before/after Source and applied status")
  count <- length(editor$writes);session$setInputs(preview_undo=1L);session$flushReact()
  undo <- undo_job()
  check(!is.null(undo) && !undo$conflict && length(editor$writes)==count,"Undo displays a comparison and waits for explicit approval")
  session$setInputs(confirm_source_undo=list(token=undo$token,nonce=1L));session$flushReact()
  session$setInputs(confirm_source_undo=list(token=undo$token,nonce=2L));session$flushReact()
  check(length(editor$writes)==count+1L && identical(app$full_text(editor$ctx),before) &&
    app$edit_store$get(entries[[1L]]$id)$status=="undone", "Undo restores the exact backup once and marks the original edit undone")
  check(any(vapply(app$edit_store$list(),function(entry)entry$operation=="undo" && entry$status=="applied",logical(1))),"Undo itself is recorded in the durable audit trail")
  send();approve();editor$ctx$contents[[1L]] <- "manual <- 99"
  count <- length(editor$writes);session$setInputs(preview_undo=2L);session$flushReact();undo<-undo_job()
  session$setInputs(confirm_source_undo=list(token=undo$token,nonce=3L));session$flushReact()
  check(undo$conflict && length(editor$writes)==count && editor$ctx$contents[[1L]]=="manual <- 99", "manual Source changes block Undo instead of being overwritten")
  editor$ctx$contents <- c("x <- 1","","","y <- 1");send();approve()
  session$setInputs(preview_undo=3L);session$flushReact();undo<-undo_job();count<-length(editor$writes)
  editor$ctx$contents <- c(editor$ctx$contents,"# typed after preview")
  session$setInputs(confirm_source_undo=list(token=undo$token,nonce=4L));session$flushReact()
  check(length(editor$writes)==count && tail(editor$ctx$contents,1L)=="# typed after preview", "a manual edit after Undo preview revokes the approval before writing")
  editor$ctx$contents <- c("x <- 1");reply <<- "```r-replace\n\nx <- 1\n\n```";send()
  check(is.null(review_context()),"a spacing-only response does not automatically open an error-fix comparison")
  msg<-tail(state$messages,1L)[[1L]];session$setInputs(review_source_change=list(message=msg$sequence,block=1L,epoch=state$upload_epoch));session$flushReact();invisible(output$source_change_preview)
  check(length(job()$changes)==0L,"manual Review excludes spacing changes by default")
  session$setInputs(toggle_source_formatting=list(token=job()$token));session$flushReact();invisible(output$source_change_preview)
  check(length(job()$changes)>0L && isTRUE(review_context()$include_formatting),"spacing changes require explicitly including them")
  clear_source_review();editor$ctx <- ctx;reply <<- "```r-replace\n#| label: clean-data\nx <- dim(X)\n```";send("fix","chunk")
  check(job()$mode=="selection" && identical(job()$after,changed$after) &&
    grepl("CURRENT R CHUNK: clean-data",tail(editor$calls,1L)[[1L]]$prompt,fixed=TRUE),"Current R chunk Fix targets only the cursor's verified chunk body")
  approve();check(identical(app$full_text(editor$ctx),changed$after),"approving a chunk fix leaves the rest of the document intact")
  editor$ctx <- ctx;editor$ctx$contents[[14L]] <- "y <-";send("fix","chunk")
  check(job()$syntax$ok && grepl("Other chunks were not checked",job()$syntax$text,fixed=TRUE),"an unrelated broken chunk does not block a valid correction to the current chunk")
  approve();check(editor$ctx$contents[[14L]]=="y <-" && editor$ctx$contents[[7L]]=="x <- dim(X)","chunk approval preserves even unrelated existing errors verbatim")
  check(!check_candidate("x <-","R",list(chunk="first",selection_start=100L,selection_end=110L),"","new_r")$ok,
    "choosing a new document from a chunk action checks the entire proposed script")
  session$setInputs(workspace_context=FALSE);reply <<- "Explained.";send()
  check(!grepl("WORKSPACE METADATA (enabled for this turn)",tail(editor$calls,1L)[[1L]]$prompt,fixed=TRUE),"Workspace objects are not inspected or sent while the context toggle is off")
  app$workspace_summary <- function()"X: matrix; dimensions=3 x 4; columns=a, b, c, d"
  session$setInputs(workspace_context=TRUE);send()
  check(grepl("WORKSPACE METADATA (enabled for this turn)",tail(editor$calls,1L)[[1L]]$prompt,fixed=TRUE) &&
    grepl("dimensions=3 x 4",tail(editor$calls,1L)[[1L]]$prompt,fixed=TRUE),"opted-in Workspace metadata reaches the request as read-only current evidence")
  editor$ctx <- list(id="doc",path="/tmp/x.R",contents="x <- 1");reply <<- "```r-replace\nx <- 2\n```"
  app$edit_store$begin <- function(...)stop("Synthetic log write failure");send();count<-length(editor$writes);approve()
  check(length(editor$writes)==count && is.null(job()),"a log write failure prevents an unrecorded Source mutation and consumes approval")
})
cat(checks,"advanced Source checks passed\n")
