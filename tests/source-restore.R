# Multi-step Undo/Redo, persistent history, branch invalidation and write guards.
source("tests/helpers.R")
app <- load_assistant();checks <- 0L
check <- function(ok,label){if(!isTRUE(ok))stop(label,call.=FALSE);checks<<-checks+1L;cat("PASS",label,"\n")}
directory <- tempfile("restore-history-");dir.create(directory)
editor <- new.env();editor$ctx <- list(id="doc",path="/tmp/restore.R",contents=c("x <- 0",""));editor$writes <- list()
set_text <- function(text)editor$ctx$contents <- strsplit(paste0(text,"\n"),"\n",fixed=TRUE)[[1L]]
configure <- function(app) {
  app$initial <- editor$ctx;app$active_doc <- function(id=NULL)editor$ctx
  app$write_source <- function(id,text){editor$writes[[length(editor$writes)+1L]]<-text;set_text(text)}
  app$ask_ai <- function(...)editor$reply
  app$edit_store <- app$source_edit_store(directory,project=directory)
  app
}
app <- configure(app)
shiny::testServer(app$server,{
  nonce <- 0L
  preview <- function(direction){nonce<<-nonce+1L;do.call(session$setInputs,setNames(list(nonce),paste0("preview_",direction)));session$flushReact();undo_job()}
  confirm <- function(job,direction=job$direction){nonce<<-nonce+1L;do.call(session$setInputs,setNames(list(list(token=job$token,nonce=nonce)),paste0("confirm_source_",direction)));session$flushReact()}
  apply_edit <- function(text){
    editor$reply <- paste0("```r-replace\n",text,"```");nonce<<-nonce+1L
    session$setInputs(chat_submit=list(text="Fix code",nonce=nonce));session$flushReact();invisible(output$source_change_preview)
    job <- get("preview_job",envir=environment(clear_source_review),inherits=FALSE)
    session$setInputs(apply_source_change=list(token=job$token,nonce=nonce));session$flushReact()
  }
  session$flushReact()
  check(!restore_controls()$undo && !restore_controls()$redo,"Undo and Redo start unavailable when the Source has no AI edit")
  session$setInputs(preview_undo=1L);session$flushReact()
  check(!state$edit_panel && is.null(undo_job()),"an unavailable Undo request quietly leaves the chat alone")
  apply_edit("x <- 1\n");apply_edit("x <- 2\n")
  check(restore_controls()$undo && !restore_controls()$redo,"an approved AI edit makes only Undo available")
  messages <- length(state$messages);count <- length(editor$writes)
  first <- preview("undo")
  check(state$edit_panel && first$direction=="undo" && length(editor$writes)==count && length(state$messages)==messages,
    "Undo opens the separate history view without inserting a chat message or editing Source")
  confirm(first,"redo")
  check(length(editor$writes)==count && !is.null(undo_job()),"an Undo approval cannot authorize Redo")
  confirm(first);check(identical(app$full_text(editor$ctx),"x <- 1\n"),"Undo restores the latest original edit")
  confirm(preview("undo"));check(identical(app$full_text(editor$ctx),"x <- 0\n"),"a second Undo restores the preceding edit")
  check(!restore_controls()$undo && restore_controls()$redo,"exhausting the Undo stack disables Undo and enables Redo")
  confirm(preview("redo"));check(identical(app$full_text(editor$ctx),"x <- 1\n"),"Redo reapplies the last undone edit first")
  confirm(preview("redo"));check(identical(app$full_text(editor$ctx),"x <- 2\n"),"a second Redo restores the next edit in order")
  check(restore_controls()$undo && !restore_controls()$redo,"exhausting the Redo stack disables Redo")
  events <- app$edit_store$list()
  check(sum(vapply(events,function(e)e$operation=="redo" && e$status=="applied",logical(1)))==2L,
    "each successful Redo has its own exact before/after audit record")
  confirm(preview("undo"));set_text("manual <- 99\n");count <- length(editor$writes)
  blocked <- preview("redo");confirm(blocked)
  check(blocked$conflict && length(editor$writes)==count,"manual Source changes before Redo preview block replacement")
  set_text("x <- 1\n");pending <- preview("redo");set_text("manual <- 88\n");confirm(pending)
  check(is.null(undo_job()) && length(editor$writes)==count,"manual typing after Redo preview consumes and revokes approval")
  set_text("x <- 1\n");pending <- preview("redo");confirm(pending);count <- length(editor$writes);confirm(pending)
  check(identical(app$full_text(editor$ctx),"x <- 2\n") && length(editor$writes)==count,"duplicate Redo confirmation cannot write twice")
  confirm(preview("undo"));old <- app$source_restore_entry(app$edit_store$list(),editor$ctx,"redo")
  apply_edit("x <- 3\n")
  check(app$edit_store$get(old$id)$status=="superseded" && is.null(preview("redo")),"a new approved edit invalidates the abandoned Redo branch")
  confirm(preview("undo"));confirm(preview("redo"))
  check(identical(app$full_text(editor$ctx),"x <- 3\n"),"the new branch still supports Undo and Redo")
  confirm(preview("undo"));pending <- preview("redo");session$setInputs(edit_history_close=nonce+1L);session$flushReact();count<-length(editor$writes);confirm(pending)
  check(!state$edit_panel && is.null(undo_job()) && length(editor$writes)==count,"Back to chat cancels a pending restore approval")
})
# A new app instance and document id may restore a saved file's durable history.
editor$ctx$id <- "reopened-doc"
app <- configure(load_assistant())
shiny::testServer(app$server,{
  session$flushReact();session$setInputs(preview_redo=1L);session$flushReact();job <- undo_job()
  check(!is.null(job) && job$direction=="redo" && !job$conflict,"Redo history survives app relaunch for the matching saved file")
  session$setInputs(confirm_source_redo=list(token=job$token,nonce=1L));session$flushReact()
  check(identical(app$full_text(editor$ctx),"x <- 3\n"),"reopened Redo restores the exact saved after-code")
  count <- length(editor$writes);session$setInputs(preview_undo=1L);session$flushReact();job<-undo_job()
  original_begin <- app$edit_store$begin;app$edit_store$begin <- function(...)stop("Log unavailable")
  session$setInputs(confirm_source_undo=list(token=job$token,nonce=2L));session$flushReact()
  check(length(editor$writes)==count && is.null(undo_job()),"a restore log failure prevents Source writes and consumes approval")
  app$edit_store$begin <- original_begin
  session$setInputs(preview_undo=2L);session$flushReact();job<-undo_job()
  app$write_source <- function(id,text){editor$writes[[length(editor$writes)+1L]]<-text;set_text(text);stop("Uncertain native write")}
  session$setInputs(confirm_source_undo=list(token=job$token,nonce=3L));session$flushReact()
  events <- app$edit_store$list()
  check(events[[1L]]$status=="uncertain" && is.null(undo_job()),"an ambiguous native restore is recorded as uncertain and cannot be retried")
  check(inherits(tryCatch(app$source_restore_entry(events,editor$ctx,"redo"),error=identity),"error"),"an uncertain latest transaction blocks further Redo")
})
entries <- app$edit_store$list();other <- editor$ctx;other$path <- "/tmp/other.R"
check(is.null(app$source_restore_entry(entries,other,"undo")),"one file cannot undo another file's edits")
entry <- entries[[length(entries)]];entry$path <- "";entry$session <- "original-session";entry$document_id <- "untitled-doc"
untitled <- list(id="untitled-doc",path="")
check(app$source_edit_matches(entry,untitled,"original-session") && !app$source_edit_matches(entry,untitled,"new-session"),
  "unsaved documents require both their original document id and app session")
# Older versions retained undone statuses after creating a new branch.
old <- entry;old$path <- "/tmp/legacy.R";old$id <- "old-edit";old$status <- "undone";old$operation <- "replace"
event <- old;event$id <- "undo-event";event$status <- "applied";event$operation <- "undo";event$original_id <- old$id
new <- old;new$id <- "new-edit";new$status <- "applied"
legacy <- list(id="legacy",path="/tmp/legacy.R")
check(is.null(app$source_restore_entry(list(new,event,old),legacy,"redo")),
  "legacy logs cannot resurrect a Redo branch after a newer original edit")
bounded <- app$source_edit_store()
for(i in 1:100){entry$id<-NULL;entry$original_id<-NULL;entry$status<-NULL;id<-bounded$begin(entry);bounded$finish(id,"applied");if(i==1L)oldest<-id}
undo <- entry;undo$operation <- "undo";undo$original_id <- oldest
id <- bounded$begin(undo);bounded$finish(id,"applied");bounded$finish(oldest,"undone")
check(length(bounded$list())==100L && bounded$get(oldest)$status=="undone",
  "retention keeps the original backup while committing an Undo at the 100-record boundary")
unlink(directory,recursive=TRUE)
cat(checks,"Source restore checks passed\n")
