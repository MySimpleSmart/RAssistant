# Granular approvals, Source navigation and selected-code actions use fake APIs.
source("tests/helpers.R")
app <- load_assistant()
checks <- 0L
check <- function(ok,label){if(!isTRUE(ok))stop(label,call.=FALSE);checks<<-checks+1L;cat("PASS",label,"\n")}
before <- "a <- 1\nkeep <- 9\nz <- 1"
after <- "a <- 2\nkeep <- 9\nz <- 2"
changes <- app$source_changes(before,after)
check(length(changes)==2L && identical(app$apply_source_changes(before,changes,1L),"a <- 2\nkeep <- 9\nz <- 1") &&
  identical(app$apply_source_changes(before,changes,2L),"a <- 1\nkeep <- 9\nz <- 2"),
  "independent changes can be applied alone without touching the other change or final newline")
cases <- list(c("","x\n"),c("a\nb\nc\n","new\na\nb\nC\n"),c("a\nb\nc","a\nc"),
  c("a\nb\n","a\nb"),c("a\nb","a\nb\n"),c("a\n\nb\n","A\n\nb\nlast\n"),c("# Монгол\nx <- 1\n","# Монгол\nx <- 2\n"))
check(all(vapply(cases,function(pair)identical(app$apply_source_changes(pair[[1L]],app$source_changes(pair[[1L]],pair[[2L]])),pair[[2L]]),logical(1))),
  "insertions, deletions, empty files, Unicode and final newlines round-trip exactly")
ctx <- list(id="doc",path="/tmp/example.R",contents=c("keep <- 1","x <- bad(X) + 5","keep <- 2"),
  selection=list(list(text="bad(X)",range=list(start=c(2,6),end=c(2,12)))))
scope <- app$source_selection(ctx)
snapshot <- app$source_snapshot(ctx);snapshot$scope <- scope
proposal <- app$prepare_source_change(snapshot,list(code="dim(X)\n",kind="R",reviewable=TRUE),"selection")
check(identical(proposal$after,"keep <- 1\nx <- dim(X) + 5\nkeep <- 2"),
  "selected expression replacement preserves code before and after it on the same line")
emoji <- ctx;emoji$contents <- "# 😀 bad";emoji$selection <- list(list(text="bad",range=list(start=c(1,6),end=c(1,9))))
check(identical(app$source_selection(emoji)$text,"bad"),"UTF-16 editor columns after emoji resolve to the exact selected text")
bad <- ctx;bad$selection[[1L]]$text <- "stale"
check(inherits(tryCatch(app$source_selection(bad),error=identity),"error"),"a mismatched selection is rejected rather than guessed")
bad$selection <- rep(ctx$selection,2L)
check(inherits(tryCatch(app$source_selection(bad),error=identity),"error"),"multiple selections require an explicit single range")
rmd <- ctx;rmd$path <- "/tmp/test.Rmd";rmd$contents <- c("---","title: Keep","---","```{r}",ctx$contents[[2L]],"```","Keep this text.")
rmd$selection <- list(list(text="bad(X)",range=list(start=c(5,6),end=c(5,12))))
snap <- app$source_snapshot(rmd);snap$scope <- app$source_selection(rmd)
change <- app$prepare_source_change(snap,list(code="dim(X)\n",kind="R",reviewable=TRUE),"selection")
check(change$syntax$ok && identical(change$after,sub("bad(X)","dim(X)",app$full_text(rmd),fixed=TRUE)),
  "an R fragment inside Rmd changes only its selected expression and preserves fences, YAML and prose")

editor <- new.env();editor$ctx <- list(id="doc",path="/tmp/example.R",contents=strsplit(before,"\n",fixed=TRUE)[[1L]])
editor$writes <- list();editor$jumps <- list();editor$calls <- list()
app$initial <- editor$ctx
app$active_doc <- function(id=NULL)editor$ctx
app$write_source <- function(id,text){editor$writes[[length(editor$writes)+1L]]<-list(id=id,text=text);editor$ctx$contents<-strsplit(paste0(text,"\n"),"\n",fixed=TRUE)[[1L]]}
app$focus_source <- function(id,row,column,end_column)editor$jumps[[length(editor$jumps)+1L]]<-list(id=id,row=row,column=column,end_column=end_column)
reply <- paste0("```r-replace\n",after,"\n```")
app$ask_ai <- function(system_prompt,prompt,images=character()){
  editor$calls[[length(editor$calls)+1L]]<-list(system=system_prompt,prompt=prompt)
  reply
}
shiny::testServer(app$server,{
  nonce <- 0L
  job <- function()get("preview_job",envir=environment(clear_source_review),inherits=FALSE)
  send <- function(action=NULL){nonce<<-nonce+1L;session$setInputs(chat_submit=list(text="Review code",selection_action=action,nonce=nonce));session$flushReact();if(!is.null(review_context()))invisible(output$source_change_preview)}
  apply_one <- function(index,token=job()$token){nonce<<-nonce+1L;session$setInputs(apply_source_change=list(token=token,change=index,nonce=nonce));session$flushReact();if(!is.null(review_context()))invisible(output$source_change_preview)}
  skip <- function(index,token=job()$token){nonce<<-nonce+1L;session$setInputs(skip_source_change=list(token=token,change=index,nonce=nonce));session$flushReact();if(!is.null(review_context()))invisible(output$source_change_preview)}
  session$flushReact();send()
  check(length(job()$changes)==2L && !length(editor$writes),"multiple changes appear as proposals before any editor mutation")
  old <- job()$token
  session$setInputs(jump_source_line=list(token=old,line=3L));session$flushReact()
  check(length(editor$jumps)==1L && identical(editor$jumps[[1L]]$id,"doc") && editor$jumps[[1L]]$row==3L && !length(editor$writes),
    "a Source link selects the verified document and exact line without editing it")
  session$setInputs(jump_source_line=list(token="forged",line=1L));session$flushReact()
  session$setInputs(jump_source_line=list(token=old,line=999L));session$flushReact()
  check(length(editor$jumps)==1L,"forged navigation tokens and out-of-range lines cannot target Source")
  apply_one(1L)
  check(length(editor$writes)==1L && identical(app$full_text(editor$ctx),"a <- 2\nkeep <- 9\nz <- 1") &&
    !is.null(job()) && length(job()$changes)==1L && !identical(job()$token,old),
    "Apply affects one change and regenerates a fresh approval for the remaining change")
  apply_one(1L,old)
  check(length(editor$writes)==1L,"an old per-change button cannot approve another remaining change")
  skip(1L)
  check(length(editor$writes)==1L && is.null(job()),"Skip leaves Source untouched and closes a completed review")
  editor$ctx$contents <- strsplit(before,"\n",fixed=TRUE)[[1L]];send();skip(1L)
  check(length(job()$changes)==1L && startsWith(job()$after,"a <- 1\n"),"Skip removes only its proposal while preserving the other proposed change")
  apply_one(1L)
  check(identical(app$full_text(editor$ctx),paste0("a <- 1\nkeep <- 9\nz <- 2\n")),"applying after Skip does not reintroduce the skipped change")
  editor$ctx$contents <- strsplit(before,"\n",fixed=TRUE)[[1L]];send();stale <- job()$token
  editor$ctx$contents[[2L]] <- "manual <- 99";count <- length(editor$writes);apply_one(1L,stale)
  check(length(editor$writes)==count && is.null(job()),"manual Source edits revoke granular approval before writing")
  editor$ctx <- ctx;reply <<- "```r-replace\ndim(X)\n```";send("fix")
  check(job()$mode=="selection" && identical(job()$after,proposal$after) &&
    grepl("Work ONLY on the SELECTED SOURCE RANGE",tail(editor$calls,1L)[[1L]]$system,fixed=TRUE) &&
    grepl("bad(X)",tail(editor$calls,1L)[[1L]]$prompt,fixed=TRUE),
    "Fix selected code sends fresh verified range instructions and prepares only that fragment")
  editor$ctx$selection <- list(list(text="keep",range=list(start=c(1,1),end=c(1,5))))
  apply_one(1L)
  check(identical(app$full_text(editor$ctx),proposal$after),"moving the selection does not redirect approval; every surrounding character is preserved")
  editor$ctx <- ctx;send("improve")
  check(job()$mode=="selection" && identical(job()$after,proposal$after),"Improve uses the same selected-range protection")
  count <- length(editor$writes);send("explain")
  check(is.null(review_context()) && length(editor$writes)==count &&
    is.null(tail(state$messages,1L)[[1L]]$source_snapshot),"Explain remains read-only even if the response contains example code")
  editor$ctx <- list(id="doc",path="/tmp/example.R",contents=c("outside <- 1","a <- 1","keep <- 9","z <- 1","outside <- 2"),
    selection=list(list(text="a <- 1\nkeep <- 9\nz <- 1",range=list(start=c(2,1),end=c(4,7)))))
  end_before <- app$source_selection(editor$ctx)$end
  reply <<- "```r-replace\na <- 100\nkeep <- 9\nz <- 2\n```";send("fix");apply_one(1L)
  check(identical(app$full_text(editor$ctx),"outside <- 1\na <- 100\nkeep <- 9\nz <- 1\noutside <- 2") &&
    review_context()$selection_end==end_before+2L && length(job()$changes)==1L,
    "partial approval within a selection adjusts its bounds while preserving all outside code")
  apply_one(1L)
  check(identical(app$full_text(editor$ctx),"outside <- 1\na <- 100\nkeep <- 9\nz <- 2\noutside <- 2"),
    "the remaining selected change applies at its rebased position without touching outside lines")
  before_calls <- length(editor$calls);editor$ctx$selection <- list();send("fix")
  check(length(editor$calls)==before_calls && !state$busy,"an empty selection makes no API call and restores controls")
})
cat(checks,"Source workflow checks passed\n")
