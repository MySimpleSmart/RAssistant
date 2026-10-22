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
# Minimal line edits are addressed against the exact document, not guessed
# from matching text or interpreted as whole-file replacements.
block <- function(text)app$source_code_blocks(as.character(app$render_markdown(text)))[[1L]]
script <- list(id="patch",path="/tmp/patch.R",contents=c("keep <- 1","x <- bad(X)","keep <- 2","y <- bad(Y)","# Keep final comment"))
snap <- app$source_snapshot(script)
first <- block("```r-edit-2-2\nx <- dim(X)\n```")
second <- block("```r-edit-4-4\ny <- dim(Y)\n```")
check(first$operation=="edit" && identical(first$lines,c(2L,2L)) && first$kind=="R",
  "an edit fence carries exact global line numbers while retaining executable R code")
expected <- "keep <- 1\nx <- dim(X)\nkeep <- 2\ny <- dim(Y)\n# Keep final comment"
check(identical(app$prepare_source_patches(snap,list(second,first)),expected),
  "disjoint patches preserve every other line, their order and the absence of a final newline")
check(inherits(tryCatch(app$prepare_source_patches(snap,list(first,first)),error=identity),"error"),
  "overlapping corrections cannot be silently combined")
for(range in c("3-2","9-9")) {
  bad <- block(paste0("```r-edit-",range,"\nx <- 9\n```"))
  check(inherits(tryCatch(app$prepare_source_patches(snap,list(bad)),error=identity),"error"),
    paste("an invalid or out-of-document range is rejected:",range))
}
check(!block("```r-edit-0-2\nx <- 9\n```")$reviewable,"zero-based line ranges do not become reviewable edits")
scope <- app$source_patch_scope(snap,first);scoped <- snap;scoped$scope <- scope
check(inherits(tryCatch(app$prepare_source_patches(scoped,list(second)),error=identity),"error"),
  "a patch cannot broaden the user's selected range")
markdown <- list(id="rmd-patch",path="/tmp/patch.qmd",contents=c("---","title: Keep","---","# Results",
  "```{r first}","#| label: analysis","x <- sum(","  1, broken(","  2)","print(x)","```","Keep this paragraph.",
  "```{r unrelated}","z <-","```"))
md_snap <- app$source_snapshot(markdown)
repair <- block("```r-edit-8-8\n  1,\n```")
check(identical(app$source_patch_scope(md_snap,repair)$chunk,"analysis"),
  "a global Rmd line edit identifies its actual enclosing chunk")
check(inherits(tryCatch(app$prepare_source_patches(md_snap,list(block("```r-edit-4-5\nx <- 1\n```"))),error=identity),"error"),
  "an R patch cannot replace prose or swallow an Rmd chunk fence")
prose <- block("```rmd-edit-12-12\nUpdated paragraph.\n```")
check(grepl("Updated paragraph.",app$prepare_source_patches(md_snap,list(prose)),fixed=TRUE),
  "Markdown edits can explicitly target prose without replacing chunks")
check(app$source_kind("notes.md")=="Rmd" && grepl("analysis: body lines 6-10",app$source_outline(markdown),fixed=TRUE),
  "Markdown documents and chunk outlines expose their real document structure")
editor$ctx <- script;app$initial <- script
reply <- "Two corrections.\n\n```r-edit-2-2\nx <- dim(X)\n```\n\n```r-edit-4-4\ny <- dim(Y)\n```"
shiny::testServer(app$server,{
  nonce <- 100L
  job <- function()get("preview_job",envir=environment(clear_source_review),inherits=FALSE)
  send <- function(){nonce<<-nonce+1L;session$setInputs(chat_submit=list(text="Fix these errors",nonce=nonce));session$flushReact();if(!is.null(review_context()))invisible(output$source_change_preview)}
  session$flushReact();count<-length(editor$writes);send()
  check(identical(job()$after,expected) && length(job()$changes)==2L && length(editor$writes)==count,
    "multiple minimal blocks combine into separately reviewable changes without writing Source")
  check(grepl("2 | x <- bad(X)",tail(editor$calls,1L)[[1L]]$prompt,fixed=TRUE) &&
    grepl("provide ONLY the corrected Source ranges",tail(editor$calls,1L)[[1L]]$system,fixed=TRUE),
    "Send supplies numbered current Source and requests only the changed ranges")
  preview <- output$source_change_preview$html
  check(length(gregexpr('class="source-proposal-text',preview,fixed=TRUE)[[1L]])==2L &&
    grepl('hidden="hidden">x &lt;- dim(X)',preview,fixed=TRUE) &&
    !grepl('hidden="hidden">keep',preview,fixed=TRUE),
    "each hunk has its own Copy containing only corrected code, without unrelated file contents")
  session$setInputs(apply_source_change=list(token=job()$token,change=1L,nonce=200L));session$flushReact();invisible(output$source_change_preview)
  check(length(editor$writes)==count+1L && editor$ctx$contents[[2L]]=="x <- dim(X)" &&
    editor$ctx$contents[[4L]]=="y <- bad(Y)" && length(job()$changes)==1L,
    "Replace applies one approved range and leaves the other correction pending")
  session$setInputs(skip_source_change=list(token=job()$token,change=1L,nonce=201L));session$flushReact()
  check(is.null(job()) && editor$ctx$contents[[4L]]=="y <- bad(Y)","Skip never applies the remaining patch")
  editor$ctx <- script;reply <<- "```r-edit-2-2\nx <- dim(X)\n```";send();token<-job()$token
  editor$ctx$contents[[1L]] <- "manual <- 99"
  session$setInputs(apply_source_change=list(token=token,nonce=202L));session$flushReact()
  check(is.null(job()) && editor$ctx$contents[[1L]]=="manual <- 99" && editor$ctx$contents[[2L]]=="x <- bad(X)",
    "a changed baseline revokes line-based proposals before they can write")
  editor$ctx <- markdown;reply <<- "```r-edit-8-8\n  1,\n```";send()
  check(job()$syntax$ok && job()$changes[[1L]]$line==8L &&
    grepl("z <-",job()$after,fixed=TRUE),"a one-line repair checks the complete repaired chunk while leaving another broken chunk alone")
  session$setInputs(apply_source_change=list(token=job()$token,nonce=203L));session$flushReact()
  check(editor$ctx$contents[[8L]]=="  1," && editor$ctx$contents[[14L]]=="z <-" && editor$ctx$contents[[12L]]=="Keep this paragraph.",
    "approving an Rmd line repair preserves prose, fences and unrelated chunk errors")
  reply <<- "```r-edit-8-8\n  3,\n```\n\n```r-edit-8-9\n  4)\n```";send()
  check(is.null(review_context()),"conflicting patch blocks do not open a guessed automatic correction")
  reply <<- "```r-edit-8-8\n  1,\n```";send()
  check(is.null(review_context()),"an unchanged line patch does not open an empty correction")
  two <- markdown;two$contents[[14L]] <- "z <- sum("
  editor$ctx <- two
  reply <<- "```r-edit-7-9\nx <- sum(\n  1,\n  3)\n```\n\n```r-edit-14-14\nz <- sum(1, 2)\n```";send()
  check(job()$syntax$ok && length(job()$changes)==2L,
    "two Rmd chunk repairs can be reviewed together without including complete chunk bodies")
  session$setInputs(apply_source_change=list(token=job()$token,change=2L,nonce=204L));session$flushReact()
  if(!is.null(review_context()))invisible(output$source_change_preview)
  check(editor$ctx$contents[[14L]]=="z <- sum(1, 2)" && !is.null(job()),
    "one repaired chunk can be applied while another proposed chunk change remains pending")
})
editor$ctx <- script;app$initial <- script;runs <- 0L
app$validate_script <- function(...){runs<<-runs+1L;list(ok=FALSE,logs="Error: could not find function bad",plots=character(),plot_dir=NULL,total_plots=0L)}
reply <- "```r-edit-2-2\nx <- dim(X)\n```"
shiny::testServer(app$server,{
  session$flushReact();count<-length(editor$writes)
  session$setInputs(chat_submit=list(text="Please run the script",nonce=300L));session$flushReact()
  check(runs==0L && !is.null(state$pending),"validation still requires approval before any execution")
  session$setInputs(yes=1L);session$flushReact();invisible(output$source_change_preview)
  candidate <- get("preview_job",envir=environment(clear_source_review),inherits=FALSE)
  check(runs==1L && !is.null(candidate) && length(candidate$changes)==1L &&
    candidate$changes[[1L]]$line==2L && length(editor$writes)==count,
    "a failed approved run produces a minimal checked correction without editing Source")
  check(grepl("1 | keep <- 1\n2 | x <- bad(X)",tail(editor$calls,1L)[[1L]]$prompt,fixed=TRUE) &&
    grepl("r-edit-START-END",tail(editor$calls,1L)[[1L]]$system,fixed=TRUE),
    "validation analysis gets the full numbered script and the same minimal-edit contract")
})
cat(checks,"Source workflow checks passed\n")
