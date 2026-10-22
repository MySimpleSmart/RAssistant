# Source proposals use fake editor APIs; no user files, credentials or AI calls.
source("tests/helpers.R")
app <- load_assistant()
checks <- 0L
check <- function(ok,label){if(!isTRUE(ok))stop(label,call.=FALSE);checks<<-checks+1L;cat("PASS",label,"\n")}
editor <- new.env(parent=emptyenv())
editor$ctx <- list(id="doc-a",path="/tmp/review-example.R",contents=c("x <- 1","y <- x + 1"))
editor$writes <- list();editor$news <- list();editor$fail <- FALSE
app$initial <- editor$ctx
app$active_doc <- function(id=NULL)editor$ctx
app$write_source <- function(id,text){
  editor$writes[[length(editor$writes)+1L]]<-list(id=id,text=text)
  if(editor$fail)stop("Synthetic editor API failure")
  editor$ctx$contents <- strsplit(paste0(text,"\n"),"\n",fixed=TRUE)[[1L]]
}
app$new_source <- function(text,kind){editor$news[[length(editor$news)+1L]]<-list(text=text,kind=kind)}
reply <- "```r-replace\nx <- 2\ny <- x + 1\n```"
app$ask_ai <- function(...)reply
app$validate_script <- function(...)stop("Reading/checking edits must never execute code")

block <- app$source_code_blocks(as.character(app$render_markdown('```r\nx <- c("<tag>", "&lt;", "&", "&#39;")\n```')))[[1L]]
check(identical(block$code,'x <- c("<tag>", "&lt;", "&", "&#39;")\n'),"review code preserves literal R syntax and HTML-like strings exactly")
check(!app$source_code_blocks(as.character(app$render_markdown("```sh\necho unsafe\n```")))[[1L]]$reviewable,
  "shell and other-language blocks do not become R editor proposals")
check(!app$check_source_syntax("x <-", "R")$ok,"invalid R syntax is rejected")
sentinel <- tempfile()
check(app$check_source_syntax(paste0("writeLines('must not run',",encodeString(sentinel,quote='"'),")"),"R")$ok && !file.exists(sentinel),
  "static syntax checking never evaluates side effects")
rmd <- "---\ntitle: Example\n---\nText with `r 1 + 1`.\n```{r}\nx <- 1\n```\n```{python}\nnot R syntax!\n```\n"
check(app$check_source_syntax(rmd,"Rmd")$ok && app$source_kind("",rmd)=="Rmd" &&
  app$source_kind("","---\ntitle: Untitled\n---\nSome text.")=="Rmd",
  "Rmd checks chunks and inline R, recognizes unsaved documents and ignores prose/Python")
check(!app$check_source_syntax("```{r}\nx <-\n```","Rmd")$ok &&
  !app$check_source_syntax("```{r}\nx <- 1","Rmd")$ok && !app$check_source_syntax("Text `r x <-`.","Rmd")$ok &&
  !app$check_source_syntax("    ```{r}\n    x <-\n    ```","Rmd")$ok,
  "invalid R chunks, inline R and unclosed Markdown fences are rejected")
snapshot <- app$source_snapshot(list(id="rmd",path="example.Rmd",contents=strsplit(rmd,"\n",fixed=TRUE)[[1L]]))
appended <- app$prepare_source_change(snapshot,list(code="z <- 3\n",kind="R",reviewable=TRUE),"append")
check(startsWith(appended$after,snapshot$text) && grepl("```{r}\nz <- 3\n```",appended$after,fixed=TRUE) && appended$syntax$ok,
  "appending an R snippet to Rmd preserves the original document and adds an R chunk")
check(any(startsWith(app$source_diff("x <- 1\n","x <- 2\n"),"-x <- 1")) &&
  any(startsWith(app$source_diff("x <- 1\n","x <- 2\n"),"+x <- 2")),"comparison displays both removed and added code")
diff <- app$source_diff_display(paste0(paste(rep("# unchanged",14L),collapse="\n"),"\nrtrtryydim(X)"),
  paste0(paste(rep("# unchanged",14L),collapse="\n"),"\ndim(X)\n"))
check(identical(diff$location,"Source lines 15 / Proposed lines 15") &&
  grepl('diff-remove" data-old-line="15"',diff$html,fixed=TRUE) &&
  grepl('diff-add" data-old-line="" data-new-line="15"',diff$html,fixed=TRUE) &&
  grepl('<mark class="diff-word">rtrtryy</mark>dim(X)',diff$html,fixed=TRUE),
  "the exact reported typo is located on line 15 and highlighted even without an original final newline")
diff <- app$source_diff_display(paste0(paste(rep("# unchanged",13L),collapse="\n"),"\nrtrtryydim(X)\n"),
  paste0(paste(rep("# unchanged",13L),collapse="\n"),"\ndim(X)\n"))
check(identical(diff$location,"Source lines 14 / Proposed lines 14"),
  "a trailing empty Source line does not mislabel the actual faulty line as the file's last line")
diff <- app$source_diff_display("x <- '<script>'\ny <- 2\nz <- 3\n","x <- '<img>'\nadded <- 1\ny <- 2\nz <- 3\n")
check(!grepl("<script>",diff$html,fixed=TRUE) && !grepl("<img>",diff$html,fixed=TRUE) &&
  grepl('diff-context" data-old-line="2" data-new-line="3"',diff$html,fixed=TRUE),
  "numbered comparisons escape code and track different old/new line positions after insertions")

shiny::testServer(app$server,{
  nonce <- 0L
  job <- function()get("preview_job",envir=environment(clear_source_review),inherits=FALSE)
  send <- function(){nonce<<-nonce+1L;session$setInputs(chat_submit=list(text="Please fix the open script",nonce=nonce));session$flushReact()}
  review <- function(){
    ids<-which(vapply(state$messages,function(m)identical(m$role,"AI") && grepl("```",m$text,fixed=TRUE),logical(1)))
    msg<-state$messages[[tail(ids,1L)]]
    nonce<<-nonce+1L
    session$setInputs(review_source_change=list(message=msg$sequence,block=1L,nonce=nonce,epoch=state$upload_epoch))
    session$flushReact();invisible(output$source_change_preview)
  }
  mode <- function(value){do.call(session$setInputs,setNames(list(value),review_context()$mode_id));session$flushReact();invisible(output$source_change_preview)}
  approve <- function(token=job()$token){nonce<<-nonce+1L;session$setInputs(apply_source_change=list(token=token,nonce=nonce));session$flushReact()}
  session$flushReact();send()
  check(!length(editor$writes) && !length(editor$news),"an AI code response never changes Source automatically")
  check(identical(state$messages[[1L]]$source_read$lines,2L) && state$messages[[1L]]$source_read$included &&
    grepl("source-review",as.character(state$messages[[2L]]$html),fixed=TRUE),"the exact sent Source metadata and Review control accompany the answer")
  review();check(!length(editor$writes) && job()$syntax$ok && job()$mode=="replace", "opening a checked diff is still read-only")
  cancelled<-job()$token
  session$setInputs(cancel_source_edit=1L);approve(cancelled)
  check(!length(editor$writes) && is.null(job()),"Cancel revokes the preview token and prevents later Apply")
  review();stale<-job()$token;mode("append");approve(stale)
  check(!length(editor$writes),"a button from the previous operation cannot apply a newly selected operation")
  mode("replace");token<-job()$token
  session$setInputs(apply_source_change=list(token=token,nonce=999L,id="#console",code="q()"));session$flushReact()
  check(length(editor$writes)==1L && identical(editor$writes[[1L]]$id,"doc-a") &&
    identical(app$full_text(editor$ctx),"x <- 2\ny <- x + 1\n"),"approved Apply writes only the reviewed document ID and exact proposed code")
  approve(token)
  check(length(editor$writes)==1L,"double Apply cannot write the same proposal again")
  check(is.null(job()) && !state$busy && grepl("unsaved",tail(state$messages,1L)[[1L]]$text,fixed=TRUE),
    "Apply consumes approval and reports unsaved, unexecuted editor changes")
  reply<<-"```r-replace\nx <- 3\ny <- x + 1\n```";send();review()
  token<-job()$token;editor$ctx$contents<-c("MANUAL_EDIT <- 99",editor$ctx$contents);approve(token)
  check(length(editor$writes)==1L && is.null(job()),"manual edits after preview cancel Apply without overwriting the user")
  review()
  check(identical(unname(review_context()$choices),"new_r"),"a stale response cannot reopen an old baseline for replacement")
  token<-job()$token;approve(token);approve(token)
  check(length(editor$news)==1L && identical(editor$news[[1L]],list(text="x <- 3\ny <- x + 1\n",kind="R")),
    "new-document approval creates exactly one unsaved script from the reviewed bytes")
  reply<<-"```r-replace\nx <-\n```";send();review()
  token<-job()$token;approve(token)
  check(!job()$syntax$ok && length(editor$writes)==1L,"invalid syntax cannot be applied even through a forged click")
  reply<<-"```r-replace\nx <- 4\n```";send();review()
  token<-job()$token;editor$ctx$path<-"/tmp/renamed-review.R";approve(token)
  check(length(editor$writes)==1L && is.null(job()),"Save As or target-path changes invalidate an open approval")
  send();review();token<-job()$token
  session$setInputs(new_chat=1L);approve(token)
  check(length(editor$writes)==1L && is.null(job()),"New chat revokes an open Source approval")
  reply<<-"```r-replace\nx <- 5\n```";send();review();editor$fail<-TRUE
  token<-job()$token;approve(token);approve(token);editor$fail<-FALSE
  check(length(editor$writes)==2L && is.null(job()) && !state$busy,
    "an editor API error consumes approval rather than retrying an uncertain write")
  reply<<-"````rmd-replace\n---\ntitle: New document\n---\n```{r}\nz <- 7\n```\n````";send();review();approve()
  check(length(editor$news)==2L && editor$news[[2L]]$kind=="Rmd" && grepl("```{r}",editor$news[[2L]]$text,fixed=TRUE),
    "a complete Rmd block can create a new document with nested chunk fences intact")
  saved<-app$snapshot_messages(state$messages)
  check(!any(vapply(saved,function(m)!is.null(m$source_snapshot),logical(1))),"Source baselines and write approvals are never persisted in chat history")
  restored<-app$restore_chat_data(list(messages=saved))
  state$messages<-restored$messages;review()
  check(all(grepl("^new_",unname(review_context()$choices))),"restored answers need a new Source read before they can edit existing files")
  session$close();approve(job()$token)
  check(length(editor$writes)==2L && length(editor$news)==2L,"a disconnected or retired session cannot apply an edit")
})

# The reported question must receive correction guidance, not just an explicit
# "fix" command. The fake response exercises rendering/proposals, not a live LLM.
original <- c("# Keep this comment", "X <- matrix(1:12, nrow = 3)",
  rep("# Unrelated line stays unchanged",12L),"rtrtryydim(X)")
corrected <- c(head(original,-1L),"dim(X)")
editor$ctx <- list(id="reported-doc",path="/tmp/v2.R",contents=original)
app$initial <- editor$ctx
request <- NULL
app$ask_ai <- function(system_prompt,prompt,images=character()) {
  request <<- list(system=system_prompt,prompt=prompt)
  paste0("The last function name is misspelled.\n\n```r-replace\n",paste(corrected,collapse="\n"),"\n```")
}
shiny::testServer(app$server,{
  session$flushReact()
  writes <- length(editor$writes)
  session$setInputs(chat_submit=list(text="Код дотор ямар алдаа байна?",nonce=1L))
  session$flushReact()
  check(grepl("even when the user only asks what is wrong",request$system,fixed=TRUE) &&
    grepl("preserve every other line, comment and string verbatim",request$system,fixed=TRUE) &&
    grepl("Respect requests for explanation only",request$system,fixed=TRUE) &&
    grepl(app$numbered_source(paste(original,collapse="\n")),request$prompt,fixed=TRUE),
    "the reported error-review question sends all 15 lines with complete-correction and explanation-only guidance")
  answer <- tail(state$messages,1L)[[1L]]
  check(!is.null(review_context()) && identical(review_context()$epoch,state$upload_epoch),
    "a single corrected file opens its comparison automatically with the final current epoch")
  invisible(output$source_change_preview)
  job <- get("preview_job",envir=environment(clear_source_review),inherits=FALSE)
  check(grepl('class="source-review"',as.character(answer$html),fixed=TRUE) &&
    identical(job$before,paste(original,collapse="\n")) &&
    identical(job$after,paste0(paste(corrected,collapse="\n"),"\n")) &&
    job$syntax$ok && length(editor$writes)==writes,
    "a corrected 15-line answer exposes Review and preserves all unrelated lines without editing Source")
  session$setInputs(apply_source_change=list(token=job$token,nonce=2L));session$flushReact()
  check(length(editor$writes)==writes+1L && identical(app$full_text(editor$ctx),job$after),
    "Replace on the automatically opened comparison applies exactly the reviewed bytes without a Review click")
  reply <- "Unchanged"
  app$ask_ai <- function(...)paste0("```r-replace\n",app$full_text(editor$ctx),"```")
  session$setInputs(chat_submit=list(text="Explain this code",nonce=3L));session$flushReact()
  check(is.null(review_context()),"an unchanged code block does not open a misleading comparison for its final newline")
  app$ask_ai <- function(...)"```r-replace\nx <- 1\n```\nOr:\n```r-replace\nx <- 2\n```"
  session$setInputs(chat_submit=list(text="Show alternatives",nonce=4L));session$flushReact()
  check(is.null(review_context()),"multiple alternative blocks are never automatically selected for replacement")
})
# A usage example is not a replacement, even if it is the only R block.
original <- c("f <- function(x) {","","  result <- x + 1","","  result","}")
editor$ctx <- list(id="function-doc",path="/tmp/test.R",contents=original)
app$initial <- editor$ctx
reply <- "Example calls:\n\n```r\nf(5)\nf(c(1, 2, 3))\n```"
app$ask_ai <- function(system_prompt,prompt,images=character()) {
  request <<- list(system=system_prompt,prompt=prompt);reply
}
block <- app$source_code_blocks(as.character(app$render_markdown("```r-replace\nf <- function(x) x + 2\n```")))[[1L]]
check(identical(block$operation,"replace") && identical(block$kind,"R") && block$reviewable &&
  identical(block$code,"f <- function(x) x + 2\n"),"replacement intent preserves the exact code and its R syntax kind")
markup <- as.character(app$render_markdown("````rmd-replace\n```{r}\nx <- 1\n```\n````"))
check(app$source_code_blocks(markup)[[1L]]$kind=="Rmd" &&
  grepl('class="code-type">RMD</span>',markup,fixed=TRUE),"document replacement intent keeps nested fences and a plain RMD label")
check(identical(app$source_code_blocks(as.character(app$render_markdown("```r\nx <- 'r-replace'\n```")))[[1L]]$operation,""),
  "an intent label inside code does not turn an example into a replacement")
shiny::testServer(app$server,{
  session$flushReact();count <- length(editor$writes)
  send <- function(text,nonce){session$setInputs(chat_submit=list(text=text,nonce=nonce));session$flushReact()}
  send("Энэ файл дахь f() функцийг тайлбарла",1L)
  check(is.null(review_context()) && length(editor$writes)==count && identical(editor$ctx$contents,original),
    "the reported single f() example never previews deletion of the function definition")
  answer <- tail(state$messages,1L)[[1L]]
  check(grepl('class="code-copy"',as.character(answer$html),fixed=TRUE) &&
    grepl("f(c(1, 2, 3))",as.character(answer$html),fixed=TRUE) &&
    grepl("Ordinary r/rmd blocks are examples",request$system,fixed=TRUE),
    "explanation examples retain Copy and the model receives the explicit intent contract")
  session$setInputs(review_source_change=list(message=answer$sequence,block=1L,epoch=state$upload_epoch));session$flushReact()
  invisible(output$source_change_preview)
  check(!any(unname(review_context()$choices) %in% c("replace","selection")) &&
    get("preview_job",envir=environment(clear_source_review))$mode=="append",
    "manual Review of an unmarked example offers addition or a new file, never whole-file replacement")
  do.call(session$setInputs,setNames(list("replace"),review_context()$mode_id));session$flushReact()
  invisible(output$source_change_preview)
  check(get("preview_job",envir=environment(clear_source_review))$mode=="append" && length(editor$writes)==count,
    "an unoffered replacement mode cannot turn an example into a destructive proposal")
  reply <<- paste0("```r-replace\n",paste(sub("x + 1","x + 2",original,fixed=TRUE),collapse="\n"),
    "\n```\n\nUsage:\n\n```r\nf(5)\n```")
  send("Change the function to add two",2L);invisible(output$source_change_preview)
  job <- get("preview_job",envir=environment(clear_source_review))
  check(!is.null(review_context()) && job$mode=="replace" &&
    grepl("f <- function",job$after,fixed=TRUE) && !grepl("f(5)",job$after,fixed=TRUE) && length(editor$writes)==count,
    "only the explicitly marked complete correction opens a diff when an example follows it")
  session$setInputs(apply_source_change=list(token=job$token,nonce=3L));session$flushReact()
  check(length(editor$writes)==count+1L && identical(app$full_text(editor$ctx),job$after),
    "an explicitly approved complete correction still applies without extra review steps")
  reply <<- "```r-append\nf(5)\n```"
  send("Add a demonstration after the function",4L);invisible(output$source_change_preview)
  job <- get("preview_job",envir=environment(clear_source_review))
  check(job$mode=="append" && startsWith(job$after,job$before) &&
    !"replace" %in% unname(review_context()$choices),"a marked addition preserves the function and offers Append")
})
cat(checks,"Source editing checks passed\n")
