# Console-to-Source navigation uses execution metadata or explicitly labelled
# unique expression matches. It never runs or edits the user's code.
source("tests/helpers.R")
checks <- 0L
check <- function(ok,label) {
  if(!isTRUE(ok))stop(label,call.=FALSE)
  checks <<- checks+1L;cat("PASS",label,"\n")
}
app <- load_assistant()
ctx <- list(id="source-test",path="analysis.R",contents=c("# Keep", "x <- missing_fn(3)","print(x)"))
failed <- paste("Error: example","[Console error 9]","[End failed Console task; full command may be unavailable]",sep="\n")
evidence <- list(marker="[Console error 9]",calls=c('missing_fn(3)'),locations=list())
target <- app$console_error_target(failed,evidence,ctx)
check(identical(target$row,2L) && target$kind=="match","unique failing calls locate a Source expression without claiming execution origin")
check(is.null(app$console_error_target(paste(failed,"[Completed Console command; no text output captured]","> x <- 1","[End Console task]",sep="\n"),evidence,ctx)),
  "a newer successful task clears the previous error's location")
check(app$console_error_target(failed,modifyList(evidence,list(marker="[Console error 8]")),ctx)$status=="unknown",
  "error metadata must belong to the failed task in the current captured tail")
duplicated <- ctx;duplicated$contents <- rep(ctx$contents,2L)
check(app$console_error_target(failed,evidence,duplicated)$status=="ambiguous","duplicate expressions do not guess a line from the cursor")
md <- list(id="markdown-test",path="report.qmd",contents=c("# Keep","```{r analysis}","x <- missing_fn(","  3)","```","Text"))
target <- app$console_error_target(failed,evidence,md)
check(identical(target$row,3L) && identical(target$end_row,4L) && target$chunk=="analysis",
  "multiline expressions use global Source rows and the Quarto chunk label")
md$contents[[2L]] <- "```{python}"
check(app$console_error_target(failed,evidence,md)$status=="unknown","code in non-R chunks cannot become an R error target")
ctx$contents <- c("# Keep","x <- missing_fn(3)","broken <- (")
check(identical(app$console_source_match(evidence$calls,ctx)$row,2L),"a later parse error does not hide an earlier matching expression")
ctx$contents <- c("# Keep",'stop("message with spaces")')
check(identical(app$console_source_match("stop('message with spaces')",ctx)$row,2L),"matching ignores code whitespace and quote style")
check(is.null(app$console_source_match("stop('message with different spaces')",ctx)),"string values remain significant when matching")
long_ctx <- list(id="long-source",path="long.R",contents=c(sprintf("value_%d <- identity(%d)",1:1000,1:1000),"missing_fn(3)"))
check(identical(app$console_source_match(evidence$calls,long_ctx)$row,1001L),
  "error matching reaches the end of a longer Source document")
ctx$contents <- c("# Keep","x <- missing_fn(3)","print(x)")
loc <- list(path=ctx$path,row=2L,end_row=2L,text=ctx$contents[[2L]])
runtime <- evidence;runtime$locations <- list(loc)
target <- app$console_error_target(failed,runtime,ctx)
check(target$kind=="runtime" && target$row==2L,"captured source references take priority over expression matching")
changed <- ctx;changed$contents[[2L]] <- "x <- corrected_fn(3)"
check(app$console_error_target(failed,runtime,changed)$status=="changed","old execution metadata does not target an edited source line")
root <- tempfile("console-source-");dir.create(root)
saved <- file.path(root,"other.Rmd")
writeLines(c("# Other file","```{r data}","missing_fn(3)","```"),saved)
other <- evidence;other$locations <- list(list(path=saved,row=3L,end_row=3L,text="missing_fn(3)"))
target <- app$console_error_target(failed,other,ctx)
check(target$kind=="runtime" && is.null(target$id) && target$name=="other.Rmd" && target$chunk=="data",
  "runtime locations can identify a saved file other than the active editor")
check(is.null(app$console_error_target("Error: no task boundary",evidence,ctx)),"incomplete output cannot reuse an old error location")

enabled <- TRUE;output_text <- failed;metadata <- runtime;calls <- list();jumps <- list();writes <- 0L;opens <- 0L
capture <- list(start=function()enabled<<-TRUE,stop=function(){enabled<<-FALSE;TRUE},dispose=function()TRUE,
  status=function()list(enabled=enabled,ready=TRUE,pending=FALSE,notice="",lines=3L),
  snapshot=function(redact)output_text,diagnostics=function(redact)metadata)
app <- load_assistant(package_info=list(version="test",console=capture,prepare_console=function()NULL))
app$api_key <- "fake-test-key"
app$initial <- ctx
app$active_doc <- function(id=NULL)if(identical(id,"other-doc"))list(id="other-doc",path=saved,
  contents=readLines(saved))else ctx
app$focus_source <- function(id,row,column,end_column)jumps[[length(jumps)+1L]]<<-list(id=id,row=row)
app$open_console_source <- function(path){stopifnot(identical(path,saved));opens<<-opens+1L;"other-doc"}
app$write_source <- function(...)writes<<-writes+1L
app$ask_ai <- function(system_prompt,prompt,images=character()){
  calls[[length(calls)+1L]]<<-list(system=system_prompt,prompt=prompt)
  "Use only the failing line."
}
shiny::testServer(app$server,{
  session$setInputs(console_context=TRUE)
  session$setInputs(chat_submit=list(text="Explain the Console error",nonce=1L))
  check(length(calls)==1L && grepl("CONSOLE ERROR LOCATION",calls[[1L]]$prompt,fixed=TRUE) &&
    grepl("analysis.R:2",calls[[1L]]$prompt,fixed=TRUE) &&
    grepl("only the faulty lines",calls[[1L]]$system,fixed=TRUE),"Send supplies the locally located error and requires a minimal reviewed fix")
  check(grepl("console-source-jump",paste(output$messages,collapse=""),fixed=TRUE) && grepl("analysis.R:2",paste(output$messages,collapse=""),fixed=TRUE),
    "the chat displays a clickable Console error location")
  token <- state$messages[[1L]]$console_error$token
  session$setInputs(jump_console_source=list(token=token,nonce=1L))
  check(length(jumps)==1L && identical(jumps[[1L]],list(id="source-test",row=2L)) && writes==0L && length(calls)==1L,
    "clicking a valid location focuses the exact row without edits or an API request")
  session$setInputs(jump_console_source=list(token="forged",path=saved,row=1L,nonce=2L))
  check(length(jumps)==1L && opens==0L,"client-supplied paths and forged navigation tokens are ignored")
  ctx$contents <<- c("# Inserted",ctx$contents)
  session$setInputs(jump_console_source=list(token=token,nonce=3L))
  check(length(jumps)==1L,"navigation refuses to highlight a stale Source snapshot")
  ctx$contents <<- ctx$contents[-1L]
  check(!any(vapply(app$snapshot_messages(state$messages),function(m)"console_error" %in% names(m),logical(1))),
    "raw execution fingerprints are transient and never saved in chat history")
  metadata <<- other
  session$setInputs(chat_submit=list(text="Explain the other file error",nonce=2L))
  token <- state$messages[[3L]]$console_error$token
  check(state$messages[[3L]]$console_error$name=="other.Rmd","a runtime error in another file is labelled independently of the active Source")
  session$setInputs(jump_console_source=list(token=token,nonce=4L))
  check(length(jumps)==2L && opens==1L && identical(jumps[[2L]],list(id="other-doc",row=3L)) && writes==0L,
    "a recorded file is opened read-only, rechecked and focused at its chunk line")
  writeLines(c("# Other file","```{r data}","corrected_fn(3)","```"),saved)
  session$setInputs(jump_console_source=list(token=token,nonce=5L))
  check(length(jumps)==2L,"changes in another document also invalidate its old location")
  output_text <<- paste(failed,"[Completed Console command; no text output captured]","> x <- 1","[End Console task]",sep="\n")
  session$setInputs(chat_submit=list(text="What happened last?",nonce=3L))
  check(is.null(state$messages[[5L]]$console_error) && !grepl("CONSOLE ERROR LOCATION (local",calls[[3L]]$prompt,fixed=TRUE),
    "a subsequent successful command sends no misleading old error target")
  session$setInputs(console_context=FALSE,chat_submit=list(text="Source only",nonce=4L))
  check(is.null(state$messages[[7L]]$console_error) && !grepl("CONSOLE ERROR LOCATION",calls[[4L]]$prompt,fixed=TRUE),
    "Console Off omits error metadata and navigation from the new turn")
})
unlink(root,recursive=TRUE)
cat(checks,"Console Source checks passed\n")
