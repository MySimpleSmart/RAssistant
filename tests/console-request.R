# Direct Console questions must use supplied output, never imagined Source
# results. These tests make no real model calls or RStudio changes.
source("tests/helpers.R")
checks <- 0L
check <- function(ok,label) {
  if(!isTRUE(ok))stop(label,call.=FALSE)
  checks <<- checks+1L;cat("PASS",label,"\n")
}
app <- load_assistant()
check(app$is_console_inspection("Console-ийг тайлбарла") && app$is_console_inspection("What is in the Console now?") &&
  app$is_console_inspection("Explain the Console output"),"Mongolian and English Console inspection requests are identified")
check(!app$is_console_inspection("What is the RStudio Console?") && !app$is_console_inspection("Console гэж юу вэ?") &&
  !app$is_console_inspection("Explain the Source code and Console") &&
  !app$is_console_inspection("Console:\n[1] 2") && !app$is_console_inspection("Console output [1] 2"),
  "concept questions, combined Source tasks and pasted output keep the normal response path")
text <- paste("             math science","S1             10      20","S2             12      24", "[1] 2",
  "[Completed Console command for preceding output]","> sum(diag(R))","[End Console task]",sep="\n")
receipt <- app$console_read_receipt(text,TRUE)
check(receipt$status=="available" && receipt$lines==7L && receipt$characters==nchar(text),"receipt counts describe the actual captured text")
html <- as.character(app$console_read_ui(modifyList(receipt,list(delivery="sent"))))
check(grepl("Console · Sent · 7 lines",html,fixed=TRUE) && grepl("S1             10      20",html,fixed=TRUE),
  "the collapsed receipt exposes exact Console evidence on expansion")
check(is.null(app$console_read_info(list(status="forged",text="not trusted"))),"invalid receipt statuses cannot be restored")
check(!grepl("screenshot|paste|хуула|screenshot",app$empty_console_reply("Console-ийг тайлбарла","empty")),
  "empty Console guidance asks for capture and code execution rather than manual copying")

enabled <- FALSE;console_text <- "";calls <- list();fail <- FALSE
capture <- list(start=function(){enabled<<-TRUE},stop=function(){enabled<<-FALSE;TRUE},dispose=function()TRUE,
  status=function()list(enabled=enabled,ready=TRUE,pending=FALSE,notice="",lines=if(nzchar(console_text))7L else 0L),
  snapshot=function(redact){out<-console_text;for(secret in redact)out<-gsub(secret,"[API key redacted]",out,fixed=TRUE);out})
app <- load_assistant(package_info=list(version="test",console=capture,prepare_console=function()NULL))
app$api_key <- "fake-console-secret"
app$initial <- list(id="fixture",path="45.R",contents=c("students <- data.frame(math=c(10,12),science=c(20,24))",
  "R <- cor(students)","sum(diag(R))"))
app$active_doc <- function(id=NULL)app$initial
app$ask_ai <- function(system_prompt,prompt,images=character()) {
  calls[[length(calls)+1L]]<<-list(system=system_prompt,prompt=prompt)
  if(fail)stop("Synthetic provider failure")
  "The supplied Console output contains the trace result 2."
}
shiny::testServer(app$server,{
  session$setInputs(console_context=TRUE,chat_submit=list(text="Console-ийг тайлбарла",nonce=1L))
  check(!length(calls) && length(state$messages)==2L && state$messages[[1L]]$console_read$status=="empty" &&
    is.null(state$messages[[1L]]$source_read) && nchar(state$messages[[2L]]$text)<180L && !state$busy,
    "an empty capture answers locally and briefly without substituting Source predictions")
  session$setInputs(console_context=FALSE,chat_submit=list(text="Explain the Console output",nonce=2L))
  check(!length(calls) && grepl("Console is Off",tail(state$messages,1L)[[1L]]$text,fixed=TRUE),
    "an explicit Console Off state gives the correct actionable explanation")
  session$setInputs(console_context=TRUE)
  console_text <<- paste(text,"fake-console-secret",sep="\n")
  session$setInputs(chat_submit=list(text="Console-ийг тайлбарла",nonce=3L))
  request <- calls[[1L]]
  check(length(calls)==1L && grepl("CONSOLE CAPTURE STATUS: available",request$prompt,fixed=TRUE) &&
    grepl("S1             10      20",request$prompt,fixed=TRUE) &&
    regexpr("RECENT R CONSOLE OUTPUT",request$prompt,fixed=TRUE)[[1L]]<regexpr("CURRENT EDITOR",request$prompt,fixed=TRUE)[[1L]],
    "actual Console results precede the unrelated Source context in the model request")
  check(grepl("never claim no Console evidence was provided",request$system,fixed=TRUE) &&
    grepl("Do not produce a line-by-line table of hypothetical Source results",request$system,fixed=TRUE),
    "Console interpretation explicitly prioritizes captured evidence over hypothetical script output")
  sent <- state$messages[[5L]]$console_read
  check(sent$delivery=="sent" && sent$lines==8L && grepl("[API key redacted]",sent$text,fixed=TRUE) &&
    !grepl("fake-console-secret",paste(output$messages,collapse=""),fixed=TRUE),
    "the receipt reports what reached the model and exposes only redacted text")
  saved <- app$snapshot_messages(state$messages)
  check(saved[[5L]]$console_read$delivery=="sent" && is.null(saved[[5L]]$console_read$text) &&
    !grepl("S1             10      20",paste(unlist(saved),collapse=""),fixed=TRUE),
    "history saves receipt metadata without persisting raw captured output")
  restored <- app$restore_chat_data(list(messages=saved))
  check(restored$messages[[5L]]$console_read$lines==8L &&
    !grepl("<details",as.character(app$console_read_ui(restored$messages[[5L]]$console_read)),fixed=TRUE),
    "restored receipts retain counts without pretending the temporary excerpt is available")
  fail <<- TRUE
  session$setInputs(chat_submit=list(text="Explain the Console output",nonce=4L))
  check(length(calls)==2L && state$messages[[7L]]$console_read$delivery=="failed" &&
    state$draft=="Explain the Console output", "request failures mark Console delivery failed and keep the retry draft")
  fail <<- FALSE;console_text <<- ""
  session$setInputs(chat_submit=list(text="Console:\n[1] 2\nExplain this result",nonce=5L))
  check(length(calls)==3L && grepl("[1] 2",calls[[3L]]$prompt,fixed=TRUE),
    "pasted real Console output remains usable with an empty capture")
  session$setInputs(chat_submit=list(text="What is the RStudio Console?",nonce=6L))
  check(length(calls)==4L,"a conceptual Console question still goes to the model")
  console_text <<- paste(c(rep("old output",240L),"LATEST_OUTPUT"),collapse="\n")
  session$setInputs(chat_submit=list(text="Explain the Console output",nonce=7L))
  receipt <- state$messages[[13L]]$console_read
  check(receipt$lines==200L && grepl("LATEST_OUTPUT",receipt$text,fixed=TRUE),
    "the app defensively bounds captured lines while retaining the newest output")
})
app <- load_assistant()
app$api_key <- "fake-console-secret";app$initial <- list(id="fixture",path="45.R",contents="sum(diag(R))")
app$active_doc <- function(id=NULL)app$initial
app$ask_ai <- function(...)stop("Should not call an API without Console evidence")
shiny::testServer(app$server,{
  session$setInputs(chat_submit=list(text="Console-ийг тайлбарла",nonce=1L))
  check(state$messages[[1L]]$console_read$status=="unavailable" &&
    grepl("RAssistant::launch()",state$messages[[2L]]$text,fixed=TRUE),
    "a standalone launch distinguishes unavailable capture from an empty enabled capture")
})
cat(checks,"Console request checks passed\n")
