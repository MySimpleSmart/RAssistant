# Draft context is read locally; Last Send freezes the exact gathered data.
source("tests/helpers.R")
checks <- 0L
check <- function(ok,label){if(!isTRUE(ok))stop(label,call.=FALSE);checks<<-checks+1L;cat("PASS",label,"\n")}
retrievals <- 0L;profile <- "Use the verified course methods."
doc <- list(id=strrep("a",32L),name="notes.md")
knowledge <- list(profile_text=function()profile,list=function()list(doc),retrieve=function(query,context,selected,work=list()){
  retrievals<<-retrievals+1L
  list(text="[K1] notes.md lines 1-2\nVerified reference excerpt.",sources=list(list(name="notes.md",label="lines 1-2")),status="matched")
})
enabled <- FALSE;console_text <- "CONSOLE_FIXTURE_42 INSPECTOR_FAKE_API_KEY"
capture <- list(start=function(){enabled<<-TRUE},stop=function(){enabled<<-FALSE;TRUE},dispose=function()TRUE,
  status=function()list(enabled=enabled,ready=TRUE,pending=FALSE,notice="",lines=1L),
  snapshot=function(redact){out<-console_text;for(key in redact)out<-gsub(key,"[API key redacted]",out,fixed=TRUE);out})
app <- load_assistant(package_info=list(version="test",knowledge=knowledge,console=capture,prepare_console=function()NULL))
app$api_key <- "INSPECTOR_FAKE_API_KEY"
root <- tempfile("inspector-");dir.create(root)
writeLines(c("value","999"),file.path(root,"data.csv"))
editor <- new.env();editor$ctx <- list(id="fixture",path=file.path(root,"analysis.R"),contents=c(
  paste0("fixture_data <- read.csv(",encodeString(file.path(root,"data.csv"),quote='"'),")"),"nrow(fixture_data)"))
app$initial <- editor$ctx;app$active_doc <- function(id=NULL)editor$ctx
data_env <- new.env(parent=emptyenv());data_env$fixture_data <- data.frame(value=543210987)
data_context <- app$workspace_data_context
reads <- 0L;scans <- 0L;calls <- list();fail <- FALSE;writes <- 0L;runs <- 0L
app$workspace_summary <- function(){reads<<-reads+1L;"fixture_data: data.frame; dimensions=1 x 1"}
app$workspace_data_context <- function(request,source="",selected=character())data_context(request,source,selected,env=data_env)
app$workspace_project_context <- function(...){scans<<-scans+1L;"Saved related file: R/helper.R\n1 | helper <- function(x) x + 7"}
app$workspace_function_context <- function(...)"Loaded function: fixture_helper\nfunction(x) x + 9"
app$knowledge_work_context <- function(...)list(functions="nrow")
app$write_source <- function(...)writes<<-writes+1L
app$validate_script <- function(...)runs<<-runs+1L
app$ask_ai <- function(system_prompt,prompt,images=character()){
  calls[[length(calls)+1L]]<<-list(system=system_prompt,prompt=prompt,images=images)
  if(fail)stop("Synthetic provider failure")
  "Explained using the supplied context."
}
shiny::testServer(app$server,{
  session$setInputs(console_context=TRUE,workspace_context=TRUE,prompt="Explain fixture_data")
  session$setInputs(context_inspector=1L)
  draft <- context_draft()
  check(state$context_panel && context_mode()=="draft" && !length(calls) && !length(state$messages) && writes==0L && runs==0L,
    "opening Context reads a draft snapshot without model requests, chat messages or Source execution")
  check(draft$source$included && draft$source$name=="analysis.R" && identical(draft$source_text,app$full_text(editor$ctx)) &&
    grepl("VERIFIED CSV REFERENCE",draft$headers,fixed=TRUE),"Source and CSV headers use the same read path as Send")
  check(grepl("CONSOLE_FIXTURE_42",draft$console$text,fixed=TRUE) && !grepl(app$api_key,draft$console$text,fixed=TRUE),
    "Console preview exposes the actual bounded redacted capture")
  check(grepl("543210987",draft$reports$data,fixed=TRUE) && grepl("R/helper.R",draft$reports$project,fixed=TRUE) &&
    grepl("fixture_helper",draft$reports$functions,fixed=TRUE),"Workspace combines real dataset excerpts, related files and loaded functions")
  check(retrievals==0L && !length(draft$reports$course$receipt$files),"an empty Knowledge Base selection does not trigger document retrieval")
  before <- c(reads,scans)
  session$elapse(4000)
  check(identical(before,c(reads,scans)),"inspector snapshots do not poll Workspace or project files in the background")
  state$knowledge_files <- list(doc)
  session$setInputs(context_refresh=1L)
  draft <- context_draft()
  check(retrievals==1L && grepl("Verified reference excerpt",draft$reports$course$text,fixed=TRUE) &&
    identical(draft$reports$course$instructions,profile),"selected document excerpts and saved instructions appear together")
  html <- paste(as.character(app$context_snapshot_ui(draft)),collapse="")
  check(all(vapply(c("Source","Console","Workspace","Knowledge Base","Images","Conversation"),grepl,logical(1),x=html,fixed=TRUE)) &&
    !grepl(app$api_key,html,fixed=TRUE),"the unified disclosure view contains every context category without API credentials")
  session$setInputs(context_back=1L)
  check(!state$context_panel && state$draft=="Explain fixture_data" && !length(calls),"Back to chat preserves the draft without sending it")
  data_env$fixture_data$value <- 543210988
  editor$ctx$contents <- c(editor$ctx$contents,"# Unsaved change before Send")
  session$setInputs(chat_submit=list(text="Explain fixture_data",nonce=1L))
  sent <- context_last()
  check(length(calls)==1L && sent$delivery=="sent" && grepl("543210988",sent$reports$data,fixed=TRUE) &&
    grepl("Unsaved change before Send",sent$source_text,fixed=TRUE),"Send rereads changed Source and values instead of using a stale preview")
  check(grepl(sent$reports$data,calls[[1L]]$prompt,fixed=TRUE) && grepl(sent$reports$project,calls[[1L]]$prompt,fixed=TRUE) &&
    grepl(sent$reports$course$text,calls[[1L]]$prompt,fixed=TRUE) && grepl(sent$console$text,calls[[1L]]$prompt,fixed=TRUE),
    "Last Send contains the exact context strings supplied in the actual model request")
  session$setInputs(prompt="Next unsent draft",context_inspector=2L)
  check(context_mode()=="last" && identical(context_last(),sent),"reopening Context defaults to the fixed last Send snapshot")
  data_env$fixture_data$value <- 543210989
  session$setInputs(context_draft=1L)
  check(grepl("543210989",context_draft()$reports$data,fixed=TRUE) && identical(context_last(),sent) && length(calls)==1L,
    "a fresh draft can change without rewriting the data shown for an earlier Send")
  before <- scans
  session$setInputs(workspace_context=FALSE,console_context=FALSE,context_refresh=2L)
  disabled <- context_draft()
  check(disabled$console$status=="off" && !disabled$workspace && !nzchar(disabled$reports$data) && scans==before,
    "Console Off and Workspace Off exclude their reads from draft preview")
  session$setInputs(context_back=2L)
  fail <<- TRUE
  session$setInputs(chat_submit=list(text="Explain fixture_data",nonce=2L))
  check(length(calls)==2L && context_last()$delivery=="failed" && state$draft=="Explain fixture_data",
    "a failed API request retains its actual attempted context and retry draft")
  saved <- app$snapshot_messages(state$messages)
  check(!grepl("543210988|543210989|CONSOLE_FIXTURE_42",paste(unlist(saved),collapse="")) &&
    !any(vapply(saved,function(m)any(c("reports","context_snapshot") %in% names(m)),logical(1))),
    "raw inspector snapshots and Workspace values are never added to saved history")
  fail <<- FALSE
  session$setInputs(chat_submit=list(text="Console-ийг тайлбарла",nonce=3L))
  check(length(calls)==2L && context_last()$delivery=="local" && is.null(context_last()$source),
    "local Console guidance is labelled as no model request instead of Sent")
  session$setInputs(context_inspector=3L)
  session$setInputs(new_chat=1L)
  check(!state$context_panel && is.null(context_last()) && is.null(context_draft()),
    "New chat closes Context and clears transient snapshots from the previous conversation")
})
validation_app <- load_assistant();validation_app$api_key <- "validation-fixture-key"
validation_app$initial <- list(id="validation",path=file.path(root,"validation.R"),contents="print(42)")
validation_app$active_doc <- function(id=NULL)validation_app$initial
validation_calls <- list()
validation_app$validate_script <- function(...)list(ok=TRUE,logs="VALIDATION_CONTEXT_LOG",plots=character(),plot_dir=NULL,total_plots=0L)
validation_app$ask_ai <- function(system_prompt,prompt,images=character()) {
  validation_calls[[length(validation_calls)+1L]]<<-list(prompt=prompt,images=images);"Verified output."
}
shiny::testServer(validation_app$server,{
  session$setInputs(chat_submit=list(text="Run the script",nonce=1L))
  check(!length(validation_calls) && context_last()$delivery=="validation","a run request is not labelled Sent before execution approval")
  session$setInputs(yes=1L)
  snapshot <- context_last()
  check(length(validation_calls)==1L && snapshot$flow=="validation" && snapshot$delivery=="sent" &&
    identical(snapshot$execution,"VALIDATION_CONTEXT_LOG") && grepl(snapshot$execution,validation_calls[[1L]]$prompt,fixed=TRUE),
    "approved validation records the exact logs sent for model analysis")
  check(grepl("Approved validation output",as.character(validation_app$context_snapshot_ui(snapshot)),fixed=TRUE),
    "validation output has its own inspector section rather than pretending to be main Console capture")
})
unlink(root,recursive=TRUE)
cat(checks,"Context inspector checks passed\n")
