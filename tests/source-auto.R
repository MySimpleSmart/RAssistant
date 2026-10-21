# Source selections reveal actions automatically; polling never invokes the model.
source("tests/helpers.R")
app <- load_assistant();checks <- 0L
check <- function(ok,label){if(!isTRUE(ok))stop(label,call.=FALSE);checks<<-checks+1L;cat("PASS",label,"\n")}
editor <- new.env();editor$ctx <- list(id="auto",path="/tmp/auto.R",contents=c("x <- bad(X)","y <- bad(Y)"),selection=NULL)
editor$reads<-0L;editor$calls<-list();editor$writes<-list();editor$reply<-"Explained."
select_row <- function(row){text<-editor$ctx$contents[[row]];editor$ctx$selection<-list(list(text=text,range=list(start=c(row,1),end=c(row,nchar(text)+1L))))}
app$initial<-editor$ctx
app$active_doc<-function(id=NULL){editor$reads<-editor$reads+1L;if(isTRUE(editor$unavailable))stop("Source unavailable");editor$ctx}
app$ask_ai<-function(system_prompt,prompt,images=character()){
  editor$calls[[length(editor$calls)+1L]]<-list(system=system_prompt,prompt=prompt);editor$reply
}
app$write_source<-function(id,text){editor$writes[[length(editor$writes)+1L]]<-text;editor$ctx$contents<-strsplit(paste0(text,"\n"),"\n",fixed=TRUE)[[1L]]}
check(is.null(app$automatic_source_target(editor$ctx)),"an unselected R script does not reserve a toolbar row")
shiny::testServer(app$server,{
  nonce<-0L
  tick<-function(){nonce<<-nonce+1L;session$setInputs(source_watch_tick=nonce);session$flushReact();code_target()}
  action<-function(name,token=code_target()$token){nonce<<-nonce+1L;session$setInputs(chat_submit=list(text="",selection_action=name,scope="smart",target_token=token,nonce=nonce));session$flushReact();if(!is.null(review_context()))invisible(output$source_change_preview)}
  session$flushReact();tick();check(is.null(code_target()),"no toolbar appears before Source has a selection or R chunk")
  select_row(1L);first<-tick()
  check(isTRUE(first$automatic) && first$mode=="selection" && first$scope$text=="x <- bad(X)" && length(editor$calls)==0L && length(editor$writes)==0L,
    "selecting Source text opens its toolbar without activation, AI requests or editor writes")
  check(identical(first,tick()),"unchanged polling preserves the token and does not rebuild the toolbar")
  select_row(2L);second<-tick()
  check(second$scope$start_row==2L && identical(first$token,second$token),"moving the selection updates the target while keeping the action token usable")
  select_row(1L);action("explain",second$token)
  check(length(editor$calls)==1L && grepl("auto.R:1-1",editor$calls[[1L]]$prompt,fixed=TRUE) && is.null(tail(state$messages,1L)[[1L]]$source_snapshot),
    "clicking Explain reads the latest selection immediately without requiring another click")
  select_row(2L);editor$reply<-"```r\ny <- dim(Y)\n```";action("fix")
  job<-get("preview_job",envir=environment(clear_source_review),inherits=FALSE)
  check(job$mode=="selection" && job$after=="x <- bad(X)\ny <- dim(Y)" && length(editor$writes)==0L,
    "Fix targets the latest selected lines even before the next watcher tick and still waits for approval")
  session$setInputs(apply_source_change=list(token=job$token,nonce=nonce+1L));session$flushReact()
  check(length(editor$writes)==1L && editor$writes[[1L]]=="x <- bad(X)\ny <- dim(Y)","approved automatic fixes preserve all unselected Source code")
  select_row(1L);tick();editor$ctx$selection<-NULL;count<-length(editor$calls);action("fix")
  check(is.null(code_target()) && length(editor$calls)==count,"losing the selection never silently broadens an automatic action to the whole file")
  select_row(1L);tick();session$setInputs(clear_code_target=1L);session$flushReact();tick()
  check(is.null(code_target()),"dismissed actions remain hidden for the same selection")
  editor$ctx$selection<-NULL;tick();select_row(1L);tick()
  check(!is.null(code_target()),"clearing and selecting again reopens the toolbar without a menu")
  reads<-editor$reads;state$busy<-TRUE;select_row(2L);tick()
  check(editor$reads==reads,"the watcher makes no native Source request while the assistant is busy")
  state$busy<-FALSE;state$edit_panel<-TRUE;tick()
  check(editor$reads==reads,"the watcher pauses on the separate history page")
  state$edit_panel<-FALSE;tick();check(code_target()$scope$start_row==2L,"returning to chat refreshes a changed Source selection")
  editor$unavailable<-TRUE;tick();check(is.null(code_target()),"an unavailable Source quietly hides automatic actions")
  editor$unavailable<-FALSE;tick();check(!is.null(code_target()),"Source recovery restores the toolbar without reopening the app")
  editor$ctx$contents<-strrep("x",50001L);editor$ctx$selection<-NULL;tick()
  check(is.null(code_target()) && is.null(source_watch_seen),"oversized Source contents are not retained by the automatic watcher")
  editor$ctx<-list(id="chunk",path="/tmp/auto.qmd",contents=c("Text.","```{r first}","x <- bad(X)","```"),
    selection=list(list(text="",range=list(start=c(3,1),end=c(3,1)))))
  tick();check(code_target()$mode=="chunk" && code_target()$scope$chunk=="first","placing the Source cursor in an R chunk shows the same automatic toolbar")
  editor$ctx$selection[[1L]]$range<-list(start=c(1,1),end=c(1,1));tick()
  check(is.null(code_target()),"moving from a chunk into prose hides its toolbar")
  select_row(1L);tick();old<-code_target();session$setInputs(new_chat=1L);session$flushReact();count<-length(editor$calls);action("explain",old$token)
  check(is.null(code_target()) && length(editor$calls)==count,"New chat revokes automatic action tokens")
  tick();check(!is.null(code_target()) && !identical(code_target()$token,old$token),"the current selection can show fresh actions in the new chat without reactivation")
})
cat(checks,"automatic Source checks passed\n")
