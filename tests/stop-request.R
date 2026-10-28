# Cancellation must stop transport, preserve drafts and reject late/stale results.
source("tests/helpers.R")
checks <- 0L
check <- function(ok,label){if(!isTRUE(ok))stop(label,call.=FALSE);checks<<-checks+1L;cat("PASS",label,"\n")}
history_env <- new.env();sys.source("R/history.R",history_env)
history_root <- tempfile("stop-history-")
history <- history_env$.conversation_store(project=tempdir(),directory=history_root)
app <- load_assistant(package_info=list(history=history));app$api_key <- "STOP_TEST_FAKE_KEY"
app$initial <- list(id="stop",path="/tmp/stop.R",contents="x <- 1")
app$active_doc <- function(id=NULL)app$initial
app$workspace_summary <- function()""
app$workspace_data_context <- app$workspace_project_context <- app$workspace_function_context <- function(...)""
registry <- new.env();registry$jobs <- list();writes <- 0L
app$write_source <- function(...)writes<<-writes+1L
app$start_ai_request <- function(system_prompt,prompt,images=character()) {
  job <- new.env();job$done<-FALSE;job$cancelled<-FALSE;job$cleaned<-FALSE
  job$value <- "```r-edit-1-1\nx <- 2\n```";job$images<-images
  registry$jobs[[length(registry$jobs)+1L]] <<- job
  list(poll=function()list(done=job$done,value=job$value),
    cancel=function(){job$cancelled<-TRUE},cleanup=function(){job$cleaned<-TRUE})
}
image <- tempfile(fileext=".png");grDevices::png(image);plot(1:3);grDevices::dev.off()
shiny::testServer(app$server,{
  session$setInputs(console_context=FALSE,workspace_context=FALSE)
  add_attachment(image,"fixture.png")
  session$setInputs(chat_submit=list(text="Explain this image",nonce=1L))
  id <- request_state()$id
  check(state$busy && length(registry$jobs)==1L && !is.null(id),"Send yields to Shiny with one cancellable request")
  session$setInputs(chat_submit=list(text="Duplicate",nonce=2L),new_chat=1L)
  check(length(registry$jobs)==1L && length(Filter(function(m)m$role=="You",state$messages))==1L,
    "another Send and New chat cannot replace a pending request")
  session$setInputs(stop_ai_request=list(id="stale",nonce=1L))
  check(state$busy && !registry$jobs[[1L]]$cancelled,"an unrelated Stop ID cannot cancel the current request")
  session$setInputs(stop_ai_request=list(id=id,nonce=2L))
  check(!state$busy && is.null(request_state()) && registry$jobs[[1L]]$cancelled && registry$jobs[[1L]]$cleaned,
    "Stop cancels transport and frees the composer")
  check(state$draft=="Explain this image" && length(state$attachments)==1L && file.exists(state$attachments[[1L]]$path),
    "the original draft and image remain available to retry")
  check(identical(state$messages[[1L]]$context,FALSE) && context_last()$delivery=="stopped" && writes==0L,
    "a stopped turn is labelled Stopped, excluded from model history and cannot edit Source")
  count <- length(state$messages);registry$jobs[[1L]]$done<-TRUE;poll_ai_request(id)
  check(length(state$messages)==count && is.null(review_context()),"a late result cannot append an answer or create an edit review")
  retry_text <- state$draft
  session$setInputs(chat_submit=list(text=retry_text,nonce=3L))
  next_id <- request_state()$id
  session$setInputs(stop_ai_request=list(id=id,nonce=3L));poll_ai_request(id)
  check(state$busy && length(registry$jobs)==2L && !registry$jobs[[2L]]$cancelled && identical(request_state()$id,next_id),
    "an old Stop or poll cannot cancel a later retry")
  registry$jobs[[2L]]$value <- "Retry completed.";registry$jobs[[2L]]$done<-TRUE;poll_ai_request(next_id)
  check(!state$busy && state$draft=="" && !length(state$attachments) &&
    sum(vapply(state$messages,function(m)identical(m$text,"Retry completed."),logical(1)))==1L,
    "a deliberate retry finishes once and clears its draft and attachments")
  session$setInputs(chat_submit=list(text="Try again",nonce=4L))
  fail_id <- request_state()$id;registry$jobs[[3L]]$value<-simpleError("Fake transport failure");registry$jobs[[3L]]$done<-TRUE
  poll_ai_request(fail_id)
  check(!state$busy && context_last()$delivery=="failed" && state$draft=="Try again",
    "background errors retain the normal retry behavior")
  # Code-action labels are not ordinary retry text: preserve the actual draft.
  line <- app$initial$contents
  app$initial$selection <- list(list(text=line,range=list(start=c(1L,1L),end=c(1L,nchar(line)+1L))))
  session$setInputs(source_watch_tick=1L)
  session$setInputs(chat_submit=list(text="",selection_action="fix",scope="smart",target_token=code_target()$token,nonce=5L))
  code_id <- request_state()$id
  session$setInputs(stop_ai_request=list(id=code_id,nonce=4L))
  check(!state$busy && state$draft=="" && writes==0L && is.null(review_context()),
    "stopping a selection action does not turn its label into a whole-file Send")
  add_attachment(image,"disconnect.png")
  session$setInputs(chat_submit=list(text="Pending at disconnect",nonce=6L))
  disconnect_id <- request_state()$id;job <- tail(registry$jobs,1L)[[1L]]
  session$close()
  check(job$cancelled && job$cleaned,
    "disconnect cancels and cleans up the pending worker")
  saved <- history$load()
  check(saved$draft=="Pending at disconnect" && length(saved$attachments)==1L &&
    !any(vapply(saved$messages,function(m)identical(m$text,"Pending at disconnect") && !identical(m$context,FALSE),logical(1))),
    "disconnect saves the interrupted draft and image without treating the stopped turn as model history")
  job$done<-TRUE
  check(is.null(poll_ai_request(disconnect_id)),"a disconnected session ignores its late poll before reading destroyed reactive state")
})
validation <- load_assistant();validation$api_key <- "VALIDATION_STOP_FAKE"
validation$initial <- list(id="validation",path="/tmp/validation.R",contents="print(42)")
validation$active_doc <- function(id=NULL)validation$initial
validation$validate_script <- function(...)list(ok=TRUE,logs="ACTUAL_RUN_OUTPUT_42",plots=character(),plot_dir=NULL)
validation$start_ai_request <- app$start_ai_request
shiny::testServer(validation$server,{
  session$setInputs(chat_submit=list(text="Run the script",nonce=1L))
  check(!state$busy && is.null(request_state()) && !is.null(state$pending),"run approval does not advertise an API Stop before execution")
  session$setInputs(yes=1L)
  id <- request_state()$id
  check(state$busy && !is.null(id) && context_last()$flow=="validation","post-run model analysis is cancellable")
  session$setInputs(stop_ai_request=list(id=id,nonce=1L))
  check(!state$busy && context_last()$delivery=="stopped" &&
    any(vapply(state$messages,function(m)identical(m$text,"ACTUAL_RUN_OUTPUT_42"),logical(1))),
    "stopping analysis preserves already approved execution output")
})
check(app$knowledge_receipt_info(list(delivery="stopped"),restore=TRUE)$delivery=="stopped" &&
  app$console_read_info(list(delivery="stopped",status="available"),restore=TRUE)$delivery=="stopped",
  "stopped Knowledge Base and Console receipts retain their status")
unlink(c(image,history_root),recursive=TRUE)

# Real background ellmer requests against a local fake provider, never user keys.
transport_checks <- function() {
  port <- httpuv::randomPort();marker <- tempfile();ready <- tempfile()
  server <- callr::r_bg(function(port,marker,ready) {
    response <- function(req) {
      body <- jsonlite::fromJSON(rawToChar(req$rook.input$read()),simplifyVector=FALSE)
      saveRDS(list(authorized=identical(req$HTTP_AUTHORIZATION,"Bearer WORKER_FAKE_KEY"),body=body),marker)
      if(grepl("ERROR_REQUEST",paste(unlist(body),collapse=""),fixed=TRUE))return(list(status=400L,
        headers=list("Content-Type"="application/json"),body='{"error":{"message":"Bad request WORKER_FAKE_KEY","type":"invalid_request_error"}}'))
      if(grepl("SLOW_REQUEST",paste(unlist(body),collapse=""),fixed=TRUE))Sys.sleep(12)
      if(isTRUE(body$stream)) {
        chunk <- function(delta,finish=NULL)paste0("data: ",jsonlite::toJSON(list(id="fake",object="chat.completion.chunk",
          choices=list(list(index=0L,delta=delta,finish_reason=finish))),auto_unbox=TRUE,null="null"),"\n\n")
        list(status=200L,headers=list("Content-Type"="text/event-stream"),body=paste0(
          chunk(list(role="assistant",content="WORKER_OK")),chunk(list(),"stop"),"data: [DONE]\n\n"))
      }else list(status=200L,headers=list("Content-Type"="application/json"),body=jsonlite::toJSON(list(
        id="fake",object="chat.completion",choices=list(list(index=0L,message=list(role="assistant",content="WORKER_OK"),finish_reason="stop"))),auto_unbox=TRUE))
    }
    httpuv::startServer("127.0.0.1",port,list(call=response));file.create(ready)
    repeat httpuv::service(100)
  },args=list(port,marker,ready),stdout="|",stderr="|",supervise=TRUE,user_profile=FALSE,system_profile=FALSE)
  on.exit({server$kill();unlink(c(marker,ready))},add=TRUE)
  wait <- function(predicate,seconds=20){deadline<-Sys.time()+seconds;while(!isTRUE(predicate())){
    if(Sys.time()>deadline)stop("Timed out waiting for fake provider");Sys.sleep(.05)
  }}
  wait(function()file.exists(ready))
  client <- load_assistant()
  client$api_request_config <- function()list(key="WORKER_FAKE_KEY",profile=list(provider="custom",model="fake-model",
    base_url=paste0("http://127.0.0.1:",port,"/v1")),spec=list(factory="chat_openai_compatible",label="Test"))
  handle <- client$real_start_ai_request("System fixture","FAST_REQUEST")
  on.exit(handle$cleanup(),add=TRUE)
  result <- NULL
  wait(function(){result<<-handle$poll();isTRUE(result$done)})
  if(inherits(result$value,"error"))stop(conditionMessage(result$value))
  check(identical(result$value,"WORKER_OK") && readRDS(marker)$authorized,
    "the real background client authenticates through stdin and returns the provider response")
  handle$cleanup()
  failed <- client$real_start_ai_request("System fixture","ERROR_REQUEST")
  on.exit(failed$cleanup(),add=TRUE)
  wait(function(){result<<-failed$poll();isTRUE(result$done)})
  check(inherits(result$value,"error") && !grepl("WORKER_FAKE_KEY",conditionMessage(result$value),fixed=TRUE) &&
    grepl("[API key redacted]",conditionMessage(result$value),fixed=TRUE),
    "background provider errors redact the captured request key before returning to chat")
  failed$cleanup();unlink(marker)
  fixture <- tempfile(fileext=".png");grDevices::png(fixture);plot(1:3);grDevices::dev.off()
  on.exit(unlink(fixture),add=TRUE)
  slow <- client$real_start_ai_request("System fixture","SLOW_REQUEST",fixture)
  on.exit(slow$cleanup(),add=TRUE)
  wait(function()file.exists(marker))
  check(slow$is_alive() && !slow$poll()$done && length(readRDS(marker)$body$messages)>=2L,
    "a pending HTTP request leaves the main R process responsive")
  owned <- environment(slow$poll)$directory;copies <- list.files(owned,full.names=TRUE)
  check(length(copies)==1L && file.exists(fixture) && normalizePath(copies)!=normalizePath(fixture),
    "the worker owns its private image copy independently of the original attachment")
  before <- Sys.time();slow$cancel()
  check(!slow$is_alive() && as.numeric(difftime(Sys.time(),before,units="secs"))<2 && !dir.exists(owned) && file.exists(fixture),
    "Stop terminates an active HTTP worker promptly and removes only its temporary files")
  slow$cancel();slow$cleanup()
  check(!slow$poll()$done,"repeated cleanup cannot expose a cancelled worker result")
}
transport_checks()
cat(checks,"Stop request checks passed\n")
