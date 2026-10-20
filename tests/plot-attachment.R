# Exercise native-plot export and the real attachment/Send flow with a fake
# RStudio API. No editor writes, code execution or external API requests.
source("tests/helpers.R")
checks <- 0L
check <- function(ok,label){
  if(!isTRUE(ok))stop(label,call.=FALSE)
  checks<<-checks+1L;cat("PASS",label,"\n")
}
fixtures <- vapply(c("First selected plot","Second selected plot"),function(label){
  path<-tempfile(fileext=".png")
  grDevices::png(path,width=400,height=200)
  par(mar=rep(0,4));plot.new();text(.5,.5,label)
  invisible(grDevices::dev.off());path
},character(1))
bytes <- lapply(fixtures,function(p)readBin(p,"raw",file.info(p)$size))
available<-TRUE;supported<-TRUE;mode<-"ok";selected<-1L;exports<-list();calls<-list();reads<-0L
console_enabled<-FALSE
console<-list(start=function(){console_enabled<<-TRUE},stop=function(){console_enabled<<-FALSE;TRUE},
  status=function()list(enabled=console_enabled,ready=TRUE,pending=FALSE,notice="",lines=1L),
  snapshot=function(redact)"[Completed Console command; no text output captured]\n> plot(1:40)\n[End Console task]")
app<-load_assistant(package_info=list(version="test",poll_update=function()list(status="disabled"),
  console=console,prepare_console=function()invisible(NULL)))
app$initial<-app$empty_source_context()
app$active_doc<-function(id=NULL){reads<<-reads+1L;NULL}
app$`::`<-function(pkg,name){
  p<-as.character(substitute(pkg));n<-as.character(substitute(name))
  if(p=="rstudioapi"){
    if(n=="isAvailable")return(function()available)
    if(n=="hasFun")return(function(name){stopifnot(name=="savePlotAsImage");supported})
    if(n=="savePlotAsImage")return(function(file,format,width,height){
      exports[[length(exports)+1L]]<<-list(path=file,format=format,width=width,height=height)
      if(mode=="error"){writeLines("partial export",file);stop("No current plot")}
      if(mode=="empty")return(invisible(NULL))
      if(mode=="invalid"){writeLines(strrep("x",50),file);return(invisible(NULL))}
      file.copy(fixtures[[selected]],file)
      if(mode=="oversized"){
        con<-base::file(file,"ab");writeBin(raw(4*1024^2),con);close(con)
      }
    })
  }
  base::getExportedValue(p,n)
}
app$ask_ai<-function(system_prompt,prompt,images=character()){
  calls[[length(calls)+1L]]<<-list(system=system_prompt,prompt=prompt,bytes=lapply(images,function(p)readBin(p,"raw",file.info(p)$size)))
  "Received the actual plot image."
}
shiny::testServer(app$server,{
  session$flushReact()
  check(!length(state$attachments) && !length(exports) && !length(calls),"plot export is opt-in and idle startup makes no export or AI request")
  request<-function(id,epoch=state$upload_epoch)session$setInputs(attach_current_plot=list(nonce=id,epoch=epoch,text="Keep this draft"))
  request(1L)
  check(length(exports)==1L && length(state$attachments)==1L && !length(calls) && reads==0L,
    "Attach current plot stages a preview without reading Source or sending to AI")
  check(identical(jsonlite::base64_dec(sub("^data:image/png;base64,","",state$attachments[[1L]]$preview)),bytes[[1L]]) &&
    identical(exports[[1L]]$format,"png") && exports[[1L]]$width>0 && exports[[1L]]$height>0,
    "the selected plot is exported as PNG and its original image bytes reach the preview")
  check(!file.exists(exports[[1L]]$path) && identical(state$draft,"Keep this draft") && !state$busy,
    "export spools are removed while the staged image and draft remain ready")
  session$setInputs(attach_current_plot=list(nonce=1L,epoch=state$upload_epoch,text="Duplicate event"))
  check(length(exports)==1L && length(state$attachments)==1L,"duplicate attachment events do not export or attach twice")
  selected<<-2L;request(2L)
  check(length(exports)==2L && identical(readBin(state$attachments[[2L]]$path,"raw",state$attachments[[2L]]$size),bytes[[2L]]),
    "changing the selected plot exports a fresh image instead of reusing the previous plot")
  mode<<-"error";request(3L)
  check(length(state$attachments)==2L && !state$busy && !file.exists(tail(exports,1L)[[1L]]$path),
    "failed native export preserves existing images and removes partial output")
  mode<<-"empty";request(4L)
  check(length(state$attachments)==2L && !state$busy,"a missing plot does not create an empty attachment")
  mode<<-"invalid";request(5L)
  check(length(state$attachments)==2L && !file.exists(tail(exports,1L)[[1L]]$path),
    "invalid export contents are rejected and cleaned up")
  mode<<-"oversized";request(50L)
  check(length(state$attachments)==2L && !state$busy && !file.exists(tail(exports,1L)[[1L]]$path),
    "a plot larger than 4 MB is rejected without losing existing attachments")
  mode<<-"ok";available<<-FALSE;before<-length(exports);request(6L)
  available<<-TRUE;supported<<-FALSE;request(7L);supported<<-TRUE
  check(length(exports)==before && length(state$attachments)==2L && !state$busy,
    "unavailable RStudio or export API leaves the draft and attachments intact")
  state$busy<-TRUE;request(8L);state$busy<-FALSE
  state$pending<-list(id=1L);request(9L);state$pending<-NULL
  request(10L,epoch=state$upload_epoch-1L)
  check(length(exports)==before && length(state$attachments)==2L,
    "busy, pending-validation and stale-chat requests cannot export a plot")
  selected<<-1L;request(11L)
  before<-length(exports);request(12L)
  check(length(exports)==before && length(state$attachments)==3L,"the three-image limit is checked before exporting")
  staged<-vapply(state$attachments,function(a)a$path,character(1))
  session$setInputs(console_context=TRUE)
  session$setInputs(chat_submit=list(text="Interpret these plots",nonce=1L))
  check(length(calls)==1L && identical(calls[[1L]]$bytes,list(bytes[[1L]],bytes[[2L]],bytes[[1L]])),
    "Send delivers the actual staged plot pixels in order")
  check(grepl("plot(1:40)",calls[[1L]]$prompt,fixed=TRUE) &&
    grepl("Only describe the current plot's appearance when its image is attached",calls[[1L]]$system,fixed=TRUE),
    "Console context and actual attached plot images can be interpreted together")
  check(!length(state$attachments) && all(!file.exists(staged)),"successful Send releases plot staging files")
  request(13L);staged<-state$attachments[[1L]]$path
  session$setInputs(clear_images=1L)
  check(!length(state$attachments) && !file.exists(staged),"Remove all clears captured plots without sending")
  request(14L);staged<-state$attachments[[1L]]$path
  session$close()
  check(!file.exists(staged) && !dir.exists(attachment_dir),"disconnecting removes staged plot images and their temporary directory")
})
unlink(fixtures)
cat(checks,"plot attachment checks passed\n")
