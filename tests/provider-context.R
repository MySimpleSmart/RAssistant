main <- function() {
source("tests/helpers.R")
config <- new.env(parent=globalenv())
for(file in c("R/credentials.R","R/providers.R"))sys.source(file,config)
root <- tempfile();dir.create(root)
on.exit(unlink(root,recursive=TRUE),add=TRUE)
vars <- config$.api_key_envvars();old <- Sys.getenv(vars,unset=NA_character_)
Sys.unsetenv(vars)
on.exit({Sys.unsetenv(vars);if(any(!is.na(old)))do.call(Sys.setenv,as.list(old[!is.na(old)]))},add=TRUE)
config$.provider_preferences_path <- function()file.path(root,"settings.rds")
store <- new.env(parent=emptyenv())
config$.credential_backend <- function()list(name="macos",
  list=function(service=NULL) {
    services <- ls(store,all.names=TRUE)
    if(!is.null(service))services <- services[services==service]
    data.frame(service=services,username=rep("default",length(services)))
  },
  get=function(service,username)get(service,store,inherits=FALSE),
  set_with_value=function(service,username,password)assign(service,password,store))
calls <- list();api_failure <- FALSE
config$.api_factory <- function(name)function(system_prompt,credentials,echo,model=NULL,...)list(
  chat=function(...) {
    calls[[length(calls)+1L]] <<- list(factory=name,key=credentials(),model=model,pieces=list(...))
    if(api_failure)stop(paste("Mock error for",credentials()))
    "Response from the selected provider."
  })
api <- config$.new_api_settings(config$.api_profile())
app <- load_assistant(package_info=list(version="test",api=api,poll_update=function()list(status="disabled")))
app$initial <- list(id="doc",path="test.R",contents="x <- 1")
app$active_doc <- function(id=NULL)app$initial
checks <- 0L
check <- function(ok,label) {
  if(!isTRUE(ok))stop(label,call.=FALSE)
  checks <<- checks+1L;cat("PASS",label,"\n")
}
shiny::testServer(app$server,{
  session$flushReact()
  session$setInputs(chat_submit=list(text="First message",nonce=1L))
  check(!length(calls) && !length(state$messages),"missing credentials open settings without sending or recording a failed chat")
  session$setInputs(prompt="Keep this draft",api_provider="anthropic",api_model="claude-test",api_form_provider="openai",
    api_key_entry="claude-context-key",api_form_revision="1",api_save=1L)
  check(!api$ready() && grepl("fields to load",api_notice(),fixed=TRUE),"a stale provider form cannot save a key under another provider")
  session$setInputs(api_form_provider="anthropic",api_form_revision="0",api_save=2L)
  check(!api$ready() && grepl("fields to load",api_notice(),fixed=TRUE),
    "an earlier opening's cached fields cannot activate a connection")
  session$setInputs(api_form_revision="1",api_save=3L)
  check(api$ready() && identical(api$get()$provider,"anthropic") && identical(input$prompt,"Keep this draft"),
    "saving a provider retains the current draft")
  session$setInputs(chat_submit=list(text="Explain x",nonce=2L))
  check(length(calls)==1L && identical(calls[[1L]]$key,"claude-context-key") &&
    identical(calls[[1L]]$factory,"chat_claude") && identical(calls[[1L]]$model,"claude-test"),
    "Send uses the newly selected native factory, key and model")
  check(grepl("CURRENT EDITOR",calls[[1L]]$pieces[[1L]],fixed=TRUE) && grepl("1 | x <- 1",calls[[1L]]$pieces[[1L]],fixed=TRUE),
    "switching providers preserves fresh numbered Source context")
  session$setInputs(api_provider="gemini",api_form_provider="gemini",api_model="gemini-test",
    api_key_entry="gemini-context-key",api_save=4L)
  api_failure <<- TRUE
  session$setInputs(chat_submit=list(text="Explain x again",nonce=3L))
  last <- tail(state$messages,1L)[[1L]]$text
  check(identical(tail(calls,1L)[[1L]]$key,"gemini-context-key") && grepl("redacted",last,fixed=TRUE) &&
    !grepl("gemini-context-key",last,fixed=TRUE),"provider errors are redacted before they enter chat history")
  check(identical(input$prompt,"Explain x again") || identical(state$draft,"Explain x again"),"failed API requests preserve the draft for retry")
  api_failure <<- FALSE
  session$setInputs(chat_submit=list(text="Explain x again",nonce=4L))
  check(length(Filter(function(m)m$role=="You",state$messages))==3L,
    "switching providers keeps the prior conversation and its retry")
  session$setInputs(api_provider="deepseek",api_form_provider="deepseek",api_model="deepseek-chat",
    api_key_entry="deepseek-context-key",api_save=5L)
  fixture <- tempfile(fileext=".png")
  grDevices::png(fixture,width=80,height=80);par(mar=rep(0,4));plot.new();points(.5,.5);invisible(grDevices::dev.off())
  app$ask_ai("system","prompt",images=fixture)
  unlink(fixture)
  sent <- tail(calls,1L)[[1L]]
  check(identical(sent$factory,"chat_deepseek") && length(sent$pieces)==3L && !is.character(sent$pieces[[2L]]),
    "image requests keep image content when switching providers instead of silently sending text alone")
  redacted <- app$redact_api_secrets("claude-context-key gemini-context-key deepseek-context-key")
  check(!grepl("context-key",redacted,fixed=TRUE),"redaction covers earlier providers after switching")
  session$close()
})
# Validation children must not inherit any supported provider's environment key.
Sys.setenv(ANTHROPIC_API_KEY="child-secret",GEMINI_API_KEY="child-secret")
run <- app$validate_script('stopifnot(Sys.getenv("ANTHROPIC_API_KEY")=="", Sys.getenv("GEMINI_API_KEY")=="")',tempdir())
Sys.unsetenv(c("ANTHROPIC_API_KEY","GEMINI_API_KEY"))
check(run$ok,"approved validation clears other provider keys from its subprocess environment")
unlink(c(root,run$plot_dir),recursive=TRUE)
cat(checks,"checks passed\n")

}
main()
