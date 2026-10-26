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
model_calls <- list()
config$.api_models_factory <- function(name)function(credentials,base_url=NULL) {
  model_calls[[length(model_calls)+1L]] <<- list(reader=name,key=credentials())
  data.frame(id=c("first-model","second-model"))
}
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
  assign("RAssistant/OpenAI","openai-context-key",store)
  open_api_settings()
  revision <- as.character(api_revision())
  before <- length(calls)
  session$setInputs(api_provider="deepseek",api_form_provider="deepseek",api_form_revision=revision,
    api_discover=list(provider="deepseek",revision=revision,base_url="",entered_key=FALSE,request=1L))
  check(length(model_calls)==1L && model_calls[[1L]]$key=="deepseek-context-key" &&
    api$get()$provider=="deepseek" && length(calls)==before,
    "Load models uses only the chosen key without a chat or provider switch")
  i <- which(vapply(api_status()$connections,function(row)row$provider=="openai",logical(1)))
  session$setInputs(prompt="Draft when returning to OpenAI",api_use_connection=list(index=i,revision="0",nonce=1L))
  check(api$get()$provider=="deepseek","cached Use buttons cannot switch a newer settings dialog")
  session$setInputs(api_use_connection=list(index=i,revision=revision,nonce=2L))
  check(api$get()$provider=="openai" && identical(input$prompt,"Draft when returning to OpenAI") && length(calls)==before,
    "Use OpenAI loads the existing key in one action and preserves chat and draft without inference")
  check(grepl("OpenAI",output$compose_model_status$html,fixed=TRUE) &&
    grepl(api$get()$model,output$compose_model_status$html,fixed=TRUE),
    "composer displays the exact active provider and model")
  session$setInputs(change_model=1L)
  revision <- as.character(api_revision())
  check(api_compact(),"composer opens the compact model picker")
  session$setInputs(api_provider="openai",api_form_provider="openai",api_form_revision=revision,
    api_key_entry="stale-other-provider-key",api_model="first-model",api_discover=list(provider="openai",revision=revision,base_url="",entered_key=TRUE,request=2L))
  check(tail(model_calls,1L)[[1L]]$key=="openai-context-key", "compact model discovery ignores cached key input and uses the selected provider's key")
  reads <- length(model_calls)
  session$setInputs(api_discover=list(provider="gemini",revision=revision,base_url="",entered_key=FALSE,request=3L))
  session$setInputs(api_discover=list(provider="openai",revision="0",base_url="",entered_key=FALSE,request=4L))
  check(length(model_calls)==reads,"stale provider or settings revisions never query another model catalog")
  session$setInputs(api_save=6L)
  check(api$get()$model=="first-model" && get("RAssistant/OpenAI",store)=="openai-context-key" && length(calls)==before &&
    grepl("first-model",output$compose_model_status$html,fixed=TRUE) && identical(input$prompt,"Draft when returning to OpenAI"),
    "compact switching updates the active model and composer without writing a cached key, sending chat or losing the draft")
  session$setInputs(chat_submit=list(text="Use the composer model",nonce=5L))
  check(tail(calls,1L)[[1L]]$model=="first-model" && tail(calls,1L)[[1L]]$key=="openai-context-key",
    "the next Send uses the model selected beside Send")
  old_revision <- api_revision();state$busy <- TRUE
  session$setInputs(change_model=2L)
  check(identical(api_revision(),old_revision),"a busy request cannot open the model picker or change its target")
  state$busy <- FALSE
  open_api_settings(compact=TRUE)
  session$setInputs(api_provider="xai",api_form_provider="xai",api_form_revision=as.character(api_revision()),
    api_model="xai-chosen-model",api_more_settings=1L)
  check(!api_compact() && grepl("xai-chosen-model",output[[api_fields_id()]]$html,fixed=TRUE) &&
    grepl("api_key_entry",output[[api_fields_id()]]$html,fixed=TRUE),
    "opening key management preserves the pending provider and model instead of reverting to the active provider")
  session$setInputs(api_form_revision=as.character(api_revision()),api_key_entry="xai-provider-key",api_save=7L)
  check(api$get()$provider=="xai" && api$get()$model=="xai-chosen-model" && get("RAssistant/OpenAI",store)=="openai-context-key" &&
    grepl("xAI / Grok",output$compose_model_status$html,fixed=TRUE),
    "the chosen provider and model activate with their own key and update the compact composer tooltip")
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
