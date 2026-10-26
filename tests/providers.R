# Credential isolation, persisted preferences and real ellmer request adapters.
# Synthetic keys and mocked HTTP only; no real Keychain or paid API is used.
library(RAssistant)
main <- function() {
  ns <- asNamespace("RAssistant")
  env <- new.env(parent = ns)
  for(name in ls(ns,all.names=TRUE)) {
    x <- get(name,ns)
    if(is.function(x))environment(x) <- env
    assign(name,x,env)
  }
  checks <- 0L
  check <- function(ok,label) {
    if(!isTRUE(ok))stop(label,call.=FALSE)
    checks <<- checks+1L;cat("PASS",label,"\n")
  }
  failed <- function(expr,pattern) {
    x <- tryCatch(force(expr),error=identity)
    inherits(x,"error") && grepl(pattern,conditionMessage(x),fixed=TRUE)
  }
  root <- tempfile("ra-api-tests-");dir.create(root)
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  env$.provider_preferences_path <- function()file.path(root,"settings.rds")
  vars <- env$.api_key_envvars()
  old <- Sys.getenv(vars,unset=NA_character_)
  Sys.unsetenv(vars)
  on.exit({Sys.unsetenv(vars);if(any(!is.na(old)))do.call(Sys.setenv,as.list(old[!is.na(old)]))},add=TRUE)
  store <- new.env(parent=emptyenv());broken <- FALSE;locked <- FALSE;key_reads <- 0L
  backend <- list(name="macos",
    list=function(service=NULL) {
    if(locked)stop("Keychain locked")
    services <- ls(store,all.names=TRUE)
    if(!is.null(service))services <- services[services==service]
    data.frame(service=services,username=rep("default",length(services)))
  },
    get=function(service,username){key_reads <<- key_reads+1L;get(service,store,inherits=FALSE)},
    set_with_value=function(service,username,password) {
      if(broken)stop("store error including synthetic-secret")
      assign(service,password,store)
    },delete=function(service,username)rm(list=service,envir=store))
  env$.credential_backend <- function()backend
  env$.interactive_session <- function()TRUE
  env$.prompt_key <- function(provider="openai")paste0(provider,"-synthetic-key")
  env$set_api_key()
  env$set_api_key("anthropic")
  check(identical(get("RAssistant/OpenAI",store),"openai-synthetic-key"),"existing OpenAI service is preserved")
  check(identical(env$.resolve_api_key("anthropic"),"anthropic-synthetic-key"),"Claude resolves its own key")
  check(is.null(env$.resolve_api_key("gemini",prompt=FALSE)),"new provider does not borrow an OpenAI key or force a prompt")
  api <- env$.new_api_settings(env$.api_profile(),"openai-synthetic-key")
  api$save("anthropic","claude-model",key="claude-new-key")
  check(api$ready() && identical(api$get()$provider,"anthropic"),"saving switches the active provider")
  text <- paste(capture.output(dput(readRDS(env$.provider_preferences_path()))),collapse="\n")
  check(!grepl("key|secret|password",text,ignore.case=TRUE),"persisted preferences contain no credentials")
  check(identical(env$.selected_api_profile(),api$get()),"last saved provider and model survive a fresh launch")
  api$save("openai","my-openai-model")
  check(identical(api$get()$model,"my-openai-model") && identical(env$.resolve_api_key(),"openai-synthetic-key"),
    "switching back reuses OpenAI without overwriting another key")
  check(identical(api$profiles()$anthropic$model,"claude-model"),"model preferences are separate per provider")
  Sys.setenv(ANTHROPIC_API_KEY="claude-env-key")
  check(identical(env$.resolve_api_key("anthropic"),"claude-env-key"),"provider environment key overrides its own saved key")
  Sys.unsetenv("ANTHROPIC_API_KEY")
  Sys.setenv(GEMINI_API_KEY="gemini-env-key")
  check(identical(env$.resolve_api_key("gemini"),"gemini-env-key"),"Gemini accepts its alternative environment variable")
  Sys.unsetenv("GEMINI_API_KEY")
  api$save("custom","custom-model","https://one.example/v1/",key="one-key")
  check(identical(api$get()$base_url,"https://one.example/v1"),"custom URL normalization binds the credential to one endpoint")
  check(failed(api$save("custom","custom-model","https://two.example/v1"),"Enter an API key") &&
    identical(api$get()$base_url,"https://one.example/v1"),"a changed endpoint cannot reuse the previous endpoint's key")
  api$save("custom","another-model","https://two.example/v1",key="two-key")
  check(identical(env$.resolve_api_key("custom","https://one.example/v1"),"one-key"),"different custom endpoints retain separate keys")
  reads <- key_reads
  saved_backend <- env$.credential_backend
  env$.credential_backend <- function()stop("Composer must not query Keychain")
  active <- api$active()
  env$.credential_backend <- saved_backend
  check(active$provider=="custom" && active$model=="another-model" && active$ready && key_reads==reads,
    "composer model metadata is accurate without reading any credential or Keychain inventory")
  snapshot <- api$get()
  for(url in c("http://example.com/v1","https://user:password@example.com/v1","https://example.com/v1?key=secret",
               "https://example.com/v1#fragment","https://example.com/@other","https://example.com\\evil"))
    check(failed(api$save("custom","test",url,key="bad-key"),"HTTPS"),paste("unsafe custom URL rejected:",url))
  check(identical(snapshot,api$get()) && identical(env$.selected_api_profile(),snapshot),"failed validation preserves current and persisted settings")
  check(failed(api$save("xai",""),"model ID"),"compatible providers require an explicit model")
  check(failed(api$save("anthropic","bad model"),"model ID"),"invalid model text fails before saving a key")
  broken <- TRUE
  check(failed(api$save("gemini","gemini-model",key="synthetic-secret"),"Could not save") && identical(snapshot,api$get()),
    "Keychain failure preserves the active connection with a generic error")
  broken <- FALSE
  check(all(c("openai-synthetic-key","claude-new-key","one-key","two-key") %in% api$secrets()),"redaction remembers keys used before provider switches")
  env$forget_api_key("anthropic")
  check(is.null(env$.resolve_api_key("anthropic",prompt=FALSE)) && identical(env$.resolve_api_key(),"openai-synthetic-key"),
    "forgetting Claude leaves the OpenAI key intact")
  check(failed(api$save("anthropic","test"),"Enter an API key"),"missing credentials never fall back to another provider")
  reads <- key_reads
  status <- api$status()
  check(key_reads==reads,"connection overview lists metadata without retrieving saved secrets")
  custom <- Filter(function(row)row$provider=="custom",status$connections)
  check(length(custom)==2L && all(vapply(custom,function(row)row$stored=="Saved",logical(1))),
    "the overview includes keys saved for both current and earlier custom endpoints")
  active <- Filter(function(row)row$active,status$connections)
  check(length(active)==1L && active[[1L]]$model=="another-model" && status$active$key_source$type=="keychain",
    "exactly one connection is active with its exact model and loaded Keychain source")
  text <- jsonlite::toJSON(status,auto_unbox=TRUE)
  check(!grepl("openai-synthetic-key|claude-new-key|one-key|two-key",text),"connection metadata never includes key values")
  api$save("gemini","",key="gemini-store-key")
  status <- api$status()
  expected <- suppressMessages(env$.api_client(env$.api_profile("gemini"),"preview",NULL)$get_model())
  check(identical(status$active$model,expected) && status$active$model_default && nzchar(expected),
    "provider defaults display the same concrete model ID used by the real client")
  Sys.setenv(GEMINI_API_KEY="new-env-key")
  status <- api$status()
  row <- Filter(function(row)row$provider=="gemini",status$connections)[[1L]]
  check(status$active$key_source$type=="keychain" && row$stored=="Saved" && row$environment=="GEMINI_API_KEY",
    "a new environment override does not falsely relabel the already loaded Keychain credential")
  api$save("gemini","")
  check(api$status()$active$key_source$name=="GEMINI_API_KEY" && api$status()$active$key_source$type=="environment",
    "Save and use identifies the environment key actually loaded into the connection")
  Sys.setenv(GEMINI_API_KEY="invalid key")
  row <- Filter(function(row)row$provider=="gemini",api$status()$connections)[[1L]]
  check(!row$environment_valid && api$ready(),"an invalid replacement environment value is marked without changing the running key")
  Sys.unsetenv("GEMINI_API_KEY")
  locked <- TRUE
  status <- api$status()
  check(!status$store_available && all(vapply(status$connections,function(row)row$stored=="Unavailable",logical(1))) && status$active$ready,
    "unavailable Keychain metadata is distinct from a missing key and does not stop the active connection")
  locked <- FALSE
  Sys.setenv(GROQ_API_KEY="groq-env-only-key")
  row <- Filter(function(row)row$provider=="groq",api$status()$connections)[[1L]]
  check(row$environment=="GROQ_API_KEY" && row$stored=="Not saved" && !row$active,
    "environment-only providers appear with no false saved-key or active status")
  Sys.unsetenv("GROQ_API_KEY")
  choices <- api$model_choices("deepseek")
  check(nzchar(choices$selected) && choices$selected %in% unname(choices$choices),
    "a provider's concrete default model is selectable before its first request")
  before <- api$get();prefs_before <- env$.read_api_preferences()
  discovery <- list();reader <- env$.api_models_factory
  env$.api_models_factory <- function(name)function(credentials,base_url=NULL) {
    discovery[[length(discovery)+1L]] <<- list(reader=name,key=credentials(),url=base_url)
    data.frame(id=c("model-beta","model-alpha","bad model","model-alpha"))
  }
  models <- api$models("anthropic",key="discovery-only-key")
  check(identical(models,c("model-alpha","model-beta")) && identical(tail(discovery,1L)[[1L]]$key,"discovery-only-key"),
    "model discovery uses the selected unsaved key and filters duplicate or invalid IDs")
  check(identical(api$get(),before) && identical(env$.read_api_preferences(),prefs_before) &&
    is.null(env$.resolve_api_key("anthropic",prompt=FALSE)),
    "loading a catalog neither activates a provider nor persists its entered key")
  Sys.setenv(OPENAI_API_KEY="openai-env-switch-key")
  api$models("openai",key="ignored-new-key")
  check(tail(discovery,1L)[[1L]]$key=="openai-env-switch-key","catalog lookup follows the same environment-key precedence as chat")
  api$use("openai")
  check(api$get()$provider=="openai" && api$status()$active$key_source$name=="OPENAI_API_KEY",
    "one-click Use OpenAI loads the environment credential without requiring key entry")
  Sys.unsetenv("OPENAI_API_KEY")
  active_before <- api$get()
  check(failed(api$use("custom","https://one.example/v1"),"Choose a model") && identical(api$get(),active_before),
    "an earlier custom endpoint cannot silently reuse another endpoint's model")
  env$.api_models_factory <- reader
  for(id in names(env$.api_providers())) {
    p <- env$.api_profile(id,"test-model",if(id=="custom")"https://mock.example/v1"else NULL)
    client <- env$.api_client(p,"synthetic-transport-key","System context")
    check(inherits(client,"Chat") && identical(client$get_model(),"test-model"),paste(id,"constructs the requested model without a network call"))
  }
  if(requireNamespace("httr2",quietly=TRUE)) {
    metadata_request <- NULL
    model_ids <- httr2::with_mocked_responses(function(req) {
      metadata_request <<- req
      httr2::response(status_code=200L,url=req$url,headers=list(`content-type`="application/json"),
        body=charToRaw('{"data":[{"id":"deepseek-first","object":"model","owned_by":"deepseek"},{"id":"deepseek-second","object":"model","owned_by":"deepseek"}]}'))
    },env$.api_fetch_models(env$.credential_profile("deepseek"),"metadata-test-key"))
    check(identical(model_ids,c("deepseek-first","deepseek-second")) && endsWith(metadata_request$url,"/models") && is.null(metadata_request$body),
      "real ellmer discovery sends a metadata request and reads the provider's actual model IDs")
    image_file <- file.path(root,"plot.png")
    grDevices::png(image_file,width=80,height=80)
    graphics::par(mar=rep(0,4));graphics::plot.new();graphics::points(.5,.5)
    invisible(grDevices::dev.off())
    for(id in c("anthropic","gemini","xai","openrouter","custom")) {
      profile <- env$.api_profile(id,"test-model",if(id=="custom")"https://mock.example/v1"else NULL)
      request <- NULL
      mock <- function(req) {
        request <<- req
        body <- switch(id,
          anthropic=list(id="msg_test",type="message",role="assistant",model="test-model",
            content=list(list(type="text",text="Adapter works")),stop_reason="end_turn",usage=list(input_tokens=1L,output_tokens=2L)),
          gemini=list(candidates=list(list(content=list(role="model",parts=list(list(text="Adapter works"))),finishReason="STOP")),
            modelVersion="test-model",usageMetadata=list(promptTokenCount=1L,candidatesTokenCount=2L,totalTokenCount=3L)),
          list(id="chat_test",object="chat.completion",model="test-model",choices=list(list(index=0L,
            message=list(role="assistant",content="Adapter works"),finish_reason="stop")),
            usage=list(prompt_tokens=1L,completion_tokens=2L,total_tokens=3L)))
        httr2::response(status_code=200L,url=req$url,headers=list(`content-type`="application/json"),
          body=charToRaw(jsonlite::toJSON(body,auto_unbox=TRUE)))
      }
      client <- env$.api_client(profile,"synthetic-transport-key","System context")
      answer <- httr2::with_mocked_responses(mock,client$chat("User context",
        ellmer::content_image_file(image_file,resize="none"),echo="none"))
      check(identical(as.character(answer),"Adapter works"),paste(id,"parses a real ellmer response through mocked HTTP"))
      headers <- unlist(lapply(request$headers,function(x)if(inherits(x,"weakref"))rlang::wref_value(x)else x))
      check(any(grepl("synthetic-transport-key",headers,fixed=TRUE)),paste(id,"authenticates with only the selected provider key"))
      check(if(id %in% c("xai","openrouter","custom"))endsWith(request$url,"/chat/completions")else
        if(id=="anthropic")endsWith(request$url,"/messages")else grepl("generateContent",request$url,fixed=TRUE),
        paste(id,"uses its correct native or compatible API endpoint"))
      payload <- jsonlite::toJSON(request$body$data,auto_unbox=TRUE)
      check(grepl("User context",payload,fixed=TRUE) && grepl("System context",payload,fixed=TRUE) &&
        grepl("base64|inline_data|inlineData",payload),paste(id,"keeps system context, user context and actual image bytes in its request"))
    }
  }
  cat(checks,"checks passed\n")
}
main()
