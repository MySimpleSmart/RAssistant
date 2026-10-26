# Non-secret connection preferences are separate from per-provider OS credentials.
.api_providers <- function() {
  entry <- function(label, factory, env, model = "", url = "")
    list(label = label, factory = factory, env = env, model = model, base_url = url)
  list(
    openai = entry("OpenAI", "chat_openai", "OPENAI_API_KEY", "gpt-6.1-sol"),
    anthropic = entry("Anthropic / Claude", "chat_claude", "ANTHROPIC_API_KEY"),
    gemini = entry("Google Gemini", "chat_google_gemini", c("GOOGLE_API_KEY", "GEMINI_API_KEY")),
    deepseek = entry("DeepSeek", "chat_deepseek", "DEEPSEEK_API_KEY"),
    groq = entry("Groq", "chat_groq", "GROQ_API_KEY"),
    mistral = entry("Mistral", "chat_mistral", "MISTRAL_API_KEY"),
    openrouter = entry("OpenRouter", "chat_openrouter", "OPENROUTER_API_KEY"),
    perplexity = entry("Perplexity", "chat_perplexity", "PERPLEXITY_API_KEY"),
    huggingface = entry("Hugging Face", "chat_huggingface", "HF_TOKEN"),
    xai = entry("xAI / Grok", "chat_openai_compatible", "XAI_API_KEY", url = "https://api.x.ai/v1"),
    together = entry("Together AI", "chat_openai_compatible", "TOGETHER_API_KEY", url = "https://api.together.ai/v1"),
    fireworks = entry("Fireworks AI", "chat_openai_compatible", "FIREWORKS_API_KEY", url = "https://api.fireworks.ai/inference/v1"),
    cerebras = entry("Cerebras", "chat_openai_compatible", "CEREBRAS_API_KEY", url = "https://api.cerebras.ai/v1"),
    custom = entry("Custom (OpenAI-compatible)", "chat_openai_compatible", "RASSISTANT_CUSTOM_API_KEY")
  )
}

.api_key_envvars <- function() unique(unlist(lapply(.api_providers(), `[[`, "env"), use.names = FALSE))
.api_text <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
.api_profile <- function(provider = "openai", model = NULL, base_url = NULL) {
  catalog <- .api_providers()
  if (!.api_text(provider) || !provider %in% names(catalog)) stop("Choose a supported API provider.", call. = FALSE)
  spec <- catalog[[provider]]
  if (is.null(model)) model <- spec$model
  if (!.api_text(model) || nchar(model) > 200L || grepl("[[:space:][:cntrl:]]", model))
    stop("Enter a model ID without spaces (up to 200 characters).", call. = FALSE)
  if (provider != "custom") base_url <- spec$base_url
  if (is.null(base_url)) base_url <- ""
  if (!.api_text(base_url)) stop("Enter the API base URL.", call. = FALSE)
  base_url <- sub("/+$", "", trimws(base_url))
  if (provider == "custom") {
    # No credentials in URLs, query strings or plaintext remote endpoints.
    if (nchar(base_url) > 1000L || !grepl("^https://[A-Za-z0-9.-]+(:[0-9]+)?(/[^?#[:space:]@\\\\]*)?$", base_url) ||
        grepl("[[:cntrl:]%]", base_url))
      stop("Use an HTTPS base URL without credentials, query parameters or a fragment (for example https://api.example.com/v1).", call. = FALSE)
  }
  if (identical(spec$factory, "chat_openai_compatible") && !nzchar(model))
    stop("Enter the model ID from this provider's console.", call. = FALSE)
  list(provider = provider, model = model, base_url = base_url)
}

.provider_preferences_path <- function() file.path(tools::R_user_dir("RAssistant", "config"), "api-providers.rds")
.read_api_preferences <- function() {
  empty <- list(active = "openai", profiles = list(openai = .api_profile()))
  path <- .provider_preferences_path()
  if (!file.exists(path)) return(empty)
  saved <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(saved) || !is.list(saved$profiles)) return(empty)
  profiles <- lapply(names(.api_providers()), function(id) {
    x <- saved$profiles[[id]]
    if (!is.list(x)) return(NULL)
    tryCatch(.api_profile(id, x$model, x$base_url), error = function(e) NULL)
  })
  names(profiles) <- names(.api_providers())
  profiles <- Filter(Negate(is.null), profiles)
  active <- if (.api_text(saved$active) && saved$active %in% names(profiles)) saved$active else "openai"
  if (is.null(profiles$openai)) profiles$openai <- .api_profile()
  list(active = active, profiles = profiles)
}
.write_api_preferences <- function(value) {
  path <- .provider_preferences_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE, mode = "0700")
  tmp <- tempfile("api-settings-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(value, tmp)
  if (!file.rename(tmp, path)) stop("Could not save API settings. Check the configuration directory permissions.", call. = FALSE)
  invisible(NULL)
}
.selected_api_profile <- function(provider = NULL, model = NULL, base_url = NULL) {
  prefs <- .read_api_preferences()
  if (is.null(provider)) provider <- prefs$active
  old <- prefs$profiles[[provider]]
  .api_profile(provider, if (is.null(model)) old$model else model,
    if (is.null(base_url)) old$base_url else base_url)
}
.api_service <- function(profile) {
  if (profile$provider == "openai") return(.key_service) # Preserve existing keys.
  if (profile$provider == "custom") return(paste0("RAssistant/custom/", profile$base_url))
  paste0("RAssistant/", profile$provider)
}
.api_factory <- function(name) getExportedValue("ellmer", name)
.api_client <- function(profile, key, system_prompt) {
  spec <- .api_providers()[[profile$provider]]
  if (!.valid_key(key)) stop("No API key configured. Open More actions (...) > API settings.", call. = FALSE)
  args <- list(system_prompt = system_prompt, credentials = function() key, echo = "none")
  if (nzchar(profile$model)) args$model <- profile$model
  if (identical(spec$factory, "chat_openai_compatible")) {
    args$base_url <- profile$base_url
    args$name <- spec$label
  }
  do.call(.api_factory(spec$factory), args)
}
.api_key_source <- function(profile, key) {
  if (!.valid_key(key)) return(list(type="none", name="Not configured"))
  vars <- .api_providers()[[profile$provider]]$env
  values <- Sys.getenv(vars)
  first <- which(nzchar(values))
  if (length(first) && identical(unname(values[[first[[1L]]]]), key))
    return(list(type="environment", name=vars[[first[[1L]]]]))
  list(type="keychain", name="Keychain")
}
.api_key_inventory <- function() {
  # List metadata once when settings opens; never retrieve other providers' keys.
  tryCatch({
    backend <- .credential_backend()
    entries <- suppressWarnings(backend$list())
    if (!is.data.frame(entries) || !all(c("service","username") %in% names(entries)))
      stop("Credential metadata unavailable")
    services <- as.character(entries$service[!is.na(entries$username) & entries$username==.key_account])
    list(available=TRUE, services=unique(services[!is.na(services) & startsWith(services,"RAssistant/")]))
  }, error=function(e) list(available=FALSE, services=character()))
}
.api_model_info <- function(profile) {
  if (nzchar(profile$model)) return(list(name=profile$model, default=FALSE))
  if (.api_providers()[[profile$provider]]$factory=="chat_openai_compatible")
    return(list(name="Not set", default=FALSE))
  # Resolve ellmer's actual default locally, without authentication or HTTP.
  model <- tryCatch(suppressMessages(.api_client(profile,"model-preview",NULL)$get_model()),
    error=function(e)NULL)
  list(name=if(.api_text(model) && nzchar(model))model else "Default unavailable", default=TRUE)
}
.api_model_choices <- function(provider, profile=NULL) {
  spec <- .api_providers()[[provider]]
  if(is.null(spec))stop("Choose a supported API provider.",call.=FALSE)
  base <- list(provider=provider,model=spec$model,base_url=spec$base_url)
  default <- .api_model_info(base)$name
  if(default %in% c("Not set","Default unavailable"))default <- ""
  saved <- if(is.null(profile))""else if(nzchar(profile$model))profile$model else .api_model_info(profile)$name
  values <- unique(c(saved,default));values <- values[nzchar(values) & !values %in% c("Not set","Default unavailable")]
  labels <- ifelse(values==default,paste0(values," (default)"),values)
  list(choices=stats::setNames(values,labels),selected=if(length(values))values[[1L]]else "")
}
.api_models_factory <- function(name)getExportedValue("ellmer",name)
.api_fetch_models <- function(profile,key) {
  reader <- switch(profile$provider,anthropic="models_claude",gemini="models_google_gemini",
    deepseek="models_deepseek",groq="models_groq",mistral="models_mistral","models_openai")
  fn <- .api_models_factory(reader)
  args <- if("credentials" %in% names(formals(fn)))list(credentials=function()key)else list(api_key=key)
  if(reader=="models_openai" && profile$provider!="openai") {
    url <- switch(profile$provider,openrouter="https://openrouter.ai/api/v1",
      huggingface="https://router.huggingface.co/v1",perplexity="https://api.perplexity.ai",profile$base_url)
    if(!nzchar(url))stop("This provider does not expose a model list. Enter a model ID.",call.=FALSE)
    args$base_url <- url
  }
  old <- options(ellmer_timeout_s=10,ellmer_max_tries=1L)
  on.exit(options(old),add=TRUE)
  result <- do.call(fn,args)
  ids <- if(is.data.frame(result)) {
    field <- intersect(c("id","name","model"),names(result))
    if(length(field))result[[field[[1L]]]]else character()
  } else if(is.character(result))result else character()
  ids <- unique(as.character(ids))
  if(profile$provider=="gemini")ids <- sub("^models/","",ids)
  ids <- sort(ids[!is.na(ids) & nzchar(ids) & nchar(ids)<=200L & !grepl("[[:space:][:cntrl:]]",ids)])
  if(!length(ids))stop("No model IDs returned. You can still enter a model ID.",call.=FALSE)
  utils::head(ids,500L)
}
.api_connection_status <- function(current, key, source) {
  catalog <- .api_providers()
  prefs <- .read_api_preferences()$profiles
  inventory <- .api_key_inventory()
  active_model <- .api_model_info(current)
  rows <- list()
  add <- function(profile, remembered=TRUE) {
    id <- profile$provider
    active <- identical(profile$provider,current$provider) &&
      (id!="custom" || identical(profile$base_url,current$base_url))
    model <- if(active)active_model else if(remembered || id!="custom").api_model_info(profile)
      else list(name="Not saved",default=FALSE)
    vars <- catalog[[id]]$env
    values <- Sys.getenv(vars)
    present <- which(nzchar(values))
    env <- if(length(present))vars[[present[[1L]]]]else ""
    env_valid <- if(length(present)).valid_key(unname(values[[present[[1L]]]]))else FALSE
    saved <- .api_service(profile) %in% inventory$services
    configured_model <- if(active)current$model else if(remembered)profile$model else catalog[[id]]$model
    ready_model <- nzchar(configured_model) || catalog[[id]]$factory!="chat_openai_compatible"
    if(!active && !remembered && !saved && !nzchar(env))return(invisible(NULL))
    rows[[length(rows)+1L]] <<- list(provider=id,label=catalog[[id]]$label,base_url=profile$base_url,
      active=active,model=model$name,model_default=model$default,
      configured_model=configured_model,can_use=ready_model &&
        (if(nzchar(env))env_valid else saved),
      stored=if(!inventory$available)"Unavailable"else if(saved)"Saved"else"Not saved",
      environment=env,environment_valid=env_valid)
  }
  for(id in setdiff(names(catalog),"custom")) {
    profile <- if(id==current$provider)current else prefs[[id]]
    remembered <- !is.null(profile)
    if(is.null(profile))profile <- list(provider=id,model=catalog[[id]]$model,base_url=catalog[[id]]$base_url)
    add(profile,remembered)
  }
  custom_urls <- unique(c(if(current$provider=="custom")current$base_url,prefs$custom$base_url,
    substring(inventory$services[startsWith(inventory$services,"RAssistant/custom/")],nchar("RAssistant/custom/")+1L)))
  for(url in custom_urls) {
    profile <- tryCatch(.credential_profile("custom",url),error=function(e)NULL)
    if(is.null(profile))next
    remembered <- !is.null(prefs$custom) && identical(url,prefs$custom$base_url)
    if(remembered)profile <- prefs$custom
    add(profile,remembered)
  }
  if(!length(custom_urls) && any(nzchar(Sys.getenv(catalog$custom$env))))
    add(list(provider="custom",model="",base_url=""),remembered=FALSE)
  list(active=list(provider=current$provider,label=catalog[[current$provider]]$label,
    model=active_model$name,model_default=active_model$default,base_url=current$base_url,
    ready=.valid_key(key),key_source=source),connections=rows,store_available=inventory$available)
}
.new_api_settings <- function(profile, key = NULL) {
  current <- profile
  secret <- key
  key_source <- .api_key_source(profile, key)
  seen <- if (.valid_key(key)) key else character()
  api <- list(
    get = function() current,
    ready = function() .valid_key(secret),
    profiles = function() .read_api_preferences()$profiles,
    catalog = .api_providers,
    active = function() {
      model <- .api_model_info(current)
      list(provider=current$provider,label=.api_providers()[[current$provider]]$label,
        model=model$name,ready=.valid_key(secret))
    },
    status = function() .api_connection_status(current, secret, key_source),
    model_choices = function(provider) {
      profile <- if(provider==current$provider)current else .read_api_preferences()$profiles[[provider]]
      .api_model_choices(provider,profile)
    },
    models = function(provider,base_url=NULL,key="") {
      profile <- .credential_profile(provider,base_url)
      if(!.api_text(key))stop("Enter an API key.",call.=FALSE)
      key <- trimws(key)
      if(nzchar(key) && !.valid_key(key))stop("The API key must not contain whitespace.",call.=FALSE)
      env <- .api_providers()[[provider]]$env
      resolved <- if(any(nzchar(Sys.getenv(env))) || !nzchar(key))
        .resolve_api_key(provider,base_url=profile$base_url,prompt=FALSE)else key
      if(!.valid_key(resolved))stop("Enter an API key or choose a provider with a saved key first.",call.=FALSE)
      seen <<- unique(c(seen,resolved,key[nzchar(key)]))
      .api_fetch_models(profile,resolved)
    },
    secrets = function() unique(c(seen, Sys.getenv(.api_key_envvars(), unset = ""))),
    save = function(provider, model, base_url = NULL, key = "") {
      next_profile <- .api_profile(provider, model, base_url)
      # Validate everything before touching the store or the active connection.
      if (!.api_text(key)) stop("Enter an API key.", call. = FALSE)
      key <- trimws(key)
      if (nzchar(key) && !.valid_key(key)) stop("The API key must not contain whitespace.", call. = FALSE)
      if (nzchar(key)) .store_api_key(key, next_profile)
      next_key <- .resolve_api_key(provider, base_url = next_profile$base_url, prompt = FALSE)
      if (!.valid_key(next_key)) stop("Enter an API key for this provider.", call. = FALSE)
      prefs <- .read_api_preferences()
      prefs$active <- provider
      prefs$profiles[[provider]] <- next_profile
      .write_api_preferences(prefs)
      current <<- next_profile
      secret <<- next_key
      key_source <<- .api_key_source(next_profile, next_key)
      seen <<- unique(c(seen, key[nzchar(key)], next_key))
      invisible(current)
    },
    client = function(system_prompt) .api_client(current, secret, system_prompt)
  )
  api$use <- function(provider,base_url=NULL) {
    profile <- .selected_api_profile(provider,base_url=base_url)
    if(provider=="custom" && !identical(profile$base_url,.read_api_preferences()$profiles$custom$base_url))
      stop("Choose a model for this endpoint below, then click Use model.",call.=FALSE)
    api$save(profile$provider,profile$model,profile$base_url)
  }
  api
}
