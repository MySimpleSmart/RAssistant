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
.new_api_settings <- function(profile, key = NULL) {
  current <- profile
  secret <- key
  seen <- if (.valid_key(key)) key else character()
  list(
    get = function() current,
    ready = function() .valid_key(secret),
    profiles = function() .read_api_preferences()$profiles,
    catalog = .api_providers,
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
      seen <<- unique(c(seen, key[nzchar(key)], next_key))
      invisible(current)
    },
    client = function(system_prompt) .api_client(current, secret, system_prompt)
  )
}
