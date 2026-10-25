# A fixed service/account pair survives package, R and RStudio updates.
# No credentials are written into the package, working directory or .Renviron.
.key_service <- "RAssistant/OpenAI"
.key_account <- "default"

.credential_backend <- function() {
  backend <- tryCatch(keyring::default_backend(), error = function(e) {
    stop("Cannot open the OS credential store. Unlock Keychain or use this provider's API key environment variable.", call. = FALSE)
  })
  if (!backend$name %in% c("macos", "wincred", "secret_service")) {
    stop("A secure OS credential store is unavailable. Configure keyring or use this provider's API key environment variable. No key was saved.", call. = FALSE)
  }
  backend
}

.valid_key <- function(key) {
  is.character(key) && length(key) == 1L && !is.na(key) &&
    nzchar(key) && !grepl("[[:space:]]", key)
}

.credential_profile <- function(provider = "openai", base_url = NULL) {
  .api_profile(provider, model = "credential", base_url = base_url)
}
.credential_exists <- function(backend, profile = .credential_profile()) {
  entries <- tryCatch(backend$list(service = .api_service(profile)), error = function(e) {
    stop("Cannot read the OS credential store. Unlock it and retry; the saved key was not changed.", call. = FALSE)
  })
  is.data.frame(entries) && "username" %in% names(entries) &&
    any(entries$username == .key_account, na.rm = TRUE)
}

.read_saved_key <- function(backend, profile = .credential_profile()) {
  key <- tryCatch(backend$get(.api_service(profile), username = .key_account), error = function(e) {
    stop("Cannot retrieve the saved API key. Unlock the OS credential store and retry, or replace this provider's key in API settings.", call. = FALSE)
  })
  if (!.valid_key(key)) stop("The saved API key is empty or invalid. Replace this provider's key in API settings.", call. = FALSE)
  key
}

.interactive_session <- function() interactive()
.prompt_key <- function(provider = "openai") askpass::askpass(paste0(.api_providers()[[provider]]$label, " API key (input hidden): "))

.save_api_key <- function(backend, profile = .credential_profile()) {
  if (!.interactive_session()) {
    stop("Run RAssistant::set_api_key(provider = ...) interactively in the RStudio Console, or use API settings.", call. = FALSE)
  }
  key <- tryCatch(if (profile$provider == "openai") .prompt_key() else .prompt_key(profile$provider), error = function(e) {
    stop("Could not open the hidden password prompt. Retry in an interactive RStudio session. No key was changed.", call. = FALSE)
  })
  if (is.null(key) || identical(key, "")) stop("API key setup cancelled. No key was changed.", call. = FALSE)
  key <- trimws(key)
  if (!.valid_key(key)) stop("The API key must be a single nonempty value without whitespace. No key was changed.", call. = FALSE)
  .store_api_key(key, profile, backend)
  invisible(NULL)
}

.store_api_key <- function(key, profile, backend = .credential_backend()) {
  if (!.valid_key(key)) stop("The API key must be a single nonempty value without whitespace.", call. = FALSE)
  tryCatch(backend$set_with_value(.api_service(profile), username = .key_account, password = key),
    error = function(e) stop("Could not save the API key in the OS credential store. Unlock it and retry.", call. = FALSE))
  invisible(NULL)
}

set_api_key <- function(provider = "openai", base_url = NULL) {
  profile <- .credential_profile(provider, base_url)
  .save_api_key(.credential_backend(), profile)
  message("API key saved in the OS credential store.")
  env <- .api_providers()[[provider]]$env
  if (any(nzchar(Sys.getenv(env)))) {
    message(paste(env, collapse = " / "), " is already set and takes precedence over the saved key.")
  }
  invisible(NULL)
}

forget_api_key <- function(provider = "openai", base_url = NULL) {
  profile <- .credential_profile(provider, base_url)
  backend <- .credential_backend()
  if (.credential_exists(backend, profile)) {
    tryCatch(backend$delete(.api_service(profile), username = .key_account),
      error = function(e) stop("Could not remove the saved key. Unlock the OS credential store and retry.", call. = FALSE))
  }
  message("No API key is saved for ", .api_providers()[[provider]]$label, ". Environment variables are unchanged.")
  invisible(NULL)
}

.resolve_api_key <- function(provider = "openai", base_url = NULL, prompt = TRUE) {
  profile <- .credential_profile(provider, base_url)
  env <- .api_providers()[[provider]]$env
  values <- Sys.getenv(env)
  key <- if (any(nzchar(values))) unname(values[which(nzchar(values))[[1L]]]) else ""
  if (nzchar(key)) {
    if (!.valid_key(key)) stop(paste(env, collapse = " / "), " contains whitespace or is invalid. Correct the environment variable and retry.", call. = FALSE)
    return(key)
  }
  backend <- .credential_backend()
  if (!.credential_exists(backend, profile)) {
    if (!prompt) return(NULL)
    message("Enter your ", .api_providers()[[provider]]$label, " API key in the hidden password prompt. It will be saved in your OS credential store.")
    .save_api_key(backend, profile)
  }
  .read_saved_key(backend, profile)
}
