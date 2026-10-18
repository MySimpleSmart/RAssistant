# A fixed service/account pair survives package, R and RStudio updates.
# No credentials are written into the package, working directory or .Renviron.
.key_service <- "RAssistant/OpenAI"
.key_account <- "default"

.credential_backend <- function() {
  backend <- tryCatch(keyring::default_backend(), error = function(e) {
    stop("Cannot open the OS credential store. Configure keyring or use OPENAI_API_KEY.", call. = FALSE)
  })
  if (!backend$name %in% c("macos", "wincred", "secret_service")) {
    stop("A secure OS credential store is unavailable. Configure keyring for Keychain, Windows Credential Manager or Linux Secret Service, or use OPENAI_API_KEY. No key was saved.", call. = FALSE)
  }
  backend
}

.valid_key <- function(key) {
  is.character(key) && length(key) == 1L && !is.na(key) &&
    nzchar(key) && !grepl("[[:space:]]", key)
}

.credential_exists <- function(backend) {
  entries <- tryCatch(backend$list(service = .key_service), error = function(e) {
    stop("Cannot read the OS credential store. Unlock it and retry; the saved key was not changed.", call. = FALSE)
  })
  is.data.frame(entries) && "username" %in% names(entries) &&
    any(entries$username == .key_account, na.rm = TRUE)
}

.read_saved_key <- function(backend) {
  key <- tryCatch(backend$get(.key_service, username = .key_account), error = function(e) {
    stop("Cannot retrieve the saved API key. Unlock the OS credential store and retry. To replace it, run RAssistant::set_api_key().", call. = FALSE)
  })
  if (!.valid_key(key)) stop("The saved API key is empty or invalid. Run RAssistant::set_api_key().", call. = FALSE)
  key
}

.interactive_session <- function() interactive()
.prompt_key <- function() askpass::askpass("OpenAI API key (input hidden): ")

.save_api_key <- function(backend) {
  if (!.interactive_session()) {
    stop("Run RAssistant::set_api_key() interactively in the RStudio Console, or configure OPENAI_API_KEY.", call. = FALSE)
  }
  key <- tryCatch(.prompt_key(), error = function(e) {
    stop("Could not open the hidden password prompt. Retry in an interactive RStudio session. No key was changed.", call. = FALSE)
  })
  if (is.null(key) || identical(key, "")) stop("API key setup cancelled. No key was changed.", call. = FALSE)
  key <- trimws(key)
  if (!.valid_key(key)) stop("The API key must be a single nonempty value without whitespace. No key was changed.", call. = FALSE)
  tryCatch(backend$set_with_value(.key_service, username = .key_account, password = key),
    error = function(e) stop("Could not save the API key in the OS credential store. Unlock it and retry.", call. = FALSE))
  invisible(NULL)
}

set_api_key <- function() {
  .save_api_key(.credential_backend())
  message("API key saved in the OS credential store.")
  if (nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    message("OPENAI_API_KEY is already set and takes precedence over the saved key.")
  }
  invisible(NULL)
}

forget_api_key <- function() {
  backend <- .credential_backend()
  if (.credential_exists(backend)) {
    tryCatch(backend$delete(.key_service, username = .key_account),
      error = function(e) stop("Could not remove the saved key. Unlock the OS credential store and retry.", call. = FALSE))
  }
  message("No API key is saved for RAssistant. An existing OPENAI_API_KEY environment variable is unchanged.")
  invisible(NULL)
}

.resolve_api_key <- function() {
  key <- Sys.getenv("OPENAI_API_KEY")
  if (nzchar(key)) {
    if (!.valid_key(key)) stop("OPENAI_API_KEY contains whitespace or is invalid. Correct the environment variable and retry.", call. = FALSE)
    return(key)
  }
  backend <- .credential_backend()
  if (!.credential_exists(backend)) {
    message("First launch: enter your OpenAI API key in the hidden password prompt. It will be saved in your OS credential store.")
    .save_api_key(backend)
  }
  .read_saved_key(backend)
}
