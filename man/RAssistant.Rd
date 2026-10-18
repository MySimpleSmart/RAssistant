\name{RAssistant}
\alias{launch}
\alias{set_api_key}
\alias{forget_api_key}
\alias{update_assistant}
\title{Launch, Configure and Update RAssistant}
\description{Open the existing chat assistant in the RStudio Viewer, manage its
optional OS-stored API credential, or update the installed package from GitHub.}
\usage{
launch()
set_api_key()
forget_api_key()
update_assistant(ref = "main")
}
\arguments{\item{ref}{A GitHub branch, tag or commit reference.}}
\details{
Run \code{launch()} in the RStudio Console. An existing
\env{OPENAI_API_KEY} takes precedence. Otherwise the first launch prompts for a
key with hidden input and saves it in macOS Keychain, Windows Credential Manager
or Linux Secret Service. The prompt may be a native password dialog.
The key is passed directly to the API client; it is not saved in the package,
project, R history or a startup file. No prompt occurs merely by loading the package.

\code{set_api_key()} replaces the saved credential after prompting.
\code{forget_api_key()} removes only RAssistant's saved credential; it does not
unset or edit an existing environment variable.

The toolbar displays the installed version and Help with copyable Console
commands. On launch, a background process checks the public GitHub package
version. A newer version displays Update now. Confirming closes the current chat
and performs the update, with progress and restart instructions in the Console.
Discovery is cached for one hour per R session, does not use the OpenAI API, and
does not send credentials or conversation data. Offline checks do not block chat.
Help offers a manual check; \code{options(RAssistant.check_updates = FALSE)}
disables automatic checks while retaining explicit manual checks.

Close the assistant before \code{update_assistant()}. Updates use the library
containing the loaded package, retain normal installation locking and run in a
separate R process. Restart R afterward. Credentials are stored separately and
remain available after updates. A new R major/minor version can use a new library,
requiring package reinstallation; the OS-stored key remains separate.

The assistant sends the current Source, detected CSV headers and manually attached
images only when Send is clicked. Approved validation can execute the entire
current R/Rmd file in a child process and send its output and captured plots.
Validation is not a sandbox. Console output must be pasted manually.
}
\value{\code{launch()} invisibly returns a Shiny app handle.
\code{set_api_key()} and \code{forget_api_key()} invisibly return \code{NULL}.
\code{update_assistant()} invisibly returns the installed path and version.}
\examples{
\dontrun{
RAssistant::launch()
RAssistant::set_api_key()
RAssistant::update_assistant()
}
}
