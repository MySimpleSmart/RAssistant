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
