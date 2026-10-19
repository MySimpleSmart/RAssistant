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
and performs the update, with progress and relaunch instructions in the Console.
Discovery is cached for one hour per R session, does not use the OpenAI API, and
does not send credentials or conversation data. Offline checks do not block chat.
Help offers a manual check; \code{options(RAssistant.check_updates = FALSE)}
disables automatic checks while retaining explicit manual checks.

New chat keeps the previous conversation in History, which offers Rename, Delete
and Restore. Chats auto-save locally per project, outside the package installation,
using \code{tools::R_user_dir("RAssistant", "data")}. The most recently used chat
is restored by \code{launch()} after closing, restarting RStudio or updating.
Opening RStudio does not automatically launch the assistant. Outside a project,
the launch working directory defines the scope. Text, code, output, images and
drafts are retained; run approvals are never restored. Source tab changes preserve
the conversation, with historical Source context distinguished from the live editor.

Follow-ups reuse up to three recent images, or older uploads named by their
Image number. Text context is bounded and includes locally retrieved excerpts;
the whole archive is not sent on every turn. Restoration does not call OpenAI.
History uses plain JSON and deduplicated image files, not encrypted storage.
Set \code{options(RAssistant.history_dir = "...")} before launch to change its
location, or \code{options(RAssistant.save_history = FALSE)} to disable persistence.
Already-closed conversations from versions before 0.2.0 cannot be recovered.

Knowledge stores lecture PDFs, notes, R code examples and course preferences per
project, separately from History. Each request is matched locally, and only a
bounded set of relevant excerpts, labelled by file and page or lines, is sent.
Set \code{options(RAssistant.knowledge_dir = "...")} before launch to change its
location, or \code{options(RAssistant.knowledge_chars = 6000)} to change the
excerpt budget. Adding PDFs requires the suggested \pkg{pdftools} package.

Close the assistant before \code{update_assistant()}. Updates use the library
containing the loaded package, retain normal installation locking and run in a
separate R process. Run \code{launch()} afterward; it reloads the new package code
without clearing the workspace or unloading dependencies. If another package
imports RAssistant or reloading fails, a restart may still be needed.
For the first upgrade from 0.1.3 or earlier, close the assistant and run
\code{unloadNamespace("RAssistant")} after updating, then \code{launch()}.
Credentials are stored separately and
remain available after updates. A new R major/minor version can use a new library,
requiring package reinstallation; the OS-stored key remains separate.

The assistant sends the current Source, detected CSV headers, bounded conversation
context and attached or retained images only when Send is clicked. Approved validation can execute the entire
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
