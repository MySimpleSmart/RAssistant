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
update_assistant(ref = "main", relaunch = TRUE)
}
\arguments{
\item{ref}{A GitHub branch, tag or commit reference.}
\item{relaunch}{Whether to reopen the assistant after a successful update.
Defaults to \code{TRUE}; only opens the app when running in RStudio.}
}
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

The toolbar keeps Console On/Off and New chat visible. More actions contains
History, Knowledge, Check updates, Help, Close and the installed version. Help has copyable
Console commands. Source shares the header row with the visible actions; long
names are truncated with a tooltip. Controls retain their original appearance. The composer starts
at two lines and grows with the draft. Attachment thumbnails above it have
individual remove controls; Clear removes all when multiple images are attached.
Each Send reads the current Source buffer, including unsaved changes, up to
50,000 characters. Chat labels the file and line count sent.
Review on an AI code block checks syntax and shows a comparison before Apply.
Users can replace or append to the captured file, or create an unsaved R/Rmd
document. R snippets appended to Rmd are wrapped in an R chunk. Static checks
parse R, R Markdown/Quarto R chunks and inline R; YAML, other languages and
runtime correctness are not checked. Save and execution remain separate.
Apply requires an unchanged document ID, path and contents. Approval is single
use and expires on Cancel, chat switches or disconnect. Source baselines and
pending edit approvals are not restored from History.
New replies and restored conversations show the latest response. Image loads,
composer resizing and status dismissal preserve that position. Intentional
reading of older messages is preserved between replies.
On launch and every 30 minutes while the assistant is open, a background
process checks the public GitHub package version. A newer version automatically
displays Update now. Confirming closes the current chat
and performs the update, with progress in the Console. After a successful update,
the assistant reopens automatically and restores any saved chat and draft.
Discovery is cached between automatic checks, does not use the OpenAI API, and
does not send credentials or conversation data. Offline checks do not block chat
or hide a previously found update. A manual up-to-date result closes after five
seconds; an available Update now banner remains. More actions has a manual check that bypasses the
cache immediately; \code{options(RAssistant.check_updates = FALSE)}
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

Knowledge stores lecture PDFs, notes, R code examples and optional custom instructions per
project, separately from History. Each request is matched locally, and only a
bounded set of relevant excerpts, labelled by file and page or lines, is sent.
Set \code{options(RAssistant.knowledge_dir = "...")} before launch to change its
location, or \code{options(RAssistant.knowledge_chars = 6000)} to change the
excerpt budget. Adding PDFs requires the suggested \pkg{pdftools} package.

Close the assistant before \code{update_assistant()}. Updates use the library
containing the loaded package, retain normal installation locking and run in a
separate R process. After a successful update, the assistant reopens automatically
in RStudio; it reloads the new package code
without clearing the workspace or unloading dependencies. If another package
imports RAssistant or reloading fails, a restart may still be needed.
Use \code{relaunch = FALSE} to install without reopening. If installation succeeds
but reopening fails, the Console gives a separate message and a manual retry command.
The first upgrade from 0.3.3 or earlier still uses the old updater; run
\code{launch()} once afterward. Subsequent updates reopen automatically.
For the first upgrade from 0.1.3 or earlier, close the assistant and run
\code{unloadNamespace("RAssistant")} after updating, then \code{launch()}.
Credentials are stored separately and
remain available after updates. A new R major/minor version can use a new library,
requiring package reinstallation; the OS-stored key remains separate.

Plot, beside Image, exports the plot selected in RStudio's Plots
pane as a PNG using \code{rstudioapi::savePlotAsImage()}. It stages a preview;
Send submits the actual image through the normal attachment flow. Source is not
rerun. Plots history selection is respected. The button is in chat, not in
RStudio's Plots toolbar. Up to three images of 4 MB each can be attached.

The package app enables Console context by default in the header beside New chat. Wait for
the On status, then run code normally. Send includes up to 200 recent lines and
12,000 characters of new R output and associated commands, including ordinary
uncaught errors, warnings and messages. Completed commands follow their output
with task boundaries. Errors include condition calls and stack entries when
available, which may be enclosing calls rather than full commands. Silent commands,
including plotting and assignments, are recorded with a no-text-output label.
Comments are not recorded; plot pixels are not included. Earlier image uploads
are historical context, not the latest plot. A fixed registration helper runs in the RStudio Console without
changing focus. Activation refuses to overwrite an unfinished Console command.
Capture starts on, can be switched off, keeps normal output visible, and clears on disable, chat
changes, disconnect, Close or update. The raw temporary capture is not saved in
chat snapshots; answers based on it remain in chat. The active API key is redacted.
Switching chats preserves the On/Off choice with a fresh buffer when enabled.
Old output, plots, external-process output and conditions caught by other handlers
may be absent. Cleanup waits for any output sink placed above ours; updates wait
for that cleanup. Large output is trimmed when control returns between commands
or to app polling, so a running command can temporarily grow the spool.

The assistant sends the current Source, detected CSV headers, bounded conversation,
enabled Console context and attached or retained images only when Send is clicked. Approved validation can execute the entire
current R/Rmd file in a child process and send its output and captured plots.
Validation is not a sandbox. Console output may also be pasted manually.
Calling \code{launch()} again reopens the running app in RStudio's Viewer,
including after its page disappears or disconnects. The existing server is
reused and the previous session is saved before restoring the chat and draft.
}
\value{\code{launch()} invisibly returns a Shiny app handle.
\code{set_api_key()} and \code{forget_api_key()} invisibly return \code{NULL}.
\code{update_assistant()} invisibly returns a list with the installed path,
version, commit SHA, \code{updated} (whether the installation changed) and
\code{relaunched} (whether the assistant reopened).}
\examples{
\dontrun{
RAssistant::launch()
RAssistant::set_api_key()
RAssistant::update_assistant()
}
}
