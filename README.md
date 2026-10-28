# RAssistant

A lightweight AI assistant for RStudio with multiple AI providers. It opens the existing
`AISource.R` chat in the Viewer, with image uploads, code Copy buttons, contextual
R help and explicitly approved R/Rmd validation.

## Install

Run in the **RStudio Console** with R 4.2 or newer (the current R release is
recommended). First choose **Session > Restart R**, so no packages are loaded:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_url("https://github.com/MySimpleSmart/RAssistant/archive/refs/heads/main.tar.gz",
  upgrade = "always")
RAssistant::launch()
```

This downloads the package archive directly instead of using
`install_github()`, which calls the GitHub API. That API allows only 60 anonymous
requests per hour per IP address, shared by everyone on a university or office
network, and then fails with **HTTP error 403. API rate limit exceeded**.
`upgrade = "always"` updates older installed dependencies, such as shiny and
ellmer, to the versions RAssistant requires; `"never"` leaves them too old and
installation fails. Use your personal R library if R offers to create one. RAssistant is installed in
an R package library, separately from the RStudio application. The RStudio Addins
menu also has an **RAssistant** entry after installation.
The header keeps **Console On/Off**, **Workspace On/Off** and **New chat** visible. The **More actions
(...)** menu groups tools under **Code**, **Chat** and **App**.
Selecting text in Source automatically shows Explain/Fix/Improve above the
composer. Placing the cursor in an Rmd/Quarto R chunk shows the same toolbar;
there is no dropdown activation step. Fix/Improve show changes in chat; Replace still
requires approval. **Code edit history** records code changes; **Chat history**
opens a separate page with local conversations, a read-only preview and
**Back to chat**. **Knowledge Base** holds reference files and instructions.
Check updates and Help each have their own full-width menu row; Close is separated below them, red,
full-width and centered.
Send has a send icon that becomes a spinner while working, using a fixed icon
slot so the button does not change width. The send symbol is centered with its
label.
App also shows the installed version.
Help contains launch, API-key and update instructions with Copy buttons.
Advanced commands are expandable; manual update results appear below the header.
The current file name shares the header row with the visible actions; long names have a tooltip.
The controls retain their original bordered appearance. The two-line composer
grows with your draft; Image, Plot, Undo/Redo arrow icons and Send share one action row.
While waiting for an AI response, **Send** becomes **Stop** with a square icon.
Stop terminates the local request process, ignores late responses and keeps your
draft and attachments for retry. Closing the Viewer session also cancels pending work.
You can stop AI analysis after an approved script run; its execution output stays
in chat. Stop does not interrupt that separate R execution. A provider may already
have processed part of a stopped request. Other work uses the existing loading
icon without a separate Thinking bar.
If the Viewer disappears or disconnects after sleep or locking the Mac, use the
Addin or `RAssistant::launch()` again. It reopens the existing server in the Viewer
and restores the saved chat and draft; RStudio does not need to be restarted.

### Windows troubleshooting

- **"HTTP error 403. API rate limit exceeded":** you used `install_github()` on
  a shared network. Use the `install_url()` command above, which does not use
  the GitHub API.
- **"cannot remove prior installation" or "lock" errors:** a package is in use.
  Choose **Session > Restart R**, then run the install command again before
  loading anything else. If a `00LOCK-*` folder remains in the library shown by
  `.libPaths()[1]`, delete it and retry.
- **"ERROR: lazy loading failed for package 'RAssistant'"** with no other
  message: a dependency (often curl) was upgraded while loaded in the same
  session. Choose **Session > Restart R** and run the install command again.
  If it still fails, this shows which dependency cannot load and why:
  `for (p in c("shiny", "ellmer", "keyring", "callr", "knitr", "later")) tryCatch(loadNamespace(p), error = function(e) message(p, ": ", conditionMessage(e)))`
- The **Rtools** warning is harmless; RAssistant has no compiled code.
- **"package 'shiny' ... was found, but >= 1.14.0 is required"** (or ellmer):
  restart R and use `upgrade = "always"` as shown above.
- **Compilation, Rtools or "binary version is not available" messages:** your R
  is too old for CRAN's Windows binaries. Install the current R from
  <https://cloud.r-project.org/bin/windows/base/>, restart RStudio and reinstall.
- **"Open RStudio and run RAssistant::launch()":** the assistant runs in the
  RStudio Viewer, not RGui or a terminal. Open RStudio and run it there.

## API settings

Open **More actions (...) > API settings**, choose a provider, enter its API key
and optionally a model ID, then **Save & use**. You can switch without restarting
or clearing the chat. A first launch without a key opens this form automatically.
**Active connection** shows the provider, exact model and loaded key source.
The **Connections** table lists each saved model, whether its key exists in
current status and key source. **Using now** marks the active connection;
**Key available** marks another configured provider ready to select. Missing or
unavailable keys have an explicit action label. Environment keys are shown by
variable name without a misleading missing-Keychain warning. Settings controls
fill the page width. Provider and Model use the same scrollable selected-row
buttons as Conversation; models can be filtered locally with Search models.
**Enter model ID** remains a collapsed fallback for endpoints without a catalog. Defaults show their concrete model ID.
Keychain status is read from metadata when settings opens; saved secrets are
not retrieved for this overview. An unavailable store is shown separately from
a missing key. Unsaved provider selections do not change the active connection.
**A compact model name appears beside Send.** Long names use an ellipsis; hover
shows the complete provider and model. Click it to open **Provider & model**,
select a **Provider**, choose its **Model**, then click **Apply**. Providers with
a saved or environment key are marked. The picker shows the full current
connection and reuses saved keys while preserving the chat and draft.
**API settings** opens key management and keeps your pending provider/model
selection, including a custom endpoint. Switching is disabled while a request or run approval is active.

Click **Use OpenAI** (or another saved provider's **Use** button) to switch in
one step without entering its key again. The model picker automatically fetches model IDs
using only the selected provider's key on open, provider changes and key entry.
**Refresh models** retries discovery. API-reported models are grouped separately
from configured IDs absent from the latest list; a failed lookup does not label
old defaults as available. Choose a reported model or type a custom ID, then
click **Use model**. Loading models does not send a chat or activate the provider.

Built-in providers: OpenAI, Anthropic / Claude, Google Gemini, DeepSeek, Groq,
Mistral, OpenRouter, Perplexity, Hugging Face, xAI / Grok, Together AI, Fireworks AI
and Cerebras. **Custom (OpenAI-compatible)** accepts another service's HTTPS
Chat Completions base URL and exact model ID. It does not support arbitrary API
protocols. Custom, xAI, Together, Fireworks and Cerebras require a model ID from
the provider's console. Other providers can use their ellmer default or an exact
model ID. Choose an image-capable model for Plot and Image attachments.

Keys are stored separately per provider in the OS credential store (macOS
Keychain). An existing OpenAI key keeps its original `RAssistant/OpenAI` service
and `default` account. A Custom key is bound to its exact base URL; changing the
URL cannot reuse another endpoint's saved key. Model preferences and the last
selected provider are stored locally without keys, outside the project and
package installation.

Provider environment variables take precedence over saved keys:
`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GOOGLE_API_KEY` / `GEMINI_API_KEY`,
`DEEPSEEK_API_KEY`, `GROQ_API_KEY`, `MISTRAL_API_KEY`, `OPENROUTER_API_KEY`,
`PERPLEXITY_API_KEY`, `HF_TOKEN`, `XAI_API_KEY`, `TOGETHER_API_KEY`,
`FIREWORKS_API_KEY`, `CEREBRAS_API_KEY` and `RASSISTANT_CUSTOM_API_KEY`.
The form indicates when an environment key overrides a saved key.

Console commands remain available; never paste a key into a Console command:

```r
RAssistant::set_api_key("anthropic")  # Hidden password prompt
RAssistant::launch(provider = "anthropic")
RAssistant::forget_api_key("anthropic") # Removes only this saved key
```

`set_api_key()` and `forget_api_key()` without arguments still target OpenAI.
Keys are never stored in preferences, chat history, the package or project.
Save does not call an AI API; authentication and model access are checked on Send.
The selected provider receives messages and enabled context, including retained
chat context. There is no automatic fallback to another provider.

## Update

On launch, a background check reads this repository's public package version.
From 0.3.6, it checks again every 30 minutes while the assistant is open, so
new versions display the update banner without opening Help or restarting the app.
From 0.2.2, it resolves the current main commit over public Git HTTP and reads
that commit's version file, avoiding stale branch-file caches and GitHub's
IP-shared REST API quota. No GitHub token or Git executable is needed for checks.
When a newer version is available, the chat shows **Update now**. Click it,
review the notice, then confirm. This saves and closes the current chat, installs
into the same R library and prints progress in the RStudio Console.
From 0.3.4, after successful installation the assistant reopens automatically
and restores the saved chat and draft (when local history is enabled).
The Console reports the old and new versions; More actions shows the installed
version when reopened.

Between automatic checks, results are cached for 30 minutes. A new launch checks
immediately. Offline/failed checks do not prevent chat or hide an update already
found. **More actions (...) > Check updates** checks immediately,
bypassing the cache.
An up-to-date result closes after five seconds. An available update keeps its
Update now banner until you act on it.
The update dialog lists the target version's release changes before installation.
The check does not call an AI provider or transmit the key, Source text or chat history.
To disable automatic discovery, set `options(RAssistant.check_updates = FALSE)`;
manual checking and Console updates still work.

For a manual update, or to get this feature from an older installation:

Click **Close** in the assistant, then run:

```r
RAssistant::update_assistant()
```

The assistant reopens automatically in RStudio. It reloads RAssistant's updated code while
preserving your R workspace and loaded dependencies. A restart is only needed
if reloading fails, such as when another loaded package imports RAssistant.
If installation succeeds but reopening fails, the Console reports this separately
and gives `RAssistant::launch()` as the retry command.
Each new server uses a different local port so a delayed disconnect from the
previous page cannot clear the replacement Viewer. The Viewer opens after server
startup, then confirms its HTTP load and Shiny connection. A lost opening retries
up to twice with a fresh page URL; a page that is still loading gets additional
time. A short connection check also covers a page cleared just after connecting.
Checks end after a stable connection, Close, or a newer launch. If confirmation
never arrives, the Console explains how to reopen the still-running app.
The updater installs from this repository's `main` branch into the **same R
library containing the loaded RAssistant package**, rather than whichever
library happens to be first. It uses a clean R process and normal installation
locking. Installation does not read, replace or delete the saved key; reopening
uses the existing key through the normal launcher. No updates run
automatically. You can choose a tag/commit with `update_assistant(ref = "...")`.
An unchanged installed version and GitHub commit reports **already up to date**
and reopens without reloading the namespace. Use
`RAssistant::update_assistant(relaunch = FALSE)` to install without reopening.
Outside RStudio, updates install without attempting to open the assistant.

The first update from **0.3.3 or earlier** still runs the old updater.
Run `RAssistant::launch()` once after that update; subsequent updates reopen
automatically.

When upgrading from 0.1.3 or earlier, the already-loaded old launcher still has
its restart guard. After the update finishes and the assistant is closed, reload
it once without clearing your workspace:

```r
unloadNamespace("RAssistant")
RAssistant::launch()
```

Inspect the installed package location and version:

```r
find.package("RAssistant")
packageVersion("RAssistant")
```

An R upgrade may change the default package library; reinstall the package for
that R version. The OS-stored key survives. Moving to another computer or OS user
requires its own key setup. Updating this GitHub repository does not immediately
modify other users' installations; they run the update command.

## Conversations and History

From 0.2.0, chats auto-save locally per RStudio project. **New chat** starts a
separate conversation and keeps the previous one. **Chat history** opens a
separate page with saved chats, a read-only preview, **Rename**, **Delete** and
**Open chat**. Conversations use the same scrollable button list as Code edit
history. Preview starts with the latest 20 complete messages; **Load earlier
messages** adds 20 at a time until the entire conversation is visible. Code
blocks, tables, formulas and images use the main chat styles; Copy works, while
Source editing and run approval controls are omitted. **Back to chat** returns
without changing the conversation or draft. Delete asks for confirmation.
The most recently used chat reopens on the next `RAssistant::launch()`, including
after restarting RStudio or updating the package. Opening RStudio alone does not
automatically launch the assistant. Previously closed pre-0.2.0 chats cannot be recovered.

New replies and restored conversations open at the latest response. Image loads,
composer resizing and status dismissal keep that position stable. You can still
scroll back to read older messages between replies; sending a new question
returns to the newest response.

Text, code, copied output, assignment context, images and unsent drafts are saved.
Empty new chats are not saved. Unsent text or images count as a draft; clearing
both before sending removes that empty entry from History.
Run approvals are never restored; any execution needs a fresh request and Yes.
The header follows Source tabs and file renames even without a code selection.
Switching Source tabs preserves the conversation and labels earlier Source
context. Only a fresh editor read is treated as the current script.

Follow-up messages reuse up to three recent images automatically. To refer to
older uploads, use the labels shown under them, for example **Image 1** or
**Compare Image 1 and Image 2**. Original pixels are read from local history;
you do not need to upload those files again. A new upload sends the new batch;
explicitly referencing an earlier image includes it too, for example **Compare
this new image with Image 1**. A request can contain at most three images in total.
If a comparison exceeds that limit, the draft and uploads stay ready while you
choose fewer images.
Text context is bounded to 32,000 characters, including recent turns and locally
retrieved older excerpts, plus the assignment and current Source. Saved history
is larger than the context supplied with each request; recall is not unlimited.
Restoring and searching local history do not call an AI provider. Sending retained image
pixels consumes image input tokens, as with a new upload.

Storage uses `tools::R_user_dir("RAssistant", "data")`, outside the project and
package library, with a separate folder per normalized project path. Outside an
RStudio project, the launch working directory defines the scope. Chats use plain
JSON and deduplicated image files, with owner-only file permissions where supported;
history is not encrypted. API credentials remain in their separate OS store.
One prior auto-save is retained for recovery; failed writes leave the live chat
available and report an error. Snapshots have a 64 MB text limit; very long
histories and image previews can still use substantial memory.

Configure storage before launch if needed:

```r
options(RAssistant.history_dir = "~/my-rassistant-data")
options(RAssistant.save_history = FALSE) # Disable durable History for this session
```

The standalone script retains in-session context; durable History is provided
by the installed package.

## Knowledge Base

**Knowledge Base** stores reference material locally for the current RStudio
project: PDFs, notes (`.txt`, `.md`) and R code examples (`.R`, `.Rmd`,
`.qmd`), up to 50 MB each. An optional **Custom instructions** box holds anything
the assistant should always follow for your work, such as preferred packages,
functions or coding conventions.

Checkboxes under **Use in this chat** save automatically; unchecking a file keeps
it in Knowledge Base. Each file has its own **Delete** button, which asks before
deleting the stored copy and leaves the original file intact. **Save instructions**
only saves the instructions text.

When files are selected for this chat, RAssistant matches your message and
pasted task against those files on your computer. Search also considers functions and package names
in the current Source, conditions and command names from the latest completed
Console task, and column names of relevant Global Environment datasets. A
dataset is relevant when referenced in the question, Source or captured command,
or selected beside Workspace; search does not read its cell values. Console Off
and Workspace Off exclude their hints. These hints are refreshed on each Send.

For a question such as “explain this error,” work context provides the search
topic. A specific new question stays authoritative, so unrelated requests do not
pull material from the open script merely because it is there. Only the
few most relevant sections (about 6,000 characters at most) are added, with
their file and page or line numbers; unrelated requests add none. Code fences,
tables, display equations and top-level R expressions stay whole, with section
headings and original indentation. A block that cannot fit is omitted and
identified in the expandable receipt instead of sending a cut-off fragment.
Search uses
the words and R function names you enter; it does not translate queries or make
extra API calls. The assistant is instructed to prefer the methods,
packages and functions taught in that material over generic alternatives and to
name the source it used. Answers that received reference material list it under
**Knowledge Base**.

In **Knowledge Base**, check up to five files under **Use in this chat** to
restrict the reference material. Each selected file contributes a short excerpt,
even for a follow-up such as “explain this.” With no files checked, no document
search or excerpts are sent. Saved custom instructions still apply.
Selected filenames appear above the message box; click **×** to stop
using one. Selection is saved with that chat and resets for a new chat.

Expand the **Knowledge Base** line under your message to see the exact excerpts
and custom instructions supplied for that request. It also reports no matches,
missing selected files, and requests that failed or were not sent. These local
receipts remain in chat History even if a reference file is later changed or
removed; they are not reused as current evidence on later requests.

Knowledge is stored per project in `tools::R_user_dir("RAssistant", "data")`,
separately from chat History and the package installation, so it survives
RStudio restarts and package updates. Only extracted text is kept, not the
original files. Re-adding a file with the same name replaces the earlier
version; deleting a chat does not affect knowledge, and removing a document does
not affect chats.

Files added before structural extraction are marked **older format**. Re-add
the original file to rebuild its formatting; this keeps its selection and does
not change excerpts saved in earlier chats. Formatting removed by an older
version cannot be recovered from the saved text alone.

PDF extraction preserves spacing present in its text layer, including aligned
text tables. It cannot reconstruct equation symbols or visual layout missing
from that layer. Adding PDFs needs the pdftools package. Scanned PDFs without a
text layer need OCR first; built-in OCR and image-based document reading are not
included in this update.

```r
install.packages("pdftools")
```

## Attach the current RStudio plot

Select a graph in RStudio's **Plots** pane, then click **Plot**
beside Image in RAssistant. The selected plot is exported as a PNG and appears
in the attachment previews. Type your question and press **Send** to submit it.
There is no manual Export or clipboard step, and Source is not rerun.
A blue dot before Plot indicates that an image is available in RStudio's Plots
pane, whether or not it is attached to chat. Hover or keyboard focus shows the
current plot's preview. While the Viewer is visible and idle, a small local
export checks availability every two seconds, without hover or focus changes.
These checks pause while hidden or busy and make no AI request.
Previews stay local and do not create attachments. Clicking Plot captures a fresh
full-size image for your next Send.

Each click captures the plot currently selected, including an older graph
selected with the Plots history arrows. This uses RStudio's `savePlotAsImage()`
API, not `ggplot2::last_plot()`. The button is in the chat; RStudio's own Plots
toolbar is not modified. The existing three-image and 4 MB per-image limits apply.
Attaching alone makes no AI request. Each thumbnail has a remove control;
Clear discards all staged images when multiple images are attached.

## Context inspector

**... > Context** opens a separate inspector page. **Draft preview** reads the
current message's Source, Console, Workspace/project excerpts, selected Knowledge
Base files, images and earlier conversation locally. **Refresh** rereads it;
opening this page makes no model request or code execution. Off switches still
exclude their context. The provider/model is shown without its key.

**Last Send** shows the actual gathered context and delivery status, including
approved full-script validation output when it was sent for analysis. It remains
fixed when you edit the draft or change Workspace values. Send always rereads
context instead of using the preview. These raw snapshots are temporary: they
are cleared on chat changes/disconnect and are not saved in History. **Back to
chat** preserves the draft.

## Console context

In the package app, **Console** starts enabled in the header beside New chat. Wait for
the status to say **On**, then run R code normally in RStudio. Ask about the result
or error and press **Send**: the request includes up to 200 recent lines and
12,000 characters of captured R output and associated commands. Copy/Paste is not required.

Capture starts on each time you open the Viewer; you can turn it off. To prepare error and warning logging, the app runs
its own fixed helper in the RStudio Console without switching focus. Finish or
clear a command already being typed there before enabling capture. The status
shows On when logging is ready; hover for detailed status. Activation does not call an AI provider.

Normal output remains visible, and ordinary uncaught errors, warnings and messages
are recorded without suppressing them. The temporary capture is cleared when you
turn the option off, change chats, disconnect, Close or update. Raw captured output
is not added to saved chat snapshots, though answers based on it remain in chat.
Switching chats preserves the On/Off choice and starts a fresh buffer when On.
The active API key is redacted from captured context before it is sent.

When captured text is used, **Console · Sent · N lines** appears below your
message. Expand it to inspect the exact redacted excerpt. A failed request is
labelled Request failed. Saved chat history retains the status and counts,
without the raw temporary excerpt. Console evidence precedes Source context in
the model request. A direct Console question with no capture or supplied image
gets a short local explanation of Empty, Off or Unavailable, rather than a
prediction of what the script would print. Run code with Console On to capture
new output; output printed before capture began is unavailable.

Completed commands are recorded after their output with task boundaries. Errors
include their condition call or stack entry when available; an enclosing call
may not show the whole command. Commands that print no text, including plotting
and assignments, are recorded with a no-text-output label. Comments are not recorded.
The assistant uses this evidence rather than guessing the originating command
from the current Source. A plotting command does not include the plot's pixels;
earlier uploaded images remain historical context, not evidence of the latest plot.

Each request also isolates the latest recorded Console task, retaining multiline
continuation code. Older failures remain labelled historical context. When output
exceeds the request limit, the newest tail is kept. Datasets named in the latest
command can supply fresh values through Workspace context, even when Source does
not mention them.

The latest failed task can show a clickable **Console error · file:line** in chat,
including the Rmd/Quarto chunk name. R source references identify the recorded
runtime location when available. Otherwise, a unique expression match in the
current editor is labelled **Matching Source**; this does not prove that file
produced the error. Repeated expressions and changed or unavailable code show a
short status instead of a guessed link. Clicking opens and checks the document
before focusing its line; it never executes or replaces code. Location evidence
is included with that request so corrections can target only the failing lines.
Raw stack metadata and navigation snapshots remain temporary, like Console capture,
and are not stored in chat history. A fresh Send can locate an error again.

Only output produced after enabling is available. This is a bounded excerpt,
not proof the current Source was executed. Plots, external-process output,
messages intercepted by other handlers and output hidden by other sinks may be
absent. If another package has temporarily put its own output sink on top of
ours, cleanup waits for that sink to close; updating waits for cleanup.
Large output is trimmed between commands and while the app polls capture status;
a long-running command can temporarily grow the spool before control returns.

## Use and limits

- **Image** accepts up to 3 PNG/JPEG/WebP images, 4 MB each. Screenshots use
  content-neutral reading guidance; include a message to request a specific task.
- **Console** starts On to include new R output automatically; you can switch it off.
- AI code stays a proposal until you review its comparison and approve Apply.
- Recent dialogue, retrieved earlier excerpts, a remembered assignment and retained
  images support multipart work. Use History to return to another conversation.
- Script validation requires **Yes**, executes the entire current R/Rmd file in
  a child process, and may affect files. Only approved output/plots are shared.
- After Replace, **Check changed code** offers a smaller, separately approved run.
- Current Source text, detected CSV headers, enabled Console/Workspace context and uploaded images are sent to
  your selected AI provider when you click **Send**. An API account/key and internet access are needed.
- Standalone `source("AISource.R")` remains supported with `OPENAI_API_KEY` set.

## Review and apply Source changes

Extra empty lines and final newlines are spacing, not code errors. Spacing-only
answers do not open an automatic correction. Manual Review groups safe spacing
changes separately and excludes them until **Include spacing changes** is chosen.
Blank lines that change a multiline string's value are still real code changes.

Each Send rereads the current Source buffer, including unsaved changes. Chat shows
the file name and line count actually sent; hovering over the header name shows
the character count. The current limit is 50,000 characters in one active file.

Corrections propose only the changed Source lines, with global document line
numbers for R, Rmd, Quarto and Markdown. One or more non-overlapping corrections
open a comparison inside the code block. Source and proposed line numbers,
removed/added lines and highlighted characters show the precise changes.
Each changed section has **Copy**, **Replace** and **Skip**. Copy includes only
that section's replacement code. Replace applies it after your click. Applying one
refreshes the remaining comparisons against the new editor contents. **Apply all**
approves the complete proposal. Skip never writes to Source. Changing Source
manually invalidates the pending edits. Partial changes remain unsaved if you close
the comparison; Cancel does not undo changes you already approved.

Click the `filename:line` link or an old line number in a comparison to select
that line in the matching Source editor, including unsaved documents.

Select code in the RStudio Source editor: a compact **Explain / Fix / Improve**
toolbar appears above the composer automatically. One verified selection takes
priority; otherwise an Rmd/Quarto R chunk under the cursor uses the same toolbar.
It shows the file and exact target. The cross hides it until the selection changes.
Nothing appears for an ordinary R file without a selection. For whole-file
questions, ask in chat as usual or select all code in Source.

While Source has focus, a local check runs about every 300 ms, with only one
request outstanding. Checks pause while the assistant is busy or hidden; returning
to the Viewer triggers a fresh check. The history page follows the Source name and
Undo/Redo availability while leaving the code toolbar alone. Checks use
RStudio's read-only Source API without moving focus, running code or sending AI
requests. Unchanged selections do not rebuild the toolbar.
Syntax errors suggest Fix; valid syntax defaults to Explain without claiming
the code is free from runtime errors. The current draft can add instructions.

Explain stays read-only. Fix/Improve propose changes only inside the displayed
target, without reproducing a whole file or chunk for a small fix. Actions reread the selection at click time, so a newly selected
range works immediately even before the next check. Losing the selection never
silently expands a request to the whole file. Once an answer arrives, its proposal remains bound
to that captured Source snapshot and still needs approval. Selected edits
preserve their surrounding text; chunk edits preserve YAML, fences, prose and
other chunks. The complete changed chunks' R syntax is checked, so unrelated broken
chunks do not prevent a local correction. Nothing is saved, run or knitted.

**Workspace On/Off** sits beside Console in the header and starts On. You can
switch it off there to exclude metadata, dataset excerpts and project scanning. When On, Send
includes up to 30 objects' types, dimensions and column names (up to 40 columns).
Loaded data.frames and matrices named in the question, latest captured Console command or current Source also
supply actual cell values. The small arrow beside Workspace opens a local dataset
picker and table preview; select datasets there to prioritize them even when your
question does not name them. Opening the picker does not call AI.

Requested datasets and the latest Console references take priority over other
automatic Source references. Each request rereads up to three relevant datasets, with their actual dimensions
and up to the first 20 rows / 20 columns. Cell text is capped at 160 characters;
excerpts shrink further to fit 5,000 characters per dataset. They are samples,
not whole-dataset statistical results or proof the current script ran. Dataset sampling skips active
bindings, lazy promises, functions, S4 objects and credential-like names.
Nested columns and credential-like columns do not supply cell values;
user-defined print/subset/summary methods are not invoked. New chat clears manual
selections. Ordinary data.frame/tibble/data.table columns and atomic matrices
are supported; specialized column classes may be omitted.

Workspace On also reads function signatures and up to three relevant in-memory
function definitions without calling them. These may differ from unsaved Source.
Each Send scans the current project root and one directory level for a bounded
file inventory. A dependency map also follows literal `source()`/`sys.source()`
imports and Rmd child references, including deeper project paths. It accounts for
`source(chdir=TRUE)`, detects cycles and maps function calls to candidate definitions
by file and global line number. Dynamic paths, changed working-directory scopes,
missing files and duplicate definitions are labelled rather than guessed.

Traversal reads at most six files over three import levels, each at most 32,000
bytes. Dependency excerpts and up to three other explicitly referenced documents
share a 10,000-character budget. When a module is too large for a complete excerpt,
relevant complete function definitions take priority. The dependency report is
bounded separately. The active editor always comes from its unsaved buffer;
related code comes from fresh disk reads, without execution or proof of runtime
bindings. To edit a related file, open it in Source first. Hidden folders,
dependency trees and links outside the project are excluded. Workspace Off
excludes the dependency scan together with other Workspace context.

**... > Code edit history** opens a separate page with time, target, status,
proposal notes and the exact before/after comparison for approved edits.
**Back to chat** preserves the conversation, draft, attachments and reading position.
Backups are stored
separately from chats under the project's hash in the local RAssistant data
directory, with private permissions. The latest 100 records are retained.
`options(RAssistant.edits_dir = "/chosen/directory")` changes the location;
setting it to `NULL` keeps edits in memory only. Log failure prevents an
unrecorded edit. Uncertain API failures are logged and cannot be retried blindly. Full comparisons include the entire saved script, with
horizontal and vertical scrolling for long lines and files. The selected row is
highlighted and its list position is preserved.

The **Undo** and **Redo** arrow icons are disabled when the current Source has no
available AI change. They remain visible beside Image and Plot, with
tooltips and accessible labels. Each opens a comparison
on the history page, then waits for **Confirm Undo** or **Confirm Redo**.
Undo reverses the last AI edit; Redo reapplies the last undone edit. Multiple
steps follow their original order. A new AI edit ends the previous Redo branch.
Restorations are logged, unsaved and unexecuted. Any intervening manual edit or
change after the preview blocks restoration. Open the matching Source document
first. Saved-file Undo and Redo remain available after relaunch;
unsaved documents require the same app instance. Document creation is logged
but Undo does not delete a newly created document. These backups contain code
and are never automatically included in AI requests.
Multiple alternative code blocks require choosing **Review** on the one you want.

You can also click **Review** beside Copy on an AI R/Rmd code block. Exact line
corrections review only their proposed ranges. Whole-file replacement is available
for an explicit complete replacement proposal; ordinary examples offer addition
or a new document. Inspect the syntax result and
the removed/added lines directly inside the chat code block. **Replace**, **Append**
or **Create document** is the approval. **Copy** copies the proposed code without
editing Source; **Cancel** closes the comparison and returns to the code.
Cancel leaves the editor unchanged. An R snippet appended to Rmd becomes a new R chunk.

When a Source review finds a concrete error, the assistant is instructed to include
only the corrected ranges for these controls, even if you only ask what is wrong.
Ask for explanation only if you do not want a code proposal.

Checks parse R syntax, R Markdown/Quarto R chunks and inline R without executing
them. They do not validate YAML, other languages, package availability or runtime
results. After Replace, **Check changed code** previews the changed complete R
expressions or affected Rmd/Quarto chunk bodies. **Run changed code** approves
only that preview in a separate R process (45 seconds); Cancel runs nothing.
The preview lists the current Workspace inputs to copy. Up to 20 ordinary inputs
and helpers, totalling 10 MB, can be copied; lazy/active bindings, unsupported
objects and missing inputs stop the check. Setup code and other chunks are not
automatically run. An edited function definition is evaluated without calling
its body, and the result says so. This is an isolated context, not a full copy of
RStudio's options, connections or session state, and code can affect local files.
Output and plots appear in chat without an automatic model request. Source and
input requirements are rechecked before starting; restored history has no live
execution approval. The existing whole-script validation remains available.

Apply targets the exact document and contents captured for that answer. A changed,
renamed or closed document invalidates replacement/append; send a fresh message.
Approvals are single use and expire on Cancel, New chat, history restore or disconnect.
Editor changes are unsaved. New R/Rmd documents are also unsaved and never run automatically.
Source baselines and pending edit approvals are not stored in History; restored answers
can create new documents, but editing an existing file requires another Source read.

## Development

Keep editing **`AISource.R`**. The package wrapper does not replace the app.
After an app change, regenerate the shipped snapshot and commit both files:

```sh
Rscript tools/sync-app.R
Rscript tools/sync-app.R --check
Rscript tests/regression.R
Rscript tests/source-context.R
Rscript tests/source-edits.R
Rscript tests/source-workflow.R
Rscript tests/source-advanced.R
Rscript tests/source-restore.R
Rscript tests/source-smart.R
Rscript tests/source-auto.R
Rscript tests/assignment-context.R
Rscript tests/image-reading.R
Rscript tests/conversation-context.R
Rscript tests/conversation-history.R
```

`inst/app/AISource.R` is a generated, byte-identical snapshot. CI fails if it is
out of sync. Increment `Version` in `DESCRIPTION` for each release. Package tests
exercise credential and update behavior using fake stores/installers, without
reading real secrets. The optional browser suite is `Rscript tests/browser.R`;
the opt-in live vision test is documented in [REVIEW.md](REVIEW.md).

No open-source license has been selected; copyright is reserved in `LICENSE`.
