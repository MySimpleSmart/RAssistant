# RAssistant

A lightweight AI assistant for RStudio, powered by OpenAI. It opens the existing
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
The toolbar shows the installed version. **Help** contains launch, API-key and
update instructions with Copy buttons, so you do not need the old local script
to find the commands.

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

## First-launch API key

If `OPENAI_API_KEY` is already configured, it is used as before. Otherwise
`launch()` asks for a key using a hidden password prompt. Depending on your OS,
the prompt appears in the terminal or a native password dialog. Paste the key
there, rather than typing a command containing it into the Console.

The saved credential uses service `RAssistant/OpenAI`, account `default`:

| OS | Credential location |
| --- | --- |
| macOS | Keychain |
| Windows | Credential Manager |
| Linux desktop | Secret Service, such as GNOME Keyring |

The key is not saved in this repository, the package installation, `.Rhistory`,
`.Renviron` or the project. A hidden file alone would still be plaintext.
Opening/loading the package does not read or prompt for a key; launching does.
If the OS store is unavailable or locked, the app gives a recovery message and
does not silently save a plaintext file. Linux may need `libsecret-1-dev`, a
running Secret Service and an unlocked login keyring. An existing environment
variable remains supported on systems without an OS credential store.

```r
RAssistant::set_api_key()     # Replace the saved key using hidden input
RAssistant::forget_api_key()  # Remove only RAssistant's saved key
```

An existing `OPENAI_API_KEY` takes precedence over a saved key. Saving a key does
not validate it against the network; authentication is checked on the first Send.
Close and relaunch the assistant after changing a key to use the new value.

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
The Console reports the old and new versions; the toolbar confirms the installed
version when reopened.

Between automatic checks, results are cached for 30 minutes. A new launch checks
immediately. Offline/failed checks do not prevent chat or hide an update already
found. **Help > Check for updates** remains available and checks immediately,
bypassing the cache.
The check does not call OpenAI or transmit the key, Source text or chat history.
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
separate conversation and keeps the previous one. **History** lists saved chats
and offers **Rename**, **Delete** and **Restore**. Delete asks for confirmation.
The most recently used chat reopens on the next `RAssistant::launch()`, including
after restarting RStudio or updating the package. Opening RStudio alone does not
automatically launch the assistant. Previously closed pre-0.2.0 chats cannot be recovered.

Text, code, copied output, assignment context, images and unsent drafts are saved.
Run approvals are never restored; any execution needs a fresh request and Yes.
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
Restoring and searching local history do not call OpenAI. Sending retained image
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

## Course knowledge

From 0.3.0, **Knowledge** stores course material for the current RStudio
project: lecture PDFs, notes (`.txt`, `.md`) and R code examples (`.R`, `.Rmd`,
`.qmd`), up to 50 MB each. An optional **Custom instructions** box holds anything
the assistant should always follow for the course, such as preferred packages,
functions or coding conventions.

For every request, RAssistant matches your message, the pasted task and the
functions in the open script against this material on your computer. Only the
few most relevant sections (about 6,000 characters at most) are added, with
their file and page or line numbers; unrelated requests add none. No extra API
call is made for searching. The assistant is instructed to prefer the methods,
packages and functions taught in that material over generic alternatives and to
name the source it used. Answers that received course material list it under
**Course material**.

Knowledge is stored per project in `tools::R_user_dir("RAssistant", "data")`,
separately from chat History and the package installation, so it survives
RStudio restarts and package updates. Only extracted text is kept, not the
original files. Re-adding a file with the same name replaces the earlier
version; deleting a chat does not affect knowledge, and removing a document does
not affect chats.

Adding PDFs needs the pdftools package. Scanned PDFs without a text layer need
OCR first.

```r
install.packages("pdftools")
```

## Attach the current RStudio plot

Select a graph in RStudio's **Plots** pane, then click **Attach current plot**
beside Upload in RAssistant. The selected plot is exported as a PNG and appears
in the attachment previews. Type your question and press **Send** to submit it.
There is no manual Export or clipboard step, and Source is not rerun.

Each click captures the plot currently selected, including an older graph
selected with the Plots history arrows. This uses RStudio's `savePlotAsImage()`
API, not `ggplot2::last_plot()`. The button is in the chat; RStudio's own Plots
toolbar is not modified. The existing three-image and 4 MB per-image limits apply.
Attaching alone makes no AI request; Remove all discards staged plots.

## Console context

In the package app, enable **Console context** below the message box, wait for
the status to say **On**, then run R code normally in RStudio. Ask about the result
or error and press **Send**: the request includes up to 200 recent lines and
12,000 characters of captured R output and associated commands. Copy/Paste is not required.

Capture starts off each time. To prepare error and warning logging, the app runs
its own fixed helper in the RStudio Console without switching focus. Finish or
clear a command already being typed there before enabling capture. The status
shows when logging is ready; activation does not call OpenAI.

Normal output remains visible, and ordinary uncaught errors, warnings and messages
are recorded without suppressing them. The temporary capture is cleared when you
turn the option off, change chats, disconnect, Close or update. Raw captured output
is not added to saved chat snapshots, though answers based on it remain in chat.
The active API key is redacted from captured context before it is sent.

Completed commands are recorded after their output with task boundaries. Errors
include their condition call or stack entry when available; an enclosing call
may not show the whole command. Commands that print no text, including plotting
and assignments, are recorded with a no-text-output label. Comments are not recorded.
The assistant uses this evidence rather than guessing the originating command
from the current Source. A plotting command does not include the plot's pixels;
earlier uploaded images remain historical context, not evidence of the latest plot.

Only output produced after enabling is available. This is a bounded excerpt,
not proof the current Source was executed. Plots, external-process output,
messages intercepted by other handlers and output hidden by other sinks may be
absent. If another package has temporarily put its own output sink on top of
ours, cleanup waits for that sink to close; updating waits for cleanup.
Large output is trimmed between commands and while the app polls capture status;
a long-running command can temporarily grow the spool before control returns.

## Use and limits

- **Upload** accepts up to 3 PNG/JPEG/WebP images, 4 MB each. Screenshots use
  content-neutral reading guidance; include a message to request a specific task.
- Enable **Console context** to include new R output automatically, or paste
  Console output into chat manually. Source remains read-only.
- Recent dialogue, retrieved earlier excerpts, a remembered assignment and retained
  images support multipart work. Use History to return to another conversation.
- Script validation requires **Yes**, executes the entire current R/Rmd file in
  a child process, and may affect files. Only approved output/plots are shared.
- Current Source text, detected CSV headers, opted-in Console context and uploaded images are sent to
  OpenAI when you click **Send**. An API account/key and internet access are needed.
- Standalone `source("AISource.R")` remains supported with `OPENAI_API_KEY` set.

## Development

Keep editing **`AISource.R`**. The package wrapper does not replace the app.
After an app change, regenerate the shipped snapshot and commit both files:

```sh
Rscript tools/sync-app.R
Rscript tools/sync-app.R --check
Rscript tests/regression.R
Rscript tests/source-context.R
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
