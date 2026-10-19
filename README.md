# RAssistant

A lightweight AI assistant for RStudio, powered by OpenAI. It opens the existing
`AISource.R` chat in the Viewer, with image uploads, code Copy buttons, contextual
R help and explicitly approved R/Rmd validation.

## Install

Run in the **RStudio Console** with R 4.1 or newer:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("MySimpleSmart/RAssistant", upgrade = "never")
RAssistant::launch()
```

Use your personal R library if R offers to create one. RAssistant is installed in
an R package library, separately from the RStudio application. The RStudio Addins
menu also has an **RAssistant** entry after installation.
The toolbar shows the installed version. **Help** contains launch, API-key and
update instructions with Copy buttons, so you do not need the old local script
to find the commands.

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
From 0.2.2, it resolves the current main commit over public Git HTTP and reads
that commit's version file, avoiding stale branch-file caches and GitHub's
IP-shared REST API quota. No GitHub token or Git executable is needed for checks.
When a newer version is available, the chat shows **Update now**. Click it,
review the notice, then confirm. This saves and closes the current chat, installs
into the same R library and prints progress in the RStudio Console.
After successful installation, run `RAssistant::launch()` to restore the chat.
The Console reports the old and new versions; the toolbar confirms the installed
version when reopened.

Checks are cached for one hour within the current R session. Offline/failed
checks do not prevent chat. **Help > Check for updates** retries immediately.
The check does not call OpenAI or transmit the key, Source text or chat history.
To disable automatic discovery, set `options(RAssistant.check_updates = FALSE)`;
manual checking and Console updates still work.

For a manual update, or to get this feature from an older installation:

Click **Close** in the assistant, then run:

```r
RAssistant::update_assistant()
```

Run `RAssistant::launch()` again. It reloads RAssistant's updated code while
preserving your R workspace and loaded dependencies. A restart is only needed
if reloading fails, such as when another loaded package imports RAssistant.
The updater installs from this repository's `main` branch into the **same R
library containing the loaded RAssistant package**, rather than whichever
library happens to be first. It uses a clean R process and normal installation
locking. It does not read, replace or delete the saved key. No updates run
automatically. You can choose a tag/commit with `update_assistant(ref = "...")`.
An unchanged installed version and GitHub commit reports **already up to date**
and does not require reloading.

When upgrading from 0.1.3 or earlier, the already-loaded old launcher still has
its restart guard. After the update finishes and the assistant is closed, reload
it once without clearing your workspace:

```r
unloadNamespace("RAssistant")
RAssistant::launch()
```

Subsequent updates through `update_assistant()` reload on the next launch.

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
you do not need to upload those files again. A new upload sends the new batch.
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

## Use and limits

- **Upload** accepts up to 3 PNG/JPEG/WebP images, 4 MB each. Screenshots use
  content-neutral reading guidance; include a message to request a specific task.
- Paste Console output into chat manually. The assistant does not read live
  Console output or edit the Source document.
- Recent dialogue, retrieved earlier excerpts, a remembered assignment and retained
  images support multipart work. Use History to return to another conversation.
- Script validation requires **Yes**, executes the entire current R/Rmd file in
  a child process, and may affect files. Only approved output/plots are shared.
- Current Source text, detected CSV headers and uploaded images are sent to
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
