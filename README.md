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

Click **Close** in the assistant, then run:

```r
RAssistant::update_assistant()
```

Restart R with **Session > Restart R**, then run `RAssistant::launch()` again.
The updater installs from this repository's `main` branch into the **same R
library containing the loaded RAssistant package**, rather than whichever
library happens to be first. It uses a clean R process and normal installation
locking. It does not read, replace or delete the saved key. No updates run
automatically. You can choose a tag/commit with `update_assistant(ref = "...")`.

Inspect the installed package location and version:

```r
find.package("RAssistant")
packageVersion("RAssistant")
```

An R upgrade may change the default package library; reinstall the package for
that R version. The OS-stored key survives. Moving to another computer or OS user
requires its own key setup. Updating this GitHub repository does not immediately
modify other users' installations; they run the update command.

## Use and limits

- **Upload** accepts up to 3 PNG/JPEG/WebP images, 4 MB each. Screenshots use
  content-neutral reading guidance; include a message to request a specific task.
- Paste Console output into chat manually. The assistant does not read live
  Console output or edit the Source document.
- Recent conversation context and a remembered assignment support multipart work.
  Context is bounded; earlier image pixels are not resent.
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
```

`inst/app/AISource.R` is a generated, byte-identical snapshot. CI fails if it is
out of sync. Increment `Version` in `DESCRIPTION` for each release. Package tests
exercise credential and update behavior using fake stores/installers, without
reading real secrets. The optional browser suite is `Rscript tests/browser.R`;
the opt-in live vision test is documented in [REVIEW.md](REVIEW.md).

No open-source license has been selected; copyright is reserved in `LICENSE`.
