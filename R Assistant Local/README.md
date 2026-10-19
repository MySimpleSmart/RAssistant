# R Assistant Local

Run this folder directly in RStudio on Windows or macOS.

1. On the first computer setup, open **Setup.R** in RStudio and click **Source**.
   It installs missing CRAN dependencies; Windows uses binary packages.
2. Open **AISource.R** and click **Source**.
3. If no key is configured, enter your OpenAI API key in the hidden RStudio
   password dialog. The assistant opens after the key is saved successfully.

For later launches, open **AISource.R** and click **Source** again.
The same files work from any local folder; `Setup.R` does not install an
RAssistant package or use GitHub. The assistant still needs internet access to
send requests to OpenAI.

You can also launch from the RStudio Console by selecting **AISource.R**:

```r
source(file.choose(), encoding = "UTF-8")
```

The key is stored under service `RAssistant/OpenAI`, account `default`, in Windows
Credential Manager or macOS Keychain. It survives RStudio restarts and replacing
this folder. An existing `OPENAI_API_KEY` environment variable takes precedence.
Cancel stops startup without saving a key. Key storage does not validate API
access; authentication is checked when sending the first message.

To replace a saved key, close the assistant, run the following command, then
Source **AISource.R** again to enter the new key:

```r
keyring::key_delete("RAssistant/OpenAI", username = "default")
```

If `OPENAI_API_KEY` is configured, update that setting instead. Store errors are
reported before launch. No key is written to this folder, `.Renviron`, `.Rhistory`
or the process environment by the assistant.

Existing chat, image uploads, Copy and explicitly approved R/Rmd validation work
as before. This local copy keeps conversation context while the app is open;
closing it ends that conversation. Console output is supplied by pasting it.

Tests (synthetic credentials and model responses), run from this folder:

```r
source("tests/startup.R", encoding = "UTF-8")
source("tests/windows-paths.R", encoding = "UTF-8")
```
