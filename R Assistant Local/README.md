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

The assistant chooses R functions and packages to suit the task, the user's
requirements and the existing code. It can use base R/stats, ggplot2, GGally,
tidyverse or specialist packages as appropriate; no particular library is forced.
Additional analysis packages are separate from the dependencies in Setup.R.

While the app is open, follow-up requests reuse the original pixels from the most
recent successful image turn (up to 3 earlier images). New uploads can be compared
with those images. Uploads are labelled **Image 1**, **Image 2**, etc.; refer to an
older image by number, for example, "Compare images 1 and 2." A request contains
at most 3 new uploads plus 3 retained images. Missing image references are identified
rather than replaced with an unrelated image. Retained pixels count toward API
input usage; earlier images are selected locally without a separate API call.

Changing Source tabs keeps the conversation and assignment. Only the freshly read
Source is used as the current code. Use **New** to start a separate conversation
and clear retained images. This local copy keeps history only while the app is
open; closing it ends that conversation. Console output is supplied by pasting it.
Code Copy and explicitly approved R/Rmd validation remain available.

Tests (synthetic credentials and model responses), run from this folder:

```r
source("tests/startup.R", encoding = "UTF-8")
source("tests/windows-paths.R", encoding = "UTF-8")
source("tests/conversation-context.R", encoding = "UTF-8")
```
