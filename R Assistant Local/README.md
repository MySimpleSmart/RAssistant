# R Assistant Local

Run this folder directly in RStudio on Windows or macOS.

1. On the first computer setup, open **Setup.R** in RStudio and click **Source**.
   It installs missing CRAN dependencies; Windows uses binary packages.
   When upgrading from the image-only local version, run **Setup.R** once again
   to add the PDF reader, then Source **AISource.R**.
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

## Upload documents and images

The same **Upload** picker accepts up to **3 files per message**, including a
mixture of images and documents:

- Images: PNG, JPEG, WebP, up to **4 MB each**; screenshot paste still works.
- Documents: PDF, TXT, Markdown (`.md`, `.markdown`), CSV, TSV, R (`.R`),
  R Markdown, Quarto, JSON and log files, up to **20 MB each**.

PDF text is extracted locally with page labels. PDFs are limited to **200 pages**
and all document text to **2,000,000 characters per file**. Scanned/image-only PDFs
need OCR first, or screenshots of the relevant pages; the reader does not claim
to have read missing text, images or diagrams. Mixed PDFs identify pages without
extracted text. Locked, corrupt and unreadable files report a clear upload error.

Text supports UTF-8, including Mongolian, and UTF-16 with a byte-order mark.
CSV/TSV preserve the original headers, identifiers and numeric spellings. Tables
are limited to 200 columns; the parsed row overview is marked as a preview for
more than 1,000 rows. Quoted commas and multiline cells are supported.

Expand a file card to inspect its text preview. Files are labelled **File 1**,
**File 2**, etc. Ask follow-up questions by filename or number, for example,
"Explain page 2 of exercise.pdf" or "Compare files 1 and 2". Up to three earlier
files can be reused alongside new uploads. Requests include at most **20,000
characters of document content**; larger documents supply locally selected
excerpts and explicitly identify partial coverage. Retrieval makes no extra API
request. Uploading reads locally; extracted text is sent when you press **Send**.

Uploaded source code is read as text, never automatically executed. CSV uploads
are not imported into the R workspace. Original paths are not exposed by a file
picker, so the assistant cannot assume an uploaded file exists in your project.
Temporary raw files are removed after a successful send; failed sends keep the
draft available for retry. Extracted document context stays in the current chat.

While the app is open, image follow-up requests reuse the original pixels from the most
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
source("tests/document-upload.R", encoding = "UTF-8")
```
