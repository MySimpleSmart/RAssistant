# AISource.R review — updated 19 October 2026

The existing single-file Shiny application was repaired incrementally. Its launcher, model, editor access, validation approval, R/Rmd execution, plot gallery, academic comments, New, and shutdown controls remain in place. The standalone first-launch flow now uses keyring for native OS credential storage; an existing environment key remains supported. The picker is labelled **Upload**, the shutdown button **Close**, and upload messages refer to images generally; generated R plots retain their plot labels.

| Audit finding | Targeted change |
| --- | --- |
| Shiny rejects custom message handlers with zero arguments; the original reset handler could interrupt initialization. | Every custom handler accepts its required message argument. Browser tests capture errors from page startup. |
| Native file inputs also received Shiny's default upload binding. | The custom picker explicitly opts out of that binding. |
| Reset and timeout could leave late FileReader callbacks or server uploads active. | Uploads use request identifiers, cancellation acknowledgements, and a reset generation. Readers are aborted and stale callbacks/uploads are ignored. |
| MIME checks rejected files with missing browser MIME metadata, while server decoding accepted mismatched MIME/signature combinations. | Extension fallback, browser image decoding, base64 validation, and signature/type matching now cover PNG, JPEG, and WebP. |
| Screenshots inherited a large R/statistics prompt, predefined answer formats, and a fabricated image-analysis request when the user supplied no text. | Image requests now use separate, content-neutral reading guidance. Actual user text leads the payload without an invented question; editor/assignment/history are optional background. Guidance preserves readable text, numbers, symbols and layout, distinguishes inference from visible evidence, and requires uncertainty about missing or unreadable content. Academic-card formatting is added only for an explicit positive request, not a mere mention or negation. Text-only R help and approved execution analysis retain their existing paths. |
| Failed API calls discarded the draft and attached files. | Drafts and verified attachments remain available for retry; successful requests and session closure clean up temporary files. |
| Sending an image was blocked by an unavailable Source context or an oversized open script. | Fresh Source lookup retries the previous document ID. Image chat continues without Source context when necessary, explicitly omits oversized code, and skips CSV preflight in those cases. Startup can also proceed without a Source document. Cached editor text and Console contents are never used as current Source evidence; execution still requires a fresh matching document. |
| Repeated clicks/Enter events could queue requests during synchronous API work. | Immediate browser locks, matching completion messages, and server replay detection prevent duplicate submissions. |
| Copy fallback could fail or change the composer's selection. | Clipboard errors fall back to legacy copy, preserving exact displayed code and restoring focus/selection. |
| Math delimiter matching could consume code; Markdown links could retain executable URL schemes. | Code is protected before math parsing, and rendered links/images restrict URL schemes. Existing HTML escaping is retained. |
| Every output update reparsed all messages and collapsed execution details. | Markdown is cached when messages are added; expanded details and reading position survive updates. |
| Follow-up context omitted academic comments, verified execution output and original image pixels. | Bounded history includes comments/logs and retrieves relevant older excerpts locally. Requests reattach up to 3 original earlier images, including alongside new uploads. Image numbers support explicit earlier-image references. Failed turns are excluded; missing/corrupt images are identified; temporary restored files are always removed. |
| A short follow-up such as Exercise 1(a), or changing Source tabs, discarded the ongoing assignment. | A complete brief stays available across part references, long follow-ups and Source focus changes. Historical Source names are labelled; only freshly read code is current evidence. New explicitly clears the conversation. |
| Generated code had no shared rule for selecting appropriate R methods/packages. | Text, requested screenshot analysis and verified execution reports share task-based guidance. User constraints and suitable existing approaches take priority; no library is forced, and additional installation is shown separately. |
| Repeated CSV preflight reads performed unnecessary I/O. | A session cache uses references, working directories, file sizes, and modification times; file changes invalidate it. |
| Any plot-analysis failure triggered a second API request, including quota/authentication failures. | Numeric fallback occurs only for image-specific errors. Shared request handling supplies empty-response checks and default per-attempt timeout/retry limits, respecting existing ellmer options. |
| Validation could remain labelled Running after completion, and log truncation could drop the final error. | Status is finalized; analysis excerpts retain the beginning and end of logs. Temporary plot cleanup also covers preparation failures. |
| Negated requests such as “without running” opened the execution flow. | Static-review and common negation phrases bypass execution approval. Explicit run requests retain the Yes/No flow and source-change guard. |

The UI keeps its existing compact RStudio-style layout. A small status label indicates uploads, work, disconnection, and pending approval.

## Verification

Run from this directory:

```sh
Rscript tests/startup.R
Rscript tests/windows-paths.R
Rscript tests/regression.R
Rscript tests/source-context.R
Rscript tests/assignment-context.R
Rscript tests/image-reading.R
Rscript tests/conversation-context.R
Rscript tests/browser.R
```

- **45 R/Shiny checks:** helper behavior, CSV caching, upload lifecycle, retries, replay suppression, context, Source switching, approval/source-change guards, real child-process R/Rmd execution, plot capture, execution errors, timeouts, cleanup, and image-analysis fallback.
- **30 Source-context checks:** Source lookup errors/NULL/invalid results, fresh document-ID fallback, document switching, Console rejection, image-only sends/retries without Source access, generic image prompts, Console-screenshot evidence, conditional statistical comments, no initial document, oversized editor omission, retained-image follow-ups, CSV preflight exclusion, loading/file cleanup, and retained validation/size guards. An oversized omitted script can never become executable validation input.
- **13 assignment-context checks:** a multipart brief survives seven part requests, long follow-ups, the recent-history limit and Source changes; a later full brief can replace a short reference; New clears it. The original suite also passed using text extracted locally from Week_8_2026_SGTA.pdf, with a mocked API. Assignment answers and numeric results were not validated against its CSV data.
- **15 image-reading checks:** inspect outgoing image-only and user-directed payloads, absence of unsolicited statistical templates, unreadable-text guidance, optional background, unchanged image bytes/order, explicit/negated/mentioned academic-comment requests, and retained text-only/verified-run behavior.
- **17 conversation-context checks:** inspect outgoing original pixels and text across follow-ups, new-image comparisons, Source loss/switches, explicit/plural image references, missing/corrupt images, failed/removed uploads, New, cleanup, bounded history and relevant earlier methods. These tests use a fake API.
- **28 Chrome checks:** the existing UI/server with a fake API, exact code/comment copying, clipboard fallback, focus/selection, native/pasted/image-only uploads, PNG/JPEG/WebP previews, limits, failed requests, validation controls, completed execution, preserved details/scroll, late-reader cancellation, simulated Source-access failure, and startup/runtime JavaScript errors.
- R parsing and extracted JavaScript syntax checks passed.

The browser suite requires the optional `chromote` test package and Chrome. Set `CHROMOTE_CHROME` to a Chrome/headless-shell executable if automatic detection is unsuitable.

Fresh document fallback uses the documented [rstudioapi document-ID argument](https://pkgs.rstudio.com/rstudioapi/reference/rstudio-editors.html); if that lookup also fails, image analysis proceeds with explicit unavailable-source context.

The regression suites above use temporary fixtures and mocked API responses. A separate live smoke test on 19 October 2026 used one synthetic text/table/code screenshot with the configured model and no accompanying question. All six critical strings were reproduced exactly; the reviewed response contained only the faithful readout, with no unsolicited analysis or academic-comment template. No project files or actual Console data were used in that request. This is a narrow smoke test, not an accuracy guarantee for other images or blurry text. The embedded RStudio Viewer remains untested. Request execution remains synchronous; the browser handles its loading state explicitly. The original explicit approval requirement for running user code remains in effect.

The opt-in live test can be repeated with `AISOURCE_LIVE_VISION=1 Rscript tests/live-image-reading.R`. It makes one live API attempt and requires configured credentials; running it without that flag skips the network call.

## Current capacity

- Uploads: up to 3 PNG/JPEG/WebP images per message, at most 4 MB each. Other file formats are not supported by this image picker.
- Text: at most 50,000 characters per message. Current Source code has a separate 50,000-character limit; oversized code is omitted for image requests.
- Model context: up to 32 recent eligible messages plus selected older relevant excerpts, within 32,000 characters overall, plus up to 8,000 characters of assignment context. Up to 3 original earlier images are resent with each request; explicit image numbers select an older image instead of the latest image turn. A request includes at most 3 new uploads plus 3 earlier images. Selection uses no extra API call; reattached pixels consume input usage.
- Execution: approved R/Rmd runs have a 45-second limit. API requests default to 120 seconds per attempt and at most 2 attempts, respecting existing ellmer options.
- Long sessions: displayed messages and base64 image previews remain in session memory; the entire message output is replaced on updates. Rendering is cached, but there is no pagination or bound on displayed history. Very long image-heavy sessions may consume substantial memory and become slower. New clears the session's conversation and previews.

These are code-enforced limits, not a measured guarantee for large workloads. Image-memory management and incremental history rendering would need separate work before claiming large-session capacity. History is session-only: closing the app ends it; this change does not add persistent multi-chat storage.

For multipart work such as Week 8 SGTA, paste the complete assignment text or upload its screenshots and ask for individual parts in the same conversation. The image picker does not accept PDFs, and automatic validation runs the entire current R/Rmd file, not only the requested part or selected chunk. RStudio can run individual R Markdown chunks; their Console output and images can then be supplied to chat. Automatic Quarto (.qmd) validation is not implemented.


## Standalone Windows startup and first-launch key

This folder runs by sourcing `AISource.R` in RStudio. `Setup.R` installs missing
CRAN dependencies as Windows binaries and checks their loaded versions. It does
not install the RAssistant package. Namespace-load errors retain their original
message and identify the dependency that needs repair.

If neither an environment key nor an OS-stored key exists, startup uses RStudio's
hidden password dialog, then saves the key in Windows Credential Manager. Later
launches read that same service/account. The key reaches ellmer through its
credentials callback; the local script does not write it into an environment
variable or a plaintext file. Cancellation and storage failures stop startup.
Native backend classes identify secure stores: Windows' display name is
`windows credential store`, rather than the factory identifier `wincred`.

Windows drive/UNC paths are recognized as absolute paths. CSV preflight parses
only captured R string literals, correctly decoding escaped separators, and
its header cache inspects the same real file. A regression fixture also now
escapes Windows temporary paths when constructing R code.

Local checks passed: 23 startup, 6 Windows-path, 45 core, 28 Source-context,
13 assignment-context, 15 image-reading and 28 Chrome checks (158 in total).
An additional three Windows-only checks successfully saved a synthetic key in
native Credential Manager, reused it in a fresh launcher and read it from a
new R process. Native password UI is replaced with a synthetic response during
automated startup tests. No live model requests or real user keys were used.


The startup baseline passed native Windows validation on R 4.6.1: **161 checks** across startup
(26, including the three native credential checks), Windows paths (6), core (45),
Source context (28), assignment context (13), image reading (15) and Chrome (28).
Dependencies were installed as CRAN binaries; no RAssistant package was installed
or loaded. This evidence predates the follow-up context changes described above.

Evidence: [Standalone Windows run](https://github.com/MySimpleSmart/RAssistant/actions/runs/39191922983).
