# AISource.R review — updated 19 October 2026

The existing single-file Shiny application was repaired incrementally. Its launcher, model, editor access, validation approval, R/Rmd execution, plot gallery, academic comments, New, and shutdown controls remain in place. The standalone app retains its dependencies; the optional package wrapper adds credential-store and installation tooling. The picker is labelled **Upload**, the shutdown button **Close**, and upload messages refer to images generally; generated R plots retain their plot labels.

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
| Follow-up context omitted academic comments and verified execution output. | Bounded history includes both, labels prior image attachments, and excludes failed-request artifacts. Assignment context is kept separately without repeating identical turns. |
| A short follow-up such as Exercise 1(a) could overwrite a previously supplied multipart assignment. | A complete brief stays available across part references and long follow-ups. A fuller brief can replace an initial short reference or generic context; New or switching Source documents clears it. |
| Repeated CSV preflight reads performed unnecessary I/O. | A session cache uses references, working directories, file sizes, and modification times; file changes invalidate it. |
| Windows drive/network paths were treated as relative paths, and escaped separators were not decoded. | CSV lookup and caching recognize Windows absolute paths; captured R string literals are parsed without evaluation. Invalid R strings are ignored. Cross-platform fixtures quote paths correctly. |
| Any plot-analysis failure triggered a second API request, including quota/authentication failures. | Numeric fallback occurs only for image-specific errors. Shared request handling supplies empty-response checks and default per-attempt timeout/retry limits, respecting existing ellmer options. |
| Validation could remain labelled Running after completion, and log truncation could drop the final error. | Status is finalized; analysis excerpts retain the beginning and end of logs. Temporary plot cleanup also covers preparation failures. |
| Negated requests such as “without running” opened the execution flow. | Static-review and common negation phrases bypass execution approval. Explicit run requests retain the Yes/No flow and source-change guard. |

The UI keeps its existing compact RStudio-style layout. A small status label indicates uploads, work, disconnection, and pending approval.

## Verification

Run from this directory:

```sh
Rscript tests/regression.R
Rscript tests/source-context.R
Rscript tests/assignment-context.R
Rscript tests/image-reading.R
Rscript tests/browser.R
```

- **50 R/Shiny checks:** helper behavior, Windows/escaped CSV paths and caching, upload lifecycle, retries, replay suppression, context, document isolation, approval/source-change guards, real child-process R/Rmd execution, plot capture, execution errors, timeouts, cleanup, image-analysis fallback, credential-based ellmer construction without deprecated arguments or a network request, and persistent update-confirmation locking.
- **28 Source-context checks:** Source lookup errors/NULL/invalid results, fresh document-ID fallback, document switching, Console rejection, image-only sends/retries without Source access, generic image prompts, Console-screenshot evidence, conditional statistical comments, no initial document, oversized editor omission, CSV preflight exclusion, loading/file cleanup, and retained validation/size guards.
- **13 assignment-context checks:** a synthetic multipart brief survives seven part requests, long follow-ups, and the recent-history limit; a later full brief can replace a short reference; New and Source document changes clear it. The suite also passed using a locally supplied assignment, with a mocked API. User documents and data are not included in the repository.
- **15 image-reading checks:** inspect outgoing image-only and user-directed payloads, absence of unsolicited statistical templates, unreadable-text guidance, optional background, unchanged image bytes/order, explicit/negated/mentioned academic-comment requests, and retained text-only/verified-run behavior.
- **39 Chrome checks:** the existing UI/server with a fake API, exact code/comment/Help-command copying, narrow Viewer layout, update/version notices, manual checking, cancellation and duplicate update confirmation, draft preservation, clipboard fallback, focus/selection, native/pasted/image-only uploads, PNG/JPEG/WebP previews, limits, failed requests, validation controls, completed execution, preserved details/scroll, late-reader cancellation, simulated Source-access failure, and startup/runtime JavaScript errors.
- R parsing and extracted JavaScript syntax checks passed.

The browser suite requires the optional `chromote` test package and Chrome. Set `CHROMOTE_CHROME` to a Chrome/headless-shell executable if automatic detection is unsuitable.

Fresh document fallback uses the documented [rstudioapi document-ID argument](https://pkgs.rstudio.com/rstudioapi/reference/rstudio-editors.html); if that lookup also fails, image analysis proceeds with explicit unavailable-source context.

The regression suites above use temporary fixtures and mocked API responses. A separate live smoke test on 19 October 2026 used one synthetic text/table/code screenshot with the configured model and no accompanying question. All six critical strings were reproduced exactly; the reviewed response contained only the faithful readout, with no unsolicited analysis or academic-comment template. No project files or actual Console data were used in that request. This is a narrow smoke test, not an accuracy guarantee for other images or blurry text. The embedded RStudio Viewer remains untested. Request execution remains synchronous; the browser handles its loading state explicitly. The original explicit approval requirement for running user code remains in effect.

The opt-in live test can be repeated with `AISOURCE_LIVE_VISION=1 Rscript tests/live-image-reading.R`. It makes one live API attempt and requires configured credentials; running it without that flag skips the network call.

## Current capacity

- Uploads: up to 3 PNG/JPEG/WebP images per message, at most 4 MB each. Other file formats are not supported by this image picker.
- Text: at most 50,000 characters per message. Current Source code has a separate 50,000-character limit; oversized code is omitted for image requests.
- Model context: up to 12 recent eligible messages, within 12,000 characters overall and 2,800 per message, plus up to 8,000 characters of assignment context. Earlier image pixels are not resent; upload those images again for a fresh comparison.
- Execution: approved R/Rmd runs have a 45-second limit. API requests default to 120 seconds per attempt and at most 2 attempts, respecting existing ellmer options.
- Long sessions: displayed messages and base64 image previews remain in session memory; the entire message output is replaced on updates. Rendering is cached, but there is no pagination or bound on displayed history. Very long image-heavy sessions may consume substantial memory and become slower. New clears the session's conversation and previews.

These are code-enforced limits, not a measured guarantee for large workloads. Image-memory management, incremental history rendering, and older-context retention would need separate work before claiming large-session capacity. This label update does not change the limits or add new upload formats.

For multipart work, paste the complete assignment text first and keep one R/Rmd Source document open while asking for individual parts. The image picker does not accept PDFs, and automatic validation runs the entire current R/Rmd file, not only the requested part or selected chunk. RStudio can run individual R Markdown chunks; their Console output and images can then be supplied to chat. Automatic Quarto (.qmd) validation is not implemented.

## Package distribution

RAssistant wraps the same application rather than replacing it. The root
`AISource.R` remains the maintained source; `tools/sync-app.R` creates the exact
snapshot shipped under `inst/app`, and CI rejects divergence. App-boundary
changes include an explicit API-key argument, optional package/version callbacks,
and an internal load-without-launch option, restored immediately by the package
loader. Standalone sourcing remains supported. Existing upload/rendering/context/validation tests and all 39 browser
checks passed after these changes.

`launch()` obtains an existing environment key or a key saved under the stable
`RAssistant/OpenAI` service in the OS credential store. First-launch setup uses
hidden password input. Secrets are passed directly to ellmer, without writing
package/project/startup files or copying them into environment variables.
Cancellation preserves the old key; store-access failures are distinguished from
a genuinely absent key. Nonpersistent/unsupported keyring backends are rejected.
Loading the package alone does not access the credential store.

`update_assistant()` runs the GitHub installer in a clean R process and targets
the library containing the loaded package. It preserves normal install locking,
blocks Console updates while the app runs, and requires restarting R after a
changed installation. An unchanged version/commit reports up to date. The
updater does not access credentials. R-version library changes still require a
package reinstall, while the OS-stored credential remains separate.

Local `R CMD build`, installation into an isolated temporary library, and
`R CMD check --no-manual --no-vignettes` passed with **Status: OK**. An additional
**29 package checks** cover credential precedence/reuse, hidden setup cancellation,
invalid input, unavailable stores, replacement/removal, environment isolation,
safe script loading, duplicate launch, update location including spaces, running
app/restart guards and installation failures. These use fake credential stores
and installers; native password dialogs and actual OS credential-store writes
have not been exercised. The source archive was checked for accidental inclusion
of credentials, backups, user documents and opt-in live API tests.

After publication, a real `RAssistant::update_assistant()` call downloaded the
GitHub package and successfully replaced the installation in the same isolated
temporary library. A fresh R process verified version 0.1.0, GitHub commit metadata,
the bundled launcher and its byte-identical application snapshot. No real
credential-store operations or live model requests were involved in this test.

## Version 0.1.2 update notice and Help

The toolbar shows the installed version and a small Help button. Help includes
copyable launch, key-replacement and update commands, hidden-input guidance,
environment-key precedence and restart instructions. A separate optional banner
appears only for a newer package version. Update now explains that the current
conversation ends, then queues one update after stopping the app. Progress and
old/new versions are printed in the Console; no-change updates avoid an unnecessary
restart. Saved credentials remain separate.

Discovery downloads only the public DESCRIPTION file in a background R process,
with a five-second download timeout, a ten-second process deadline and a one-hour
per-session cache. The reader retains its helper environment; the worker clears
OPENAI_API_KEY after startup files are read. No project or conversation data is
passed. Offline checks leave chat available. Help can force a manual check;
automatic checks can be disabled with options(RAssistant.check_updates = FALSE).

An additional **21 update checks** cover background work, cache expiry, manual
refresh, newer/equal/older versions, invalid metadata, offline errors, stuck worker
cleanup, opt-out, duplicate scheduling, stop-before-install, failed-install recovery,
no-change updates, and a real subprocess retaining its helpers without inheriting
the API key. No live network or real installs occur in these package tests.
Together with the existing suites and browser checks, 195 checks passed locally.

A separate live public-metadata smoke test read GitHub successfully; starting the
background check returned in about 0.08 seconds. A real Shiny app handle was also
stopped before a fake installer callback ran. The narrow Help dialog was inspected
in Chrome. These checks did not call OpenAI, access the real key store, or run a
real installation from the browser. Native RStudio Viewer interaction remains
untested. Local package validation finished with Status: OK.

## Version 0.1.3 smoke update

Removed the composer shortcut hint, including unused hint styles, while keeping
Send aligned to the right and retaining the existing keyboard and paste handlers.
The 39 browser checks passed against the updated UI.

The live update test exposed stale raw-branch metadata: a 0.1.2 client saw 0.1.2
after 0.1.3 was published, while the exact Git commit and GitHub contents API both
reported 0.1.3. Discovery now requests raw DESCRIPTION content from the public
contents API with cache revalidation. Existing session caching, timeouts, process
isolation, no-key handling and version validation remain in place. Older clients
using the raw URL may need a Console update once to obtain this fix.

## Version 0.1.4 relaunch after updates

The next `launch()` after a managed update reloads RAssistant's namespace from
the same library and delegates to its new launcher. The R workspace and dependency
namespaces stay loaded. A previous `library(RAssistant)` attachment is restored;
captured older launch functions delegate without unloading a newly running app.
Launching during a queued update is blocked, and an app still running is reused.
The updater resolves its own lazy bindings before replacing the installed code
database and stops its background metadata worker. Reload failure or a package
importing RAssistant retains an explicit restart recovery message.

Help, the confirmation dialog and successful-update messages now instruct users
to run `RAssistant::launch()`. An already-loaded 0.1.3 or earlier launcher still
needs one explicit `unloadNamespace("RAssistant")` after upgrading to obtain the
new behavior; README documents this without requiring the workspace to be cleared.

All **218 checks** passed locally: 50 core, 28 Source-context, 13 assignment,
15 image-reading, 39 Chrome, 33 package, 21 discovery/lifecycle and 19 namespace
reload checks. The added namespace checks use actual unloading/loading in isolated
R processes with disposable copies of the installed package. They cover both
namespace-only and attached use, a different library appearing first, fresh helper
code/state, old-worker cleanup, workspace/dependency preservation and stale
function references. Credentials and the app are synthetic; native RStudio GUI
launching and real credential-store writes remain untested.

Local `R CMD check --no-manual --no-vignettes` finished with **Status: OK**.
A separate smoke test used the new updater with a baseline fixture labelled
0.1.3, performed a real local archive installation of 0.1.4 in a child process,
and launched through the reloaded namespace in the same parent R session. It
verified the new loaded version, removal of a baseline-only helper, the same
library, retained attachment, workspace and Shiny namespace. No OpenAI calls or
real credential-store operations occurred.
