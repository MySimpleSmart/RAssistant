# RAssistant 0.19.0

- After an approved Source edit, offer Check changed code inline. Preview only changed complete R expressions or affected Rmd/Quarto chunks, with referenced Workspace input names, before a separate Run changed code approval.
- Run the approved scope in an isolated R process using bounded copies of referenced Workspace values and helpers. Do not submit unchanged expressions or other chunks; show local output and captured plots without an automatic AI request. Definition-only checks explicitly leave function bodies untested.
- Recheck Source and input requirements before execution, reject stale or replayed approvals, and preserve Cancel, history and the existing whole-script validation. Missing or unsupported inputs stop the scoped check instead of rerunning setup.

# RAssistant 0.18.0

- Follow literal source()/sys.source() imports and Rmd child references on Send with Workspace On, including nested project files and source(chdir=TRUE). Parse paths without executing project code; label dynamic, missing and excluded references.
- Map function calls to candidate definitions by file and global line number. Send fresh related code, using complete relevant function excerpts when a module exceeds the shared excerpt budget; keep duplicate definitions and runtime bindings unverified.
- Bound dependency traversal to three levels and six files, with cycle, file-size and shared excerpt limits. Keep the active unsaved editor authoritative and related files read-only; Workspace Off excludes the scan.

# RAssistant 0.17.1

- Put captured Console output before Source context in model requests. Direct Console questions prioritize observed output rather than a table of hypothetical script results.
- Show a compact Console receipt with delivery status and line count. Expand it to inspect the exact redacted text supplied for that turn; saved history retains counts without raw Console output.
- Answer direct Console questions with a short local message when capture is empty, Off or unavailable. Ask for code to be run with Console On instead of requesting manual copying or screenshots; pasted output and image questions keep their normal path.

# RAssistant 0.17.0

- Link the latest Console error to its recorded Source file and line, including Rmd/Quarto chunk names. Click the location in chat to open the failing code without running or editing it.
- When R provides no file reference, show a uniquely matching Source expression with an explicit Matching Source label. Ambiguous, changed or unavailable locations do not get a guessed line link.
- Send the local location evidence to the selected model for focused corrections. Keep navigation tied to the captured task and Source snapshot; never reuse an older error after a newer successful command.

# RAssistant 0.16.7

- Use Conversation-style full-width button lists for provider and model selection, with selected rows, model search and the existing automatic catalog lookup. Keep manual IDs available in a collapsed advanced control.
- Center the Send icon with its label and use a vertically balanced send symbol; preserve the fixed spinner slot while loading.
- Show complete code edit history comparisons, including unchanged file edges. Keep long code lines and their formatting inside a preview with horizontal and vertical scrolling; preserve list position and highlight the selected edit.

# RAssistant 0.16.6

- Make Provider, Model, API key and custom endpoint fields fill the settings page width. Model options use a bounded scrolling list.
- Identify providers with Using now, Key available, Add key or an actionable key-status label. Show the credential source without implying an environment-configured key is missing from Keychain.
- Add a send icon to Send and replace it with a spinner while working. Keep the icon slot and button width stable, with the existing button styles and reduced-motion support.

# RAssistant 0.16.5

- Keep the model button beside Send on one compact line. Show the model name with an ellipsis when needed; retain the complete provider and model in its tooltip and inside the picker.
- Label the picker Provider & model and its confirmation Apply. Show the current connection and indicate which providers already have a key.
- Preserve the selected provider, model and custom endpoint when opening API settings to add a key, instead of reverting to the active connection. Automatic model discovery stays enabled.

# RAssistant 0.16.4

- Automatically load the selected provider's model catalog when opening the picker, changing providers, or entering a key. Refresh models retries discovery without making a chat request.
- Show API-reported models separately from configured IDs absent from the latest catalog. Remove obsolete options before each lookup; keep the current model until the user explicitly selects Use model.
- Reject stale discovery responses after provider, endpoint or credential changes. Report discovery failures without presenting saved defaults as verified available models.

# RAssistant 0.16.3

- Show the active provider and exact model beside Send. Keep the label up to date after model or provider changes, with long IDs wrapping inside narrow Viewers.
- Click the model label to choose a model or provider in a compact picker. Reuse its saved key, load available model IDs on request, and keep the conversation and draft. API settings remains available for managing keys.
- Block model changes while a request or run approval is active. Read composer model metadata locally without credential lookups, and never use cached key fields from another settings form.

# RAssistant 0.16.2

- Use the same scrollable button list for conversations and Code edit history. Keep the list position when selecting another conversation.
- Render Chat history previews with the main chat renderer, including complete code blocks, Copy, tables, formulas, images and source receipts. Preview does not offer Source edits or run approvals.
- Show the latest 20 complete messages with a visible count. Load earlier messages adds 20 at a time until the whole conversation is available, without cutting message text.

# RAssistant 0.16.1

- Put Check updates and Help on separate full-width menu rows, keeping existing button styles.
- Open Chat history on a separate page like Code edit history, with a read-only preview, Rename, Delete, Open chat and Back to chat. Preserve the current conversation and draft while browsing.
- Use Knowledge Base documents only when selected for this chat. Clearing the last selection stops retrieval and new source labels; saved instructions and historical source receipts remain available.

# RAssistant 0.16.0

- Switch to a saved provider with a single Use button, including environment-configured OpenAI keys. Keep the chat and draft while switching; highlight the active provider and model.
- Select a model from a searchable picker with its concrete default already filled in. Load available models from the selected provider, or enter another model ID; loading a list does not send a chat or switch the active connection.
- Keep provider keys separate during model discovery and reject stale settings actions. Clarify when an environment key is available without a Keychain copy.

# RAssistant 0.15.1

- Use the same button component for Cancel and Close across dialogs; API settings now follows the existing button styles.
- Show the active provider, exact model and loaded key source. List configured models, saved Keychain keys and environment overrides without exposing key values or reading other saved secrets.
- Resolve provider-default model names locally and distinguish missing keys from unavailable Keychain status. Include keys saved for earlier custom endpoints. Fresh settings forms keep cached inputs from overwriting new model choices.

# RAssistant 0.15.0

- Choose OpenAI, Claude, Gemini, DeepSeek, Groq, Mistral, OpenRouter, Perplexity, Hugging Face, xAI, Together AI, Fireworks AI or Cerebras in API settings. Add other services through an HTTPS OpenAI-compatible endpoint and an exact model ID.
- Keep separate provider keys in the OS credential store and remember non-secret model settings locally. Switch providers without reopening the chat; existing OpenAI keys keep working. First launch can configure any provider without an OpenAI key.
- Keep credentials out of update checks, validation subprocesses and captured Console context. Keep failed messages and their attachments ready to retry after authentication or model errors.

# RAssistant 0.14.1

- Reopen on a fresh local port after updates so a delayed disconnect from the previous app cannot clear the new Viewer. Open the Viewer after server startup completes and confirm the page's Shiny connection.
- Retry an unconfirmed Viewer opening up to two times, then report how to reopen the still-running app. Cancel pending retries after connection, Close or a newer launch.

# RAssistant 0.14.0

- Propose fixes as exact Source line ranges, with Copy and Replace for each changed part. Preserve unrelated code, Markdown, chunk fences and options; reject stale, overlapping or out-of-selection proposals.
- Read fresh numbered Source and Markdown/chunk structure. Scan a bounded project file inventory and referenced saved R/Markdown files with Workspace On; include function signatures and relevant in-memory definitions without executing code.
- Isolate the latest captured Console task, retain multiline commands and include datasets referenced there. Keep the newest output when the capture exceeds the request limit. Use the same minimal fixes after approved validation.

# RAssistant 0.13.1

- Keep explanation examples separate from Source corrections. Open inline comparisons only for explicitly marked replacements or additions; a single example block no longer proposes deleting the current file.
- Preserve Copy for examples, and require an explicit replacement proposal before offering Replace. Keep reviewed fixes, selected-code edits and additions available.

# RAssistant 0.13.0

- Preserve complete code blocks, tables, display equations and R expressions in Knowledge Base excerpts. Keep indentation, long lines and section headings; retain PDF text-layer spacing and page labels.
- Send complete blocks within the excerpt budget and clearly identify blocks that could not fit. Mark older document formatting and rebuild it when the original file is re-added, keeping its selection and saved chat receipts.

# RAssistant 0.12.0

- Connect Knowledge Base search to current Source functions and packages, the latest completed Console task, and relevant dataset column names. Respect Console and Workspace Off, with no extra API call.
- Use work context for questions such as "explain this error" while keeping a specifically requested topic and manually selected files in charge of retrieval. Refresh hints on each Send without executing Source or reading dataset cells for search.

# RAssistant 0.11.0

- Select up to five Knowledge Base files for each chat, or leave all unchecked for automatic local search. Restore the selection with chat History; start new chats with automatic search.
- Expand Knowledge Base under a message to inspect the exact excerpts and instructions supplied. Keep receipts in History, and distinguish missing material, unsent validation and failed requests.

# RAssistant 0.10.3

- Rename Course knowledge to Knowledge Base, using general reference-document labels. Keep existing local, project-specific files and instructions unchanged.

# RAssistant 0.10.2

- Automatically show a blue dot before Plot when RStudio has an image, without hovering or attaching it. Hide the dot when Plots is empty; keep the default button and hover preview.

# RAssistant 0.10.1

- Keep Plot as a text button with a checkmark only while a plot is attached. Remove the duplicate button image and retain the hover preview.

# RAssistant 0.10.0

- Disable Undo and Redo when the current Source has no available AI edit, without an unnecessary notification.
- Preview the current RStudio plot on the Plot button and on hover. Refresh when returning to the Viewer; click to attach a fresh full-size image.
- Read relevant Global Environment datasets automatically, or select and preview them beside Workspace. Send bounded current cell values with their real dimensions; Workspace Off disables both routes.

# RAssistant 0.9.1

- Follow Source tab changes and file renames promptly, including files with no selected code. Refresh the label independently of the code actions toolbar.
- Keep empty new chats out of History. Preserve unsent text and attachments; clearing an unsent draft removes its empty history entry.

# RAssistant 0.9.0

- Selecting code in Source automatically shows Explain, Fix and Improve. Rmd/Quarto chunks use the same toolbar, without a dropdown activation step.
- Detect Source changes locally without AI requests; actions reread the current selection on click, and never silently expand it to a whole file.
- Separate the red Close button and center its label. Use accessible Undo/Redo arrow icons with tooltips.

# RAssistant 0.8.0

- One Code actions entry detects the selected code, current R chunk or whole file and opens a compact toolbar with its exact target.
- Explain, Fix and Improve stay visible after activation. Syntax failures suggest Fix; changing Source or selection refreshes the target before sending another request.
- Put Check updates and Help on one row, with a separate full-width red Close button.

# RAssistant 0.7.0

- Open Code edit history on a separate page with Back to chat, preserving the conversation, draft and attachments.
- Keep Undo and Redo visible beside Image and Plot. Preview each reversal on the history page before confirming.
- Support multiple Undo/Redo steps and saved-file Redo after relaunch. Manual changes block restoration, and new AI edits end the old Redo branch.

# RAssistant 0.6.2

- Group More actions into Code actions, Chat and App, with distinct Code edit history and Chat history labels.
- Show clear arrows, brief Source instructions and Explain/Fix/Improve buttons for selected code and Rmd/Quarto chunks. Open one code section at a time.

# RAssistant 0.6.1

- Move Workspace On/Off beside Console in the header and enable it by default, freeing space in More actions.

# RAssistant 0.6.0

- Extra blank lines are spacing, not errors. Spacing-only edits stay out of automatic fixes and need explicit inclusion.
- Keep a local edit history with exact before/after code, and preview Undo before restoring an AI change. Manual edits block Undo.
- Optional Workspace context reads object types, dimensions and column names without sending data rows.
- Explain, fix or improve the current R chunk in Rmd/Quarto while preserving other chunks, fences and prose.

# RAssistant 0.5.0

- Apply or skip individual changes, with fresh comparisons after each approval.
- Click a file-and-line link or Source line number to select that line in RStudio.
- Selected code in More actions offers Explain, Fix and Improve. Fixes stay within the selected range.

# RAssistant 0.4.3

- A single Source correction now opens its comparison automatically inside the chat code block, with Copy and Replace.
- Show exact old/new line numbers and highlight changed characters. Request precise file, line, cause and correction in error explanations.

# RAssistant 0.4.2

- Code reviews that find a concrete error now request a complete corrected file for Copy and inline Review, including when you ask what is wrong.
- Preserve unrelated lines, comments and strings. Explanation-only requests still stay explanations.

# RAssistant 0.4.1

- Compare Source changes directly inside the chat code block, without a separate dialog.
- Copy the proposed code or approve Replace, Append or Create. Cancel returns to the code.

# RAssistant 0.4.0

- Review code changes with a syntax check and comparison before approving Apply to Source.
- Replace or append to the current file, or create a new unsaved R/Rmd document. Save and Run stay separate.
- See which file and how many lines were sent. Changed files, Cancel and stale approvals cannot overwrite Source.

# RAssistant 0.3.19

- Move Check updates into More actions (...).
- Put Console On/Off in the header beside New chat.

# RAssistant 0.3.18

- Show only the current file name in the header, without the Source prefix.
- Enable Console context by default. Keep your On/Off choice when switching chats while clearing old captured output.

# RAssistant 0.3.17

- See release changes before updating.
- Shorter Help, with advanced commands in expandable sections and no update-status text.
- Simpler Course knowledge labels, file list and instructions.

# RAssistant 0.3.16

- Reopen the running app in RStudio's Viewer when launch or the Addin is used
  again, including after the Viewer disappears or loses its connection.
- Save and retire the previous Viewer session before restoring its replacement.
  Delayed disconnect callbacks cannot overwrite the new session's chat or draft
  or stop its Console capture. Reopening reuses the same Shiny server.

# RAssistant 0.3.15

- Show request loading on the Send button instead of a Thinking notification
  or a separate Working status row. Restore the button after success or failure.

# RAssistant 0.3.14

- Dismiss the manual up-to-date message after five seconds, without hiding
  a newer check or the Update now banner for an available version.
- Follow the newest chat reply after Send, validation and conversation restore.
  Keep the viewport stable as images load or the composer and status change size.
  Preserve intentional reading of older messages between replies.

# RAssistant 0.3.13

- Restore the existing bordered button appearance, light-blue Send button
  and original control colors and corner shapes while keeping the new layout.
- Place Source in the same header row as the visible actions, truncating long
  names with the full value in a tooltip instead of spending a separate row.

# RAssistant 0.3.12

- Keep Check updates and New chat in the header; move History, Knowledge,
  Help, Close and the version into a keyboard-accessible More actions menu.
- Show Source as a compact context line and give the conversation more space.
- Start the composer at two lines and grow it with the draft. Keep Image,
  Plot, Console Off/On and Send in one compact action row.
- Show small attachment previews above the draft with individual remove
  controls, retaining the other images and their order. Keep bulk Clear for
  multiple attachments and move detailed Console status into its tooltip.

# RAssistant 0.3.11

- Move **Check for updates** from Help to the main toolbar before Help.
  Show manual check progress and results in the main view; newer versions
  continue to use the existing Update now banner.

# RAssistant 0.3.10

- Add **Attach current plot** beside Upload. Export the plot selected in
  RStudio's Plots pane as a PNG and stage it in chat with a preview. Send
  submits the image through the normal attachment flow without rerunning Source.
- Preserve the draft, enforce the three-image and 4 MB limits, and report
  missing plots or unavailable RStudio export support without sending to AI.

# RAssistant 0.3.9

- Capture completed Console commands even when they print no text, including
  plotting commands, assignments and invisible results. Label absent text
  explicitly and keep the internal activation helper out of the transcript.
- Explain that a captured plot command is execution context, not an image of
  the plot. Keep earlier image uploads distinct from the current Console.
- Distinguish an enabled but empty capture from a disabled capture in requests.

# RAssistant 0.3.8

- Include completed Console commands alongside their captured output. Preserve
  error condition calls and stack entries before unwinding, including direct
  `stop()` calls with no condition call. Mark task boundaries and partial evidence.
- Tell the assistant to use captured execution evidence for Console questions
  and never guess which Source expression produced a message.
- Fix the Windows Console test runner to use Rterm's interactive `--ess` flag.

# RAssistant 0.3.7

- Add an opt-in **Console context** control below the composer. New R output,
  uncaught errors, warnings and messages can accompany Send automatically,
  with a limit of 200 lines and 12,000 characters per request.
- Keep Console output visible, redact the active API key from captured context,
  and keep the raw capture in temporary storage outside saved chats.
- Stop and clear capture on disable, chat changes, disconnect, Close and update.
  Preserve other output sinks and condition handlers during cleanup.
- Start condition logging through a fixed RStudio Console helper after returning
  to the prompt; refuse activation over an unfinished Console command.
- Existing output, plots, external-process output and messages intercepted by
  other handlers are not guaranteed to be captured.

# RAssistant 0.3.6

- Check for new versions automatically on launch and every 30 minutes while
  the assistant stays open, showing the update banner when a newer version is found.
- Keep **Help > Check for updates** for immediate manual checks.
- Preserve a known update banner during background checks and temporary
  connection failures. Automatic checks still respect the existing opt-out.

# RAssistant 0.3.5

- Publish a version-only update to verify automatic relaunch from 0.3.4,
  including restoration of the saved conversation and draft.

# RAssistant 0.3.4

- Automatically reopen the assistant in RStudio after a successful update from
  **Update now** or `update_assistant()`, restoring the saved conversation and
  draft through the normal launcher without clearing the R workspace.
- Release the update lock before relaunching, and keep installation failures
  separate from successful updates that need a manual launch retry.
- Add `update_assistant(relaunch = FALSE)` for installation without reopening.
  Updates outside RStudio continue to install without starting the app.
- The first update from 0.3.3 or earlier still uses the old updater: run
  `RAssistant::launch()` once afterward to activate automatic relaunch for
  subsequent updates.

# RAssistant 0.3.3

- Reject validation of Source text over 50,000 characters even when the chat
  retains images, preventing an empty run from being reported as successful.
- Preserve directories in CSV references pasted into chat, including quoted
  paths with spaces and escaped Windows paths, instead of checking a different
  same-named file in the project root.
- Treat Mongolian prohibitions against running code as explanation requests.
- Include explicitly referenced earlier images alongside new uploads, label
  their order, and preserve the draft when a comparison exceeds three images.
- Keep the course-document upload allowance active until the app stops and
  restore the previous option on shutdown or startup failure.
- Show script launch, environment-key setup and script-update instructions in
  standalone Help; preserve package instructions for package launches.

# RAssistant 0.3.2

- Find datasets named in pasted tasks even when text copied from a PDF lost
  its spaces ("thefilecrime.csv"), by matching CSV files present in the
  project. Follow-ups keep checking files named in the stored task, and named
  files are checked even when no script is open.
- Read named data files exactly as the task states them; never answer with
  path placeholders such as REPLACE_WITH_PATH or ask for a path the task gives.
- Apply custom instructions to every answer, using the packages and functions
  they name instead of base-R alternatives.

# RAssistant 0.3.1

- Simplify the Knowledge dialog: one optional **Custom instructions** box
  replaces the four preference fields. Preferences saved by 0.3.0 are combined
  into it without loss; the saved file is rewritten only when you save.
- Style **Add files** like the other dialog buttons, with a plain hint listing
  the accepted file types.

# RAssistant 0.3.0

- Add project course knowledge. **Knowledge** stores lecture PDFs, notes and
  R code examples, plus course instructions, preferred packages, functions and
  coding conventions, per RStudio project.
- Retrieve relevant sections locally for every chat, image and validation
  request, without extra API calls. Send only a bounded set of labelled
  excerpts, and nothing for unrelated requests. Instruct the assistant to prefer
  lecture-taught methods and to cite the file and page or lines it used.
  Answers list the course material they received.
- Keep knowledge separate from chat History and package installations, so both
  survive restarts and updates. Chats store source labels, not excerpts.
- PDF text extraction uses the suggested pdftools package, or Poppler's
  `pdftotext` when available.
- Fix loading on Windows: the assistant script is now pure ASCII, using Unicode
  escapes for its Mongolian request patterns, so it loads identically in every
  locale and with RStudio's UTF-8 Source command.
- Require R 4.2 or newer, the first Windows release that uses UTF-8 natively.
- Install and update without the GitHub REST API, whose anonymous quota of 60
  requests per hour is shared per IP and fails with HTTP 403 on campus networks.
  `update_assistant()` resolves the commit over public Git HTTP, downloads that
  commit's archive and still records its SHA. The README uses `install_url()`.
- Install with `upgrade = "always"` in a freshly restarted R session, so older
  shiny and ellmer installations are upgraded rather than failing installation.
  Add Windows install troubleshooting to the README.

# RAssistant 0.2.2

- Resolve the current main commit through public Git HTTP references, then read
  its immutable version file. Avoid both the shared REST API quota and stale
  raw-branch caches. No GitHub token or Git executable is required for checks.
- Bound both metadata downloads to four seconds each, retaining the ten-second
  background worker deadline and one-hour session cache.

# RAssistant 0.2.1

- Check versions through the public raw DESCRIPTION file without consuming the
  IP-shared GitHub REST API quota. Use a fresh request URL and cache-revalidation
  headers while retaining background checks, timeouts and the existing one-hour
  session cache. Raw branch responses can still lag behind new commits.

# RAssistant 0.2.0

- Add project-local History with New chat, Rename, confirmed Delete, Restore,
  auto-save and automatic restoration of the most recently used chat on launch.
- Store conversation data outside package installations; retain text, code,
  copied output, images and drafts across RStudio restarts and package updates.
  Deduplicate image assets and retain a complete prior snapshot for recovery.
- Reuse recent image pixels on follow-ups and support older uploads by Image
  number. Preserve the conversation when changing Source tabs or losing Source
  access; label historical Source context and keep fresh execution approval.
- Include longer recent dialogue and locally retrieved older excerpts within a
  bounded request, without extra model calls for summarizing or restoring.
- Rebuild safe message rendering and Copy controls on restore. Never resume an
  old run approval, serialize credentials or cached HTML, or alter the user's RNG.

# RAssistant 0.1.5

- Use the shared compact button styling for Help Close and update Cancel.
- Place Send on the right of the Upload row to save vertical space, keeping
  upload/request status messages and attachment previews visible.

# RAssistant 0.1.4

- Reload RAssistant's namespace on the next launch after an update, using the
  same library and preserving the R workspace and loaded dependency packages.
- Replace routine restart instructions with the launch command. Restart guidance
  remains available if another package prevents reloading or loading fails.
- Resolve lazy package code before installation and stop version-check workers;
  prevent launching while an update is queued.

# RAssistant 0.1.3

- Remove the composer shortcut hint and keep Send aligned to the right. Enter,
  Shift+Enter and screenshot paste retain their existing behavior.
- Read public version metadata through GitHub's contents API with cache
  revalidation, avoiding stale raw branch-file responses during update discovery.

# RAssistant 0.1.2

- Show the installed version and a newer-version Update now notice in the chat.
- Check public version metadata in the background, with a one-hour session cache,
  a deadline and graceful offline handling. Manual checks remain available in Help.
- Add Help with copyable launch, API-key replacement and update instructions.
- Confirm chat closure before updating, stop the app before installing, suppress
  duplicate update requests and report old/new versions with restart instructions.
- Report unchanged version/commit installs as already up to date without requiring
  a restart. Existing credentials stay in the same OS store.

# RAssistant 0.1.1

- Recognize Windows drive/network CSV paths and safely decode escaped R string
  literals when inspecting headers and caching file metadata.
- Use ellmer's current credentials interface, avoiding deprecated API-key
  arguments while passing the saved key directly to the client.

# RAssistant 0.1.0

- Package the existing AISource.R application without replacing its UI or features.
- Add RStudio launch/Addin support, hidden first-launch API-key setup using the OS
  credential store, key replacement/removal and updates in the installed R library.
- Add package regression tests and Linux/macOS/Windows CI.
