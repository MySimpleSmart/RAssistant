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
