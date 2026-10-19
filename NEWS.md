# RAssistant 0.2.1

- Check versions through the public raw DESCRIPTION file without consuming the
  IP-shared GitHub REST API quota. Use a fresh request URL and cache-revalidation
  headers to avoid stale version metadata while retaining background checks,
  timeouts and the existing one-hour session cache.

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
