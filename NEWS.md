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
