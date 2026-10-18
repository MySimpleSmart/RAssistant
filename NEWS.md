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
