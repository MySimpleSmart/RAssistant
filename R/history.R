.history_project <- function() {
  project <- tryCatch(rstudioapi::getActiveProject(), error = function(e) NULL)
  if (is.null(project) || !nzchar(project)) project <- getwd()
  normalizePath(project, winslash = "/", mustWork = TRUE)
}

.history_id <- function() {
  nonce <- gsub("[^a-z0-9]", "", tolower(basename(tempfile(pattern = ""))))
  nonce <- substr(paste0(nonce, strrep("0", 12L)), 1L, 12L)
  paste0(format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"), "-",
    Sys.getpid(), "-", nonce)
}

.conversation_store <- function(project = .history_project(),
    directory = getOption("RAssistant.history_dir", tools::R_user_dir("RAssistant", "data"))) {
  if (!isTRUE(getOption("RAssistant.save_history", TRUE))) return(NULL)
  # Hash only the normalized project path, never Source contents or credentials.
  probe <- tempfile()
  on.exit(unlink(probe), add = TRUE)
  writeBin(charToRaw(enc2utf8(project)), probe)
  root <- file.path(directory, "history", unname(tools::md5sum(probe)))
  assets <- file.path(root, "images")
  cache <- new.env(parent = emptyenv())
  draft_cache <- list()
  valid_id <- function(id) is.character(id) && length(id) == 1L && !is.na(id) &&
    grepl("^[0-9]{8}T[0-9.]+-[0-9]+-[a-z0-9]{12}$", id)
  valid_ref <- function(ref) is.character(ref) && length(ref) == 1L && !is.na(ref) &&
    grepl("^[a-f0-9]{32}\\.(png|jpeg|webp)$", ref)
  make_dir <- function(path) {
    if (!dir.exists(path) && !dir.create(path, recursive = TRUE, mode = "0700")) {
      stop("Could not create the local chat-history directory.", call. = FALSE)
    }
    invisible(path)
  }
  read_json <- function(path, limit = 64 * 1024^2) {
    size <- file.info(path)$size
    if (is.na(size) || size > limit) stop("Invalid chat-history file size.", call. = FALSE)
    jsonlite::fromJSON(path, simplifyVector = FALSE)
  }
  atomic_json <- function(value, path) {
    make_dir(dirname(path))
    temp <- tempfile(".pending-", tmpdir = dirname(path))
    on.exit(unlink(temp), add = TRUE)
    jsonlite::write_json(value, temp, auto_unbox = TRUE, null = "null", digits = NA)
    if (file.info(temp)$size > 64 * 1024^2) stop("The conversation exceeds the 64 MB text-history limit.", call. = FALSE)
    Sys.chmod(temp, "0600")
    # Destinations are unique: Windows can also publish them with an atomic rename.
    if (!file.rename(temp, path)) stop("Could not save the local conversation.", call. = FALSE)
  }
  meta_files <- function(id) {
    if (!valid_id(id)) stop("Invalid conversation identifier.", call. = FALSE)
    paths <- list.files(file.path(root, id), pattern = "\\.meta\\.json$", full.names = TRUE)
    paths[order(basename(paths), decreasing = TRUE)]
  }
  read_meta <- function(path) {
    meta <- read_json(path, 1024^2)
    if (!identical(meta$schema, 1L) || !identical(meta$project, project) ||
        !valid_id(meta$id) || !is.character(meta$title) || length(meta$title) != 1L ||
        is.na(meta$title) || !is.character(meta$updated_at) || length(meta$updated_at) != 1L ||
        is.na(meta$updated_at) || !is.character(meta$snapshot) || length(meta$snapshot) != 1L ||
        is.na(meta$snapshot) ||
        !grepl("^[0-9]{8}T[0-9.]+-[0-9]+-[a-z0-9]{12}\\.json$", meta$snapshot) ||
        !file.exists(file.path(dirname(path), meta$snapshot))) {
      stop("Invalid conversation metadata.", call. = FALSE)
    }
    meta
  }
  list_chats <- function() {
    dirs <- list.dirs(root, recursive = FALSE, full.names = FALSE)
    ids <- Filter(valid_id, dirs)
    result <- lapply(ids, function(id) {
      for (path in meta_files(id)) {
        meta <- tryCatch(read_meta(path), error = function(e) NULL)
        if (!is.null(meta)) return(meta)
      }
      NULL
    })
    result <- Filter(Negate(is.null), result)
    result[order(vapply(result, function(x) x$updated_at, character(1)), decreasing = TRUE)]
  }
  store_image <- function(image, key = NULL) {
    if (!is.list(image)) return(image)
    ref <- if (!is.null(key)) cache[[key]] else NULL
    if (valid_ref(ref) && file.exists(file.path(assets, ref))) {
      image$preview <- NULL; image$image_ref <- ref
      return(image)
    }
    preview <- image$preview
    if (is.null(preview)) return(image)
    if (is.null(key)) for (entry in draft_cache) {
      if (identical(entry$preview, preview) && file.exists(file.path(assets, entry$ref))) {
        image$preview <- NULL; image$image_ref <- entry$ref
        return(image)
      }
    }
    if (!is.character(preview) || length(preview) != 1L || is.na(preview) ||
        nchar(preview, type = "bytes") > 6 * 1024^2) {
      stop("Invalid image in the conversation.", call. = FALSE)
    }
    encoded <- gsub("[\r\n\t ]", "", preview)
    if (!grepl("^data:image/(png|jpeg|webp);base64,[A-Za-z0-9+/]+=*$", encoded)) {
      stop("Invalid image in the conversation.", call. = FALSE)
    }
    kind <- sub("^data:image/([^;]+);.*$", "\\1", encoded)
    raw <- jsonlite::base64_dec(sub("^data:image/[^;]+;base64,", "", encoded))
    if (length(raw) > 4 * 1024^2 || length(raw) < 12L) stop("Invalid saved image size.", call. = FALSE)
    make_dir(assets)
    temp <- tempfile(".image-", tmpdir = assets)
    on.exit(unlink(temp), add = TRUE)
    writeBin(raw, temp)
    ref <- paste0(unname(tools::md5sum(temp)), ".", kind)
    target <- file.path(assets, ref)
    if (!file.exists(target)) {
      Sys.chmod(temp, "0600")
      if (!file.rename(temp, target)) stop("Could not save a conversation image.", call. = FALSE)
    }
    if (!is.null(key)) cache[[key]] <- ref
    if (is.null(key)) draft_cache <<- utils::head(c(list(list(preview=preview,ref=ref)),draft_cache),6L)
    image$preview <- NULL; image$image_ref <- ref
    image
  }
  restore_image <- function(image, key = NULL) {
    if (!is.list(image)) return(image)
    ref <- image$image_ref
    image$preview <- NULL
    if (valid_ref(ref)) {
      path <- file.path(assets, ref)
      size <- file.info(path)$size
      if (!is.na(size) && size >= 12L && size <= 4 * 1024^2) {
        image$preview <- paste0("data:image/", sub(".*\\.", "", ref), ";base64,",
          jsonlite::base64_enc(readBin(path, "raw", size)))
        if (!is.null(key)) cache[[key]] <- ref
      }
    }
    image
  }
  map_images <- function(chat, transform) {
    for (i in seq_along(chat$messages)) {
      message <- chat$messages[[i]]
      key <- paste(chat$id, message$sequence, sep = "/")
      message$attachments <- lapply(seq_along(message$attachments), function(j)
        transform(message$attachments[[j]], paste(key, "attachment", j, sep = "/")))
      if (identical(message$type, "plots")) message$images <- lapply(seq_along(message$images), function(j)
        transform(message$images[[j]], paste(key, "plot", j, sep = "/")))
      chat$messages[[i]] <- message
    }
    # Draft attachments can be replaced, so they must not use a positional cache.
    chat$attachments <- lapply(chat$attachments, transform)
    chat
  }
  load_chat <- function(id = NULL) {
    if (is.null(id)) {
      chats <- list_chats()
      if (!length(chats)) return(NULL)
      id <- chats[[1L]]$id
    }
    paths <- meta_files(id)
    if (!length(paths)) return(NULL)
    for (i in seq_along(paths)) {
      path <- paths[[i]]
      chat <- tryCatch({
        meta <- read_meta(path)
        value <- read_json(file.path(dirname(path), meta$snapshot))
        if (!is.list(value) || !identical(value$id, id) || !is.list(value$messages)) stop("Invalid conversation.")
        map_images(value, restore_image)
      }, error = function(e) NULL)
      if (!is.null(chat)) {
        if (i>1L) chat$recovered <- TRUE
        return(chat)
      }
    }
    stop("Could not restore this conversation. Its saved files were kept.", call. = FALSE)
  }
  save_chat <- function(chat) {
    if (!is.list(chat) || !valid_id(chat$id)) stop("Invalid conversation.", call. = FALSE)
    chat$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
    saved <- map_images(chat, store_image)
    refs <- unique(unlist(lapply(saved$messages, function(m) c(
      vapply(m$attachments, function(a) if (is.null(a$image_ref)) "" else a$image_ref, character(1)),
      if (identical(m$type, "plots")) vapply(m$images, function(a) if (is.null(a$image_ref)) "" else a$image_ref, character(1))))))
    refs <- unique(c(refs, vapply(saved$attachments, function(a)
      if (is.null(a$image_ref)) "" else a$image_ref, character(1))))
    stamp <- .history_id()
    folder <- file.path(root, chat$id)
    meta <- list(schema = 1L, project = project, id = chat$id, title = chat$title,
      updated_at = chat$updated_at, snapshot = paste0(stamp, ".json"), assets = refs[nzchar(refs)])
    atomic_json(saved, file.path(folder, meta$snapshot))
    atomic_json(meta, file.path(folder, paste0(stamp, ".meta.json")))
    # Keep one complete prior snapshot for recovery from a damaged latest file.
    old <- utils::tail(meta_files(chat$id), -2L)
    if (length(old)) unlink(c(old, sub("\\.meta\\.json$", ".json", old)))
    invisible(meta)
  }
  delete_chat <- function(id) {
    if (!valid_id(id)) stop("Invalid conversation identifier.", call. = FALSE)
    folder <- file.path(root, id)
    if (dir.exists(folder) && unlink(folder, recursive = TRUE) != 0L) {
      stop("Could not delete this conversation.", call. = FALSE)
    }
    # Only remove assets no remaining recovery snapshot references.
    ids <- Filter(valid_id,list.dirs(root,recursive=FALSE,full.names=FALSE))
    metas <- unlist(lapply(ids,meta_files),use.names=FALSE)
    records <- lapply(metas, function(path) tryCatch(read_meta(path), error = function(e) NULL))
    if (all(vapply(records, Negate(is.null), logical(1)))) {
      used <- unique(unlist(lapply(records, function(x) unlist(x$assets, use.names = FALSE))))
      paths <- list.files(assets, pattern = "^[a-f0-9]{32}\\.(png|jpeg|webp)$", full.names = TRUE)
      unlink(paths[!basename(paths) %in% used])
    }
    invisible(NULL)
  }
  list(list = list_chats, load = load_chat, save = save_chat, delete = delete_chat,
    new_id = .history_id, path = root)
}
