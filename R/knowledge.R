# Course knowledge is stored per project under the user data directory, apart
# from chat history and package installations, so it survives restarts and
# updates. Only extracted text is kept. Retrieval is local: it makes no API
# calls and sends only a small budget of the most relevant excerpts.

.knowledge_kinds <- c(pdf = "Lecture PDF", txt = "Notes", md = "Notes", markdown = "Notes",
  r = "R code", rmd = "R code", qmd = "R code")

.knowledge_stopwords <- c("the", "and", "for", "are", "but", "not", "you", "your", "with",
  "this", "that", "these", "those", "from", "have", "has", "had", "was", "were", "will",
  "would", "can", "could", "should", "what", "which", "when", "where", "how", "why", "who",
  "use", "using", "used", "into", "than", "then", "there", "their", "they", "them", "its",
  "our", "out", "about", "also", "all", "any", "each", "some", "such", "only", "one", "two",
  "may", "might", "must", "per", "via", "please", "explain", "show", "give", "write", "help",
  "want", "need", "like", "make", "does", "did", "doing", "done", "is", "it", "in", "of",
  "to", "on", "or", "an", "as", "at", "be", "by", "do", "if", "no", "so", "we", "me", "my")

.knowledge_tokens <- function(text) {
  text <- tolower(enc2utf8(paste(text, collapse = "\n")))
  words <- unlist(strsplit(text, "[^\\p{L}\\p{N}_.]+", perl = TRUE), use.names = FALSE)
  words <- gsub("^[._]+|[._]+$", "", words)
  # Keep R names such as t.test whole, and also index their parts.
  dotted <- words[grepl("[._]", words)]
  words <- c(words, unlist(strsplit(dotted, "[._]+"), use.names = FALSE))
  words <- words[nchar(words) >= 2L & !words %in% .knowledge_stopwords & !grepl("^[0-9.]+$", words)]
  # Light suffix folding matches "models" with "model" and "clustering" with
  # fviz_cluster() without a stemmer.
  plural <- nchar(words) > 4L & grepl("[^s]s$", words)
  words[plural] <- substr(words[plural], 1L, nchar(words[plural]) - 1L)
  gerund <- nchar(words) > 6L & grepl("ing$", words)
  words[gerund] <- substr(words[gerund], 1L, nchar(words[gerund]) - 3L)
  words
}

.knowledge_pdf_pages <- function(path) {
  if (requireNamespace("pdftools", quietly = TRUE)) return(pdftools::pdf_text(path))
  tool <- Sys.which("pdftotext")
  if (nzchar(tool)) {
    out <- tempfile(fileext = ".txt")
    on.exit(unlink(out), add = TRUE)
    status <- suppressWarnings(system2(tool, c("-enc", "UTF-8", shQuote(path), shQuote(out)),
      stdout = FALSE, stderr = FALSE))
    if (identical(as.integer(status), 0L) && file.exists(out)) {
      text <- paste(readLines(out, encoding = "UTF-8", warn = FALSE), collapse = "\n")
      return(strsplit(text, "\f", fixed = TRUE)[[1L]])
    }
  }
  stop("Adding PDFs needs the pdftools package. Run install.packages(\"pdftools\") in the Console, then add the PDF again.",
    call. = FALSE)
}

.knowledge_read_lines <- function(path) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  bad <- !validUTF8(lines)
  if (any(bad)) lines[bad] <- iconv(lines[bad], "latin1", "UTF-8", sub = "")
  sub("^\uFEFF", "", substr(lines, 1L, 2000L))
}

# Break at blank lines or headings once a chunk is half full, and always at the
# size limit, so excerpts stay readable and labelled by page or line range.
.knowledge_chunks <- function(lines, label, offset = 0L, size = 1200L) {
  if (!length(lines)) return(list())
  chunks <- list()
  start <- 1L
  chars <- 0L
  emit <- function(from, to) {
    text <- trimws(paste(lines[from:to], collapse = "\n"))
    if (nchar(gsub("[[:space:]]", "", text)) >= 40L) {
      chunks[[length(chunks) + 1L]] <<- list(label = label(from + offset, to + offset), text = text)
    }
  }
  for (i in seq_along(lines)) {
    n <- nchar(lines[[i]]) + 1L
    boundary <- !nzchar(trimws(lines[[i]])) || grepl("^[[:space:]]*(#|```|---)", lines[[i]])
    if (i > start && (chars + n > size || (boundary && chars >= size %/% 2L))) {
      emit(start, i - 1L)
      start <- i
      chars <- 0L
    }
    chars <- chars + n
  }
  emit(start, length(lines))
  chunks
}

.knowledge_extract <- function(path, extension) {
  if (identical(extension, "pdf")) {
    pages <- .knowledge_pdf_pages(path)
    chunks <- unlist(lapply(seq_along(pages), function(page) {
      lines <- strsplit(enc2utf8(pages[[page]]), "\n", fixed = TRUE)[[1L]]
      lines <- trimws(gsub("[ \t\u00A0]+", " ", lines))
      .knowledge_chunks(lines, function(from, to) paste0("p. ", page))
    }), recursive = FALSE)
    return(list(chunks = chunks, pages = length(pages)))
  }
  lines <- sub("[[:space:]]+$", "", .knowledge_read_lines(path))
  label <- function(from, to) if (from == to) paste0("line ", from) else paste0("lines ", from, "-", to)
  list(chunks = .knowledge_chunks(lines, label), pages = NA_integer_)
}

.knowledge_store <- function(project = .history_project(),
    directory = getOption("RAssistant.knowledge_dir", tools::R_user_dir("RAssistant", "data"))) {
  probe <- tempfile()
  on.exit(unlink(probe), add = TRUE)
  writeBin(charToRaw(enc2utf8(project)), probe)
  root <- file.path(directory, "knowledge", unname(tools::md5sum(probe)))
  docs_dir <- file.path(root, "documents")
  profile_path <- file.path(root, "profile.json")
  profile_fields <- c("instructions", "packages", "functions", "conventions")
  docs <- NULL
  index <- NULL
  valid_id <- function(id) is.character(id) && length(id) == 1L && !is.na(id) && grepl("^[a-f0-9]{32}$", id)
  text_value <- function(x, limit) {
    if (!is.character(x) || length(x) != 1L || is.na(x)) return("")
    substr(trimws(x), 1L, limit)
  }
  write_atomic <- function(value, path) {
    if (!dir.exists(dirname(path)) && !dir.create(dirname(path), recursive = TRUE, mode = "0700")) {
      stop("Could not create the local course-knowledge directory.", call. = FALSE)
    }
    temp <- tempfile(".pending-", tmpdir = dirname(path))
    on.exit(unlink(temp), add = TRUE)
    jsonlite::write_json(value, temp, auto_unbox = TRUE, null = "null", digits = NA)
    Sys.chmod(temp, "0600")
    if (!file.rename(temp, path)) stop("Could not save course knowledge.", call. = FALSE)
  }
  read_doc <- function(path) {
    size <- file.info(path)$size
    if (is.na(size) || size > 64 * 1024^2) stop("Invalid course document size.", call. = FALSE)
    doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    if (!identical(doc$schema, 1L) || !identical(doc$project, project) || !valid_id(doc$id) ||
        !is.character(doc$name) || !is.list(doc$chunks)) {
      stop("Invalid course document.", call. = FALSE)
    }
    doc$chunks <- Filter(function(x) is.list(x) && is.character(x$text) && is.character(x$label), doc$chunks)
    doc
  }
  load_docs <- function() {
    if (is.null(docs)) {
      paths <- list.files(docs_dir, pattern = "^[a-f0-9]{32}\\.json$", full.names = TRUE)
      loaded <- lapply(paths, function(path) tryCatch(read_doc(path), error = function(e) NULL))
      docs <<- Filter(Negate(is.null), loaded)
      index <<- NULL
    }
    docs
  }
  summary_of <- function(doc) list(id = doc$id, name = doc$name, kind = doc$kind,
    added_at = doc$added_at, pages = doc$pages, chunks = length(doc$chunks))
  list_docs <- function() {
    result <- lapply(load_docs(), summary_of)
    result[order(tolower(vapply(result, function(x) x$name, character(1))))]
  }
  add_doc <- function(path, name = basename(path)) {
    name <- substr(gsub("[[:cntrl:]]", "", basename(name)), 1L, 120L)
    extension <- tolower(tools::file_ext(name))
    if (!extension %in% names(.knowledge_kinds)) {
      stop("Add PDF, text, Markdown, R, R Markdown or Quarto files.", call. = FALSE)
    }
    size <- file.info(path)$size
    if (is.na(size) || size < 1L || size > 50 * 1024^2) stop("Each file must be between 1 byte and 50 MB.", call. = FALSE)
    id <- unname(tools::md5sum(path))
    existing <- load_docs()
    same <- Filter(function(doc) identical(doc$id, id), existing)
    if (length(same)) return(c(summary_of(same[[1L]]), list(status = "duplicate")))
    extracted <- .knowledge_extract(path, extension)
    if (!length(extracted$chunks)) {
      stop(if (identical(extension, "pdf")) "No text could be extracted. Scanned PDFs need OCR before they can be added."
        else "The file contains no usable text.", call. = FALSE)
    }
    if (length(extracted$chunks) > 5000L) stop("The file is too large to index (5,000 sections max).", call. = FALSE)
    doc <- list(schema = 1L, project = project, id = id, name = name,
      kind = unname(.knowledge_kinds[[extension]]), pages = extracted$pages,
      added_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), chunks = extracted$chunks)
    write_atomic(doc, file.path(docs_dir, paste0(id, ".json")))
    # Re-adding a file with the same name replaces the earlier version.
    replaced <- Filter(function(old) identical(tolower(old$name), tolower(name)), existing)
    for (old in replaced) unlink(file.path(docs_dir, paste0(old$id, ".json")))
    docs <<- c(Filter(function(old) !identical(tolower(old$name), tolower(name)), existing), list(doc))
    index <<- NULL
    c(summary_of(doc), list(status = if (length(replaced)) "replaced" else "added"))
  }
  remove_doc <- function(id) {
    if (!valid_id(id)) stop("Invalid course document identifier.", call. = FALSE)
    path <- file.path(docs_dir, paste0(id, ".json"))
    if (file.exists(path) && unlink(path) != 0L) stop("Could not remove the course document.", call. = FALSE)
    docs <<- Filter(function(doc) !identical(doc$id, id), load_docs())
    index <<- NULL
    invisible(NULL)
  }
  read_profile <- function() {
    empty <- stats::setNames(as.list(rep("", length(profile_fields))), profile_fields)
    if (!file.exists(profile_path)) return(empty)
    value <- tryCatch(jsonlite::fromJSON(profile_path, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.list(value) || !identical(value$project, project)) return(empty)
    stats::setNames(lapply(profile_fields, function(f) text_value(value[[f]], 4000L)), profile_fields)
  }
  save_profile <- function(profile) {
    value <- stats::setNames(lapply(profile_fields, function(f) text_value(profile[[f]], 4000L)), profile_fields)
    write_atomic(c(list(schema = 1L, project = project), value,
      list(updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))), profile_path)
    invisible(value)
  }
  profile_text <- function() {
    profile <- read_profile()
    parts <- c(
      if (nzchar(profile$instructions)) paste0("Course instructions:\n", profile$instructions),
      if (nzchar(profile$packages)) paste0("Preferred R packages: ", profile$packages),
      if (nzchar(profile$functions)) paste0("Preferred R functions: ", profile$functions),
      if (nzchar(profile$conventions)) paste0("Coding conventions:\n", profile$conventions))
    paste(parts, collapse = "\n")
  }
  build_index <- function() {
    chunks <- unlist(lapply(load_docs(), function(doc) lapply(doc$chunks, function(chunk)
      list(name = doc$name, label = chunk$label, text = chunk$text))), recursive = FALSE)
    tokens <- lapply(chunks, function(chunk) .knowledge_tokens(chunk$text))
    lengths <- lengths(tokens)
    # Postings list chunk positions per term; chunk ids are ascending, so run
    # lengths give term frequencies at query time.
    postings <- split(rep(seq_along(tokens), lengths), unlist(tokens, use.names = FALSE))
    list(chunks = chunks, lengths = lengths, average = max(1, mean(lengths)), postings = postings)
  }
  retrieve <- function(query, context = "", budget = getOption("RAssistant.knowledge_chars", 6000L),
      max_chunks = 5L) {
    none <- list(text = "", sources = list())
    if (!length(load_docs())) return(none)
    if (is.null(index)) index <<- build_index()
    n <- length(index$chunks)
    if (!n) return(none)
    # The latest request dominates; assignment text and editor calls refine it.
    # A request with its own terms must match course material itself; only a
    # request without them (an image turn, "explain (b)") relies on context.
    weights <- c(table(.knowledge_tokens(query)))
    if (length(weights) && !any(names(weights) %in% names(index$postings))) return(none)
    extra <- 0.4 * c(table(.knowledge_tokens(context)))
    for (term in names(extra)) weights[term] <- sum(weights[term], extra[[term]], na.rm = TRUE)
    weights <- pmin(weights[names(weights) %in% names(index$postings)], 3)
    if (!length(weights)) return(none)
    scores <- numeric(n)
    matched <- integer(n)
    for (term in names(weights)) {
      runs <- rle(index$postings[[term]])
      df <- length(runs$values)
      idf <- log(1 + (n - df + 0.5) / (df + 0.5))
      tf <- runs$lengths
      norm <- 1.2 * (0.25 + 0.75 * index$lengths[runs$values] / index$average)
      scores[runs$values] <- scores[runs$values] + weights[[term]] * idf * tf * 2.2 / (tf + norm)
      matched[runs$values] <- matched[runs$values] + 1L
    }
    # Retrieve only when some section matches two query terms (when the query
    # offers them), then keep strong single-term matches such as a function
    # name, and drop weak tails so unrelated material does not consume tokens.
    strong <- scores > 0 & matched >= min(2L, length(weights))
    if (!any(strong)) return(none)
    top <- max(scores)
    eligible <- which(strong | scores >= 0.5 * top)
    eligible <- eligible[order(scores[eligible], decreasing = TRUE)]
    eligible <- eligible[scores[eligible] >= 0.35 * top]
    selected <- integer()
    used <- 0L
    for (i in eligible) {
      if (length(selected) >= max_chunks) break
      size <- nchar(index$chunks[[i]]$text)
      if (length(selected) && used + size > budget) next
      selected <- c(selected, i)
      used <- used + min(size, budget)
    }
    sources <- lapply(seq_along(selected), function(k) {
      chunk <- index$chunks[[selected[[k]]]]
      list(ref = paste0("K", k), name = chunk$name, label = chunk$label)
    })
    text <- paste(vapply(seq_along(selected), function(k) {
      chunk <- index$chunks[[selected[[k]]]]
      paste0("[K", k, "] ", chunk$name, ", ", chunk$label, "\n", substr(chunk$text, 1L, budget))
    }, character(1)), collapse = "\n\n")
    list(text = text, sources = sources)
  }
  list(list = list_docs, add = add_doc, remove = remove_doc, profile = read_profile,
    save_profile = save_profile, profile_text = profile_text, retrieve = retrieve, path = root)
}
