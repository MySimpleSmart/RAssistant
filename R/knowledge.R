# Knowledge Base is stored per project under the user data directory, apart
# from chat history and package installations, so it survives restarts and
# updates. Only extracted text is kept. Retrieval is local: it makes no API
# calls and sends only a small budget of the most relevant excerpts.

.knowledge_kinds <- c(pdf = "PDF", txt = "Notes", md = "Notes", markdown = "Notes",
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

.knowledge_query_topics <- function(query) {
  # These phrases refer to the current work rather than naming a new topic.
  generic <- .knowledge_tokens(paste("code script console error warning problem issue result output",
    "dataset data column variable package function fix correct improve check review interpret analyze analyse analysis",
    "run running execute current latest recent open selected attached here happening wrong broken failing failed failure",
    "explanation again below above look tell see happen getting choose select fail compare",
    "\u0438\u0439\u043d \u044b\u043d \u0438\u0439\u0433 \u044b\u0433 \u0442\u044d\u0439 \u0442\u0430\u0439 \u044e\u043c \u0433\u044d\u044d\u0434 \u0433\u044d\u0436 \u0433\u044d\u0445 \u0431\u043e\u043b\u043e\u043d \u0431\u04af\u0445 \u0430\u043b\u044c"))
  words <- unique(.knowledge_tokens(query))
  words <- words[!words %in% generic]
  words[!grepl(paste0("^(\u044d\u043d\u044d|\u04af\u04af\u043d|\u0438\u043d\u0433\u044d|\u043e\u0434\u043e\u043e|\u043e\u0434\u043e\u043e\u0433|\u043c\u0438\u043d\u0438\u0439|\u043c\u0430\u043d\u0430\u0439|\u0442\u044d\u0440|\u044f\u043c\u0430\u0440|\u044e\u0443|\u044f\u0430\u0433\u0430\u0430\u0434|\u044f\u0430\u0436|\u0445\u044d\u0440\u0445\u044d\u043d|",
    "\u043a\u043e\u0434|\u0441\u043a\u0440\u0438\u043f\u0442|\u0430\u043b\u0434\u0430\u0430|\u0430\u043d\u0445\u0430\u0430\u0440\u0443\u0443\u043b|\u043a\u043e\u043d\u0441\u043e\u043b|\u04e9\u0433\u04e9\u0433\u0434|\u0434\u0430\u0442\u0430\u0441\u0435\u0442|\u0431\u0430\u0433\u0430\u043d|\u0445\u0443\u0432\u044c\u0441\u0430\u0433\u0447|\u0444\u0443\u043d\u043a\u0446|",
    "\u0442\u0430\u0439\u043b\u0431\u0430\u0440|\u0437\u0430\u0441|\u0448\u0430\u043b\u0433\u0430|\u0441\u0430\u0439\u0436\u0440\u0443\u0443\u043b|\u0445\u0430\u0440\u0443\u0443\u043b|\u0433\u0430\u0440|\u0430\u0436\u0438\u043b|\u0434\u043e\u0442\u043e\u0440|\u0431\u0430\u0439|\u0431\u043e\u043b|\u0443\u0443|\u0432\u044d|\u043d\u044c|\u0434\u044d\u044d\u0440|\u0434\u043e\u043e\u0440|\u0442\u0430\u043b\u0430\u0430\u0440|",
    "\u0445\u0438\u0439|\u0442\u0443\u0441\u0430\u043b|\u0443\u043d\u0448|\u04af\u0440\u0433\u044d\u043b\u0436\u043b\u04af\u04af\u043b|\u0445\u0430\u0440\u0430|\u043c\u044d\u0434\u044d\u044d\u043b|\u043c\u04e9\u0440|\u0441\u04af\u04af\u043b\u0438\u0439\u043d)"),words,perl=TRUE)]
}

.knowledge_pdf_pages <- function(path) {
  if (requireNamespace("pdftools", quietly = TRUE)) return(pdftools::pdf_text(path))
  tool <- Sys.which("pdftotext")
  if (nzchar(tool)) {
    out <- tempfile(fileext = ".txt")
    on.exit(unlink(out), add = TRUE)
    status <- suppressWarnings(system2(tool, c("-layout", "-enc", "UTF-8", shQuote(path), shQuote(out)),
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
  sub("^\uFEFF", "", lines)
}

# Locate complete top-level R expressions without evaluating any code. Keep a
# malformed trailing expression together so even syntax-error examples survive.
.knowledge_r_blocks <- function(lines) {
  if(!length(lines))return(list())
  parse_options <- options(keep.parse.data=TRUE)
  on.exit(options(parse_options),add=TRUE)
  src <- srcfilecopy("knowledge-reference",lines)
  parsed <- tryCatch({parse(text=lines,srcfile=src,keep.source=TRUE);TRUE},error=function(e)FALSE)
  data <- utils::getParseData(src)
  ends <- if(is.null(data))integer()else sort(unique(data$line2[data$parent==0L & data$token=="expr"]))
  if(!parsed) {
    # An unfinished call can leave internal parse nodes with parent zero.
    # Accept only complete prefixes and keep the first malformed remainder.
    safe <- integer();start <- 1L
    for(end in ends) {
      complete <- tryCatch({parse(text=lines[seq.int(start,end)],keep.source=FALSE);TRUE},error=function(e)FALSE)
      if(!complete)break
      safe <- c(safe,end);start <- end+1L
    }
    ends <- safe
  }
  ends <- sort(unique(c(ends,length(lines))))
  start <- 1L;result <- list()
  for(end in ends) {
    result[[length(result)+1L]] <- list(from=start,to=end,type="code",heading="")
    start <- end+1L
  }
  result
}

# Structural blocks are indivisible. The target size controls grouping, never
# cuts through a fence, table, display equation, paragraph or R expression.
.knowledge_blocks <- function(lines,format="text") {
  if(format=="r")return(.knowledge_r_blocks(lines))
  n <- length(lines);if(!n)return(list())
  result <- list();titles <- character();heading <- ""
  fence <- function(x)regmatches(x,regexec("^[[:blank:]]{0,3}(`{3,}|~{3,})(.*)$",x,perl=TRUE))[[1L]]
  atx <- function(x)regmatches(x,regexec("^[[:blank:]]{0,3}(#{1,6})[[:blank:]]+(.+)$",x,perl=TRUE))[[1L]]
  separator <- function(x)grepl("^[[:blank:]]*\\|?[[:blank:]]*:?-{3,}:?[[:blank:]]*(\\|[[:blank:]]*:?-{3,}:?[[:blank:]]*)+\\|?[[:blank:]]*$",x)
  columns <- function(x)grepl("\\S(?:[[:blank:]]{2,}|\t)\\S",x,perl=TRUE)
  heading_at <- function(i) {
    value <- atx(lines[[i]])
    if(length(value))return(list(level=nchar(value[[2L]]),text=sub("[[:blank:]]+#+[[:blank:]]*$","",value[[3L]]),end=i))
    if(i<n && nzchar(trimws(lines[[i]])) && grepl("^[[:blank:]]{0,3}(={3,}|-{3,})[[:blank:]]*$",lines[[i+1L]]))
      return(list(level=if(grepl("=",lines[[i+1L]],fixed=TRUE))1L else 2L,text=trimws(lines[[i]]),end=i+1L))
    if(format=="pdf" && nchar(trimws(lines[[i]]))<=100L &&
        grepl("^[[:blank:]]*([0-9]+[.)][[:blank:]]+[[:alpha:]]|[[:upper:]][[:upper:][:blank:]]{3,}$)",lines[[i]]) &&
        (i==1L || !nzchar(trimws(lines[[i-1L]]))) && (i==n || !nzchar(trimws(lines[[i+1L]]))))
      return(list(level=1L,text=trimws(lines[[i]]),end=i))
    NULL
  }
  math <- function(x) {
    if(grepl("^[[:blank:]]*\\$\\$",x))return(list(open="$$",close="$$",type="math"))
    if(grepl("^[[:blank:]]*\\\\\\[",x))return(list(open="\\[",close="\\]",type="math"))
    value <- regmatches(x,regexec("^[[:blank:]]*\\\\begin\\{(equation\\*?|align\\*?|gather\\*?|multline\\*?|displaymath|eqnarray\\*?|tabular\\*?)\\}",x,perl=TRUE))[[1L]]
    if(length(value))return(list(open=value[[1L]],close=paste0("\\end{",value[[2L]],"}"),
      type=if(startsWith(value[[2L]],"tabular"))"table"else"math"))
    NULL
  }
  special <- function(i)length(fence(lines[[i]])) || !is.null(heading_at(i)) || !is.null(math(lines[[i]])) ||
    (i<n && separator(lines[[i+1L]]) && grepl("|",lines[[i]],fixed=TRUE)) ||
    (i<n && columns(lines[[i]]) && columns(lines[[i+1L]])) ||
    (format!="pdf" && grepl("^( {4,}|\t)[[:blank:]]*\\S",lines[[i]]))
  i <- 1L
  while(i<=n) {
    if(!nzchar(trimws(lines[[i]]))){i<-i+1L;next}
    from <- i;type <- "prose";h <- heading_at(i);f <- fence(lines[[i]]);m <- math(lines[[i]])
    yaml_end <- if(from==1L && format!="pdf" && trimws(lines[[i]])=="---" && n>1L)
      which(grepl("^(---|\\.\\.\\.)[[:blank:]]*$",lines[-1L]))else integer()
    if(length(yaml_end)) {
      type <- "metadata";i <- yaml_end[[1L]]+1L
    } else if(length(f)) {
      type <- if(grepl("^[[:blank:]]*math([[:blank:]]|$)",f[[3L]]))"math"else"code"
      close <- paste0("^[[:blank:]]{0,3}",substr(f[[2L]],1L,1L),"{",nchar(f[[2L]]),",}[[:blank:]]*$")
      i <- i+1L
      while(i<=n && !grepl(close,lines[[i]],perl=TRUE))i <- i+1L
      i <- min(i,n)
    } else if(!is.null(m)) {
      type <- m$type
      rest <- substring(trimws(lines[[i]]),nchar(trimws(m$open))+1L)
      if(!grepl(m$close,rest,fixed=TRUE)) {
        i <- i+1L
        while(i<=n && !grepl(m$close,lines[[i]],fixed=TRUE))i <- i+1L
        i <- min(i,n)
      }
    } else if(!is.null(h)) {
      type <- "heading";i <- h$end
      length(titles) <- h$level
      titles[[h$level]] <- h$text
      heading <- substr(paste(titles[!is.na(titles) & nzchar(titles)],collapse=" / "),1L,500L)
    } else if(i<n && separator(lines[[i+1L]]) && grepl("|",lines[[i]],fixed=TRUE)) {
      type <- "table";i <- i+2L
      while(i<=n && nzchar(trimws(lines[[i]])) && grepl("|",lines[[i]],fixed=TRUE))i <- i+1L
      i <- i-1L
    } else if(i<n && columns(lines[[i]]) && columns(lines[[i+1L]])) {
      type <- "table";i <- i+1L
      while(i<n && columns(lines[[i+1L]]))i <- i+1L
    } else if(format!="pdf" && grepl("^( {4,}|\t)[[:blank:]]*\\S",lines[[i]])) {
      type <- "code"
      while(i<n && (!nzchar(trimws(lines[[i+1L]])) || grepl("^( {4}|\t)",lines[[i+1L]])))i <- i+1L
    } else {
      while(i<n && nzchar(trimws(lines[[i+1L]])) && !special(i+1L))i <- i+1L
    }
    result[[length(result)+1L]] <- list(from=from,to=i,type=type,heading=heading)
    i <- i+1L
  }
  result
}

.knowledge_chunks <- function(lines, label, offset=0L, size=1200L,format="text",page_map=NULL) {
  if (!length(lines)) return(list())
  blocks <- .knowledge_blocks(lines,format)
  chunks <- list();pending <- list()
  emit <- function() {
    if(!length(pending))return()
    from <- pending[[1L]]$from;to <- utils::tail(pending,1L)[[1L]]$to
    while(from<to && !nzchar(trimws(lines[[from]])))from <- from+1L
    while(to>from && !nzchar(trimws(lines[[to]])))to <- to-1L
    text <- paste(lines[seq.int(from,to)],collapse="\n")
    if(nzchar(trimws(text)))chunks[[length(chunks)+1L]] <<- list(label=label(from+offset,to+offset),
      text=text,heading=utils::tail(pending,1L)[[1L]]$heading,
      type=if(length(unique(vapply(pending,function(x)x$type,character(1))))==1L)pending[[1L]]$type else"mixed",whole=TRUE)
  }
  for(block in blocks) {
    has_content <- any(vapply(pending,function(x)!x$type %in% c("heading","metadata"),logical(1)))
    previous_page <- if(length(pending) && !is.null(page_map))utils::tail(stats::na.omit(
      page_map[seq.int(utils::tail(pending,1L)[[1L]]$from,utils::tail(pending,1L)[[1L]]$to)]),1L)else integer()
    new_page <- length(previous_page)>0L && !is.na(page_map[[block$from]]) && previous_page!=page_map[[block$from]]
    if(length(pending) && has_content && (block$type=="heading" || new_page ||
        sum(nchar(lines[seq.int(pending[[1L]]$from,block$to)]))+block$to-pending[[1L]]$from>size)) {
      emit();pending <- list()
    }
    pending[[length(pending)+1L]] <- block
  }
  emit()
  chunks
}

.knowledge_extract <- function(path, extension) {
  if (identical(extension, "pdf")) {
    pages <- .knowledge_pdf_pages(path)
    lines <- character();page_map <- integer()
    for(page in seq_along(pages)) {
      body <- strsplit(enc2utf8(pages[[page]]),"\n",fixed=TRUE)[[1L]]
      lines <- c(lines,body,"");page_map <- c(page_map,rep(page,length(body)),NA_integer_)
    }
    chunks <- .knowledge_chunks(lines,function(from,to){
      numbers <- unique(stats::na.omit(page_map[seq.int(from,to)]))
      paste0("p. ",if(length(numbers)>1L)paste0(numbers[[1L]],"-",utils::tail(numbers,1L))else numbers[[1L]])
    },format="pdf",page_map=page_map)
    return(list(chunks = chunks, pages = length(pages)))
  }
  lines <- .knowledge_read_lines(path)
  label <- function(from, to) if (from == to) paste0("line ", from) else paste0("lines ", from, "-", to)
  list(chunks = .knowledge_chunks(lines,label,format=if(extension=="r")"r"else"text"), pages = NA_integer_)
}

.knowledge_store <- function(project = .history_project(),
    directory = getOption("RAssistant.knowledge_dir", tools::R_user_dir("RAssistant", "data"))) {
  probe <- tempfile()
  on.exit(unlink(probe), add = TRUE)
  writeBin(charToRaw(enc2utf8(project)), probe)
  root <- file.path(directory, "knowledge", unname(tools::md5sum(probe)))
  docs_dir <- file.path(root, "documents")
  profile_path <- file.path(root, "profile.json")
  docs <- NULL
  index <- NULL
  valid_id <- function(id) is.character(id) && length(id) == 1L && !is.na(id) && grepl("^[a-f0-9]{32}$", id)
  text_value <- function(x, limit) {
    if (!is.character(x) || length(x) != 1L || is.na(x)) return("")
    substr(trimws(x), 1L, limit)
  }
  write_atomic <- function(value, path) {
    if (!dir.exists(dirname(path)) && !dir.create(dirname(path), recursive = TRUE, mode = "0700")) {
      stop("Could not create the local Knowledge Base directory.", call. = FALSE)
    }
    temp <- tempfile(".pending-", tmpdir = dirname(path))
    on.exit(unlink(temp), add = TRUE)
    jsonlite::write_json(value, temp, auto_unbox = TRUE, null = "null", digits = NA)
    if(file.info(temp)$size>64*1024^2)stop("Extracted reference text is too large to save. Use a smaller file.",call.=FALSE)
    Sys.chmod(temp, "0600")
    if (!file.rename(temp, path)) stop("Could not save the Knowledge Base.", call. = FALSE)
  }
  read_doc <- function(path) {
    size <- file.info(path)$size
    if (is.na(size) || size > 64 * 1024^2) stop("Invalid reference document size.", call. = FALSE)
    doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    if (!identical(doc$schema, 1L) || !identical(doc$project, project) || !valid_id(doc$id) ||
        !is.character(doc$name) || !is.list(doc$chunks)) {
      stop("Invalid reference document.", call. = FALSE)
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
    added_at = doc$added_at, pages = doc$pages, chunks = length(doc$chunks),legacy=!identical(doc$format_version,2L))
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
    if (length(same) && identical(same[[1L]]$format_version,2L))return(c(summary_of(same[[1L]]),list(status="duplicate")))
    if(length(same)){name<-same[[1L]]$name;extension<-tolower(tools::file_ext(name))}
    extracted <- .knowledge_extract(path, extension)
    if (!length(extracted$chunks)) {
      stop(if (identical(extension, "pdf")) "No text could be extracted. Scanned PDFs need OCR before they can be added."
        else "The file contains no usable text.", call. = FALSE)
    }
    if (length(extracted$chunks) > 5000L) stop("The file is too large to index (5,000 sections max).", call. = FALSE)
    doc <- list(schema = 1L, format_version=2L, project = project, id = id, name = name,
      kind = unname(.knowledge_kinds[[extension]]), pages = extracted$pages,
      added_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), chunks = extracted$chunks)
    write_atomic(doc, file.path(docs_dir, paste0(id, ".json")))
    # Re-adding a file with the same name replaces the earlier version.
    replaced <- Filter(function(old) identical(tolower(old$name), tolower(name)), existing)
    for (old in replaced)if(!identical(old$id,id))unlink(file.path(docs_dir,paste0(old$id,".json")))
    docs <<- c(Filter(function(old) !identical(tolower(old$name), tolower(name)), existing), list(doc))
    index <<- NULL
    c(summary_of(doc),list(status=if(length(same))"reindexed"else if(length(replaced))"replaced"else"added"))
  }
  remove_doc <- function(id) {
    if (!valid_id(id)) stop("Invalid reference document identifier.", call. = FALSE)
    path <- file.path(docs_dir, paste0(id, ".json"))
    if (file.exists(path) && unlink(path) != 0L) stop("Could not remove the reference document.", call. = FALSE)
    docs <<- Filter(function(doc) !identical(doc$id, id), load_docs())
    index <<- NULL
    invisible(NULL)
  }
  # One optional text holds the custom instructions. Version 1 files kept four
  # fields; they are combined on reading, and rewritten only when saved.
  read_profile <- function() {
    if (!file.exists(profile_path)) return(list(instructions = ""))
    value <- tryCatch(jsonlite::fromJSON(profile_path, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.list(value) || !identical(value$project, project)) return(list(instructions = ""))
    legacy <- c(packages = "Preferred R packages: ", functions = "Preferred R functions: ",
      conventions = "Coding conventions:\n")
    parts <- c(text_value(value$instructions, 20000L), vapply(names(legacy), function(field) {
      text <- text_value(value[[field]], 4000L)
      if (nzchar(text)) paste0(legacy[[field]], text) else ""
    }, character(1)))
    list(instructions = paste(parts[nzchar(parts)], collapse = "\n\n"))
  }
  save_profile <- function(profile) {
    value <- list(instructions = text_value(profile$instructions, 20000L))
    write_atomic(c(list(schema = 2L, project = project), value,
      list(updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))), profile_path)
    invisible(value)
  }
  profile_text <- function() read_profile()$instructions
  build_index <- function() {
    chunks <- unlist(lapply(load_docs(), function(doc) lapply(doc$chunks, function(chunk)
      list(id=doc$id,name=doc$name,label=chunk$label,text=chunk$text,
        heading=if(is.character(chunk$heading) && length(chunk$heading)==1L)chunk$heading else"",
        legacy=!identical(doc$format_version,2L)))), recursive=FALSE)
    tokens <- lapply(chunks,function(chunk).knowledge_tokens(paste(chunk$heading,chunk$text)))
    lengths <- lengths(tokens)
    postings <- split(rep(seq_along(tokens),lengths),unlist(tokens,use.names=FALSE))
    list(chunks=chunks,lengths=lengths,average=max(1,mean(lengths)),postings=postings)
  }
  retrieve <- function(query, context="", budget=getOption("RAssistant.knowledge_chars",6000L),
      max_chunks=5L, selected=character(), work=list()) {
    documents <- load_docs()
    selected <- unique(as.character(selected))
    missing <- setdiff(selected,vapply(documents,function(doc)doc$id,character(1)))
    none <- list(text="",sources=list(),status=if(!length(documents))"empty"else"none",
      mode=if(length(selected))"selected"else"automatic",missing=missing,notices=character())
    if(length(missing)){none$status<-"missing";return(none)}
    if(!length(documents))return(none)
    budget <- suppressWarnings(as.integer(budget));max_chunks <- suppressWarnings(as.integer(max_chunks))
    if(length(budget)!=1L || is.na(budget) || budget<1L || length(max_chunks)!=1L || is.na(max_chunks) || max_chunks<1L)return(none)
    budget <- min(budget,24000L);max_chunks <- min(max_chunks,5L)
    if(is.null(index))index <<- build_index()
    n <- length(index$chunks);if(!n)return(none)
    scope <- if(length(selected))which(vapply(index$chunks,function(x)x$id %in% selected,logical(1)))else seq_len(n)
    if(!length(scope))return(none)
    weights <- c(table(.knowledge_tokens(query)))
    signals <- list()
    if(!is.list(work))work <- list()
    for(field in c("functions","packages","console","columns")) {
      value <- work[[field]]
      if(is.character(value))signals[[field]] <- unique(.knowledge_tokens(substr(paste(value,collapse=" "),1L,4000L)))
    }
    signal_terms <- unique(unlist(signals,use.names=FALSE))
    anchors <- NULL
    if(length(signal_terms)) {
      topics <- setdiff(.knowledge_query_topics(query),.knowledge_tokens(work$datasets))
      if(length(topics))anchors <- topics else {
        focus <- if(identical(work$focus,"console"))"console"else if(identical(work$focus,"columns"))"columns"else"source"
        anchors <- if(focus %in% names(signals))signals[[focus]]else unique(c(signals$functions,signals$packages,signals$columns))
        if(!length(anchors))anchors <- signal_terms
      }
      # Context may rank results or supply a missing topic for "this error".
      # It must not turn a specific unrelated question into an editor search.
      anchors <- intersect(anchors,names(index$postings))
      if(!length(anchors) && !length(selected))return(none)
    } else if(length(weights) && !any(names(weights) %in% names(index$postings)) && !length(selected))return(none)
    extra <- 0.4*c(table(.knowledge_tokens(context)))
    for(term in names(extra))weights[term] <- sum(weights[term],extra[[term]],na.rm=TRUE)
    strengths <- c(functions=0.55,packages=0.8,console=1.2,columns=0.7)
    for(field in names(signals))for(term in signals[[field]])weights[term] <- sum(weights[term],strengths[[field]],na.rm=TRUE)
    weights <- pmin(weights[names(weights) %in% names(index$postings)],3)
    scores <- numeric(n);primary_scores <- numeric(n);matched <- integer(n)
    for(term in names(weights)) {
      runs <- rle(index$postings[[term]]);idf <- log(1+(n-length(runs$values)+0.5)/(length(runs$values)+0.5))
      norm <- 1.2*(0.25+0.75*index$lengths[runs$values]/index$average)
      scores[runs$values] <- scores[runs$values]+weights[[term]]*idf*runs$lengths*2.2/(runs$lengths+norm)
      if(term %in% anchors)primary_scores[runs$values] <- primary_scores[runs$values]+idf*runs$lengths*2.2/(runs$lengths+norm)
      matched[runs$values] <- matched[runs$values]+1L
    }
    # The actual topic determines relevance; auxiliary context can refine its
    # rank by at most 30%, never outweigh a clearly stronger topic match.
    if(!is.null(anchors))scores <- primary_scores*(1+0.3*scores/(1+scores))
    if(length(selected)) {
      # Explicit files always contribute; a short question must not silently
      # fall back to other documents or drop a selected file for lack of words.
      first <- vapply(utils::head(selected,max_chunks),function(id){
        candidates <- scope[vapply(index$chunks[scope],function(x)identical(x$id,id),logical(1))]
        candidates[order(scores[candidates],decreasing=TRUE)][[1L]]
      },integer(1))
      relevant <- scope[scores[scope]>0]
      eligible <- unique(c(first,relevant[order(scores[relevant],decreasing=TRUE)]))
    } else {
      if(!is.null(anchors)) {
        anchored <- unique(unlist(index$postings[anchors],use.names=FALSE))
        scope <- intersect(scope,anchored)
      }
      strong <- if(!is.null(anchors))scope[primary_scores[scope]>0]else
        scope[scores[scope]>0 & matched[scope]>=min(2L,length(weights))]
      if(!length(strong))return(none)
      top <- max(scores[scope]);eligible <- scope[scores[scope]>=0.35*top &
        (scope %in% strong | scores[scope]>=0.5*top)]
      eligible <- eligible[order(scores[eligible],decreasing=TRUE)]
    }
    excerpts <- list();used <- 0L;sent <- integer();limited <- integer()
    header <- function(chunk,number)paste0("[K",number,"] ",chunk$name,", ",chunk$label,"\n",
      if(nzchar(chunk$heading))paste0("Section: ",chunk$heading,"\n")else"")
    admit <- function(i,fair=FALSE) {
      chunk <- index$chunks[[i]]
      prefix <- header(chunk,length(excerpts)+1L)
      represented <- vapply(excerpts,function(x)x$id,character(1))
      remaining_files <- if(fair)max(1L,length(setdiff(selected,represented)))else 1L
      room <- floor((budget-used)/remaining_files)-nchar(prefix)-if(length(excerpts))2L else 0L
      if(nchar(chunk$text)>room){limited <<- unique(c(limited,i));return(FALSE)}
      excerpts[[length(excerpts)+1L]] <<- list(id=chunk$id,ref=paste0("K",length(excerpts)+1L),name=chunk$name,
        label=chunk$label,text=chunk$text,prefix=prefix)
      used <<- used+nchar(prefix)+nchar(chunk$text)+if(length(excerpts)>1L)2L else 0L
      sent <<- c(sent,i)
      TRUE
    }
    # Give each selected file a first chance, then reconsider deferred blocks
    # with the actual remaining budget. No block is shortened to make it fit.
    if(length(selected))for(i in first) {
      if(length(excerpts)>=max_chunks)break
      admit(i,TRUE)
    }
    candidates <- eligible
    if(length(selected)) {
      represented <- vapply(excerpts,function(x)x$id,character(1))
      missing_files <- setdiff(selected,represented)
      preferred <- eligible[vapply(index$chunks[eligible],function(x)x$id %in% missing_files,logical(1))]
      # An explicitly selected file may contribute a shorter complete section
      # when its best section cannot fit; do not fill space with unrelated
      # sections of files already represented.
      fallback <- scope[vapply(index$chunks[scope],function(x)x$id %in% missing_files,logical(1))]
      candidates <- unique(c(preferred,fallback[order(scores[fallback],decreasing=TRUE)],eligible))
    }
    for(i in setdiff(candidates,sent)) {
      if(length(excerpts)>=max_chunks)break
      admit(i)
    }
    limited <- setdiff(limited,sent)
    notices <- unique(c(vapply(utils::head(limited,5L),function(i){
      chunk <- index$chunks[[i]]
      paste0(chunk$name," (",chunk$label,"): complete block did not fit; not sent.")
    },character(1)),vapply(Filter(function(x)x$legacy,index$chunks[sent]),function(x)
      paste0(x$name,": older formatting; re-add the file to preserve its full structure."),character(1))))
    none$notices <- utils::head(notices,5L)
    if(!length(excerpts)){if(length(limited))none$status<-"limit";return(none)}
    list(text=paste(vapply(excerpts,function(x)paste0(x$prefix,x$text),character(1)),collapse="\n\n"),
      sources=lapply(excerpts,function(x)x[c("ref","name","label")]),status="matched",mode=none$mode,missing=missing,
      notices=none$notices)
  }
  list(list = list_docs, add = add_doc, remove = remove_doc, profile = read_profile,
    save_profile = save_profile, profile_text = profile_text, retrieve = retrieve, path = root)
}
