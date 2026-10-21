# Durable project course knowledge, using temporary directories and synthetic files.
library(RAssistant)
main <- function() {
  factory <- getFromNamespace(".knowledge_store","RAssistant")
  history_factory <- getFromNamespace(".conversation_store","RAssistant")
  root <- tempfile("ra-knowledge-");dir.create(root)
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  project <- file.path(root,"Project A");dir.create(project)
  other <- file.path(root,"Project B");dir.create(other)
  data <- file.path(root,"user data")
  checks <- 0L
  check <- function(ok,description){
    if(!isTRUE(ok))stop(description,call.=FALSE)
    checks <<- checks+1L;cat("PASS",description,"\n")
  }
  store <- factory(project=project,directory=data)
  check(!length(store$list()) && identical(store$retrieve("kmeans clustering")$text,"") &&
    !nzchar(store$profile_text()) && !dir.exists(store$path),
    "an unused knowledge store returns nothing and creates no files")

  notes <- file.path(root,"week4.md")
  writeLines(c("# Week 4: Clustering","",
    "In this unit we run k-means with kmeans(x, centers = k, nstart = 25) after set.seed(123).",
    "Choose the number of clusters k with the elbow method: fviz_nbclust(x, kmeans, method = \"wss\").",
    "",strrep("Filler paragraph about unrelated administration and assessment dates. ",30L),"",
    "# Week 5: Regression","","Fit linear models with lm(y ~ x) and report the adjusted R-squared."),notes)
  code <- file.path(root,"lab5.R")
  writeLines(c("# Lab 5: simple regression","model <- lm(mpg ~ wt, data = mtcars)","summary(model)$adj.r.squared"),code)
  added <- store$add(notes)
  check(identical(added$status,"added") && identical(added$kind,"Notes") && added$chunks>=2L,
    "notes are split into labelled sections when added")
  check(identical(store$add(code)$kind,"R code") && identical(store$add(notes)$status,"duplicate"),
    "R code is accepted and re-adding identical content is reported as a duplicate")
  bad <- file.path(root,"slides.pptx");writeLines("x",bad)
  check(inherits(tryCatch(store$add(bad),error=function(e)e),"error") && length(store$list())==2L,
    "unsupported file types are rejected without changing knowledge")

  found <- store$retrieve("How should I choose k for kmeans clustering?")
  check(grepl("fviz_nbclust",found$text,fixed=TRUE) && grepl("^\\[K1\\] week4\\.md, lines ",found$text) &&
    identical(found$sources[[1L]]$name,"week4.md"),
    "a request retrieves the relevant lecture section with its file and line label")
  check(!grepl("Filler paragraph",found$text,fixed=TRUE) && !grepl("lab5.R",found$text,fixed=TRUE),
    "unrelated sections of the same and other files are not sent")
  check(identical(store$retrieve("What is the capital of France?")$text,""),
    "an unrelated request retrieves no course material")
  regression <- store$retrieve("adjusted r squared for a linear model")
  check(grepl("lab5.R",regression$text,fixed=TRUE) && grepl("adj.r.squared",regression$text,fixed=TRUE),
    "R function names in course code are matched by request terms")
  check(nchar(store$retrieve("kmeans clustering regression model",budget=300L)$text)<=300L,
    "retrieved excerpts and source headers together respect the character budget")
  docs <- store$list()
  cluster_id <- Filter(function(x)identical(x$name,"week4.md"),docs)[[1L]]$id
  code_id <- Filter(function(x)identical(x$name,"lab5.R"),docs)[[1L]]$id
  pinned <- store$retrieve("Explain this",selected=code_id)
  check(identical(pinned$status,"matched") && identical(pinned$mode,"selected") &&
    identical(unique(vapply(pinned$sources,function(x)x$name,character(1))),"lab5.R") &&
    grepl("adj.r.squared",pinned$text,fixed=TRUE),"an explicit document is read for a generic follow-up")
  scoped <- store$retrieve("kmeans clustering",selected=code_id)
  check(!grepl("week4.md",scoped$text,fixed=TRUE) && grepl("lab5.R",scoped$text,fixed=TRUE),
    "selection restricts retrieval even when another file matches better")
  both <- store$retrieve("Explain this",selected=c(cluster_id,code_id),budget=600L)
  check(nchar(both$text)<=600L && length(both$sources)==2L &&
    identical(sort(vapply(both$sources,function(x)x$name,character(1))),c("lab5.R","week4.md")),
    "selected documents share a bounded budget instead of the first file consuming it")
  absent <- store$retrieve("kmeans clustering",selected=c(code_id,strrep("0",32L)))
  check(identical(absent$status,"missing") && !nzchar(absent$text) && identical(absent$missing,strrep("0",32L)),
    "a missing selection cannot silently expand retrieval to other documents")
  check(identical(store$retrieve("zzzzzzz")$status,"none") &&
    !nzchar(store$retrieve("clustering",budget=1L)$text),"no matches and insufficient budgets return no excerpts")
  work <- list(functions="kmeans",packages="factoextra",columns="mpg wt",focus="source")
  check(grepl("fviz_nbclust",store$retrieve("Fix my code",work=work)$text,fixed=TRUE),
    "current function names provide a local topic for a generic code question")
  columns <- store$retrieve("Explain these columns",work=list(columns="mpg wt",focus="columns"))
  check(identical(unique(vapply(columns$sources,function(x)x$name,character(1))),"lab5.R"),
    "live column names retrieve the matching code example without needing the names in the question")
  check(!nzchar(store$retrieve("Translate hello into French",context="kmeans clustering",work=work)$text),
    "work and earlier task hints cannot replace a specific unrelated topic")
  check(grepl("Fit linear models",store$retrieve("linear regression",work=work)$text,fixed=TRUE) &&
    !grepl("fviz_nbclust",store$retrieve("linear regression",work=work)$text,fixed=TRUE),
    "an explicit new topic remains authoritative over active clustering functions")

  # Version 0.3.0 saved four preference fields; they must all survive.
  profile_path <- file.path(store$path,"profile.json")
  jsonlite::write_json(list(schema=1L,project=project,instructions="STAT1170: follow the lecture approach.",
    packages="ggplot2, factoextra",functions="kmeans(), lm()",conventions="Use <- for assignment.",
    updated_at="2026-10-19T00:00:00Z"),profile_path,auto_unbox=TRUE)
  legacy <- readLines(profile_path)
  combined <- store$profile()$instructions
  check(identical(combined,paste("STAT1170: follow the lecture approach.","Preferred R packages: ggplot2, factoextra",
    "Preferred R functions: kmeans(), lm()","Coding conventions:\nUse <- for assignment.",sep="\n\n")) &&
    identical(store$profile_text(),combined) && identical(readLines(profile_path),legacy),
    "earlier preference fields are combined into custom instructions without rewriting the file")
  store$save_profile(list(instructions=combined))
  saved <- jsonlite::fromJSON(profile_path,simplifyVector=FALSE)
  check(identical(saved$schema,2L) && identical(saved$instructions,combined) && is.null(saved$packages) &&
    identical(store$profile()$instructions,combined),"saving stores one custom instructions text with no data loss")

  child <- callr::r(function(project,directory) {
    store <- getFromNamespace(".knowledge_store","RAssistant")(project=project,directory=directory)
    list(names=vapply(store$list(),function(x)x$name,character(1)),profile=store$profile()$instructions,
      text=store$retrieve("choose k for kmeans clustering")$text)
  },args=list(project,data),libpath=.libPaths(),user_profile=FALSE,system_profile=FALSE)
  check(identical(sort(child$names),c("lab5.R","week4.md")) && identical(child$profile,combined) &&
    grepl("fviz_nbclust",child$text,fixed=TRUE),"documents and custom instructions survive a completely new R process")

  revised <- file.path(root,"revised","week4.md");dir.create(dirname(revised))
  writeLines(c("# Week 4 revised","Use hierarchical clustering with hclust(dist(x), method = \"ward.D2\") and cutree."),revised)
  check(identical(store$add(revised)$status,"replaced") && length(store$list())==2L &&
    grepl("hclust",store$retrieve("hierarchical clustering method")$text,fixed=TRUE) &&
    !grepl("fviz_nbclust",store$retrieve("choose k for kmeans clustering elbow")$text,fixed=TRUE),
    "re-adding a file with the same name replaces the earlier version")

  separate <- factory(project=other,directory=data)
  check(!length(separate$list()) && !nzchar(separate$profile_text()) && !identical(separate$path,store$path),
    "different projects have isolated course knowledge")
  history <- history_factory(project=project,directory=data)
  check(!identical(dirname(history$path),dirname(store$path)) && !startsWith(store$path,history$path) &&
    !startsWith(history$path,store$path),"course knowledge is stored separately from chat history")

  target <- Filter(function(x)identical(x$name,"lab5.R"),store$list())[[1L]]$id
  store$remove(target)
  check(length(store$list())==1L && identical(factory(project=project,directory=data)$retrieve("adjusted r squared linear model")$text,""),
    "removing a document deletes it from disk and from retrieval")

  # CI sets this flag so the PDF checks cannot silently skip or use pdftotext.
  if (identical(Sys.getenv("RASSISTANT_REQUIRE_PDFTOOLS"),"true")) {
    check(requireNamespace("pdftools",quietly=TRUE),"pdftools is installed for the required PDF checks")
  }
  pdf_ready <- requireNamespace("pdftools",quietly=TRUE) || nzchar(Sys.which("pdftotext"))
  if (pdf_ready) {
    cat("PDF extraction via",if(requireNamespace("pdftools",quietly=TRUE))"pdftools" else "pdftotext","\n")
    slides <- file.path(root,"Lecture 6 - t tests.pdf")
    grDevices::pdf(slides);plot.new();text(.5,.5,"Two sample t.test with var.equal = TRUE")
    plot.new();text(.5,.5,"Wilcoxon rank sum test with wilcox.test for skewed data");invisible(grDevices::dev.off())
    lecture <- store$add(slides)
    hit <- store$retrieve("wilcoxon rank sum test for skewed data")
    check(identical(lecture$kind,"PDF") && identical(lecture$pages,2L) &&
      grepl("Lecture 6 - t tests.pdf, p. 2",hit$text,fixed=TRUE),"PDF text is retrieved with its page number")
  } else cat("SKIP PDF extraction: install pdftools to test lecture PDFs\n")
  cat("Completed",checks,"knowledge checks\n")
}
main()
