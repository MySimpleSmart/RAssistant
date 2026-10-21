# Reference text retains complete structural units at extraction and retrieval.
# Uses synthetic files; parsing examples must never run them.
library(RAssistant)
main <- function() {
  chunks <- getFromNamespace(".knowledge_chunks","RAssistant")
  extract <- getFromNamespace(".knowledge_extract","RAssistant")
  factory <- getFromNamespace(".knowledge_store","RAssistant")
  root <- tempfile("ra-structure-");dir.create(root)
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  checks <- 0L
  check <- function(ok,label){if(!isTRUE(ok))stop(label,call.=FALSE);checks<<-checks+1L;cat("PASS",label,"\n")}
  label <- function(a,b)if(a==b)paste("line",a)else paste0("lines ",a,"-",b)
  containing <- function(parts,text)Filter(function(x)grepl(text,x$text,fixed=TRUE),parts)
  fence <- c("```{r model}","fit <- function(x) {", "  centered <- x - mean(x)","",
    "  centered / sd(x)","}","```")
  lines <- c("# Analysis","",strrep("Background sentence. ",40L),"","## Standardization","",fence,
    "","A short explanation after the complete example.")
  parts <- chunks(lines,label,size=100L)
  code <- containing(parts,"centered <-")[[1L]]
  check(grepl(paste(fence,collapse="\n"),code$text,fixed=TRUE) &&
    grepl("\n  centered / sd(x)",code$text,fixed=TRUE),"a fenced R block retains its delimiters, indentation and internal blank lines beyond the target chunk size")
  check(identical(code$heading,"Analysis / Standardization"),"nested section headings remain associated with a later code block")
  check(all(vapply(parts,function(x)isTRUE(x$whole),logical(1))),"all extracted chunks consist of complete structural units")
  nested <- c("````markdown","```r","x <- 1","```","````")
  check(identical(chunks(nested,label,size=8L)[[1L]]$text,paste(nested,collapse="\n")),
    "an inner triple fence cannot close a longer surrounding code fence")
  tilde <- c("~~~r","f <- function() {","","  1","}","~~~")
  check(identical(chunks(tilde,label,size=8L)[[1L]]$text,paste(tilde,collapse="\n")),"tilde fences are also kept whole")
  indent <- c("        f <- function(x) {","","          x + 1","        }")
  check(identical(chunks(indent,label,size=8L)[[1L]]$text,paste(indent,collapse="\n")),
    "indented code preserves more than four spaces and internal empty lines")
  table <- c("| Variable | Meaning |","| :--- | ---: |","| gestage | Weeks |","| birthwt | Grams |")
  check(identical(chunks(table,label,size=15L)[[1L]]$text,paste(table,collapse="\n")),
    "Markdown table headers, alignment markers and all rows stay in one chunk")
  grid <- c("Variable        Value","gestage            40","birthwt          3200")
  check(identical(chunks(grid,label,size=15L,format="pdf")[[1L]]$text,paste(grid,collapse="\n")),
    "text-layer PDF table column spacing is retained exactly")
  dollar <- c("$$","\\hat{beta} = (X^T X)^{-1} X^T y","","\\mathrm{Var}(\\hat{beta}) = \\sigma^2 (X^T X)^{-1}","$$")
  check(identical(chunks(dollar,label,size=15L)[[1L]]$text,paste(dollar,collapse="\n")),
    "a display equation retains its delimiters and complete multiline formula")
  bracket <- c("\\[","f(x) = x^2","","g(x) = x^3","\\]")
  check(identical(chunks(bracket,label,size=8L)[[1L]]$text,paste(bracket,collapse="\n")),
    "bracket-delimited display math is not split at blank lines")
  align <- c("\\begin{align*}","a &= b + c \\\\","","d &= e + f","\\end{align*}")
  check(identical(chunks(align,label,size=8L)[[1L]]$text,paste(align,collapse="\n")),
    "a LaTeX alignment environment is kept as a complete equation block")
  inline <- c("An inline formula $y = x^2 + 1$ belongs to this paragraph.","Its explanation follows on another line.")
  check(identical(chunks(inline,label,size=15L)[[1L]]$text,paste(inline,collapse="\n")),
    "paragraph boundaries protect inline formulas and their accompanying explanation")
  yaml <- c("---","title: Reference","format: html","---","# Model","","Use the supplied model example.")
  check(length(chunks(yaml,label))==1L && identical(chunks(yaml,label)[[1L]]$text,paste(yaml,collapse="\n")),
    "YAML front matter stays intact and attaches to the first section")
  setext <- chunks(c("Models","======","","Regression","----------","","A complete reference paragraph."),label)
  check(identical(setext[[1L]]$heading,"Models / Regression"),"Setext headings retain their hierarchy")
  short <- chunks(c("$$","y=x^2","$$"),label)
  check(length(short)==1L && grepl("y=x^2",short[[1L]]$text,fixed=TRUE),"a short useful formula is not discarded by a minimum character threshold")
  unclosed <- c("```r","f <- function(x) {","  x + 1")
  check(identical(chunks(unclosed,label,size=8L)[[1L]]$text,paste(unclosed,collapse="\n")),
    "an incomplete reference example is retained as supplied without inventing closing code")

  code_path <- file.path(root,"example.R")
  raw_code <- c("# Keep this example","f <- function(x) {",'  note <- "line one',"",'line three"',
    "  x + 1","}","","knowledge_execution_sentinel <<- TRUE", "","answer <- f(2)")
  writeLines(raw_code,code_path)
  code_parts <- chunks(raw_code,label,size=20L,format="r")
  function_part <- containing(code_parts,"f <- function")[[1L]]
  check(grepl(paste(raw_code[1:7],collapse="\n"),function_part$text,fixed=TRUE),
    "R files split only between complete top-level expressions, preserving multiline strings and function bodies")
  check(!exists("knowledge_execution_sentinel",envir=.GlobalEnv,inherits=FALSE),"extracting R expression boundaries never evaluates the code")
  malformed <- c("x <- 1","","f <- function(x) {","","  x +")
  bad_parts <- chunks(malformed,label,size=8L,format="r")
  check(identical(tail(bad_parts,1L)[[1L]]$text,paste(malformed[3:5],collapse="\n")),
    "a malformed trailing R expression stays together instead of losing lines after a parse error")
  long_line <- paste0('long_value <- "',strrep("a",3000L),'"')
  writeLines(long_line,code_path)
  check(identical(extract(code_path,"r")$chunks[[1L]]$text,long_line),"a line longer than 2,000 characters is no longer silently truncated")

  # Override only this copy's PDF extractor, keeping the real extraction and
  # chunking implementation and avoiding dependency on PDF font rendering.
  pdf_extract <- extract
  pdf_env <- new.env(parent=environment(extract));environment(pdf_extract) <- pdf_env
  pages <- c(paste(c("MODELS","",grid),collapse="\n"),paste(c("",dollar),collapse="\n"))
  pdf_env$.knowledge_pdf_pages <- function(path)pages
  pdf_parts <- pdf_extract("fixture.pdf","pdf")$chunks
  check(identical(containing(pdf_parts,"gestage")[[1L]]$text,paste(c("MODELS","",grid),collapse="\n")) &&
    identical(containing(pdf_parts,"gestage")[[1L]]$label,"p. 1"),"PDF extraction preserves table layout and the exact source page")
  check(identical(containing(pdf_parts,"hat{beta}")[[1L]]$label,"p. 2") &&
    identical(containing(pdf_parts,"hat{beta}")[[1L]]$heading,"MODELS"),"a subsequent PDF page retains its page label and preceding section heading")
  pages <- c("```r\nf <- function(x) {", "  x + 1\n}\n```")
  across <- pdf_extract("fixture.pdf","pdf")$chunks
  check(length(across)==1L && identical(across[[1L]]$label,"p. 1-2") && endsWith(across[[1L]]$text,"```"),
    "a fenced block crossing PDF pages stays whole and cites the full page range")

  store <- factory(project=root,directory=file.path(root,"data"))
  structured <- file.path(root,"reference.md");writeLines(lines,structured)
  added <- store$add(structured)
  found <- store$retrieve("Standardization centered")
  check(!added$legacy && grepl("Section: Analysis / Standardization",found$text,fixed=TRUE) &&
    grepl(paste(fence,collapse="\n"),found$text,fixed=TRUE),"retrieval includes the heading context and complete original code block")
  tight <- store$retrieve("Standardization centered",budget=80L)
  check(!nzchar(tight$text) && identical(tight$status,"limit") && length(tight$notices)>0L,
    "a block that cannot fit is reported as omitted rather than cut through code")
  huge <- file.path(root,"huge.md")
  huge_lines <- c("```r",paste0('oversize_sentinel <- "',strrep("b",7000L),'"'),"```")
  huge_doc <- store$add({writeLines(huge_lines,huge);huge})
  too_big <- store$retrieve("oversize_sentinel",selected=huge_doc$id)
  check(!nzchar(too_big$text) && identical(too_big$status,"limit") && !length(too_big$sources),
    "a selected block larger than the default budget is never partially sent")
  enlarged <- store$retrieve("oversize_sentinel",selected=huge_doc$id,budget=10000L)
  check(grepl(paste(huge_lines,collapse="\n"),enlarged$text,fixed=TRUE) && nchar(enlarged$text)<=10000L,
    "a larger configured budget admits the complete block without a truncation marker")
  large <- file.path(root,"large.md");small <- file.path(root,"small.md")
  writeLines(c("```r",paste0('large_sentinel <- "',strrep("c",3200L),'"'),"```"),large)
  writeLines(c("```r",paste0('small_sentinel <- "',strrep("d",800L),'"'),"```"),small)
  a <- store$add(large);b <- store$add(small)
  both <- store$retrieve("Explain this",selected=c(a$id,b$id))
  check(length(both$sources)==2L && nchar(both$text)<=6000L && !length(both$notices) &&
    grepl(strrep("c",3200L),both$text,fixed=TRUE) && grepl(strrep("d",800L),both$text,fixed=TRUE),
    "deferred large blocks are reconsidered after smaller selected files leave sufficient room")
  fallback_file <- file.path(root,"fallback.md")
  writeLines(c(huge_lines,"","A complete short explanation for the selected file."),fallback_file)
  fallback_doc <- store$add(fallback_file)
  peers <- lapply(1:4,function(i){
    path <- file.path(root,paste0("peer-",i,".md"))
    writeLines(c(paste0("oversize_sentinel peer",i," ",strrep("first reference paragraph. ",27L)),"",
      paste0("oversize_sentinel peer",i," ",strrep("second reference paragraph. ",27L))),path)
    store$add(path)
  })
  selected <- c(fallback_doc$id,vapply(peers,function(x)x$id,character(1)))
  spread <- store$retrieve("oversize_sentinel",selected=selected)
  check(length(spread$sources)==5L && length(unique(vapply(spread$sources,function(x)x$name,character(1))))==5L &&
    grepl("A complete short explanation",spread$text,fixed=TRUE),
    "a shorter section from an unrepresented selected file precedes extra sections of files already supplied")

  # Simulate a document saved before structural extraction was available.
  path <- file.path(store$path,"documents",paste0(added$id,".json"))
  legacy <- jsonlite::fromJSON(path,simplifyVector=FALSE)
  legacy$format_version <- NULL
  legacy$chunks <- list(list(label="lines 1-15",text="Standardization centered legacy_example: earlier formatting was flattened."))
  jsonlite::write_json(legacy,path,auto_unbox=TRUE)
  old <- factory(project=root,directory=file.path(root,"data"))
  check(isTRUE(Filter(function(x)x$id==added$id,old$list())[[1L]]$legacy) &&
    grepl("older formatting",paste(old$retrieve("legacy_example")$notices,collapse=" "),fixed=TRUE),
    "older documents remain searchable and explicitly identify their formatting limitation")
  upgraded <- old$add(structured)
  check(identical(upgraded$status,"reindexed") && identical(upgraded$id,added$id) && !upgraded$legacy && file.exists(path),
    "re-adding the same bytes rebuilds formatting without changing the id or deleting the new snapshot")
  check(identical(old$add(structured)$status,"duplicate"),"re-adding an already structured file still reports a duplicate")
  restarted <- factory(project=root,directory=file.path(root,"data"))
  check(grepl(paste(fence,collapse="\n"),restarted$retrieve("Standardization centered")$text,fixed=TRUE) &&
    !Filter(function(x)x$id==added$id,restarted$list())[[1L]]$legacy,
    "complete formatting and section metadata survive a fresh store instance after reindexing")
  cat("Completed",checks,"structure checks\n")
}
main()
