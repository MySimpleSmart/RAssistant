# Fresh local work hints affect retrieval, without executing Source or reading
# dataset cells; the existing Console/Workspace switches apply to search too.
source("tests/helpers.R")
checks <- 0L
check <- function(ok,label){if(!isTRUE(ok))stop(label,call.=FALSE);checks<<-checks+1L;cat("PASS",label,"\n")}
app <- load_assistant()
terms <- app$knowledge_source_terms(paste(
  '# library(fakeComment)', 'text <- "library(fakeString)"',
  'library(ggplot2)', 'require(package="dplyr")', 'requireNamespace("broom",quietly=TRUE)',
  'stats::lm(y ~ x)', 'library(dynamic,character.only=TRUE)', 'requireNamespace(dynamic)',sep="\n"))
check(setequal(terms$packages,c("ggplot2","dplyr","broom","stats")),
  "package hints come from literal imports and namespaces, excluding comments, strings and dynamic names")
check("lm" %in% terms$functions && !"fakeString" %in% terms$functions,
  "function hints come from parsed code rather than examples in quoted strings")
partial <- app$knowledge_source_terms('library(ggplot2)\nstats::lm(y ~ x)\nbroken <- (')
check(setequal(partial$packages,c("ggplot2","stats")) && "lm" %in% partial$functions,
  "a later syntax error does not discard earlier package and function hints")
qmd <- paste('---','title: library(fakeYaml)','---','Prose: library(fakeProse)',
  '```{python}','library(fakePython)','```','```{r}','library(tidyr)','```',sep="\n")
check(identical(app$knowledge_source_terms(qmd,"report.qmd")$packages,"tidyr"),
  "Quarto search hints use R chunks without treating prose, YAML or another language as R")
check(!exists("knowledge_executed",envir=.GlobalEnv,inherits=FALSE) &&
  "ggplot2" %in% app$knowledge_source_terms('knowledge_executed <<- TRUE; library(ggplot2)')$packages &&
  !exists("knowledge_executed",envir=.GlobalEnv,inherits=FALSE),"preparing search hints never evaluates Source")
old_parse <- options(keep.parse.data=FALSE)
forced <- app$knowledge_source_terms("library(ggplot2)")
check(identical(forced$packages,"ggplot2") && identical(getOption("keep.parse.data"),FALSE),
  "package parsing works with disabled global parse data and restores the user's option")
options(old_parse)

old <- c("Error: obsolete_error_sentinel","[End failed Console task; full command may be unavailable]")
latest <- c("numeric_output_sentinel", "[Completed Console command for preceding output]",
  "> ggplot2::ggplot(birth_data)","[End Console task]")
console <- app$knowledge_console_terms(paste(c(old,latest),collapse="\n"))
check(grepl("ggplot2",console$text,fixed=TRUE) && !grepl("obsolete_error_sentinel",console$text,fixed=TRUE) &&
  !grepl("numeric_output_sentinel",console$text,fixed=TRUE),
  "only the latest completed Console task supplies hints, without arbitrary printed values")
error <- "Error in arithmetic(): non-numeric argument to binary operator\n[End failed Console task; full command may be unavailable]"
check(grepl("non-numeric argument",app$knowledge_console_terms(error)$text,fixed=TRUE),
  "captured error messages supply local search hints even without a full command")

data_env <- new.env(parent=emptyenv())
data_env$birth_data <- data.frame(gestage=40,birthwt=3200,password="cell_private_sentinel")
data_env$other_data <- data.frame(unrelated_column="other_cell_sentinel")
touched <- 0L
makeActiveBinding("active_data",function(){touched<<-touched+1L;stop("must not run")},data_env)
delayedAssign("lazy_data",{touched<<-touched+1L;stop("must not run")},assign.env=data_env)
`[.knowledge_scan_test` <- function(...)stop("must not subset an object")
class(data_env$birth_data) <- c("knowledge_scan_test","data.frame")
work <- app$knowledge_work_context("Explain these columns",source="identity(birth_data)",workspace=TRUE,env=data_env)
check(grepl("gestage",work$columns,fixed=TRUE) && grepl("birthwt",work$columns,fixed=TRUE) &&
  !grepl("password|private_sentinel|other_cell|unrelated_column",paste(work,collapse=" ")) && touched==0L,
  "workspace hints read relevant column names without cells, credential fields, active bindings, promises or subset methods")
manual <- app$knowledge_work_context("Explain these columns",workspace=TRUE,selected="other_data",env=data_env)
check(grepl("unrelated_column",manual$columns,fixed=TRUE),"a manually selected dataset supplies its column names")
check(!nzchar(app$knowledge_work_context("Explain this",workspace=TRUE,env=data_env)$columns),
  "unreferenced Global Environment datasets do not fill the search with unrelated columns")
check(!nzchar(app$knowledge_work_context("birth_data",workspace=FALSE,selected="birth_data",env=data_env)$columns),
  "Workspace Off excludes even manually selected datasets from search hints")

main <- function() {
  root <- tempfile("ra-knowledge-work-");dir.create(root)
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  stores <- new.env(parent=globalenv())
  for(file in c("R/history.R","R/knowledge.R"))sys.source(file,stores)
  knowledge <- stores$.knowledge_store(project=root,directory=file.path(root,"local"))
  add <- function(name,text){path<-file.path(root,name);writeLines(text,path);knowledge$add(path)}
  pkg <- add("graphics.md","The ggplot2 package uses layers to draw statistical graphics. Add geom_smooth for a fitted trend.")
  columns <- add("birth.md","The gestage column records gestational age in weeks; birthwt records birth weight in grams. Check gestage before interpreting birthwt.")
  add("arithmetic.md","A non-numeric argument to binary operator means arithmetic received character input. Inspect typeof and convert with as.numeric before arithmetic.")
  add("lipids.md","The triglycerides and HDL columns measure blood lipids. Inspect triglycerides and HDL units before interpreting clinical measurements.")
  add("regression.md","Linear regression estimates coefficients using lm. Interpret the fitted linear regression coefficients and examine residual diagnostics.")
  sources <- function(found)vapply(found$sources,function(x)x$name,character(1))
  check(identical(sources(knowledge$retrieve("Explain my code",work=list(packages="ggplot2"))),"graphics.md"),
    "a package mentioned only in Source can retrieve its reference guide for a generic code question")
  found <- knowledge$retrieve("Энэ алдаа юунаас болоод байна?",work=list(console=error,functions="lm",focus="console"))
  check(identical(sources(found),"arithmetic.md"),"a Mongolian question about this error uses the actual English diagnostic without translating the query")
  check(identical(sources(knowledge$retrieve("Explain these columns",work=work)),"birth.md"),
    "actual dataset column names can retrieve the matching data dictionary")
  named <- app$knowledge_work_context("Explain birth_data",workspace=TRUE,env=data_env)
  check(identical(sources(knowledge$retrieve("Explain birth_data",work=named)),"birth.md"),
    "a live dataset's local object name can select a dictionary that only names its columns")
  noisy <- list(functions=paste(rep("ggplot",300L),collapse=" "),packages="ggplot2",columns="gestage birthwt",console=error)
  check(identical(sources(knowledge$retrieve("Explain linear regression",work=noisy)),"regression.md"),
    "a specific new topic stays authoritative despite unrelated package, dataset and Console hints")
  check(!nzchar(knowledge$retrieve("Translate hello into French",context="linear regression",work=noisy)$text) &&
    !nzchar(knowledge$retrieve("Explain volcanology errors",work=noisy)$text),
    "neither old task text nor a generic error word can override an unrelated requested topic")
  check(identical(sources(knowledge$retrieve("Explain linear regression",selected=pkg$id,work=noisy)),"graphics.md"),
    "manual Knowledge Base selection still restricts files when work hints point elsewhere")
  check(nchar(knowledge$retrieve("Explain this",work=noisy,budget=250L)$text)<=250L,
    "contextual retrieval keeps the complete source headers and text inside the existing budget")

  enabled <- FALSE;console_reads <- 0L;catalog_reads <- 0L;calls <- list();captured <- error
  capture <- list(start=function(){enabled<<-TRUE},stop=function(){enabled<<-FALSE;TRUE},
    status=function()list(enabled=enabled,ready=TRUE,pending=FALSE,notice="",lines=1L),
    snapshot=function(redact){console_reads<<-console_reads+1L;captured})
  history <- stores$.conversation_store(project=root,directory=file.path(root,"local"))
  app <- load_assistant(package_info=list(version="test",history=history,knowledge=knowledge,
    console=capture,prepare_console=function()invisible(NULL)))
  app$initial <- list(id="work-source",path=file.path(root,"analysis.R"),contents="library(ggplot2)")
  app$active_doc <- function(id=NULL)app$initial
  catalog <- app$workspace_catalog
  app$workspace_catalog <- function(env=data_env){catalog_reads<<-catalog_reads+1L;catalog(data_env)}
  app$workspace_summary <- function(...)"Workspace metadata fixture."
  app$workspace_data_context <- function(...)""
  app$ask_ai <- function(system_prompt,prompt,images=character()){
    calls[[length(calls)+1L]]<<-list(system=system_prompt,prompt=prompt);"Reference received."
  }
  shiny::testServer(app$server,{
    session$flushReact();session$setInputs(console_context=FALSE,workspace_context=FALSE)
    send <- function(text,nonce)session$setInputs(chat_submit=list(text=text,nonce=nonce))
    refs <- function()vapply(tail(state$messages,1L)[[1L]]$references,function(x)x$name,character(1))
    before <- catalog_reads
    send("Explain my code",1L)
    check(!length(refs()) && length(calls)==1L && catalog_reads==before,
      "matching Source hints do not retrieve library files without selection")
    session$setInputs(knowledge_pin_change=list(ids=list(pkg$id),epoch=state$upload_epoch,nonce=2L))
    send("Explain my code",2L)
    check(identical(refs(),"graphics.md"),"selected references are supplied without an extra AI search request")
    session$setInputs(knowledge_unpin=list(id=pkg$id,epoch=state$upload_epoch,nonce=3L))
    app$initial$contents <- "identity(birth_data)"
    session$setInputs(workspace_context=TRUE)
    send("Explain these columns",3L)
    check(!length(refs()),"workspace column matches cannot silently reactivate document retrieval")
    session$setInputs(knowledge_pin_change=list(ids=list(columns$id),epoch=state$upload_epoch,nonce=4L))
    send("Explain these columns",4L)
    check(identical(refs(),"birth.md"),"selected data dictionaries remain usable with live Workspace context")
    session$setInputs(knowledge_unpin=list(id=columns$id,epoch=state$upload_epoch,nonce=5L))
    session$setInputs(workspace_context=FALSE,console_context=TRUE)
    send("Энэ алдаа юунаас болоод байна?",5L)
    check(!length(refs()) && console_reads==1L,
      "Console remains readable while matching reference documents are unselected")
    session$setInputs(console_context=FALSE)
    before <- console_reads;send("Энэ алдаа юунаас болоод байна?",6L)
    check(!length(refs()) && console_reads==before,"Console Off stops capture reads")
    check(length(calls)==6L,"six user sends make exactly six model calls")
    snapshots <- app$snapshot_messages(state$messages)
    check(!grepl("non-numeric argument",paste(unlist(lapply(snapshots,function(x)x$text)),collapse=" ")) &&
      !any(vapply(snapshots,function(x)"work" %in% names(x),logical(1))),
      "search hints and raw Console diagnostics are not added as conversational history or stored work metadata")
  })
}
main()
cat(checks,"work-aware knowledge checks passed\n")
