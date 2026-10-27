# Project dependencies are parsed and read, never sourced or executed.
source("tests/helpers.R")
app <- load_assistant();checks <- 0L
check <- function(ok,label){if(!isTRUE(ok))stop(label,call.=FALSE);checks<<-checks+1L;cat("PASS",label,"\n")}
root <- tempfile("dependency-project-");dir.create(root)
dir.create(file.path(root,"R"));dir.create(file.path(root,"R","nested"))
dir.create(file.path(root,"parts"))
put <- function(path,lines){writeLines(lines,file.path(root,path));invisible(file.path(root,path))}
put("analysis.R","DISK_CURRENT_MUST_NOT_REPLACE_BUFFER <- 1")
put("R/helper.R",c('source("R/nested/math.R")',"helper <- function(x) offset(x)"))
put("R/nested/math.R",c("offset <- function(x) {","  x + 7","}"))
source_text <- paste(c('source(file.path("R", "helper.R"))',"helper(3)"),collapse="\n")
scan <- function(source=source_text,path=file.path(root,"analysis.R"),console="",seeds=character(),budget=10000L)
  app$workspace_dependency_context(source,path,console,root,root,seeds,budget)
result <- scan()
check(length(result$files)==2L && grepl("R/nested/math.R",result$text,fixed=TRUE) &&
  grepl("analysis.R:1 --source--> R/helper.R",result$text,fixed=TRUE),"static imports follow a nested dependency beyond the shallow inventory")
check(grepl("analysis.R:2 calls helper() -> R/helper.R:2-2",result$text,fixed=TRUE) &&
  grepl("R/helper.R:2 calls offset() -> R/nested/math.R:1-3",result$text,fixed=TRUE),
  "caller rows map to the exact file and global lines of reachable function definitions")
check(grepl("1 | offset <- function(x) {\n2 |   x + 7\n3 | }",result$text,fixed=TRUE),
  "related function bodies retain their original line numbers and formatting")
check(!grepl("DISK_CURRENT_MUST_NOT_REPLACE_BUFFER",result$text,fixed=TRUE),"the active document always uses the supplied unsaved buffer")
put("R/nested/math.R",c("offset <- function(x) {","  x + 9","}"))
check(grepl("x + 9",scan()$text,fixed=TRUE) && !grepl("x + 7",scan()$text,fixed=TRUE),"each Send rereads changed dependencies rather than caching old definitions")
put("R/local.R",c('source("nested/math.R")',"local_helper <- function(x) offset(x)"))
changed <- scan('source("R/local.R", chdir=TRUE)')
check(grepl("R/nested/math.R (read-only",changed$text,fixed=TRUE),"source(chdir=TRUE) resolves nested relative imports beside the sourced file")
ordinary <- scan('source("R/local.R")')
check(grepl("nested/math.R (not found",ordinary$text,fixed=TRUE) && length(ordinary$files)==1L,
  "source's default working directory is not guessed from the helper's location")
check(length(scan('base::sys.source("R/local.R", envir=.GlobalEnv, chdir=TRUE)')$files)==2L,
  "namespaced sys.source supports its own chdir argument")
check(length(scan('knitr::read_chunk(path="R/helper.R")')$files)==2L,
  "knitr code-file references accept the named path argument without loading chunks")
put("parts/child.Rmd",c("# Child","```{r child}",'source("R/helper.R")',"helper(4)","```"))
rmd <- paste(c("# Main","","```{r main, child='parts/child.Rmd'}","```"),collapse="\n")
child <- scan(rmd,path=file.path(root,"report.Rmd"))
check(grepl("report.Rmd:3 [main] --child--> parts/child.Rmd",child$text,fixed=TRUE) &&
  grepl("1 | # Child",child$text,fixed=TRUE),"Rmd child options follow literal files and preserve global document/chunk locations")
put("R/child.Rmd",c("# Nested","```{r}","z <- 1","```"))
check(length(scan('knitr::knit_child("parts/child.Rmd")')$files)>=1L,"explicit knitr child calls are recognized without knitting")
check(grepl("unresolved dynamic path",scan("source(selected_path)")$text,fixed=TRUE),"dynamic path variables are labelled unresolved without reading their bindings")
sentinel <- file.path(root,"MUST_NOT_EXECUTE")
dynamic <- paste0('source({writeLines("bad", ',encodeString(sentinel,quote='"'),'); "R/helper.R"})')
check(grepl("unresolved dynamic path",scan(dynamic)$text,fixed=TRUE) && !file.exists(sentinel),
  "expressions that could have side effects are never evaluated to discover paths")
check(!length(scan('quote(source("R/helper.R"))\n# source("R/helper.R")\ntext <- "source(\\\"R/helper.R\\\")"')$files),
  "quoted examples, comments and strings do not become dependency imports")
check(!length(scan('source("https://example.invalid/code.R")')$files) &&
  grepl("not fetched",scan('source("https://example.invalid/code.R")')$text,fixed=TRUE),"remote code is described without a network fetch")
check(grepl("unresolved working directory",scan('setwd("R")\nsource("helper.R")')$text,fixed=TRUE) &&
  !length(scan('setwd("R")\nsource("helper.R")')$files),"a preceding setwd prevents a guessed relative import")
check(grepl("unresolved working directory",scan('withr::with_dir("R", source("helper.R"))')$text,fixed=TRUE),
  "temporary working-directory scopes are also marked unresolved")
check(grepl("unresolved chdir",scan('source("R/helper.R", chdir=toggle)')$text,fixed=TRUE),"dynamic chdir settings are not coerced or executed")
check(length(scan('source(here::here("R", "helper.R"))')$files)==2L,
  "literal here paths resolve from the project root without calling here or project code")
put("R/cycle.R",'source("R/cycle.R")')
check(length(scan('source("R/cycle.R")')$files)==1L && grepl("cycle",scan('source("R/cycle.R")')$text,fixed=TRUE),
  "cycles are detected without repeatedly reading or executing a file")
put("R/duplicate.R","helper <- function(x) x * 99")
duplicates <- scan('source("R/helper.R")\nsource("R/duplicate.R")\nhelper(3)')
check(grepl("multiple definitions; runtime binding unresolved",duplicates$text,fixed=TRUE),
  "duplicate function definitions are reported instead of claiming a runtime binding")
index <- app$project_code_index('outer <- function(x) { inner <- function(y) y; inner(x) }\nquote(fake <- function(x) x)',"test.R")
check(identical(vapply(index$definitions,function(x)x$name,character(1)),"outer"),
  "nested local functions and quoted definitions are not advertised as global helpers")
partial <- scan('source("R/helper.R")\nbroken <- (')
check(length(partial$files)==2L && grepl("partial parse",partial$text,fixed=TRUE),"a later syntax error retains earlier complete imports with an explicit partial-parse notice")
put("R/large.R",strrep("x",32001L))
check(!length(scan('source("R/large.R")')$files) && grepl("file size limit",scan('source("R/large.R")')$text,fixed=TRUE),
  "large files are labelled omitted without sending cut-off code")
small <- scan(budget=40L)
check(length(small$files)==2L && !grepl("1 | offset",small$text,fixed=TRUE) && grepl("body omitted",small$text,fixed=TRUE),
  "dependency locations remain available when the shared excerpt budget omits bodies")
put("R/tall.R",c(rep(paste0("# ",strrep("padding",8L)),200L),"wanted <- function(x) {","  x + 12345","}"))
focused <- scan('source("R/tall.R")\nwanted(1)',budget=1200L)
check(grepl("201 | wanted <- function(x) {\n202 |   x + 12345\n203 | }",focused$text,fixed=TRUE) &&
  !grepl("1 | # padding",focused$text,fixed=TRUE),"large modules supply complete relevant function excerpts within the shared budget")
late <- scan(paste(c('source("R/tall.R")',rep("identity(1)",80L),"wanted(1)"),collapse="\n"),budget=1200L)
check(grepl("analysis.R:82 calls wanted() -> R/tall.R:201-203",late$text,fixed=TRUE),
  "unrelated earlier calls do not hide a relevant helper call near the end of Source")
outside <- tempfile(fileext=".R");writeLines("OUTSIDE_SECRET_BODY <- 1",outside)
invisible(file.symlink(outside,file.path(root,"R","escape.R")))
check(!grepl("OUTSIDE_SECRET_BODY",scan('source("R/escape.R")')$text,fixed=TRUE) &&
  !length(scan(source="",seeds=file.path(root,"R","escape.R"))$files),"external symlink targets are excluded for both imports and requested-file seeds")
put("R/secrets.R","PRIVATE_SECRET_BODY <- 1")
dir.create(file.path(root,".hidden"));put(".hidden/helper.R","HIDDEN_BODY <- 1")
dir.create(file.path(root,"renv"));put("renv/helper.R","DEPENDENCY_BODY <- 1")
private <- scan('source("R/secrets.R")\nsource(".hidden/helper.R")\nsource("renv/helper.R")')
check(!length(private$files) && !grepl("PRIVATE_SECRET_BODY|HIDDEN_BODY|DEPENDENCY_BODY",private$text),
  "private filenames, hidden folders and dependency trees stay outside the graph's file reads")
for(i in 1:5)put(paste0("R/depth",i,".R"),paste0('source("R/depth',i+1L,'.R")'))
depth <- scan('source("R/depth1.R")')
check(length(depth$files)==3L && grepl("depth limit (3)",depth$text,fixed=TRUE),"dependency traversal stops after three import levels")
for(i in 1:9)put(paste0("R/file",i,".R"),paste0("v",i," <- ",i))
many <- scan(paste(sprintf('source("R/file%d.R")',1:9),collapse="\n"))
check(length(many$files)==6L && grepl("file limit (6)",many$text,fixed=TRUE),"each request reads at most six dependency files")
console <- scan(source="",console='source("R/helper.R")')
check(grepl("Latest Console command:1 --source--> R/helper.R",console$text,fixed=TRUE),
  "a source import mentioned only in the latest Console command can supply dependencies")

calls <- list();writes <- 0L
original <- app$workspace_project_context
app$workspace_project_context <- function(request,source="",path="",console="",working=root)original(request,source,path,console,working=root)
app$initial <- list(id="fixture",path=file.path(root,"analysis.R"),contents=strsplit(source_text,"\n",fixed=TRUE)[[1L]])
app$active_doc <- function(id=NULL)app$initial
app$write_source <- function(...)writes<<-writes+1L
app$ask_ai <- function(system_prompt,prompt,images=character()){
  calls[[length(calls)+1L]]<<-list(system=system_prompt,prompt=prompt);"The helper definition is in R/helper.R."
}
shiny::testServer(app$server,{
  session$setInputs(workspace_context=TRUE,chat_submit=list(text="Explain helper()",nonce=1L))
  check(length(calls)==1L && grepl("calls helper() -> R/helper.R:2-2",calls[[1L]]$prompt,fixed=TRUE) &&
    grepl("x + 9",calls[[1L]]$prompt,fixed=TRUE),"Workspace On supplies actual linked function bodies and file/line mappings in the ordinary Send request")
  check(grepl("do not replace CURRENT EDITOR with another file's code",calls[[1L]]$system,fixed=TRUE) && writes==0L,
    "linked files stay read-only context and cannot silently replace the active Source document")
  session$setInputs(workspace_context=FALSE,chat_submit=list(text="Explain helper()",nonce=2L))
  check(length(calls)==2L && !grepl("Dependency map|x \\+ 9|Saved related file",calls[[2L]]$prompt) && writes==0L,
    "Workspace Off excludes dependency scans, bodies and mappings from the request")
})
unlink(root,recursive=TRUE);unlink(outside)
cat(checks,"Project dependency checks passed\n")
