# Actual dataset excerpts are bounded, fresh, read-only and opt out with Workspace.
source("tests/helpers.R")
app <- load_assistant();checks <- 0L
check <- function(ok,label){if(!isTRUE(ok))stop(label,call.=FALSE);checks<<-checks+1L;cat("PASS",label,"\n")}
env <- new.env(parent=emptyenv())
env$birth_data <- data.frame(weight=c(3100,NA,2800),group=factor(c("A","B","A")),date=as.Date("2026-10-21")+0:2)
env$other_data <- data.frame(actual_value=c(42,91))
env$X <- matrix(1:12,3,4,dimnames=list(NULL,c("a","b","c","d")))
env$credentials <- data.frame(password="private-sentinel")
env$numbers <- 1:10
touched <- 0L
makeActiveBinding("active_data",function(){touched<<-touched+1L;stop("must not run")},env)
delayedAssign("lazy_data",{touched<<-touched+1L;stop("must not run")},assign.env=env)
catalog <- app$workspace_catalog(env)
check(setequal(names(catalog),c("birth_data","other_data","X")) && touched==0L,"catalog skips promises, active bindings, credentials and non-tabular objects")
preview <- app$workspace_dataset("birth_data",env)
check(identical(preview$values$weight,c("3100",NA_character_,"2800")) && identical(preview$values$group,c("A","B","A")) && preview$values$date[[1L]]=="2026-10-21","dataset previews contain real values, factor labels, dates and missing cells")
check(identical(app$workspace_dataset("X",env)$values$d,c("10","11","12")),"matrix values use the actual row and column order")
report <- app$workspace_data_context("Explain birth_data",env=env)
check(grepl("3100",report,fixed=TRUE) && !grepl("other_data",report,fixed=TRUE) && grepl("actual dimensions=3 x 3",report,fixed=TRUE),"a named dataset supplies its current cells and verified dimensions only")
check(!nzchar(app$workspace_data_context("birth_database other_data_extra",env=env)),"partial object names do not accidentally select datasets")
check(grepl("other_data",app$workspace_data_context("Explain this",selected="other_data",env=env),fixed=TRUE),"manual selections include a dataset even when the question does not name it")
check(grepl("birth_data",app$workspace_data_context("Explain this plot",source="ggplot(birth_data, aes(weight))",env=env),fixed=TRUE),"Source references automatically identify relevant loaded datasets")
env$birth_data$weight[[1L]] <- 3200
check(grepl("3200",app$workspace_data_context("birth_data",env=env),fixed=TRUE),"each request rereads values instead of reusing a stale preview")
env$large <- as.data.frame(matrix(seq_len(100*30),100,30));env$large$V1 <- rep(strrep("z",500),100)
large <- app$workspace_dataset("large",env)
check(large$rows==100L && large$columns==30L && large$shown_rows==20L && large$shown_columns==20L && nchar(large$values$V1[[1L]])==160L,"large data limits rows, columns and individual cell text")
report <- app$workspace_data_context("large",env=env)
check(nchar(report)<5500L && grepl("actual dimensions=100 x 30",report,fixed=TRUE),"serialized excerpts remain bounded without misrepresenting full dataset dimensions")
check(grepl('"columns"',report,fixed=TRUE) && !inherits(tryCatch(jsonlite::fromJSON(sub("^[^\n]*\n","",report)),error=identity),"error"),"bounded excerpts retain valid JSON rather than cutting through a row")
env$wide <- env$large[1:2,1:20]
names(env$wide) <- paste0(strrep("n",100),seq_len(20))
for(i in seq_along(env$wide))attr(env$wide[[i]],"class") <- strrep("custom",30)
report <- app$workspace_data_context("wide",env=env)
check(nchar(report)<5500L && !inherits(tryCatch(jsonlite::fromJSON(sub("^[^\n]*\n","",report)),error=identity),"error"),"long column names and class labels still fit the valid excerpt budget")
env$private_column <- data.frame(value=1,password="private-cell-sentinel")
check(!grepl("private-cell-sentinel",app$workspace_data_context("private_column",env=env),fixed=TRUE),"credential-like columns do not expose cell values")
`[.hostile` <- function(...)stop("user subset must not run")
print.hostile <- function(...)stop("user print must not run")
env$hostile <- structure(data.frame(value=c(7,9)),class=c("hostile","data.frame"))
check(identical(app$workspace_dataset("hostile",env)$values$value,c("7","9")),"custom data.frame print and subset methods are not executed")
env$nested <- data.frame(x=1:2);env$nested$list <- list(function()stop("not run"),new.env())
check(all(app$workspace_dataset("nested",env)$values$list=="[nested column omitted]"),"nested columns are described without evaluating their contents")
# The same helpers used by the real server, reading a controlled local fixture.
original_catalog <- app$workspace_catalog;original_dataset <- app$workspace_dataset;original_context <- app$workspace_data_context
original_summary <- app$workspace_summary
data_env <- env
app$workspace_catalog <- function(env=data_env)original_catalog(env)
app$workspace_dataset <- function(name,env=data_env,...)original_dataset(name,env,...)
app$workspace_data_context <- function(request,source="",selected=character(),env=data_env)original_context(request,source,selected,env)
app$workspace_summary <- function()"birth_data: data.frame; dimensions=3 x 3"
app$initial <- list(id="doc",path="/tmp/example.R",contents="x <- 1")
app$active_doc <- function(id=NULL)app$initial
calls <- list();app$ask_ai <- function(system_prompt,prompt,images=character()){calls[[length(calls)+1L]]<<-list(system=system_prompt,prompt=prompt);"Explained."}
shiny::testServer(app$server,{
  session$flushReact();session$setInputs(refresh_workspace=1L)
  check("birth_data" %in% names(workspace_choices()) && !length(calls),"opening the picker lists local datasets without an AI request")
  session$setInputs(workspace_datasets_selected="other_data",workspace_context=TRUE)
  check(grepl("42",paste(as.character(output$workspace_preview),collapse=""),fixed=TRUE) && !length(calls),"choosing a dataset renders its local values before Send")
  env$other_data$actual_value[[1L]] <- 43
  session$setInputs(refresh_workspace=2L)
  check(grepl("43",paste(as.character(output$workspace_preview),collapse=""),fixed=TRUE),"reopening the picker refreshes changed values even when dataset names and dimensions stay the same")
  env$other_data$actual_value[[1L]] <- 42
  session$setInputs(chat_submit=list(text="Explain these data",nonce=1L))
  check(grepl("WORKSPACE DATASET EXCERPTS",calls[[1L]]$prompt,fixed=TRUE) && grepl("42",calls[[1L]]$prompt,fixed=TRUE),"manual dataset values reach the normal chat request")
  session$setInputs(workspace_context=FALSE,chat_submit=list(text="birth_data",nonce=2L))
  check(!grepl("WORKSPACE DATASET EXCERPTS",calls[[2L]]$prompt,fixed=TRUE) && !grepl("3200",calls[[2L]]$prompt,fixed=TRUE),"Workspace Off prevents both automatic and manual dataset reads from reaching AI")
  session$setInputs(workspace_context=TRUE,workspace_datasets_selected=character(),chat_submit=list(text="birth_data",nonce=3L))
  check(grepl("3200",calls[[3L]]$prompt,fixed=TRUE),"automatic dataset values reach Send when Workspace is On")
  check(grepl("never present calculations on an excerpt as statistics for the whole dataset",calls[[3L]]$system,fixed=TRUE),"the model is told not to report sample calculations as full-dataset statistics")
  session$setInputs(workspace_datasets_selected="other_data",new_chat=1L)
  check(!length(workspace_selected()),"New chat clears explicitly selected datasets")
})
env$loaded_helper <- function(x,scale=2)stop("Inventory must not execute functions")
check(grepl("loaded_helper: function(x, scale)",original_summary(env),fixed=TRUE),
  "Workspace inventory includes loaded function signatures without executing their bodies")
functions <- app$workspace_function_context("Explain loaded_helper",env=env)
check(grepl('stop("Inventory must not execute functions")',functions,fixed=TRUE) && touched==0L,
  "a relevant loaded function is read without execution or forcing active/lazy bindings")
check(!nzchar(app$workspace_function_context("Unrelated question",env=env)),
  "unrelated loaded function bodies are not sent")
env$loaded_helper <- function(x,scale=3)x*scale
check(grepl("scale = 3",app$workspace_function_context("loaded_helper",env=env),fixed=TRUE),
  "changed in-memory function definitions refresh on the next request")
priorities <- new.env(parent=emptyenv())
for(name in c("a_data","b_data","c_data","y_requested","z_console"))assign(name,data.frame(value=42),envir=priorities)
priority <- original_context("Explain y_requested",source="z_console\na_data\nb_data\nc_data",env=priorities)
check(grepl("Dataset: y_requested",priority,fixed=TRUE) && grepl("Dataset: z_console",priority,fixed=TRUE) &&
  !grepl("Dataset: c_data",priority,fixed=TRUE),"requested and latest-Console datasets take priority over other Source references")
root <- tempfile("project-scan-");dir.create(root);dir.create(file.path(root,"R"));dir.create(file.path(root,"renv"))
outside <- tempfile(fileext=".R");writeLines("OUTSIDE_PROJECT_SENTINEL <- 1",outside)
writeLines("saved_old_value <- 1",file.path(root,"analysis.R"))
writeLines(c("helper <- function(x) {","","  x + 7","}"),file.path(root,"R","helper.R"))
writeLines(c("# Methods","","Use scale() before clustering."),file.path(root,"notes.md"))
writeLines("UNREQUESTED_BODY_SENTINEL <- 1",file.path(root,"unrelated.R"))
writeLines("DEPENDENCY_BODY_SENTINEL <- 1",file.path(root,"renv","dependency.R"))
writeLines("HIDDEN_BODY_SENTINEL <- 1",file.path(root,".private.R"))
invisible(file.symlink(outside,file.path(root,"linked.R")))
scan <- app$workspace_project_context("Explain R/helper.R and notes.md",source='source("R/helper.R")',
  path=file.path(root,"analysis.R"),working=root)
check(grepl("Project: ",scan,fixed=TRUE) && grepl("R/helper.R",scan,fixed=TRUE) && grepl("notes.md",scan,fixed=TRUE),
  "a project scan reports actual working location and bounded source/data inventory")
check(grepl("1 | helper <- function(x) {\n2 | \n3 |   x + 7\n4 | }",scan,fixed=TRUE) &&
  grepl("Use scale() before clustering.",scan,fixed=TRUE),"referenced R and Markdown files are read intact with line numbers")
check(!grepl("UNREQUESTED_BODY_SENTINEL|DEPENDENCY_BODY_SENTINEL|HIDDEN_BODY_SENTINEL|OUTSIDE_PROJECT_SENTINEL",scan),
  "unrelated bodies, hidden files, dependency trees and external symlink targets are not read")
check(!grepl("saved_old_value",app$workspace_project_context("analysis.R",path=file.path(root,"analysis.R"),working=root),fixed=TRUE),
  "a saved copy of the active document never substitutes for its current unsaved editor")
writeLines("helper <- function(x) x + 9",file.path(root,"R","helper.R"))
check(grepl("x + 9",app$workspace_project_context("Explain helper.R",working=root),fixed=TRUE),
  "related file contents refresh on the next scan")
writeLines(strrep("x",32001),file.path(root,"oversized.R"))
check(!grepl(strrep("x",100),app$workspace_project_context("oversized.R",working=root),fixed=TRUE),
  "large saved files are omitted instead of sending cut-off code")
dir.create(file.path(root,"R","nested"));writeLines("DEEP_BODY_SENTINEL <- 1",file.path(root,"R","nested","deep.R"))
check(!grepl("DEEP_BODY_SENTINEL|deep.R",app$workspace_project_context("deep.R",working=root)),
  "the stated one-directory scan limit is real")
scans <- 0L
app$workspace_project_context <- function(...){scans<<-scans+1L;"PROJECT_SCAN_SENTINEL"}
app$workspace_function_context <- function(...)"LOADED_FUNCTION_SENTINEL"
shiny::testServer(app$server,{
  session$flushReact();before<-length(calls)
  session$setInputs(workspace_context=TRUE,chat_submit=list(text="Explain workspace",nonce=40L))
  check(scans==1L && grepl("PROJECT SCAN (fresh; read-only):\nPROJECT_SCAN_SENTINEL",calls[[before+1L]]$prompt,fixed=TRUE),
    "Send scans the current project with Workspace On without another AI call")
  check(grepl("LOADED_FUNCTION_SENTINEL",calls[[before+1L]]$prompt,fixed=TRUE),
    "relevant loaded function definitions reach the same Send request")
  session$setInputs(workspace_context=FALSE,chat_submit=list(text="Explain workspace",nonce=41L))
  check(scans==1L && !grepl("PROJECT_SCAN_SENTINEL",calls[[before+2L]]$prompt,fixed=TRUE),
    "Workspace Off excludes project scanning as well as Global Environment data")
  check(!grepl("LOADED_FUNCTION_SENTINEL",calls[[before+2L]]$prompt,fixed=TRUE),
    "Workspace Off also excludes loaded function definitions")
})
unlink(root,recursive=TRUE);unlink(outside)
cat(checks,"Workspace data checks passed\n")
