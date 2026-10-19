# Multipart assignments must remain available across short part-by-part requests.
# Optionally provide a plain-text assignment fixture as the first argument.
# Run from the project directory: Rscript tests/assignment-context.R
source("tests/helpers.R")
app <- load_assistant()
checks <- 0L
check <- function(ok,description) {
  if(!isTRUE(ok))stop(description,call.=FALSE)
  checks <<- checks+1L;cat("PASS",description,"\n")
}
args <- commandArgs(trailingOnly=TRUE)
brief <- if(length(args))paste(readLines(args[[1L]],warn=FALSE),collapse="\n") else paste(
  "Exercise 1: Read synthetic-a.csv and fit two k-means clusters.",
  "(a) Obtain and interpret centroids. (b) Plot the sample measurements by cluster.",
  "(c) Compare known labels with cluster membership.",
  "Exercise 2: Read synthetic-b.csv. (a) Compare several K on five simulated indicators.",
  "(b) Repeat with standardized indicators and explain the choice.",
  "Exercise 3: Read synthetic-c.csv, remove incomplete rows and retain group A.",
  "(a) Cluster observations using Euclidean distance and single linkage.",
  "(b) Cluster the simulated measurement variables.",sep="\n")
brief <- trimws(brief)
app$initial <- list(id="assignment-doc",path=file.path(tempdir(),"assignment.R"),contents="x <- 1")
app$active_doc <- function(id=NULL) app$initial
calls <- new.env();calls$prompts <- list()
app$ask_ai <- function(system_prompt,prompt,images=character()) {
  calls$prompts <- append(calls$prompts,list(prompt))
  "Response to the requested part."
}
shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(chat_submit=list(text=brief,nonce=1L))
  check(identical(state$assignment,brief),"the complete multipart assignment is retained")
  requests <- c("Exercise 1(a): Give code for this part only.",
                "Exercise 1(b): Continue with this part only.",
                "Exercise 1(c): Interpret my output.",
                "Exercise 2(a): Show code for the raw indicators.",
                "Exercise 2(b): Show the standardized version.",
                "Exercise 3(a): Show code for the sample analysis.",
                "Exercise 3(b): Show code for clustering variables.")
  for(i in seq_along(requests)) {
    session$setInputs(chat_submit=list(text=requests[[i]],nonce=i+1L))
    check(identical(state$assignment,brief) && grepl(brief,tail(calls$prompts,1L)[[1L]],fixed=TRUE),
          paste("the complete assignment survives part request",i))
  }
  session$setInputs(chat_submit=list(text=paste("Exercise 2(b): Explain this output only.",strrep("Measured output. ",40L)),nonce=15L))
  check(identical(state$assignment,brief),"a long follow-up with an exercise reference does not replace the full brief")
  for(i in 1:8)session$setInputs(chat_submit=list(text=paste("Explain the current result",i),nonce=20L+i))
  check(grepl(brief,tail(calls$prompts,1L)[[1L]],fixed=TRUE),"the assignment survives beyond the recent-history window")
  session$setInputs(new_chat=1L)
  check(!nzchar(state$assignment),"New clears the stored assignment")
  session$setInputs(chat_submit=list(text="Exercise 1(a): Help with this part.",nonce=40L))
  session$setInputs(chat_submit=list(text=brief,nonce=41L))
  check(identical(state$assignment,brief),"a full brief can replace an initial short part reference")
  app$initial$id <- "different-doc"
  session$setInputs(chat_submit=list(text="Help with this new script.",nonce=42L))
  check(identical(state$assignment,brief),"switching Source documents preserves the ongoing assignment")
})
# Text copied from a PDF can lose its spaces. A dataset named in the task must
# still be found in the project and used, without path placeholders.
project <- tempfile("ra-glued-");dir.create(project)
writeLines(c("LGA,VCRS,TVS","Ashfield,517,377"),file.path(project,"crime.csv"))
glued <- paste("Exercise1 ThedatagivenonbelowlistthenumberofvariousitemsstoleninarangeofLocalGovernmentAreas",
  "LocalGovernmentArea VCRS TVS Ashfield 517 377 Auburn 506 327",
  "The dataareinthefilecrime.csv.ReadthedataintoRandcall itcrime. 1.Obtainascatterplotmatrix.Comment.")
calls <- list()
app <- load_assistant()
app$initial <- list(id="untitled",path="",contents="")
app$active_doc <- function(id=NULL)app$initial
app$ask_ai <- function(system_prompt,prompt,images=character()) {
  calls[[length(calls)+1L]] <<- list(system=system_prompt,prompt=prompt)
  "Answer."
}
old_wd <- setwd(project)
shiny::testServer(app$server, {
  session$setInputs(chat_submit=list(text=glued,nonce=1L))
  first <- tail(calls,1L)[[1L]]
  header <- sub("(?s).*VERIFIED CSV HEADER CHECK:\n(.*?)\n\n.*","\\1",first$prompt,perl=TRUE)
  check(identical(header,"crime.csv: VERIFIED CSV REFERENCE; columns (read.csv defaults): LGA, VCRS, TVS"),
    "a dataset named in space-less pasted text is verified in the project")
  session$setInputs(chat_submit=list(text="Follow instruction, start again",nonce=2L))
  again <- tail(calls,1L)[[1L]]
  check(grepl("crime.csv: VERIFIED CSV REFERENCE",again$prompt,fixed=TRUE),
    "follow-ups keep verifying the dataset named in the stored task")
  check(grepl("Never write placeholders",again$system,fixed=TRUE) &&
    !grepl("ask for the correct path",again$system,fixed=TRUE),
    "the prompt forbids path placeholders when the task names the file")
})
setwd(old_wd)
unlink(project,recursive=TRUE)

# Explicit paths must not resolve to an unrelated same-named root file.
paths <- c("data/sample.csv", "../data/sample.csv", "/tmp/sample.csv",
  "~/data/sample.csv", "C:\\data\\sample.csv", "\\\\server\\share\\sample.csv")
check(identical(app$csv_files_mentioned(paste(paths,collapse="\n")),paths),
  "chat references retain relative, absolute, home, Windows and network paths")
check(identical(app$csv_references(paste0("read.csv(",encodeString(paths,quote='"'),")",collapse="\n")),paths),
  "CSV inspection safely round-trips escaped path literals")
check(identical(app$csv_files_mentioned('Use "data/my sample.csv" and `data/other sample.csv`.'),
  c("data/my sample.csv","data/other sample.csv")),"quoted CSV paths retain spaces")
check(identical(app$csv_files_mentioned("Dataset:data/sample.csv; https://example.com/remote.csv"),"data/sample.csv"),
  "prose punctuation is not part of a local path and web URLs are not inspected as files")
project <- tempfile("ra-csv-paths-");dir.create(project);dir.create(file.path(project,"data"))
writeLines(c("WRONG_ROOT_COLUMN","1"),file.path(project,"sample.csv"))
for(name in c("sample.csv","student's sample.csv"))
  writeLines(c("RIGHT_SUBDIR_COLUMN","1"),file.path(project,"data",name))
old_wd <- setwd(project)
shiny::testServer(app$server, {
  session$flushReact()
  header <- function()sub("(?s).*VERIFIED CSV HEADER CHECK:\n(.*?)\n\n.*","\\1",
    tail(calls,1L)[[1L]]$prompt,perl=TRUE)
  requests <- c("Use data/sample.csv and explain its columns.",
    'Use "data/student\'s sample.csv" and explain its columns.',
    paste0('Use "',normalizePath(file.path(project,"data","sample.csv"),winslash="/"),'".'))
  for(i in seq_along(requests)) {
    session$setInputs(chat_submit=list(text=requests[[i]],nonce=i))
    check(grepl("RIGHT_SUBDIR_COLUMN",header(),fixed=TRUE) && !grepl("WRONG_ROOT_COLUMN",header(),fixed=TRUE),
      paste("the requested file supplies verified columns for path case",i))
  }
  session$setInputs(chat_submit=list(text="Use missing/sample.csv",nonce=4L))
  check(grepl("missing/sample.csv: NOT FOUND",header(),fixed=TRUE) &&
    !grepl("WRONG_ROOT_COLUMN",header(),fixed=TRUE),"a missing explicit path never falls back to its basename")
})
setwd(old_wd)
unlink(project,recursive=TRUE)
cat(checks,"assignment-context checks passed\n")
