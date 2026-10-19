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
cat(checks,"assignment-context checks passed\n")
