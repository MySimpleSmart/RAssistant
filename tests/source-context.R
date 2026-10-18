# Regressions for image chat when the RStudio Source lookup is unavailable.
# Run from the project directory: Rscript tests/source-context.R
source("tests/helpers.R")
app <- load_assistant()
checks <- 0L
check <- function(ok, description) {
  if (!isTRUE(ok)) stop(description, call.=FALSE)
  checks <<- checks+1L
  cat("PASS",description,"\n")
}
app$initial <- list(id="source1",path=file.path(tempdir(),"example.R"),contents="old_source_marker <- 1")
calls <- new.env(); calls$n <- 0L; calls$source <- 0L
app$active_doc <- function(id=NULL) {
  calls$source <- calls$source+1L
  stop("Simulated RStudio Source lookup failure")
}
app$ask_ai <- function(system_prompt,prompt,images=character()) {
  calls$n <- calls$n+1L;calls$prompt <- prompt;calls$images <- images;calls$system <- system_prompt
  stopifnot(length(images)==1L, file.exists(images[[1L]]))
  if (isTRUE(calls$fail)) stop("Simulated API failure")
  "Image analysis completed."
}
original_csv_report <- app$csv_header_report
app$csv_header_report <- function(...) stop("CSV preflight must not run without fresh Source context")
fixture <- tempfile(fileext=".png")
grDevices::png(fixture,width=80,height=80)
par(mar=rep(0,4));plot.new();points(.5,.5)
invisible(grDevices::dev.off())
data_url <- paste0("data:image/png;base64,",jsonlite::base64_enc(readBin(fixture,"raw",file.info(fixture)$size)))
shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(paste_image=list(image=data_url,filename="plot.png",nonce=1L,epoch=state$upload_epoch))
  check(length(state$attachments)==1L && calls$source==0L,"image upload does not depend on Source editor access")
  staged <- state$attachments[[1L]]$path
  session$setInputs(chat_submit=list(text="Interpret the image",nonce=2L))
  check(calls$n==1L,"image chat proceeds when Source lookup throws an error")
  check(!grepl("not Console output",calls$prompt,fixed=TRUE),"uploaded screenshots are not excluded from Console output evidence")
  check(grepl("Do not assume a content type or task",calls$system,fixed=TRUE),"ordinary uploads do not default to plot interpretation")
  check(!grepl("ACADEMIC_COMMENT",calls$system,fixed=TRUE),"uploading an image alone does not request a statistical comment")
  check(!grepl("old_source_marker",calls$prompt,fixed=TRUE),"unavailable Source contents are not taken from a stale snapshot")
  check(!length(state$attachments) && !file.exists(staged) && !state$busy,"successful image chat cleans up files and loading state")
  check(grepl("No local CSV files were inspected",calls$prompt,fixed=TRUE) &&
        !grepl("CSV preflight must not run",calls$prompt,fixed=TRUE),"image chat skips local CSV reads when Source is unavailable")
  check(identical(output$filename,"No Source document"),"toolbar indicates that no current Source document was read")
  before <- calls$n
  session$setInputs(chat_submit=list(text="Please run the script",nonce=3L))
  check(calls$n==before && is.null(state$pending) && !state$busy,"validation cannot start without a freshly readable script")
  session$setInputs(paste_image=list(image=data_url,filename="retry.png",nonce=4L,epoch=state$upload_epoch))
  staged <- state$attachments[[1L]]$path
  calls$fail <- TRUE
  session$setInputs(chat_submit=list(text="",nonce=5L))
  check(length(state$attachments)==1L && file.exists(staged) && !state$busy,"image-only API failure preserves its attachment without Source access")
  calls$fail <- FALSE
  session$setInputs(chat_submit=list(text="",nonce=6L))
  check(!length(state$attachments) && !file.exists(staged) &&
        grepl("UPLOADED IMAGES THIS TURN: 1",calls$prompt,fixed=TRUE),"image-only retry succeeds without Source access")
})

# Default reads can fail while a fresh read of the known document still succeeds.
app$csv_header_report <- original_csv_report
fresh <- list(id="source1",path=app$initial$path,contents="fresh_source_marker <- 2")
seen_ids <- list()
app$active_doc <- function(id=NULL) {
  seen_ids[[length(seen_ids)+1L]] <<- list(id=id)
  if (is.null(id)) return(NULL)
  fresh
}
check(identical(app$read_source_context("source1"),fresh) && length(seen_ids)==2L &&
      identical(seen_ids[[2L]]$id,"source1"),"unavailable default lookup retries the known document ID for fresh text")
shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(paste_image=list(image=data_url,filename="fresh.png",nonce=10L,epoch=state$upload_epoch))
  session$setInputs(chat_submit=list(text="Interpret this",nonce=11L))
  check(grepl("fresh_source_marker",calls$prompt,fixed=TRUE) &&
        !grepl("old_source_marker",calls$prompt,fixed=TRUE),"image requests use the freshly re-read source instead of cached contents")
  check(identical(state$current$contents,fresh$contents),"fresh document fallback updates the current Source state")
})
latest <- list(id="source2",path=file.path(tempdir(),"other.R"),contents="other_source_marker <- 3")
app$active_doc <- function(id=NULL) if(is.null(id)) latest else fresh
check(identical(app$read_source_context("source1"),latest),"a newly selected valid document takes precedence over the previous ID")
app$active_doc <- function(id=NULL) if(is.null(id)) NULL else latest
check(is.null(app$read_source_context("source1")),"fallback rejects a different document returned for the requested ID")
app$active_doc <- function(id=NULL) list(id="#console",path="",contents="console_marker")
check(is.null(app$read_source_context("source1")),"Console contents are never treated as a Source document")
app$active_doc <- function(id=NULL) NULL
check(is.null(app$read_source_context("source1")),"NULL Source lookup is handled without an exception")
for (ctx in list(list(id=NA_character_,contents="x"),list(id=character(),contents="x"),
                 list(id=c("one","two"),contents="x"),list(id="source1",contents=NULL))) {
  check(!app$is_source_context(ctx),"malformed Source contexts are rejected")
}

# Image chat also works when no Source document was available at launch.
app$initial <- app$empty_source_context()
shiny::testServer(app$server, {
  session$flushReact()
  check(identical(output$filename,"No Source document"),"a session can initialize without an open Source document")
  session$setInputs(paste_image=list(image=data_url,filename="standalone.png",nonce=20L,epoch=state$upload_epoch))
  before <- calls$n
  session$setInputs(chat_submit=list(text="",nonce=21L))
  check(calls$n==before+1L && !state$busy && !length(state$attachments),"standalone image chat works with no initial Source document")
})
app$initial <- list(id="large-source",path=file.path(tempdir(),"large.R"),
                    contents=paste0("LARGE_EDITOR_PAYLOAD",strrep("x",50001L)))
app$active_doc <- function(id=NULL) app$initial
shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(paste_image=list(image=data_url,filename="large-source.png",nonce=30L,epoch=state$upload_epoch))
  before <- calls$n
  session$setInputs(chat_submit=list(text="Interpret the plot",nonce=31L))
  check(calls$n==before+1L && !length(state$attachments) && !state$busy,
        "an oversized open script does not block image chat")
  check(!grepl("LARGE_EDITOR_PAYLOAD",calls$prompt,fixed=TRUE) && grepl("50,000",calls$prompt,fixed=TRUE),
        "oversized Source text is explicitly omitted from the image request")
  before <- calls$n
  session$setInputs(chat_submit=list(text="Review my code",nonce=32L))
  check(calls$n==before && !state$busy,"text-only code requests retain the Source size limit")
})
unlink(fixture)
cat(checks,"Source-context checks passed\n")
