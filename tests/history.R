# Durable project-local storage, using temporary directories and synthetic data.
library(RAssistant)
main <- function() {
  factory <- getFromNamespace(".conversation_store","RAssistant")
  root <- tempfile("ra-history-");dir.create(root)
  on.exit(unlink(root,recursive=TRUE),add=TRUE)
  project <- file.path(root,"Project A");dir.create(project)
  other <- file.path(root,"Project B");dir.create(other)
  data <- file.path(root,"user data")
  opts <- options(RAssistant.save_history=TRUE)
  on.exit(options(opts),add=TRUE)
  checks <- 0L
  check <- function(ok,description){
    if(!isTRUE(ok))stop(description,call.=FALSE)
    checks <<- checks+1L;cat("PASS",description,"\n")
  }
  store <- factory(project=project,directory=data)
  set.seed(2026)
  rng <- .Random.seed
  store$new_id()
  check(identical(.Random.seed,rng),"chat identifiers do not change the user's R random-number state")
  check(is.null(store$load()) && !length(store$list()) && !dir.exists(store$path),
    "opening an unused history store does not create files")
  empty <- list(id=store$new_id(),title="New chat",messages=list(),draft=" \n\t ",attachments=list())
  store$save(empty)
  check(!length(store$list()) && !dir.exists(store$path),"saving an empty chat creates no directory or snapshots")
  draft <- empty;draft$draft <- "Keep this unsent question"
  store$save(draft)
  check(identical(store$load(draft$id)$draft,draft$draft),"unsent text alone is meaningful history")
  store$save(empty)
  check(!length(store$list()) && is.null(store$load(draft$id)),"clearing a previously saved draft removes its empty record")
  fixture <- file.path(root,"synthetic.png")
  grDevices::png(fixture,width=80,height=80);par(mar=rep(0,4));plot.new();points(.5,.5)
  invisible(grDevices::dev.off())
  bytes <- readBin(fixture,"raw",file.info(fixture)$size)
  preview <- paste0("data:image/png;base64,",jsonlite::base64_enc(bytes))
  image <- list(label="Screenshot.png",preview=preview)
  upload <- empty;upload$attachments <- list(image)
  store$save(upload)
  check(identical(store$load(upload$id)$attachments[[1L]]$preview,preview),"an unsent image alone is meaningful history")
  store$save(empty)
  check(!length(store$list()) && !length(list.files(file.path(store$path,"images"))),"clearing the last unsent image removes the empty record and its unused asset")
  chat <- list(id=store$new_id(),title="First chat",title_custom=FALSE,created_at="now",
    messages=list(list(role="You",type="text",text="Explain the screenshot",sequence=1L,attachments=list(image)),
      list(role="AI",type="text",text="The value is 42.",academic="Verified result.",sequence=2L)),
    attachments=list(image),draft="Unsent follow-up",assignment="Exercise 1: synthetic task")
  store$save(chat)
  check(identical(store$load()$id,chat$id) && identical(store$load()$draft,chat$draft),
    "messages, assignment and draft survive a saved-chat round trip")
  assets <- list.files(file.path(store$path,"images"),full.names=TRUE)
  check(length(assets)==1L && identical(readBin(assets[[1L]],"raw",file.info(assets[[1L]])$size),bytes),
    "identical image bytes are stored once, separately from text snapshots")
  snapshots <- list.files(file.path(store$path,chat$id),pattern="\\.json$",full.names=TRUE)
  check(!any(grepl("data:image",unlist(lapply(snapshots,readLines)),fixed=TRUE)),
    "auto-saves contain image references instead of repeated base64 copies")
  fresh <- factory(project=project,directory=data)
  restored <- fresh$load()
  check(identical(restored$messages[[1L]]$attachments[[1L]]$preview,preview) &&
    identical(restored$attachments[[1L]]$preview,preview),
    "a fresh store restores original sent images and unsent attachments")
  separate <- factory(project=other,directory=data)
  check(is.null(separate$load()) && !length(separate$list()) && !identical(separate$path,store$path),
    "different projects have isolated local histories")
  child <- callr::r(function(project,directory) {
    factory <- getFromNamespace(".conversation_store","RAssistant")
    value <- factory(project=project,directory=directory)$load()
    list(id=value$id,text=value$messages[[2L]]$text,preview=value$messages[[1L]]$attachments[[1L]]$preview)
  },args=list(project,data),libpath=.libPaths(),user_profile=FALSE,system_profile=FALSE)
  check(identical(child$id,chat$id) && identical(child$preview,preview) && identical(child$text,"The value is 42."),
    "history survives a completely new R process")
  second <- chat;second$id <- store$new_id();second$title <- "Second chat"
  store$save(second)
  check(length(store$list())==2L && identical(fresh$load()$id,second$id),
    "multiple chats are retained and the most recently used is restored")
  renamed <- fresh$load(chat$id);renamed$title <- "Renamed conversation";renamed$title_custom <- TRUE
  fresh$save(renamed)
  check(identical(store$load()$id,chat$id) && identical(store$list()[[1L]]$title,renamed$title),
    "renaming and revisiting an older chat updates its metadata and recent selection")
  store$save(renamed);store$save(renamed)
  check(length(list.files(file.path(store$path,chat$id),pattern="\\.meta\\.json$"))==2L,
    "auto-save retains one complete recovery snapshot")
  metas <- sort(list.files(file.path(store$path,chat$id),pattern="\\.meta\\.json$",full.names=TRUE),decreasing=TRUE)
  latest <- jsonlite::fromJSON(metas[[1L]])
  writeLines("{damaged",file.path(dirname(metas[[1L]]),latest$snapshot))
  check(isTRUE(fresh$load(chat$id)$recovered) && identical(fresh$load(chat$id)$title,renamed$title),
    "a damaged latest snapshot recovers the complete previous save")
  check(inherits(tryCatch(store$delete("../Project B"),error=identity),"error"),
    "conversation identifiers cannot traverse filesystem paths")
  store$delete(chat$id)
  check(length(store$list())==1L && identical(store$list()[[1L]]$id,second$id) && file.exists(assets[[1L]]),
    "deleting one chat preserves other chats and shared image assets")
  store$delete(second$id)
  check(!length(store$list()) && !length(list.files(file.path(store$path,"images"))),
    "deleting the final referencing chat removes unused images")
  options(RAssistant.save_history=FALSE)
  check(is.null(factory(project=project,directory=data)),"local history can be disabled explicitly")
  cat(checks,"history-storage checks passed\n")
}
main()
