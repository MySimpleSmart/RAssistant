# Scoped post-edit execution requires its own approval and exact Source match.
source("tests/helpers.R")
app <- load_assistant();checks <- 0L
check <- function(ok,label){if(!isTRUE(ok))stop(label,call.=FALSE);checks<<-checks+1L;cat("PASS",label,"\n")}
scope <- app$post_edit_scope('x <- bad(X)\nstop("UNTOUCHED")','x <- dim(X)\nstop("UNTOUCHED")')
check(scope$ok && identical(scope$code,"x <- dim(X)") && !scope$definition_only,"a one-line repair excludes unchanged neighboring expressions")
multi <- app$post_edit_scope("x <- sum(\n  1,\n  bad(2))\nstop('UNTOUCHED')","x <- sum(\n  1,\n  2)\nstop('UNTOUCHED')")
check(multi$ok && identical(multi$code,"x <- sum(\n  1,\n  2)"),"a multiline repair selects its complete enclosing R expression")
same_line <- app$post_edit_scope('x <- bad(X); stop("UNTOUCHED")','x <- dim(X); stop("UNTOUCHED")')
check(same_line$ok && identical(same_line$code,"x <- dim(X)"),"unchanged expressions on the same edited line are excluded")
check(!app$post_edit_scope("bad(X)\nstop('UNTOUCHED')","stop('UNTOUCHED')")$ok,
  "deleting an expression cannot select the next unchanged command")
deletion <- app$post_edit_scope("x <- sum(\n  1,\n  2,\n  3)","x <- sum(\n  1,\n  3)")
check(deletion$ok && grepl("sum",deletion$code,fixed=TRUE),"deleting part of an expression still checks the complete remaining expression")
check(!app$post_edit_scope("# old\nx <- 1","# new\nx <- 1")$ok &&
  !app$post_edit_scope("x <- 1","\nx <- 1\n")$ok,"comment and blank-line changes do not offer pointless execution")
definition <- app$post_edit_scope("f <- function(x) bad(x)","f <- function(x) dim(x)")
check(definition$ok && definition$definition_only,"function edits identify definition-only evaluation without claiming the body was tested")
before <- paste(c("# Keep","```{r first}","x <- bad(X)","```","Text","```{r other}","broken <- (","```"),collapse="\n")
after <- sub("bad(X)","dim(X)",before,fixed=TRUE)
chunk <- app$post_edit_scope(before,after,"Rmd")
check(chunk$ok && identical(chunk$code,"x <- dim(X)\n") && chunk$units[[1L]]$start==3L,
  "a changed Rmd chunk uses global rows and excludes another chunk's syntax error")
check(!app$post_edit_scope(sub("first}","first, eval=FALSE}",before,fixed=TRUE),
  sub("first}","first, eval=FALSE}",after,fixed=TRUE),"Rmd")$ok,"eval-disabled chunks are never offered for execution")
check(!app$post_edit_scope(before,sub("# Keep","# Updated",before,fixed=TRUE),"Rmd")$ok,
  "Markdown-only changes do not trigger an R run")
check(!app$post_edit_scope(paste0("x",1:4," <- 1",collapse="\n"),paste0("x",1:4," <- 2",collapse="\n"))$ok,
  "too many changed scopes are declined instead of silently checking the whole file")
check("x" %in% app$scope_input_names("x <- x + 1") && !"x" %in% app$scope_input_names("{x <- 1; x <- x + 1}"),
  "self-updates require an existing value while preceding local assignments satisfy it")
env <- new.env(parent=emptyenv());env$X <- matrix(1:6,2,3);touched <- 0L
makeActiveBinding("active",function(){touched<<-touched+1L;stop("Must not run")},env)
delayedAssign("lazy",{touched<<-touched+1L;stop("Must not run")},assign.env=env)
inputs <- app$scope_workspace_inputs("dim(X)",env=env,copy=TRUE)
check(identical(inputs$bindings$X,env$X) && touched==0L,"only referenced ordinary Workspace inputs are copied")
inputs_missing <- app$scope_workspace_inputs("print(active); print(lazy)",env=env)
check(setequal(inputs_missing$missing,c("active","lazy")) && touched==0L,"preview never forces lazy values or active bindings")
check(inherits(tryCatch(app$scope_workspace_inputs("dim(not_loaded)",env=env,copy=TRUE),error=identity),"error"),
  "missing inputs stop the check without rerunning setup code")
env$unsafe <- new.env()
check(inherits(tryCatch(app$scope_workspace_inputs("print(unsafe)",env=env,copy=TRUE),error=identity),"error"),
  "environment-valued inputs cannot leak a complete session into the worker")
env$large <- numeric(1400000L)
check(inherits(tryCatch(app$scope_workspace_inputs("length(large)",env=env,copy=TRUE),error=identity),"error"),
  "large Workspace copies are rejected before the worker starts")
run <- app$validate_script(scope$code,tempdir(),bindings=list(X=env$X),scoped=TRUE)
check(run$ok && grepl("Scoped code completed",run$logs,fixed=TRUE) && !grepl("UNTOUCHED",run$logs,fixed=TRUE),
  "the real subprocess checks changed code with copied inputs and does not run its neighbor")
unlink(run$plot_dir,recursive=TRUE)
scope_fixture_factor <- 4
scope_fixture_helper <- function(x)x*scope_fixture_factor
helper_inputs <- app$scope_workspace_inputs("scope_fixture_helper(3)",copy=TRUE)
check(setequal(names(helper_inputs$bindings),c("scope_fixture_helper","scope_fixture_factor")),
  "global helper functions include their transitive ordinary Workspace dependencies")
run <- app$validate_script("scope_fixture_helper(3)",tempdir(),bindings=helper_inputs$bindings,scoped=TRUE)
check(run$ok && grepl("12",run$logs,fixed=TRUE) && scope_fixture_factor==4,
  "a copied helper resolves its globals inside the worker without changing the main session")
unlink(run$plot_dir,recursive=TRUE);rm(scope_fixture_helper,scope_fixture_factor)
run <- app$validate_script("x <- x + 1; print(x)",tempdir(),bindings=list(x=10),scoped=TRUE)
check(run$ok && grepl("11",run$logs,fixed=TRUE),"scoped self-updates use the copied value in the isolated process")
unlink(run$plot_dir,recursive=TRUE)
run <- app$validate_script("stop('EXPECTED_SCOPED_ERROR')",tempdir(),scoped=TRUE)
check(!run$ok && grepl("EXPECTED_SCOPED_ERROR",run$logs,fixed=TRUE),"runtime failures remain actual failed checks")
unlink(run$plot_dir,recursive=TRUE)

editor <- new.env();editor$ctx <- list(id="doc",path=file.path(tempdir(),"scoped.R"),contents=c("x <- bad(X)","stop('UNTOUCHED')"))
editor$writes <- 0L;editor$calls <- 0L;runs <- list();inputs_changed <- FALSE
app$initial <- editor$ctx;app$active_doc <- function(id=NULL)editor$ctx
app$write_source <- function(id,text){editor$writes<-editor$writes+1L;editor$ctx$contents<-strsplit(text,"\n",fixed=TRUE)[[1L]]}
app$ask_ai <- function(...){editor$calls<-editor$calls+1L;"```r-edit-1-1\nx <- dim(X)\n```"}
app$scope_workspace_inputs <- function(code,definition_only=FALSE,copy=FALSE,...)
  list(names=if(inputs_changed)"Y"else"X",missing=character(),bindings=if(copy)list(X=env$X)else list(),packages=character())
app$validate_script <- function(code,...){runs[[length(runs)+1L]]<<-list(code=code,args=list(...));
  list(ok=TRUE,logs="[1] 2 3",plots=character(),plot_dir=NULL)}
shiny::testServer(app$server,{
  session$setInputs(chat_submit=list(text="Fix the code",nonce=1L));invisible(output$source_change_preview)
  job <- get("preview_job",envir=environment(clear_source_review),inherits=FALSE)
  session$setInputs(apply_source_change=list(token=job$token,nonce=2L))
  check(editor$writes==1L && !length(runs) && is.null(state$pending),"Replace only writes the approved change and offers a check without execution")
  offer <- tail(state$messages,1L)[[1L]]$post_edit_check
  check(offer$scope$ok && grepl("post-edit-preview",paste(output$messages,collapse=""),fixed=TRUE),
    "the applied message exposes an inline Check changed code button")
  session$setInputs(preview_post_edit_check=list(token="forged",nonce=3L))
  check(is.null(state$pending) && !length(runs),"a forged offer cannot create run approval")
  session$setInputs(preview_post_edit_check=list(token=offer$token,nonce=4L))
  approval <- state$pending
  check(identical(approval$scope,"post_edit") && !length(runs) &&
    grepl("Code and inputs",paste(output$messages,collapse=""),fixed=TRUE),"preview presents exact code and input names before requesting approval")
  session$setInputs(yes=1L)
  check(!length(runs) && !is.null(state$pending),"an old full-script Yes cannot approve a scoped check")
  session$setInputs(run_post_edit_check=list(token="forged",nonce=5L))
  check(!length(runs),"a forged scoped run token cannot execute")
  session$setInputs(cancel_post_edit_check=list(token=approval$token,nonce=6L))
  check(is.null(state$pending) && !length(runs),"Cancel dismisses run approval without executing code")
  session$setInputs(preview_post_edit_check=list(token=offer$token,nonce=7L))
  fresh <- state$pending
  session$setInputs(run_post_edit_check=list(token=approval$token,nonce=8L))
  check(!length(runs) && identical(state$pending$token,fresh$token),"a cancelled preview's token cannot approve a later preview")
  editor$ctx$contents[[2L]] <- "manual <- 42"
  session$setInputs(run_post_edit_check=list(token=fresh$token,nonce=9L))
  check(!length(runs) && is.null(state$pending),"any Source change revokes pending scoped execution")
  editor$ctx$contents[[2L]] <- "stop('UNTOUCHED')"
  session$setInputs(preview_post_edit_check=list(token=offer$token,nonce=10L))
  token <- state$pending$token
  saved <- app$snapshot_messages(state$messages)
  check(!any(vapply(saved,function(m)"post_edit_check" %in% names(m),logical(1))) &&
    is.null(app$restore_chat_data(list(messages=saved))$messages[[length(saved)]]$post_edit_check),
    "saved history never restores a live run offer, input copies or approval token")
  session$setInputs(run_post_edit_check=list(token=token,nonce=11L))
  check(length(runs)==1L && identical(runs[[1L]]$code,"x <- dim(X)") && isTRUE(runs[[1L]]$args$scoped) &&
    identical(runs[[1L]]$args$bindings$X,env$X),"explicit approval sends only the scoped code and its copied inputs to the worker")
  check(is.null(state$pending) && !state$busy && editor$calls==1L && editor$writes==1L,
    "completion unlocks chat without a second model call or another Source write")
  session$setInputs(run_post_edit_check=list(token=token,nonce=12L))
  check(length(runs)==1L,"consumed execution approval cannot be replayed")
  session$setInputs(preview_post_edit_check=list(token=offer$token,nonce=13L))
  token <- state$pending$token;inputs_changed <<- TRUE
  session$setInputs(run_post_edit_check=list(token=token,nonce=14L))
  check(length(runs)==1L && is.null(state$pending) && !state$busy,
    "changed input requirements revoke approval before starting the worker")
})
cat(checks,"Post-edit check checks passed\n")
