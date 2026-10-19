# Run from the project directory: Rscript tests/regression.R
source("tests/helpers.R")
app <- load_assistant()
checks <- 0L
check <- function(ok, description) {
  if (!isTRUE(ok)) stop(description, call. = FALSE)
  checks <<- checks + 1L
  cat("PASS", description, "\n")
}

html <- as.character(app$render_markdown("```r\nx <- 1\nx > 0 & x < 2\n```"))
check(grepl('class="code-copy"', html, fixed = TRUE), "fenced code has a Copy control")
check(grepl("x &lt;- 1", html, fixed = TRUE), "R operators retain one HTML escape layer")
html <- as.character(app$render_markdown('[link](javascript:alert%281%29)'))
check(!grepl('href="javascript:', html, fixed = TRUE), "Markdown links cannot execute JavaScript")
history <- app$summarize_history(list(list(role="AI", type="text", text="", academic="Verified result.")))
check(grepl("Verified result.", history, fixed=TRUE), "academic comments survive in follow-up context")
check(!app$is_run_request("Review this without running the script"), "static review does not request execution")
check(app$is_run_request("Please run the script"), "explicit run requests still work")
check(!app$is_image_error(simpleError("429 image quota exceeded")), "quota errors do not trigger a second API call")
check(app$is_image_error(simpleError("Unsupported image format")), "image-specific errors allow numeric fallback")
history <- app$summarize_history(lapply(1:30, function(i) list(role="AI", type="text", text=paste0(i, strrep("x", 4000)))))
check(nchar(history)<=12000L && grepl("AI: 30",history,fixed=TRUE), "history is bounded and retains the newest turn")
app$api_key <- "synthetic-provider-key"
old_lifecycle <- options(lifecycle_verbosity = "error")
client <- app$create_ai_client("Synthetic system instructions")
options(old_lifecycle)
check(inherits(client,"Chat") && identical(client$get_model(),"gpt-6.1-sol"),
      "saved keys create an ellmer client without deprecated arguments or a network request")
app$api_key <- NULL
windows_path <- "C:\\data\\sample.csv"
windows_code <- paste0("read.csv(", encodeString(windows_path, quote = '"'), ")")
check(identical(app$csv_references(windows_code), windows_path),
      "CSV preflight decodes escaped Windows paths as string data")
check(all(app$is_absolute_path(c("C:/data/sample.csv", windows_path, "\\\\server\\share\\sample.csv", "/tmp/sample.csv", "~/sample.csv"))) &&
      !any(app$is_absolute_path(c("sample.csv", "data/sample.csv", "C:sample.csv"))),
      "CSV paths distinguish Windows drive and network roots from relative references")
check(!length(app$csv_references('read.csv("C:\\invalid\\unescaped.csv")')),
      "invalid R path strings are not inspected as CSV files")
history <- app$summarize_history(list(list(role="AI",type="logs",text="verified output: 42")))
check(grepl("verified output: 42",history,fixed=TRUE), "execution evidence survives in follow-up context")

fixture <- tempfile(fileext=".png")
grDevices::png(fixture, width=80, height=80)
par(mar=rep(0,4)); plot.new(); points(.5,.5)
invisible(grDevices::dev.off())
data_url <- paste0("data:image/png;base64,",jsonlite::base64_enc(readBin(fixture,"raw",file.info(fixture)$size)))
upload_dir <- tempfile(); dir.create(upload_dir)
decoded <- app$decode_image_upload(data_url,upload_dir)
check(identical(app$image_type(decoded),"png"), "PNG uploads decode with a verified file signature")
unlink(decoded)
check(inherits(tryCatch(app$decode_image_upload(sub("image/png","image/jpeg",data_url),upload_dir),error=identity),"error"), "MIME mismatches are rejected")
check(inherits(tryCatch(app$decode_image_upload("data:image/png;base64,!!!!",upload_dir),error=identity),"error"), "invalid base64 is rejected")
check(!length(list.files(upload_dir)), "rejected uploads leave no temporary files")

app$initial <- list(id="doc1", path=file.path(tempdir(),"example.R"), contents="x <- 1")
app$active_doc <- function(id=NULL) app$initial
header_reads <- 0L
original_header_report <- app$csv_header_report
app$csv_header_report <- function(...) {
  header_reads <<- header_reads+1L
  original_header_report(...)
}
calls <- new.env(); calls$n <- 0L; calls$fail <- FALSE; calls$prompts <- list()
app$ask_ai <- function(system_prompt,prompt,images=character()) {
  calls$n <- calls$n+1L; calls$prompts <- append(calls$prompts,list(prompt))
  if(calls$fail)stop("Mock API unavailable")
  "```r\nx <- 1\n```\n[[ACADEMIC_COMMENT]]Verified result.[[/ACADEMIC_COMMENT]]"
}
shiny::testServer(app$server, {
  session$flushReact()
  check(grepl("No simple literal",get_csv_headers("x <- 1",tempdir(),""),fixed=TRUE), "scripts without CSV references skip header reads")
  csv <- tempfile(fileext=".csv");writeLines(c("first,second","1,2"),csv)
  probe <- paste0("read.csv(",encodeString(csv,quote='"'),")")
  check(grepl("first, second",get_csv_headers(probe,tempdir(),""),fixed=TRUE), "CSV preflight preserves verified default column names")
  reads <- header_reads
  get_csv_headers(probe,tempdir(),"")
  check(header_reads==reads, "unchanged CSV headers are served from the session cache")
  writeLines(c("changed_name,second","1,2"),csv)
  check(grepl("changed_name",get_csv_headers(probe,tempdir(),""),fixed=TRUE) && header_reads==reads+1L,
        "CSV metadata changes invalidate the header cache")
  unlink(csv)
  epoch <- state$upload_epoch
  session$setInputs(paste_image=list(image=data_url,filename="plot.png",nonce=101L,epoch=epoch))
  check(length(state$attachments)==1L, "upload observer stages one verified image")
  staged <- state$attachments[[1]]$path
  session$setInputs(paste_image=list(image=data_url,filename="plot.png",nonce=101L,epoch=epoch))
  check(length(state$attachments)==1L, "replayed upload is not duplicated")
  session$setInputs(clear_images=1L)
  check(!length(state$attachments) && !file.exists(staged), "Remove all clears files and previews")
  session$setInputs(paste_image=list(image=data_url,filename="late.png",nonce=102L,epoch=epoch))
  check(!length(state$attachments), "late uploads cannot reappear after reset")
  epoch <- state$upload_epoch
  session$setInputs(cancel_image=list(nonce=103L,epoch=epoch))
  session$setInputs(paste_image=list(image=data_url,filename="cancelled.png",nonce=103L,epoch=epoch))
  check(!length(state$attachments), "timed-out uploads cannot become ready later")
  session$setInputs(paste_image=list(image=data_url,filename="retry.png",nonce=104L,epoch=epoch))
  staged <- state$attachments[[1]]$path
  calls$fail <- TRUE
  session$setInputs(chat_submit=list(text="Interpret the plot",nonce=201L))
  check(length(state$attachments)==1L && file.exists(staged) && !state$busy, "API failure preserves attachments and releases loading state")
  calls$fail <- FALSE
  session$setInputs(chat_submit=list(text="Interpret the plot",nonce=202L))
  check(!length(state$attachments) && !file.exists(staged), "successful retry clears staged files")
  check(length(state$messages[[3L]]$attachments)==1L, "sent image previews remain in chat after cleanup")
  n <- calls$n
  session$setInputs(chat_submit=list(text="Duplicate event",nonce=202L))
  check(calls$n==n, "replayed submissions do not call the API twice")
  session$setInputs(chat_submit=list(text="Explain the comment",nonce=203L))
  check(grepl("Verified result.",tail(calls$prompts,1)[[1]],fixed=TRUE), "follow-up API requests include the academic comment")
  app$initial$id <- "doc2"
  session$setInputs(chat_submit=list(text="New document question",nonce=204L))
  check(length(state$messages)>2L && grepl("Explain the comment",tail(calls$prompts,1)[[1]],fixed=TRUE),
        "switching documents preserves the ongoing conversation")
  session$setInputs(chat_submit=list(text="Please run the script",nonce=205L))
  check(!is.null(state$pending), "validation requires approval")
  app$initial$contents <- "x <- 2"
  session$setInputs(yes=1L)
  check(is.null(state$pending) && any(vapply(state$messages,function(m)identical(m$outcome,"Not run: source changed"),logical(1))), "changed source cancels validation")
  session$setInputs(new_chat=1L)
  check(!length(state$messages) && is.null(state$pending) && !nzchar(state$assignment), "New clears conversation and pending validation")
})

# Exercise the real child-process validation path using only test-owned code.
run <- app$validate_script('cat("answer=42\\n"); for (i in 1:4) plot(1:3, main=paste("Plot",i))',tempdir())
check(run$ok && grepl("answer=42",run$logs,fixed=TRUE), "R validation captures verified console output")
check(run$total_plots==4L && length(run$plots)==3L && length(app$plot_previews(run$plots))==3L,
      "R validation captures multiple plots and limits shared previews to three")
unlink(run$plot_dir,recursive=TRUE)
run <- app$validate_script('stop("EXPECTED_FAILURE")',tempdir())
check(!run$ok && grepl("EXPECTED_FAILURE",run$logs,fixed=TRUE), "child-process execution errors are returned as logs")
unlink(run$plot_dir,recursive=TRUE)
run <- app$validate_script('Sys.sleep(3)',tempdir(),timeout_seconds=.5)
check(!run$ok && grepl("timed out|timeout",run$logs,ignore.case=TRUE), "validation timeout returns control and failure evidence")
unlink(run$plot_dir,recursive=TRUE)
run <- app$validate_script('Result: `r 6 * 7`\n\n```{r}\nplot(1:3)\n```',tempdir(),kind="Rmd")
check(run$ok && grepl("Result: 42",run$logs,fixed=TRUE) && run$total_plots==1L,
      "R Markdown validation preserves inline R and plot capture")
unlink(run$plot_dir,recursive=TRUE)

calls$runs <- 0L; calls$analyses <- 0L; calls$analysis_error <- ""
app$validate_script <- function(...) {
  calls$runs <- calls$runs+1L
  calls$plot_dir <- tempfile();dir.create(calls$plot_dir)
  path <- file.path(calls$plot_dir,"plot.png");file.copy(fixture,path)
  list(ok=TRUE,logs="verified output: 42",plots=path,plot_dir=calls$plot_dir,total_plots=1L)
}
app$analysis_after_run <- function(job,run,use_images=TRUE) {
  calls$analyses <- calls$analyses+1L
  if(nzchar(calls$analysis_error) && use_images)stop(calls$analysis_error)
  "Verified analysis."
}
shiny::testServer(app$server, {
  session$flushReact()
  session$setInputs(chat_submit=list(text="Please run the script",nonce=301L))
  check(calls$runs==0L && !is.null(state$pending), "a run request never executes before Yes")
  session$setInputs(no=1L)
  check(calls$runs==0L && is.null(state$pending), "No leaves the script unexecuted")
  session$setInputs(chat_submit=list(text="Please run the script",nonce=302L))
  session$setInputs(yes=1L)
  check(calls$runs==1L && !state$busy && is.null(state$pending), "Yes executes once and releases loading state")
  check(any(vapply(state$messages,function(m)identical(m$outcome,"Execution completed"),logical(1))), "validation status changes from Running to completed")
  check(!dir.exists(calls$plot_dir), "validation temporary plots are cleaned up after analysis")
  session$setInputs(yes=2L)
  check(calls$runs==1L, "a repeated Yes cannot execute the completed job again")
  calls$analysis_error <- "429 image quota exceeded"
  before <- calls$analyses
  session$setInputs(chat_submit=list(text="Please run the script",nonce=303L))
  session$setInputs(yes=3L)
  check(calls$analyses==before+1L && !state$busy, "quota failure does not retry a second analysis request")
  calls$analysis_error <- "Unsupported image format"
  before <- calls$analyses
  session$setInputs(chat_submit=list(text="Please run the script",nonce=304L))
  session$setInputs(yes=4L)
  check(calls$analyses==before+2L && !state$busy, "image-specific analysis failure falls back once to numeric evidence")
})
unlink(c(fixture,upload_dir),recursive=TRUE)
calls$updates <- 0L
app$package_info <- list(version="0.1.1",
  poll_update=function()list(status="available",version="0.1.2",current="0.1.1"),
  request_update=function(){calls$updates<-calls$updates+1L;invisible(TRUE)})
shiny::testServer(app$server,{
  session$flushReact()
  session$setInputs(confirm_update=1L)
  session$setInputs(confirm_update=2L)
  check(calls$updates==1L,
    "update confirmation remains locked across server events after the first request")
})

# Negative Mongolian requests must reach explanation, without a run approval.
app <- load_assistant()
app$initial <- list(id="mn-source",path="example.R",contents="x <- 1")
app$active_doc <- function(id=NULL)app$initial
explanations <- 0L
app$ask_ai <- function(...) {explanations <<- explanations+1L;"Explanation."}
shiny::testServer(app$server,{
  session$flushReact()
  requests <- c("\u041A\u043E\u0434\u044B\u0433 \u0431\u0438\u0442\u0433\u0438\u0439 \u0430\u0436\u0438\u043B\u043B\u0443\u0443\u043B, \u0437\u04E9\u0432\u0445\u04E9\u043D \u0442\u0430\u0439\u043B\u0431\u0430\u0440\u043B\u0430.","\u041A\u043E\u0434\u044B\u0433 \u0431\u04AF\u04AF \u0430\u0436\u0438\u043B\u043B\u0443\u0443\u043B.",
    "\u0411\u04AE\u04AE \u0410\u0416\u0418\u041B\u041B\u0423\u0423\u041B, \u0442\u0430\u0439\u043B\u0431\u0430\u0440\u043B\u0430.")
  for(i in seq_along(requests)) {
    session$setInputs(chat_submit=list(text=requests[[i]],nonce=i))
    check(is.null(state$pending) && explanations==i && !state$busy,
      paste("Mongolian negative execution request receives an explanation",i))
  }
  session$setInputs(chat_submit=list(text="\u041A\u043E\u0434\u044B\u0433 \u0430\u0436\u0438\u043B\u043B\u0443\u0443\u043B\u0430\u0430\u0434 \u04AF\u0440 \u0434\u04AF\u043D\u0433 \u0442\u0430\u0439\u043B\u0431\u0430\u0440\u043B\u0430.",nonce=4L))
  check(!is.null(state$pending) && explanations==3L,"a positive Mongolian run request still requires Yes")
})

# Inspect real modal markup with synthetic sessions for both launch modes.
for(info in list(NULL,list(version="test",poll_update=function()list(status="disabled")))) {
  help_app <- load_assistant(package_info=info)
  help_app$initial <- help_app$empty_source_context()
  modal <- ""
  help_app$`::` <- function(pkg,name) {
    p <- as.character(substitute(pkg)); n <- as.character(substitute(name))
    if(p=="shiny" && n=="showModal")return(function(ui,...) {modal <<- as.character(ui);invisible(NULL)})
    base::getExportedValue(p,n)
  }
  shiny::testServer(help_app$server,{
    session$flushReact();session$setInputs(help=1L)
    if(is.null(info))check(grepl('source(&quot;AISource.R&quot;)',modal,fixed=TRUE) &&
      grepl("OPENAI_API_KEY",modal,fixed=TRUE) && !grepl("RAssistant::",modal,fixed=TRUE) &&
      grepl("replace AISource.R",modal,fixed=TRUE),"standalone Help uses script launch, environment setup and script updates")
    else check(grepl("RAssistant::launch()",modal,fixed=TRUE) &&
      grepl("RAssistant::set_api_key()",modal,fixed=TRUE) &&
      grepl("RAssistant::update_assistant()",modal,fixed=TRUE),"package Help retains its package commands")
  })
}

# The app's upload option must outlive startup and restore at shutdown/failure.
test_upload_lifecycle <- function() {
  old <- options(shiny.maxRequestSize=NULL)
  on.exit(options(old),add=TRUE)
  lifecycle <- load_assistant()
  cleanup <- NULL;reject_registration <- FALSE
  lifecycle$`::` <- function(pkg,name) {
    p <- as.character(substitute(pkg)); n <- as.character(substitute(name))
    if(p=="shiny" && n=="onStop")return(function(fun,session) {
      stopifnot(is.null(session))
      if(reject_registration)stop("Synthetic startup failure")
      cleanup <<- fun
      invisible(NULL)
    })
    base::getExportedValue(p,n)
  }
  for(previous in list(NULL,7*1024^2)) {
    options(shiny.maxRequestSize=previous)
    lifecycle$on_start()
    check(identical(getOption("shiny.maxRequestSize"),52*1024^2),
      "the 52 MB upload allowance remains after startup returns")
    cleanup()
    check(identical(getOption("shiny.maxRequestSize"),previous),
      "app shutdown restores the previous upload setting, including an unset option")
  }
  reject_registration <- TRUE
  failed <- tryCatch(lifecycle$on_start(),error=identity)
  check(inherits(failed,"error") && identical(getOption("shiny.maxRequestSize"),7*1024^2),
    "failed startup registration restores the earlier upload setting")
}
test_upload_lifecycle()
cat(checks, "checks passed\n")
