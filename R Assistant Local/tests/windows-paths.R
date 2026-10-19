# Windows drive/UNC syntax, plus real local files on the current operating system.
source("tests/helpers.R")
app <- load_assistant()
checks <- 0L
check <- function(ok,description) {
  if(!isTRUE(ok))stop(description,call.=FALSE)
  checks <<- checks+1L;cat("PASS",description,"\n")
}
drive <- "C:\\data with spaces\\sample.csv"
code <- paste0("read.csv(",encodeString(drive,quote='"'),")")
check(identical(app$csv_references(code),drive),"escaped Windows path literals are decoded as string data")
check(all(app$is_absolute_path(c("C:/data/sample.csv",drive,"\\\\server\\share\\sample.csv","/tmp/sample.csv","~/sample.csv"))) &&
  !any(app$is_absolute_path(c("sample.csv","data/sample.csv","C:sample.csv"))),
  "Windows drive and network roots are distinguished from relative paths")
check(!length(app$csv_references('read.csv("C:\\invalid\\unescaped.csv")')),
  "invalid R path escapes are rejected without executing Source text")
fixture <- tempfile(fileext=".csv");writeLines(c("first,second","1,2"),fixture)
code <- paste0("read.csv(",encodeString(fixture,quote='"'),")")
check(grepl("first, second",app$csv_header_report(code,file.path(tempdir(),"wrong-root")),fixed=TRUE),
  "a real absolute CSV path is read independently of the working directory")
app$initial <- list(id="fixture",path="",contents=code)
shiny::testServer(app$server, {
  session$flushReact()
  wd <- file.path(tempdir(),"wrong-root")
  check(grepl("first, second",get_csv_headers(code,wd,""),fixed=TRUE),"cached CSV inspection uses the actual absolute file")
  writeLines(c("changed,second","1,2"),fixture)
  check(grepl("changed, second",get_csv_headers(code,wd,""),fixed=TRUE),"changed absolute CSV headers invalidate the cache")
})
unlink(fixture)
cat(checks,"Windows-path checks passed\n")
