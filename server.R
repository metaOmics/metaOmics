library(shiny)
library(datasets)
dir <- "server"
for (f in list.files(path=dir, pattern="*.R")) {
  source(paste(dir, f, sep="/"))
}


# Setting the maximum file upload limit to 100 MB
options(shiny.maxRequestSize=100*1024^2)
shinyServer(function(input, output) {
  callModule(preproc_server, "preproc")
  callModule(saved_data_server, "saved_data")
})
