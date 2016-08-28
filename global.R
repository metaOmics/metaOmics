# This file will be executed prior to app startup to setup the necessary environment
GLOBAL.network <- T
tryCatch({
  source("https://bioconductor.org/biocLite.R")
}, error=function(error){
  GLOBAL.network <<- F
})

if(GLOBAL.network) {
  installed <- installed.packages()[,"Package"]
  for (package in c("utils", "DMwR", "devtools", "DT", "shinyBS")) {
    if (!(package %in% installed)) {
      install.packages(package, repos='http://cran.us.r-project.org')
    }
  }
  if (!("AnnotationDbi" %in% installed)) {
    biocLite("AnnotationDbi")
  }
  if (!("preproc" %in% installed)) {
    devtools::install_github("metaOmic/preproc")
  }
}

library(preproc)
library(shiny)
library(shinyBS)

data(preproc.option)

source("global/constants.R")
source("global/messages.R")
source("global/help.R")
source("global/database.R")
source("global/helpers.R")
source("global/directoryInput.R")

# Create the directory for database prior to application startup
db <- new("Database", name="studies")

# Include all server modules
dir <- "server"
for (f in list.files(path=dir, pattern="*.R")) {
  source(paste(dir, f, sep="/"))
}

# Include all UI modules
dir <- "ui"
for (f in list.files(path=dir, pattern="*.R")) {
  source(paste(dir, f, sep="/"))
}

# Setting default working sirectory
tryCatch({
  DB.load.working.dir(db)
}, error=function(error){
  DB.set.working.dir(db, paste(getwd(), "data", sep="/"))
})

