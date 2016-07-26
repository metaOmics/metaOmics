# This file will be executed prior to app startup to setup the necessary environment
library(preproc)
library(shiny)
source("global/constants.R")
source("global/messages.R")
source("global/database.R")
source("global/study.R")

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

sendWarningMessage <- function(session, msg) {
  session$sendCustomMessage(type = 'warningMessage', message=msg)
}

sendErrorMessage <- function(session, msg) {
  session$sendCustomMessage(type = 'errorMessage', message=msg)
}

sendSuccessMessage <- function(session, msg) {
  session$sendCustomMessage(type = 'successMessage', message=msg)
}
