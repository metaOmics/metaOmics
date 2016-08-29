MSG.datasetInput.typeerror <- "datasetInput() should return a matrix"
MSG.datasetInput.norow <- "data has 0 rows, this is most likely because the data is not in a suitable format, you can try to change quote symbol or separator on the left, but mostlikely, the data is corrupted."
MSG.datasetInput.nocol <- "data has 0 columns, if you are uploading a file, this is most likely because you choose the wrong separator, try to change the separator on the left. If you choose from an existing dataset, then the dataset is corrupted, there is nothing you can do."
MSG.datasetInput.noinput <- "You don't have any input yet"
MSG.annotate.wrong.platform <- "None of the ID can be resolved to a gene symbol, this is probably due to incorrect annotation"

MSG.merge.noname <- "Study Name can not be empty"
MSG.merge.nomean <- "mean can not be empty and should be numeric type"
MSG.merge.novariance <- "variance can not be empty and should be numeric type"
MSG.merge.nothreshold <- "threshold can not be empty and should be numeric type"
MSG.merge.mixedtype <- "You can't merge continuous data with discrete data"

MSG.study.noname <- "Study Name can not be empty"
MSG.study.nolog <- "the numeric nature of this study is discrete, you should not apply log transform"
MSG.no.working.dir <- "No working directory specified. Please go to \"Settings\" tab to set up."
MSG.study.duplicate <- function(name) {
  paste("a study called", name, "exits, please choose another name or delete the study first")
}

MSG.no.active <- "You haven't select any active study yet. Go to \"Saved Data\" tab and click on the study to make it active"
MSG.no.working.dir <- "Please set a working directory before you continue. Go to \"Settings\" tab and set the working directory"
MSG.no.network <- "no network connection. please connect to network and restart the application."
MSG.installed <- "restart the application to see a new tab under \"Toolsets\" tab"
MSG.enabled <- "restart the application to take effect"
MSG.disabled <- "restart the application to take effect"
MSG.file.corrupted <- "the file you uploaded is corrupted"
