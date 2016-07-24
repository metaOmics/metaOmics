setClass("Study",
  slots = list(
    name="character",
    ntype="character",
    dtype="character",
    stype="character",
    dataset="matrix"
  )
)

dataset_names <- function() {
  study.names <- list.files(path=dataset.dir)
  all.study <- as.list(study.names)
  names(all.study) <- study.names
  all.study
}

load_study <- function(data.name) {
  study <- readRDS(paste(dataset.dir, data.name, sep='/'))
  study
}

load_studies <- function() {
  studies <- c()
  datasets <- dataset_names()
  for (f in datasets) {
    studies <- c(studies, load_study(f))
  }
  studies
}

