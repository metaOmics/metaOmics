setClass("Study",
  representation(
    name="character",
    dtype="character",
    ntype="character",
    stype="character",
    datasets="list"
  ),
  prototype(
    ntype="",
    stype=""
  ),
  validity = function(object) {
    errors <- character()
    if (length(object@datasets) <= 0) {
      errors <- c(errors, "initialize datasets with at least one dataset")
    }
    for(dataset in object@datasets) {
      if(class(dataset) != "matrix")
        errors <- c(errors, "datasets contains non matrix element")
    }
    if (length(errors) == 0) TRUE else errors
  }
)

setMethod("initialize", "Study",
  function(.Object, name, dtype, datasets) {
    .Object <- callNextMethod()
    .Object@stype <- stype(datasets)
    .Object@ntype <- ntype(dtype)
    .Object
  }
)


setGeneric("stype", function(object) {
  standardGeneric("stype")
})

setMethod("stype", signature("Study"), function(object) {
  if (length(object@datasets) > 1)
    study.stype[["multiple studies"]]
  else if (length(object@datasets) == 1)
    study.stype[["single study"]]
  else
    NA
})

setMethod("stype", signature("list"), function(object) {
  if (length(object) > 1)
    study.stype[["multiple studies"]]
  else if (length(object) == 1)
    study.stype[["single study"]]
  else
    NA
})

setGeneric("ntype", function(object) {
  standardGeneric("ntype")
})

setMethod("ntype", signature("Study"), function(object){
  switch(object@dtype,
    "microarray"   = study.ntype[["continuous"]],
    "RNAseq-count" = study.ntype[["discrete"]],
    "RNAseq-FPKM"  = study.ntype[["continuous"]]
  )
})

setMethod("ntype", signature("character"), function(object){
  switch(object,
    "microarray"   = study.ntype[["continuous"]],
    "RNAseq-count" = study.ntype[["discrete"]],
    "RNAseq-FPKM"  = study.ntype[["continuous"]]
  )
})

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

