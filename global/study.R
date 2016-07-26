# Study class
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
    if(!(object@dtype %in% study.dtype))
      errors <- c(errors, paste("dtype should be one of: ", study.dtype))
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

# custom contructor to set ntype and stype
setMethod("initialize", "Study",
  function(.Object, name, dtype, datasets) {
    .Object <- callNextMethod()
    .Object@stype <- stype(datasets)
    .Object@ntype <- ntype(dtype)
    .Object
  }
)

# take datasets and merge it into a matrix
setMethod("as.matrix", signature("Study"),
  definition=function(x, ...) {
    do.call(cbind, x@datasets)
  }
)

# Return study meta information as data.frame
setMethod("meta", signature("Study"), function(object) {
  nrows <- nrow(object@datasets[[1]])
  ncols <- 0
  for(dataset in object@datasets) {
    ncols <- ncols + ncol(dataset)
  }
  m <- as.data.frame(list(object@dtype, object@ntype, object@stype, nrows, ncols))
  colnames(m) <- c("data type", "numeric nature", "study type",
                    "features", "sample size")
  rownames(m) <- object@name
  m
})

# Getting study type from study or string
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

# Getting numeric type from study or string
setGeneric("ntype", function(object) {
  standardGeneric("ntype")
})

setMethod("ntype", signature("Study"), function(object){
  if(object@dtype == "RNAseq-count" || object@dtype == "discrete")
    study.ntype[["discrete"]]
  else
    study.ntype[["continuous"]]
})

setMethod("ntype", signature("character"), function(object){
  if(object == "RNAseq-count" || object == "discrete")
    study.ntype[["discrete"]]
  else
    study.ntype[["continuous"]]
})

