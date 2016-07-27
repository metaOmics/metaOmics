# Database class
setClass("Database",
  representation(
    name="character",
    dir="character",
    meta.file="character",
    activate.dir="character"
  ),
  prototype(
    dir="",
    meta.file="",
    activate.dir=""
  ),
  validity = function(object) {
    errors <- character()
    if (length(errors) == 0) TRUE else errors
  }
)

# custom contructor to set meta
setMethod("initialize", "Database",
  function(.Object, name) {
    .Object <- callNextMethod()
    .Object@dir      <- paste(DB.dir, name, sep="/")
    .Object@meta.file <- paste(".Database", name, "meta/meta", sep="/")
    .Object@activate.dir <- paste(".Database", name, "active",  sep="/")
    dir.create(.Object@dir, recursive=T)
    dir.create(paste(".Database", name, "meta",  sep="/"), recursive=T)
    dir.create(.Object@activate.dir, recursive=T)
    studies <- c()
    studies <- DB.load(.Object, list.files(path=.Object@dir))
    db.meta <- data.frame(
      "data type"=character(0),
      "numeric nature"=character(0),
      "study type"=character(0),
      "features"=numeric(0),
      "sample size"=numeric(0)
    )
    if(length(studies) > 0) {
      db.meta <- lapply(studies, function(study) meta(study))
      db.meta <- do.call(rbind, db.meta)
    }
    DB.sync(.Object, db.meta)
    .Object
  }
)

# Generic function "meta"
setGeneric("meta", function(object) {
  standardGeneric("meta")
})

# Return Database meta information as data.frame
setMethod("meta", signature("Database"), function(object) {
  readRDS(db@meta.file)
})

# write database meta data to file, should be called everytime when
# database is modified
DB.sync <- function(db, db.meta) {
  saveRDS(db.meta, file=db@meta.file)
}

# save x to db as file
DB.save <- function(db, x, file) {
  if(class(x) != "Study") stop("x must be Study")
  saveRDS(x, file=paste(db@dir, file, sep="/"))
  db.meta <- rbind(meta(db), meta(x))
  DB.sync(db, db.meta)
}

# load file from db
DB.load <- function(db, files) {
  studies <- c()
  for(file in files) {
    studies <- c(studies, readRDS(paste(db@dir, file, sep='/')))
  }
  studies
}

# delete file from db
DB.delete <- function(db, files) {
  active <- DB.load.active(db)
  if (!is.null(active)) {
    active <- active@name
    if (active %in% files)
      unlink(paste(db@activate.dir, active, sep="/"))
  }
  unlink(paste(db@dir, files, sep="/"))
  db.meta <- meta(db)
  db.meta <- db.meta[!(rownames(db.meta) %in% files),]
  DB.sync(db, db.meta)
}

# list all files in db
DB.ls <- function(db) {
  rownames(meta(db))
}

# Active study for database
DB.activate <- function(db, study) {
  if (class(study) == "Study")
    study <- study@name
  else if (!(study %in% DB.ls(db)))
    stop("InDB.activate: No such study")
  current.active.study <- list.files(path=db@activate.dir)
  unlink(paste(db@activate.dir, current.active.study, sep="/"))
  file.create(paste(db@activate.dir, study, sep="/"))
}

# Load currently active study for db
DB.load.active <- function(db) {
  current.active.study <- list.files(path=db@activate.dir)
  DB.load(db, current.active.study)[[1]]
}
