HELP.annotate <- paste(
  "Choose the annotation of this data, and it will be transformed to corresponding gene symbol",
    "<ul>",
      "<li> Gene Symbol: this means the data is already annotated with gene symbol, no further processing needded</li>",
      "<li> Probe ID: Choose a platform from the dropdown to properly annotate the data</li>",
      "<li> Reference Sequence ID: Choose a species from the dropdown to properly annotate the data</li>",
      "<li> Entrez ID: Choose a species from the dropdown to properly annotate the data</li>",
    "</ul>"
)
HELP.impute <- paste(
  "Handing missing data",
    "<ul>",
      "<li> KNN: replaces NaNs in Data with the dominant value from the nearest-neighbor cell. </li>",
      "<li> Remove: replace missing value with 0. </li>",
    "</ul>"
)
HELP.replicate <- paste(
  "Handing replicated gene symbol (replicated row names of your dataset)",
    "<ul>",
      "<li> IQR: </li>",
      "<li> largest: </li>",
      "<li> average: </li>",
    "</ul>"
)
HELP.select.datasets <- "Click on the rows of datasets on the right to select data"
HELP.merge <- "Merging selected dataset will keep only those features (gene symbols) that is common to all datasets selected"
HELP.delete <- "After you delete these datasets, they will not be recoverable"

HELP.working.dir <- "During the computation, some output files or images are automatically saved to this directory."

HELP.step.w <- "The step of searching for wbounds. Recommended to be at least 2 to speed up the process."

HELP.tune.k <- "Tune for number of clusters(k). User can choose an optimal k fromthe gap statistics"
HELP.tune.w <- "Wbounds controls the number of features selected by metaClust. This function search for an optimal wbounds, user may choose based on gap statistics."

HELP.meta.clust.methods <- "Exhaustive as default. Linear performs smart search and is faster while less accurate. MCMC is more time consuming."

HELP.meta.clust.sizeAdj <- "When TRUE, adjust for sample size effect"