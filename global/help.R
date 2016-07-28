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

