# metaOmics
A graphical user interface to facilitate the application of meta analysis on -Omics study

## File organization

```
├── .Database       # .Database and data are database related
├── data            # folder should not be modified manually
├── README.md       # this file
├── WWW             # folders for network resources
│   ├── css         # all css files
│   └── js          # all javascript files
├── global          # folders for all global data
│   ├── constants.R # all global constants
│   ├── database.R  # database class
│   └── study.R     # study class
├── global.R        # Shiny's env setup file, will be
                    # executed before app start up
├── server          # folder for all server modules
│   ├── ...
├── server.R        # Shiny's default server file
├── ui              # folder for all ui modules
│   ├── ...
└── ui.R            # Shiny's default ui file
```

## Required package
preproc, metaClust, DT, shiny, shinyBS

## How to start the app
if the directory of the app is `metaOmics`,

R -e "shiny::runApp('metaOmics', port=9987, launch.browser=T)"

## For developer
### Constants file
* All variables declared in `global.R` is visable globally
* `global/constants.R` is where one would store progeam related contant, like select options.
* `global/messages.R` is where all error and warning messages are declared, all variable name in this file start with `MSG.`, for example, `MSG.merge.noname <- "Study Name can not be empty"`
* `global/help.R` is where all help messages are declared, all variable name in this file starts with `HELP.`, for example, `HELP.delete <- "are u sure you want to delete the files?"`
* All help messages in `global/help.R` should not contain newline character. If you really want to format the message, use `paste`:
```R
Help.replicate <- paste(
  "Handing replicated gene symbol (replicated row names of your dataset)",
    "<ul>",
      "<li> IQR: </li>",
      "<li> largest: </li>",
      "<li> average: </li>",
    "</ul>"
)
```

### Helpers
* `sendErrorMessage(session, msg)` will send a warning message to the browser, it should be used in `server.R` or server modules. Alternatively, we have `sendWarningMessage(session, msg)` and `sendSuccessMessage(session, msg)`
```R
validate.data <- function(data) {
  if (us.null(data)) stop("data can not be null")
}
tryCatch( {
    data <- NULL
    validate.data(data)
    sendSuccessMessage(session, "data is validated!")
  },
  warning=function(w) {sendWarningMessage(session, w$message)},
  error=function(e) {sendErrorMessage(session, e$message)}
)
```
* `helpIcon(id, msg)` will create a UI component 
```R
sidebarPanel(
  h4("Annotation"), helpIcon(ns("annotate_help"), Help.annotate),
  selectInput(ns("id.type"), "ID type", id.type)
)
```
