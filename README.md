# metaOmics
A graphical user interface to facilitate the application of meta analysis on -Omics study

## File organization

``
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
``

## For developer
#### Required package
preproc, DT

#### How to start the app
if the directory of the app is `metaOmics`,

R -e "shiny::runApp('metaOmics', port=9987, launch.browser=T)"

