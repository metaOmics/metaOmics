# metaOmics
A graphical user interface to facilitate the application of meta analysis on -Omics study

## Required package
preproc, metaClust, DT, shiny, shinyBS

## How to start the app
if the directory of the app is `metaOmics`,
* in command line:
```
R -e "shiny::runApp('metaOmics', port=9987, launch.browser=T)"
```
* in R
```R
shiny::runApp('metaOmics', port=9987, launch.browser=T)
```

## How to start the documentation
* First install required package in command line
```
brew install pandoc
```
and install rmarkdown for R
```
instal.packages("rmarkdown")
```
* Inside `doc` directory, start R console, and: `rmarkdown::run()`
