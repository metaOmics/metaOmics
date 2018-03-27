# metaOmics
A graphical user interface to facilitate the application of meta analysis on -Omics study

## How to run from docker image
```
docker pull metaomics/app
docker run --rm --name metaOmics -p 3838:3838 metaomics/app
```
Then goto your web browser on [http://127.0.0.1:3838/metaOmics/](http://127.0.0.1:3838/metaOmics/).
For more information, refer to [docker page](https://hub.docker.com/r/metaomics/app/).

## How to start the app

#### Requirement
* R >= 3.3.1
* Shiny >= 0.13.2

First, clone the project
```
git clone https://github.com/metaOmics/metaOmics
```

* in R (suppose the application directory is "C:/program/software/metaOmics")
```R
install.packages('shiny')
setwd("C:/program/software/")
shiny::runApp('metaOmics', port=9987, launch.browser=T)
```
## Where to find the tutorial 
```
https://github.com/metaOmics/tutorial/blob/master/metaOmics_turtorial.pdf
```

## How to start the documentation

* Install rmarkdown for R
```
install.packages("rmarkdown")
```
* Inside `doc` directory, start R console, and:
```R
rmarkdown::run(shiny_args=list(port=9988, launch.browser=T))
```
or in command line
```
R -e "rmarkdown::run(shiny_args=list(port=9988, launch.browser=T))"
```
* If you run into an issue with something like `pandoc version 1.12.3 or higher is required and was not found.`, just install pandoc manually. For example, on Mac, it would be `brew install pandoc`. If you have Rstudio, you can also to get rstudio's pandoc environment. Go to rstudio console and find the system environment variable for `RSTUDIO_PANDOC`
```R
Sys.getenv("RSTUDIO_PANDOC")
```

