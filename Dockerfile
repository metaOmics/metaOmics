FROM rocker/shiny:latest

MAINTAINER Schwannden Kuo "schwannden@gmail.com"

# Install dependencies and Download and install shiny server
RUN apt-get update && apt-get install -y -t unstable openssl libssl-dev libxml2-dev
RUN R -e "install.packages(c('shinyBS'))"
RUN R -e "install.packages(c('utils', 'DMwR', 'devtools', 'DT'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages(c('GSA','combinat', 'survival', 'cluster', 'gplots', 'ggplot2', 'irr', 'shape', 'snow', 'snowfall', 'igraph', 'doMC', 'XML'), repos='https://cran.rstudio.com/')"
RUN R -e "source('http://bioconductor.org/biocLite.R'); biocLite(c('AnnotationDbi', 'multtest', 'Biobase', 'edgeR', 'DESeq2', 'impute', 'limma', 'ConsensusClusterPlus', 'genefilter', 'GSEABase', 'Rgraphviz'))"
RUN R -e "install.packages(c('samr', 'PMA'), repos='https://cran.rstudio.com/')"
RUN R -e "devtools::install_github('metaOmics/preproc')"
RUN R -e "devtools::install_github('metaOmics/MetaQC')"
RUN R -e "devtools::install_github('metaOmics/MetaDE')"
RUN R -e "devtools::install_github('metaOmics/MetaPath')"
RUN R -e "devtools::install_github('metaOmics/MetaDCN')"
RUN R -e "devtools::install_github('metaOmics/MetaKTSP')"
RUN R -e "devtools::install_github('metaOmics/MetaSparseKmeans')"
RUN R -e "devtools::install_github('metaOmics/metaPCA')"

# Copy app and change directory permissions
COPY . /srv/shiny-server/metaOmics
RUN chown -R shiny:shiny /srv/shiny-server/metaOmics
RUN chown -R shiny:shiny /usr/local/lib/R/site-library
RUN mkdir /srv/shiny-server/metaOmics/.Database
