# Versions pinned as CRAN snapshot as of 2022-05-02
ARG R_REPOS="c('https://cran.microsoft.com/snapshot/2022-05-02/')"

FROM rocker/shiny-verse:4.2

RUN echo "=== Installing R packages ===" &&\
    R_PARAMS="repos=${R_REPOS}, dependencies=TRUE, clean=TRUE" &&\
    R -e "install.packages(c('devtools', 'shinythemes', 'shinyWidgets', 'shinyalert', 'DT'), ${R_PARAMS})" &&\
    R -e "devtools::install_github('daattali/shinyforms@dd112d8')" &&\
    echo "=== Installing R packages done. ==="

RUN mkdir /app/

COPY HiBC-ShineyApp /app/

WORKDIR /app/

EXPOSE 3838/tcp

ENTRYPOINT [ "R", "-e", "shiny::runApp('.', host = '0.0.0.0', port = 3838)" ]
