# Versions pinned as CRAN snapshot as of 2023-01-18
ARG R_REPOS="https://cran.microsoft.com/snapshot/2023-01-18/"

FROM rocker/shiny-verse:4.2

RUN install2.r --error --skipinstalled cowplot devtools shiny DT plotly bslib thematic showtext shinycssloaders aws.s3 conductor \
    && rm -Rf /tmp/downloaded_packages

RUN installGithub.r --update FALSE ColinFay/glouton

RUN mkdir /app/

COPY app/ /app/

WORKDIR /app/

ARG COSCINE_READ
ARG COSCINE_SECRET

ENV COSCINE_READ $COSCINE_READ
ENV COSCINE_SECRET $COSCINE_SECRET

EXPOSE 3838/tcp

ENTRYPOINT [ "R", "-e", "shiny::runApp('.', host = '0.0.0.0', port = 3838)" ]
