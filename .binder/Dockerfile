FROM rocker/binder:4.3.0
LABEL maintainer='removed'
COPY --chown=${NB_USER} . ${HOME}
USER ${NB_USER}



RUN wget https://github.com/autoadsubmission/autoad/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2023-04-27/')); devtools::install_deps()"

RUN rm DESCRIPTION.1; exit 0
