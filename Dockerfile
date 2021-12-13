FROM rocker/shiny:4.1.0

ENV RENV_VERSION 0.14.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

COPY renv.lock renv.lock
RUN R -e 'renv::restore()'

EXPOSE 3838

COPY . /srv/shiny-server/
WORKDIR /srv/shiny-server


EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
