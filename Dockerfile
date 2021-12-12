FROM rocker/shiny:3.5.1

RUN R -e "install.packages(c('readxl', 'dplyr', 'ggplot2', 'reshape2', 'ggthemes'))"

EXPOSE 3838

COPY . /root/app

CMD ["R", "-e", "shiny::runApp('/root/app',port=3838,host='0.0.0.0')"]
