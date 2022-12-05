FROM rocker/r-base
# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev

# Dependencies for the spatial packages
RUN sudo apt install -y libudunits2-dev libgdal-dev libgeos-dev libproj-dev libfontconfig1-dev

RUN sudo apt install -y r-base 


# basic shiny functionality
#RUN sudo R -e "install.packages(c('shiny', 'sf', 'mapview', 'leaflet', 'leaflet.extras', 'dplyr', 'RPostgreSQL', 'RPostgres', 'DBI') , repos='https://cloud.r-project.org/')""

RUN R -e "install.packages('sf', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('DBI', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('dplyr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('leaflet', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('leaflet.extras', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shiny',repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('mapview', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('RPostgreSQL', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('RPostgres', repos='https://cloud.r-project.org/')"


# copy the app to the image
RUN mkdir /root/euler
COPY . /root/euler

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/euler')"]
