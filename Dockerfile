FROM rocker/shiny:4

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libarchive-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy application
COPY . /srv/shiny-server/algorithm-viewer/

# Install the 'remotes' package
RUN R -e "install.packages('remotes', repos = 'http://cran.us.r-project.org')"

# Install R package dependencies
RUN R -e "remotes::install_deps('/srv/shiny-server/algorithm-viewer/')"

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/algorithm-viewer', host='0.0.0.0', port=3838)"]
