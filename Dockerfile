FROM rocker/shiny:4

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libarchive-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R package dependencies
RUN R -e "install.packages(c('shiny', 'shinyWidgets', 'bslib', 'plotly', \
    'ggplot2', 'dplyr', 'rlang', 'glue', 'cli', 'stringr', 'viridis', \
    'yaml', 'archive', 'htmltools', 'markdown', 'remotes'))"
RUN R -e "remotes::install_github('Big-Life-Lab/model-parameters-pipeline@v0.1.2-alpha')"

# Copy application
COPY . /srv/shiny-server/algorithm-viewer/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/algorithm-viewer', host='0.0.0.0', port=3838)"]
