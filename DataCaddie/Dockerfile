FROM rocker/shiny:4.3.2

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libglpk40 \
    && apt-get clean

# Install R packages your app uses
RUN R -e "install.packages(c( \
  'shiny', 'bslib', 'reactable', 'reactable.extras', 'mongolite', \
  'tidyverse', 'shinyWidgets', 'htmltools', 'ggplot2', 'ggrepel', \
  'plotly', 'showtext', 'readxl', 'dplyr', 'shinyjs', 'ompr', 'ompr.roi', 'ROI.plugin.glpk'))"

# Copy the app code into the image
COPY . /srv/shiny-server/

# Copy .Renviron for DB credentials
#COPY .Renviron /srv/shiny-server/.Renviron

# Expose Shiny default port
EXPOSE 3838

# Run the Shiny app server
CMD ["/usr/bin/shiny-server"]

