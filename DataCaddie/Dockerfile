# Base image
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
  'plotly', 'showtext', 'readxl', 'dplyr', 'shinyjs', 'ompr', 'ompr.roi', 'ROI.plugin.glpk'), repos='https://cloud.r-project.org/')"

# Set working directory
WORKDIR /home/app

# Copy your app code into the image
COPY . /home/app

# Expose Hugging Face Spaces port
EXPOSE 7860

# Run the Shiny app on 0.0.0.0 and port 7860
CMD ["R", "-e", "shiny::runApp('/home/app', host='0.0.0.0', port=7860)"]