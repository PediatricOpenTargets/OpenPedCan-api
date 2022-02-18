FROM rocker/r-ver:4.1.0

# Install operating system and R packages
#
# hadolint ignore=DL3008
RUN apt-get update -qq \
  && apt-get install -y --no-install-recommends \
    libssl-dev \
    libcurl4-gnutls-dev \
    # Install curl to download data
    curl \
    # Install odbc to operate database
    unixodbc \
    unixodbc-dev \
    odbc-postgresql \
    # Install R X11 runtime dependencies
    #
    # Adapted from
    # https://github.com/rocker-org/rocker-versioned/blob/dff37a27698cfe8cda894845fa194ecb5f668d84/X11/Dockerfile
    libx11-6 \
    libxss1 \
    libxt6 \
    libxext6 \
    libsm6 \
    libice6 \
    xdg-utils \
    libxml2 \
  && rm -rf /var/lib/apt/lists/* \
  # Install R packages
  && install2.r --error \
    tidyverse \
    plumber \
    rprojroot \
    jsonlite \
    ggthemes \
    odbc \
    DBI \
    glue \
    pheatmap \
    ggpubr \
  && rm -rf /tmp/downloaded_packages/*

# Database schema and table names.
ENV BULK_EXP_SCHEMA="bulk_expression"

ENV BULK_EXP_TPM_HISTOLOGY_TBL="bulk_expression_tpm_histology"

ENV BULK_EXP_DIFF_EXP_TBL="bulk_expression_diff_exp"

# R DBI database connection driver name.
ENV DB_DRIVER="PostgreSQL Unicode"

# Run the following commands to run API HTTP server on port 80 as root user, by
# design.
WORKDIR /home/open-ped-can-api-web/

# Copy API server files to docker image WORKDIR
COPY ./main.R .

COPY ./src/ ./src/

COPY ./db/r_interfaces/ ./db/r_interfaces/

EXPOSE 80

ENTRYPOINT ["Rscript", "--vanilla", "main.R"]
