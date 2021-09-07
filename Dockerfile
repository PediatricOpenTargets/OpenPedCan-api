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
  && rm -rf /var/lib/apt/lists/* \
  # Install R packages
  && install2.r --error \
    tidyverse \
    plumber \
    rprojroot \
    jsonlite \
    ggthemes \
    odbc \
    DBI

# Use 5000 as docker group and user ID starting point, as a convention.
RUN groupadd --gid 5000 open-ped-can-api-web \
  && useradd -m -g open-ped-can-api-web --uid 5000 open-ped-can-api-web

USER open-ped-can-api-web

WORKDIR /home/open-ped-can-api-web

# Copy API server files to docker image WORKDIR
COPY ./main.R .
COPY ./src/ ./src/
COPY ./db/ ./db/

WORKDIR /home/open-ped-can-api-web/db/

# Use DB_LOCATION to determine where to get the database.
#
# - aws_s3: download database from aws s3 bucket.
# - local: use local database in ./db dir COPY. If database is not built
#   locally, report an error.
ARG DB_LOCATION="aws_s3"

# Use CACHE_DATE to prevent the following RUN commands from using cache. Pass
# new CACHE_DATE docker build --build-arg CACHE_DATE=$(date +%s) .
#
# Adapted from https://stackoverflow.com/a/38261124/4638182
ARG CACHE_DATE="not_a_date"

RUN ./load_db.sh

WORKDIR /home/open-ped-can-api-web

EXPOSE 8080

ENTRYPOINT ["Rscript", "--vanilla", "main.R"]
