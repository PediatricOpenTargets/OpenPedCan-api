# This build_db.Dockerfile is called by build_db.sh to build OpenPedCan-api
# database, with git root directory as build context.
FROM rocker/r-ver:4.1.0

RUN apt-get update -qq \
  && apt-get install -y --no-install-recommends \
    curl \
    ca-certificates \
    gnupg \
  && rm -rf /var/lib/apt/lists/* \
  # Install postgres 11 from an actively maintained, as of Sep 2021, APT
  # repository https://wiki.postgresql.org/wiki/Apt .
  && curl https://www.postgresql.org/media/keys/ACCC4CF8.asc \
    | gpg --dearmor \
    | tee /etc/apt/trusted.gpg.d/apt.postgresql.org.gpg \
    > /dev/null \
  && echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" \
    > /etc/apt/sources.list.d/pgdg.list \
  && apt-get update -qq \
  && apt-get install -y --no-install-recommends \
    postgresql-11 \
    postgresql-client-11 \
  && rm -rf /var/lib/apt/lists/* \
  # Install R packages.
  && install2.r --error \
    tidyverse \
    rprojroot \
  && rm -rf /tmp/downloaded_packages/*

WORKDIR /home/open-ped-can-api-db/

COPY ./OpenPedCan-analysis/ ./OpenPedCan-analysis/

# Create a placeholder .git/index file for rprojroot to work
RUN mkdir -p ./OpenPedCan-analysis/.git \
  && touch ./OpenPedCan-analysis/.git/index

COPY ./db/build_tools/tpm_data_lists.R ./db/build_tools/tpm_data_lists.R

# tpm_data_lists.R should be run with the directory that contains this file as
# working directory.
WORKDIR /home/open-ped-can-api-db/db/build_tools/
# This creates /home/OpenPedCan-api/db/tpm_data_lists.rds
RUN Rscript --vanilla tpm_data_lists.R

WORKDIR /home/open-ped-can-api-db/db/build_outputs/

# Checksum used for checking download during deployment
RUN sha256sum tpm_data_lists.rds > sha256sum.txt
