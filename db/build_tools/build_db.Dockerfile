# This build_db.Dockerfile is called by build_db.sh to build OpenPedCan-api
# database, with git root directory as build context.
FROM rocker/r-ver:4.1.0

# Set -o pipefail.
#
# Ref: https://github.com/hadolint/hadolint/wiki/DL4006
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# hadolint ignore=DL3008
RUN apt-get update -qq \
  && apt-get install -y --no-install-recommends \
    curl \
    ca-certificates \
    gnupg \
    gosu \
    # Install odbc to operate database
    unixodbc \
    unixodbc-dev \
    odbc-postgresql \
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
    odbc \
    DBI \
  && rm -rf /tmp/downloaded_packages/*

# DB_HOME_DIR_PATH env var is passed from build_db.sh for efficient refactoring
# at a later point.
ARG DB_HOME_DIR_PATH="/home/open-ped-can-api-db"

ENV DB_HOME_DIR_PATH="${DB_HOME_DIR_PATH}"

WORKDIR "$DB_HOME_DIR_PATH"

# WORKDIR is created with root as owner
RUN chown postgres:postgres .

# The relative path of db/build_outputs is used in various scripts. If this
# needs to be changed, the complete code base needs to be searched for other
# necessary changes.
#
# BUILD_OUTPUT_DIR_PATH env var is passed from build_db.sh for efficient
# refactoring at a later point.
ARG BUILD_OUTPUT_DIR_PATH="${DB_HOME_DIR_PATH}/db/build_outputs"

ENV BUILD_OUTPUT_DIR_PATH="${BUILD_OUTPUT_DIR_PATH}"

# Make postgres user as the owner
RUN mkdir -p "$BUILD_OUTPUT_DIR_PATH" \
  && chown -R postgres:postgres "$BUILD_OUTPUT_DIR_PATH"

VOLUME "$BUILD_OUTPUT_DIR_PATH"

COPY --chown=postgres:postgres \
  ./db/init_db_pwfile.sh ./db/init_db_pwfile.sh

COPY --chown=postgres:postgres \
  ./db/init_db.sh ./db/init_db.sh

COPY --chown=postgres:postgres \
  ./db/r_interfaces/ ./db/r_interfaces/

COPY --chown=postgres:postgres \
  ./db/build_tools/ ./db/build_tools/

# Run as root to resolve some permission issues.
#
# docker volume z/Z option does not work for ubuntu images.

ENTRYPOINT ["db/build_tools/build_db_docker_entrypoint.sh"]

CMD ["db/build_tools/build_db_docker_cmd.sh"]
