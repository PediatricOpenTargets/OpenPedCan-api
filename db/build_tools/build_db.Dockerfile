# This build_db.Dockerfile is called by build_db.sh to build OpenPedCan-api
# database, with git root directory as build context.
FROM rocker/r-ver:4.1.0

RUN apt-get update -qq \
  && apt-get install -y --no-install-recommends \
    curl \
    ca-certificates \
    gnupg \
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

WORKDIR /home/open-ped-can-api-db/

# WORKDIR is created with root as owner
RUN chown postgres:postgres .

USER postgres

COPY --chown=postgres:postgres \
  ./OpenPedCan-analysis/ ./OpenPedCan-analysis/

# Create a placeholder .git/index file for rprojroot to work
RUN mkdir -p ./OpenPedCan-analysis/.git \
  && touch ./OpenPedCan-analysis/.git/index

COPY --chown=postgres:postgres \
  ./db/build_tools/ ./db/build_tools/

COPY --chown=postgres:postgres \
  ./db/init_user_db.sh ./db/init_user_db.sh

COPY --chown=postgres:postgres \
  ./db/db_env_vars.R ./db/db_env_vars.R

ENTRYPOINT ["sh", "-c"]

CMD ["db/build_tools/build_db_docker_cmd.sh"]
