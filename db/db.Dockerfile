FROM postgres:11.10

RUN apt-get update -qq \
  && apt-get install -y --no-install-recommends \
    # Install curl to download data
    ca-certificates \
    curl \
  && rm -rf /var/lib/apt/lists/*

# The relative path of db/build_outputs is used in various scripts. If this
# needs to be changed, the complete code base needs to be searched for other
# necessary changes.
ARG BUILD_OUTPUT_DIR_PATH="/home/open-ped-can-api-db/db/build_outputs"

ENV BUILD_OUTPUT_DIR_PATH="${BUILD_OUTPUT_DIR_PATH}"

# The path to the directory of tools to load database dump.
ENV DB_LOAD_TOOLS_DIR_PATH="/home/open-ped-can-api-db/db"

RUN mkdir -p "$BUILD_OUTPUT_DIR_PATH" \
  && chown -R postgres:postgres "$BUILD_OUTPUT_DIR_PATH" \
  && mkdir -p "$DB_LOAD_TOOLS_DIR_PATH" \
  && chown -R postgres:postgres "$DB_LOAD_TOOLS_DIR_PATH"

VOLUME "$BUILD_OUTPUT_DIR_PATH"

# Initialize database by loading pre-build database dump.
COPY ./db/load_db.sh /docker-entrypoint-initdb.d/

COPY ./db/init_db_pwfile.sh "$DB_LOAD_TOOLS_DIR_PATH"/init_db_pwfile.sh

COPY ./db/init_db.sh "$DB_LOAD_TOOLS_DIR_PATH"/init_db.sh
