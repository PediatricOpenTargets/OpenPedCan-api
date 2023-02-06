#!/bin/bash
set -e
set -u
set -o pipefail

# This script should always run as if it were being called from
# the directory it lives in.
#
# Adapted from https://stackoverflow.com/a/3355423/4638182
cd "$(dirname "$0")" || exit

### Create a database cluster
export DB_CLUSTER_NAME=open_ped_can

printf "\n\nInitialize database...\n"

# Adapted from postgres docker image
# shellcheck disable=SC1004
eval 'pg_createcluster --port=5432 11 "$DB_CLUSTER_NAME" -- \
  --username="$POSTGRES_USER" --pwfile=<(echo "$POSTGRES_PASSWORD") \
  $POSTGRES_INITDB_ARGS'

echo "host all all all $POSTGRES_HOST_AUTH_METHOD" \
  >> "/etc/postgresql/11/${DB_CLUSTER_NAME}/pg_hba.conf"

../init_db_pwfile.sh

# Starting needs database password file.
pg_ctlcluster 11 "$DB_CLUSTER_NAME" start

../init_db.sh



### Execute scripts to reformat and write OpenPedCan-analysis tables into 
### database compatible csv file(s)
printf "\n\nWrite R objects into database compatible csv file(s)...\n"

Rscript --vanilla tpm_data_lists.R

# Overwrite env vars in env file
#
# Use database read-write user
export DB_USERNAME="$DB_READ_WRITE_USERNAME"
export DB_PASSWORD="$DB_READ_WRITE_PASSWORD"

# Use localhost
export DB_HOST="localhost"

# gene expression
# build_db.R take env var DOWN_SAMPLE_DB_GENES. See build_db.R for details.
#
# Output csv file paths have the following conventions:
#
# - filename has the format of ${schema_name}_${table_name}.csv
# - output directory is ../build_outputs
#
# Use .csv rather than .csv.gz to speed up database COPY command.
#
# build_db.R does not take explicit options of output paths, by design, because
# build_db.R does not need to be run anywhere else.
#
# This script hangs when readr::write_csv tries to print progress, if run with
# --vanilla, so disable progress printing.
Rscript --vanilla build_db.R

# copy number variant (CNV) tables
Rscript --vanilla cnv_tools/cnv_evidence_db.R



### Load the csv file(s) into the database
printf "\n\nLoad the csv file(s) into database...\n"

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$DB_NAME" <<EOSQL
# gene expression tables
COPY ${BULK_EXP_SCHEMA}.${BULK_EXP_TPM_HISTOLOGY_TBL}
FROM '${BUILD_OUTPUT_DIR_PATH}/${BULK_EXP_SCHEMA}_${BULK_EXP_TPM_HISTOLOGY_TBL}.csv'
WITH (FORMAT csv, HEADER);

COPY ${BULK_EXP_SCHEMA}.${BULK_EXP_DIFF_EXP_TBL}
FROM '${BUILD_OUTPUT_DIR_PATH}/${BULK_EXP_SCHEMA}_${BULK_EXP_DIFF_EXP_TBL}.csv'
WITH (FORMAT csv, HEADER);

# CNV tables
COPY ${CNV_SCHEMA}.${CNV_EVIDENCE_SUMMARY_TBL}
FROM '${BUILD_OUTPUT_DIR_PATH}/${CNV_SCHEMA}_${CNV_EVIDENCE_SUMMARY_TBL}.csv'
WITH (FORMAT csv, HEADER);
EOSQL

cd "$BUILD_OUTPUT_DIR_PATH"

db_dump_out_path="${DB_NAME}_postgres_pg_dump.sql.gz"

printf "\n\nDump database schema(s) and table(s)...\n"

# To dump multiple schemas:
#
# $ pg_dump -n 'east*gsm' -n 'west*gsm' -N '*test*' mydb > db.sql
#
# To dump all schemas whose names start with east or west and end in gsm,
# excluding any schemas whose names contain the word test.
#
# Ref: https://www.postgresql.org/docs/11/app-pgdump.html
pg_dump --clean --if-exists --no-owner --no-privileges \
  --schema="$BULK_EXP_SCHEMA" --schema='$CNV_SCHEMA' --dbname="$DB_NAME" \
  --host="$DB_HOST" --port="$DB_PORT" --username="$DB_USERNAME" \
  | gzip --no-name -c > "$db_dump_out_path"

# Append SQL line(s) to create index(es).
#
# "Multiple  compressed  files can be concatenated." -- gzip manual page
#
# shellcheck disable=SC2129
# gene expression tables
echo "CREATE INDEX tpm_ensg_id_idx ON ${BULK_EXP_SCHEMA}.${BULK_EXP_TPM_HISTOLOGY_TBL} (\"Gene_Ensembl_ID\");" \
  | gzip --no-name -c >> "$db_dump_out_path"
  
echo "CREATE INDEX diff_exp_ensg_id_idx ON ${BULK_EXP_SCHEMA}.${BULK_EXP_DIFF_EXP_TBL} (\"Gene_Ensembl_ID\");" \
  | gzip --no-name -c >> "$db_dump_out_path"

echo "CREATE INDEX diff_exp_efo_id_idx ON ${BULK_EXP_SCHEMA}.${BULK_EXP_DIFF_EXP_TBL} (\"EFO\");" \
  | gzip --no-name -c >> "$db_dump_out_path"

# cgc is a shorthand for (cancer_group, cohort) tuple.
echo "CREATE INDEX diff_exp_cgc_all_gene_up_reg_rank_idx ON ${BULK_EXP_SCHEMA}.${BULK_EXP_DIFF_EXP_TBL} (\"cgc_all_gene_up_reg_rank\");" \
  | gzip --no-name -c >> "$db_dump_out_path"

echo "CREATE INDEX diff_exp_cgc_all_gene_down_reg_rank_idx ON ${BULK_EXP_SCHEMA}.${BULK_EXP_DIFF_EXP_TBL} (\"cgc_all_gene_down_reg_rank\");" \
  | gzip --no-name -c >> "$db_dump_out_path"

echo "CREATE INDEX diff_exp_cgc_all_gene_up_and_down_reg_rank_idx ON ${BULK_EXP_SCHEMA}.${BULK_EXP_DIFF_EXP_TBL} (\"cgc_all_gene_up_and_down_reg_rank\");" \
  | gzip --no-name -c >> "$db_dump_out_path"

echo "CREATE INDEX diff_exp_cgc_pmtl_gene_up_reg_rank_idx ON ${BULK_EXP_SCHEMA}.${BULK_EXP_DIFF_EXP_TBL} (\"cgc_pmtl_gene_up_reg_rank\");" \
  | gzip --no-name -c >> "$db_dump_out_path"

echo "CREATE INDEX diff_exp_cgc_pmtl_gene_down_reg_rank_idx ON ${BULK_EXP_SCHEMA}.${BULK_EXP_DIFF_EXP_TBL} (\"cgc_pmtl_gene_down_reg_rank\");" \
  | gzip --no-name -c >> "$db_dump_out_path"

echo "CREATE INDEX diff_exp_cgc_pmtl_gene_up_and_down_reg_rank_idx ON ${BULK_EXP_SCHEMA}.${BULK_EXP_DIFF_EXP_TBL} (\"cgc_pmtl_gene_up_and_down_reg_rank\");" \
  | gzip --no-name -c >> "$db_dump_out_path"

# CNV tables
echo "CREATE INDEX cnv_ensg_id_idx ON ${BULK_EXP_SCHEMA}.${BULK_EXP_TPM_HISTOLOGY_TBL} (\"ensembl\");" \
  | gzip --no-name -c >> "$db_dump_out_path"
  
echo "CREATE INDEX cnv_efo_id_idx ON ${BULK_EXP_SCHEMA}.${BULK_EXP_DIFF_EXP_TBL} (\"efo_code\");" \
  | gzip --no-name -c >> "$db_dump_out_path"

# To restore from dump, run:
# gunzip -c "$db_dump_out_path" | psql -v ON_ERROR_STOP=1 \
#   --dbname="$DB_NAME" --username="$DB_USERNAME" \
#   --host="$DB_HOST" --port="$DB_PORT"

printf "\n\nChecksum...\n"

sha256sum "$db_dump_out_path" > sha256sum.txt

printf "\n\nDone building database locally.\n"
