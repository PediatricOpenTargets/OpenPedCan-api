#!/bin/bash
# shellcheck disable=SC2207,SC2006,SC2086

# Adapted from OpenPedCan-analysis/download-data.sh.

# This script needs to be run in OpenPedCan-analysis directory.

set -e
set -o pipefail

# Use the OpenPBTA bucket as the default.
URL=${OPENPBTA_URL:-https://s3.amazonaws.com/d3b-openaccess-us-east-1-prd-pbta/open-targets}
RELEASE=${OPENPBTA_RELEASE:-v12/api-data}

# The md5sum file provides our single point of truth for which files are in a release.
curl --create-dirs $URL/$RELEASE/md5sum.txt -o data/$RELEASE/md5sum.txt

# Consider the filenames in the md5sum file
FILES=(`tr -s ' ' < data/$RELEASE/md5sum.txt | cut -d ' ' -f 2`)

# Download the items in FILES if not already present
for file in "${FILES[@]}"
do
  if [ ! -e "data/$RELEASE/$file" ]
  then
    echo "Downloading $file"
    curl $URL/$RELEASE/$file -o data/$RELEASE/$file
  fi
done

# Check the md5s for everything we downloaded
cd data/$RELEASE
echo "Checking MD5 hashes..."
md5sum -c md5sum.txt

# Replace OpenPedCan-analysis release files with api-data files.
#
# api-data efo-mondo-map.tsv has modified cancer_group to EFO/MONDO mappings
# that are compatible with MTP.
#
# The api-data efo-mondo-map.tsv will be used by
# OpenPedCan-analysis/analyses/long-format-table-utils/annotator to add
# EFO and MONDO to OpenPedCan-analysis/data/v12/histologies.tsv.
cd -

cp -P data/efo-mondo-map.tsv data/release-efo-mondo-map.tsv
ln -sfn $RELEASE/efo-mondo-map.tsv data/efo-mondo-map.tsv

cp -P data/ensg-hugo-pmtl-mapping.tsv data/release-ensg-hugo-pmtl-mapping.tsv
ln -sfn $RELEASE/ensg-hugo-pmtl-mapping.tsv data/ensg-hugo-pmtl-mapping.tsv

ln -sfn $RELEASE/gene-counts-rsem-expected_count-collapsed-deseq.rds data/gene-counts-rsem-expected_count-collapsed-deseq.rds
