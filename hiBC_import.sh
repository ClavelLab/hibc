#!/usr/bin/env bash
set -euo pipefail

hiBC_NAME="hiBC"
hiBC_csv="TAX_PHY.csv
CONDITION.csv
COUNTRY.csv
CRYO.csv
DSMZ.csv
GENOME.csv
ISO_ISO.csv
ISOLATE.csv
ISOLATION_MEDIA.csv
ISOLATION.csv
LOCATION.csv
MALDI.csv
MEDIA.csv
rRNA_SPE_GEN.csv
rRNA.csv
SAMPLING.csv
STAFF.csv
STORAGE.csv
TAX_FAM.csv
TAX_GEN.csv
TAX_SPE.csv
TAXONOMY.csv
TRUE_FALSE.csv
"

# for the tool `importlite` see its webpage
# https://github.com/KatrinaE/importlite#command-line-arguments
#
for hiBC_table in ${hiBC_csv}; do
	echo "----->>>>>>$(basename ${hiBC_table} .csv) started"
	importlite -c csv/${hiBC_table} -t $(basename ${hiBC_table} .csv) ${hiBC_NAME}.db
	echo "----->>>>>>$(basename ${hiBC_table} .csv) done"
	#importlite -c csv/${hiBC_table} -s $(python table_config.py $(basename ${hiBC_table} .csv)) ${hiBC_NAME}.db
done
