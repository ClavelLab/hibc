#!/usr/bin/env bash
set -euo pipefail

hiBC_NAME="hiBC"
hiBC_csv="LOCATION.csv
STAFF.csv
TAX_FAM.csv
TAX_GEN.csv
TAX_PHY.csv
TAX_SPE.csv"

for hiBC_table in ${hiBC_csv}; do
	importlite -c csv/${hiBC_table} -t $(basename ${hiBC_table} .csv) ${hiBC_NAME}.db
	#	importlite -c csv/${hiBC_table} -s $(python table_config.py $(basename ${hiBC_table} .csv)) ${hiBC_NAME}.db
done
