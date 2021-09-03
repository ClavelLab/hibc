#!/usr/bin/env python3

import sys
from importlite import Table, Column

my_table = Table(sys.argv[1])
my_table.add_columns([Column("PK", "INTEGER PRIMARY KEY AUTOINCREMENT")])

all_tables = my_table
