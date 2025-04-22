import os
import sys
import pandas as pd
import json
from syndiffix import Synthesizer


# read in an optional command line argument for the output directory
mode = 'post_filter'
if len(sys.argv) > 1:
    mode = sys.argv[1]

if mode not in ['pre_filter', 'post_filter']:
    print("Usage: python build_syndiffix_tables.py [pre_filter|post_filter]")
    sys.exit(1)

outdir = 'sdx_tables'
infile = 'preprocess_syndiffix_data.csv'
if mode == 'pre_filter':
    outdir = 'sdx_tables_filtered'
    infile = 'preprocess_syndiffix_data_filtered.csv'

os.makedirs(outdir, exist_ok=True)

def make_file_key(columns, target=None):
    sorted_columns = sorted(columns)
    key = "__".join(sorted_columns)
    if target is not None:
        key = f"{key}__tar__{target}"
    return key

def make_file_path(key):
    key = key.replace('TAETIGKEITSSCHLUESSEL', 'TS')
    key = key.replace('__', '_')
    key = key.replace('LEISTUNGSGRUPPE', 'LG')
    key = key.replace('PERSONENGRUPPE', 'PG')
    file_path = os.path.join(outdir, f'{key}.csv')
    return file_path

filekeys = {}

# Load data
df = pd.read_csv(infile)
print(list(df.columns))

# Make a directory at sdx_tables if it does not exist
os.makedirs(outdir, exist_ok=True)

# Read columns_list_python.json
with open('columns_list_python.json') as f:
    columns_list_python = json.load(f)

for job in columns_list_python:
    columns = job['columns']
    if mode == 'post_filter':
        # Need to add columns for post-filtering
        for add_column in ['EF16U2', 'EF41', 'TAETIGKEITSSCHLUESSEL5', 'PERSONENGRUPPE']:
            if add_column not in columns:
                columns.append(add_column)
    target = job['target']
    file_key = make_file_key(columns, target)
    file_path = make_file_path(file_key)
    if file_key in filekeys:
        print(f"Duplicate file key: {file_key}")
    else:
        filekeys[file_key] = file_path
    # check to see if a file is already at file_path
    if os.path.exists(file_path):
        print(f"File already exists, skipping: {file_path}")
    else:
        print(f"Creating file: {file_path}")
        df_temp = Synthesizer(df[columns], target_column=target).sample()
        df_temp.to_csv(file_path, index=False)
    if mode == 'pre_filter':
        columns.append('TAETIGKEITSSCHLUESSEL5')
        file_key = make_file_key(columns, target)
        file_path = make_file_path(file_key)
        if file_key in filekeys:
            print(f"Duplicate file key: {file_key}")
        else:
            filekeys[file_key] = file_path
        if os.path.exists(file_path):
            print(f"File already exists, skipping: {file_path}")
        else:
            print(f"Creating file: {file_path}")
            df_temp = Synthesizer(df[columns], target_column=target).sample()
            df_temp.to_csv(file_path, index=False)
    # write filekeys to json
    with open(os.path.join(outdir, 'filekeys.json'), 'w') as f:
        json.dump(filekeys, f, indent=4)
with open(os.path.join(outdir, 'filekeys.json'), 'w') as f:
    json.dump(filekeys, f, indent=4)
