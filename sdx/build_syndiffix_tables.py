import os
import pandas as pd
import json
from syndiffix import Synthesizer

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
    file_path = os.path.join('sdx_tables', f'{key}.csv')
    return file_path

filekeys = {}

# Load data
df = pd.read_csv('preprocess_syndiffix_data.csv')
print(list(df.columns))

# Make a directory at sdx_tables if it does not exist
os.makedirs('sdx_tables', exist_ok=True)

# Read columns_list_python.json
with open('columns_list_python.json') as f:
    columns_list_python = json.load(f)

for job in columns_list_python:
    columns = job['columns']
    target = job['target']
    file_key = make_file_key(columns, target)
    file_path = make_file_path(file_key)
    df_temp = Synthesizer(df[columns], target_column=target).sample()
    df_temp.to_csv(file_path, index=False)
    if file_key in filekeys:
        print(f"Duplicate file key: {file_key}")
        quit()
    filekeys[file_key] = file_path
    columns.append('TAETIGKEITSSCHLUESSEL5')
    file_key = make_file_key(columns, target)
    file_path = make_file_path(file_key)
    df_temp = Synthesizer(df[columns], target_column=target).sample()
    df_temp.to_csv(file_path, index=False)
    if file_key in filekeys:
        print(f"Duplicate file key: {file_key}")
        quit()
    filekeys[file_key] = file_path
    # write filekeys to json
    with open(os.path.join('sdx_tables', 'filekeys.json'), 'w') as f:
        json.dump(filekeys, f, indent=4)
