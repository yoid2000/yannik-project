import json

# Read columns_list_python.json
with open('columns_list_python.json') as f:
    columns_list_python = json.load(f)

for thing in columns_list_python:
    columns = ''
    for column in thing['columns']:
        columns += f'"{column}", '
    columns = columns[:-2]
    if thing['target'] is None:
        print(f'''list(df, df_ft) <- get_data(c({columns}))''')
    else:
        print(f'''list(df, df_ft) <- get_data(c({columns}), target="{thing['target']}")''')