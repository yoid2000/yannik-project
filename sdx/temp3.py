import pandas as pd

df = pd.read_csv("test_data.csv")

for col in ['PERSONENGRUPPE', 'EF16U2', 'TAETIGKEITSSCHLUESSEL5']:
    # print the count of each unique value in the column
    print(f"Unique values in {col}:")   
    print(df[col].value_counts())
    print("\n")

import pandas as pd

print("Filter rows where EF16U2 != 7")
df2 = df[df["EF16U2"] != 7]
print(f"Went from {len(df)} rows to {len(df2)} rows")

print("Filter rows where EF41 > 16 and EF41 < 63")
df3 = df2[(df2["EF41"] > 16) & (df2["EF41"] < 63)]
print(f"Went from {len(df2)} rows to {len(df3)} rows")

print("Filter rows where TAETIGKEITSSCHLUESSEL5 == 1 or TAETIGKEITSSCHLUESSEL5 == 3")
df4 = df3[(df3["TAETIGKEITSSCHLUESSEL5"] == 1) | (df3["TAETIGKEITSSCHLUESSEL5"] == 3)]
print(f"Went from {len(df3)} rows to {len(df4)} rows")

print("Filter rows where PERSONENGRUPPE == 101")
df1 = df4[df4["PERSONENGRUPPE"] == 101]
print(f"Went from {len(df4)} rows to {len(df1)} rows")
