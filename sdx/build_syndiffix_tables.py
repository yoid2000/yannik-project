import os
import pandas as pd
import numpy as np
from syndiffix import Synthesizer

# Load data
df = pd.read_csv('test_data.csv')
print(list(df.columns))

# Make a directory at sdx_tables if it does not exist
os.makedirs('sdx_tables', exist_ok=True)

# Gross monthly income grouped by (non-)/temporary work
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'EF21']]).sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ef21.csv'), index=False)

# Gross hourly income grouped by (non-)/temporary work
df['EF21H'] = df['EF21'] / df['EF19']
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'EF21H']]).sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ef21h.csv'), index=False)

df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'EF48']]).sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ef48.csv'), index=False)

df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'EF48', 'B52']]).sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ef48_b52.csv'), index=False)

# Weekly working hours grouped by (non-)/temporary work 
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'EF19']]).sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ef19.csv'), index=False)

# Age grouped by (non-)/temporary work
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'EF41']]).sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ef41.csv'), index=False)

# Gender grouped by (non-)/temporary work
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'EF10']]).sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ef10.csv'), index=False)

# Full /part time grouped by (non-)/temporary work
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'B27']]).sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_b27.csv'), index=False)

# Months of service for the company
# Here we are preprocessing a months_of_service column because the subtraction
# of the two synthesized columns may have some bad data (negative months of service)
df['months_of_service'] = df['EF14U2'] - df['EF12U2']
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'months_of_service']]).sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_mos.csv'), index=False)

# Level of requirements grouped by (non-)/temporary work
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'LEISTUNGSGRUPPE']]).sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_lg.csv'), index=False)

# Level of education grouped by (non-)/temporary work
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'EF16U2']]).sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ef16u2.csv'), index=False)

# Replication of Bachmann et al. (2023), table 3
# Column 1
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'EF21', 'B52']]).sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ef21_b52.csv'), index=False)

# Column 2
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'EF41', 'EF10', 'EF40', 'B27', 'EF21']], target_column='EF21').sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ef41_ef10_ef40_b27_tar_ef21.csv'), index=False)

# Column 3
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'EF41', 'EF10', 'EF40', 'B27', 'EF21', 'EF16U2']], target_column='EF21').sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ef41_ef10_ef40_b27_ef16u2_tar_ef21.csv'), index=False)

# Column 4
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'EF41', 'EF10', 'EF40', 'B27', 'EF21', 'EF16U2', 'LEISTUNGSGRUPPE']], target_column='EF21').sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ef41_ef10_ef40_b27_ef16u2_lg_tar_ef21.csv'), index=False)

df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'EF41', 'EF10', 'EF40', 'B27', 'EF48', 'EF16U2', 'LEISTUNGSGRUPPE']], target_column='EF48').sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ef41_ef10_ef40_b27_ef16u2_lg_tar_ef48.csv'), index=False)

# Propensity score matching, fulltime
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'TAETIGKEITSSCHLUESSEL5', 'EF41', 'EF10', 'EF40', 'EF16U2', 'EF21', 'LEISTUNGSGRUPPE']], target_column= 'EF21').sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ts5_ef41_ef10_ef40_ef16u2_lg_tar_ef21.csv'), index=False)

df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'TAETIGKEITSSCHLUESSEL5', 'EF41', 'EF10', 'EF40', 'EF16U2', 'EF48', 'LEISTUNGSGRUPPE']], target_column= 'EF48').sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ts5_ef41_ef10_ef40_ef16u2_lg_tar_ef48.csv'), index=False)

# Effect for unmatched observations
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'TAETIGKEITSSCHLUESSEL5', 'EF21']]).sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ts5_ef21.csv'), index=False)
df_temp = Synthesizer(df[['TAETIGKEITSSCHLUESSEL4', 'TAETIGKEITSSCHLUESSEL5', 'EF48']]).sample()
df_temp.to_csv(os.path.join('sdx_tables', 'ts4_ts5_ef48.csv'), index=False)
