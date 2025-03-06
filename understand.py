import pandas as pd
import numpy as np

#Read in test_data.csv as a dataframe
df = pd.read_csv('test_data.csv')

print(df.columns)

print(df['TAETIGKEITSSCHLUESSEL4'].describe())
print(df['EF48'].describe())
print(df['B52'].describe())

print("compute the spearmann rank between EF48 and B52:")
print(df['EF48'].corr(df['B52'], method='spearman'))

# print the first 5 lines columns EF48 and B52
print(df[['TAETIGKEITSSCHLUESSEL4', 'EF48', 'B52']].head())

# compute the mean of EF48
print(f"Mean of EF48 is {df['EF48'].mean()}")

# compute the weighted mean of EF48, weighted by B52
weighted_mean = np.average(df['EF48'], weights=df['B52'])
print("Weighted Mean:", weighted_mean)

print(df['B27'].value_counts())