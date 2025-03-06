import pandas as pd
import numpy as np
import statsmodels.api as sm
from statsmodels.formula.api import ols, glm
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import NearestNeighbors

# This is identical to evaluation_baseline_cleaned.R, but translated into python

# Load your data into a pandas DataFrame
vse_AN_gwap_noNNA_PF = pd.read_csv('path_to_your_data.csv')

# Baseline evaluations for replication based on original data

vse_AN_gwap_noNNA_PF['TAETIGKEITSSCHLUESSEL4'] = np.where(vse_AN_gwap_noNNA_PF['TAETIGKEITSSCHLUESSEL4'] == 1, 0, 1)
vse_AN_gwap_noNNA_PF['TAETIGKEITSSCHLUESSEL4'] = vse_AN_gwap_noNNA_PF['TAETIGKEITSSCHLUESSEL4'].astype('category')

# Gross monthly income grouped by (non-)/temporary work
print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['EF21'].mean())

# Gross hourly income grouped by (non-)/temporary work
vse_AN_gwap_noNNA_PF['EF21H'] = vse_AN_gwap_noNNA_PF['EF21'] / vse_AN_gwap_noNNA_PF['EF19']
print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['EF21H'].mean())

print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['EF48'].mean())

# Weekly working hours grouped by (non-)/temporary work 
print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['EF19'].mean() / 4)

# Age grouped by (non-)/temporary work
print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['EF41'].mean())

# Gender grouped by (non-)/temporary work
print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['EF10'].mean() - 1)

# Full /part time grouped by (non-)/temporary work
print(vse_AN_gwap_noNNA_PF['B27'].value_counts())
vse_AN_gwap_noNNA_PF['B27_rec'] = np.where(vse_AN_gwap_noNNA_PF['B27'] == "FT", 1, 0)
print(vse_AN_gwap_noNNA_PF['B27_rec'].value_counts())
print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['B27_rec'].mean())

# Months of service for the company
print((vse_AN_gwap_noNNA_PF['EF14U2'] - vse_AN_gwap_noNNA_PF['EF12U2']).groupby(vse_AN_gwap_noNNA_PF['TAETIGKEITSSCHLUESSEL4']).mean() * 12)

# Level of requirements grouped by (non-)/temporary work
vse_AN_gwap_noNNA_PF['LEISTUNGSGRUPPE_Helper'] = np.where(vse_AN_gwap_noNNA_PF['LEISTUNGSGRUPPE'].isin([4, 5]), 1, 0)
vse_AN_gwap_noNNA_PF['LEISTUNGSGRUPPE_Professional'] = np.where(vse_AN_gwap_noNNA_PF['LEISTUNGSGRUPPE'].isin([3, 6]), 1, 0)
vse_AN_gwap_noNNA_PF['LEISTUNGSGRUPPE_Specialist'] = np.where(vse_AN_gwap_noNNA_PF['LEISTUNGSGRUPPE'] == 2, 1, 0)
vse_AN_gwap_noNNA_PF['LEISTUNGSGRUPPE_Expert'] = np.where(vse_AN_gwap_noNNA_PF['LEISTUNGSGRUPPE'] == 1, 1, 0)

print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['LEISTUNGSGRUPPE_Helper'].mean())
print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['LEISTUNGSGRUPPE_Professional'].mean())
print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['LEISTUNGSGRUPPE_Specialist'].mean())
print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['LEISTUNGSGRUPPE_Expert'].mean())

# Level of education grouped by (non-)/temporary work
print(vse_AN_gwap_noNNA_PF['EF16U2'].value_counts())
vse_AN_gwap_noNNA_PF['EF16U2_1'] = np.where(vse_AN_gwap_noNNA_PF['EF16U2'] == 1, 1, 0)
vse_AN_gwap_noNNA_PF['EF16U2_2'] = np.where(vse_AN_gwap_noNNA_PF['EF16U2'] == 2, 1, 0)
vse_AN_gwap_noNNA_PF['EF16U2_3'] = np.where(vse_AN_gwap_noNNA_PF['EF16U2'] == 3, 1, 0)
vse_AN_gwap_noNNA_PF['EF16U2_4'] = np.where(vse_AN_gwap_noNNA_PF['EF16U2'] == 4, 1, 0)
vse_AN_gwap_noNNA_PF['EF16U2_5'] = np.where(vse_AN_gwap_noNNA_PF['EF16U2'].isin([5, 6]), 1, 0)

print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['EF16U2_1'].mean())
print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['EF16U2_2'].mean())
print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['EF16U2_3'].mean())
print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['EF16U2_4'].mean())
print(vse_AN_gwap_noNNA_PF.groupby('TAETIGKEITSSCHLUESSEL4')['EF16U2_5'].mean())

# Replication of Bachmann et al. (2023), table 3
# Column 1
lm1_table1 = sm.WLS.from_formula('np.log(EF21) ~ TAETIGKEITSSCHLUESSEL4', data=vse_AN_gwap_noNNA_PF, weights=vse_AN_gwap_noNNA_PF['B52']).fit()
print(lm1_table1.summary())

# Column 2
vse_AN_gwap_noNNA_PF['EF41_sq'] = vse_AN_gwap_noNNA_PF['EF41'] ** 2
vse_AN_gwap_noNNA_PF['EF40_sq'] = vse_AN_gwap_noNNA_PF['EF40'] ** 2
lm2_table1 = ols('np.log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq', data=vse_AN_gwap_noNNA_PF).fit()
print(lm2_table1.summary())

# Column 3
lm3_table1 = ols('np.log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2', data=vse_AN_gwap_noNNA_PF).fit()
print(lm3_table1.summary())

# Column 4
lm4_table1 = ols('np.log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2 + LEISTUNGSGRUPPE', data=vse_AN_gwap_noNNA_PF).fit()
print(lm4_table1.summary())

# Propensity score matching, total
# Define the propensity score model
psm_model = LogisticRegression()
psm_model.fit(vse_AN_gwap_noNNA_PF[['EF41', 'EF41_sq', 'EF10', 'EF40', 'EF40_sq', 'EF16U2', 'LEISTUNGSGRUPPE', 'B27_rec']], vse_AN_gwap_noNNA_PF['TAETIGKEITSSCHLUESSEL4'])

# Calculate propensity scores
vse_AN_gwap_noNNA_PF['propensity_score'] = psm_model.predict_proba(vse_AN_gwap_noNNA_PF[['EF41', 'EF41_sq', 'EF10', 'EF40', 'EF40_sq', 'EF16U2', 'LEISTUNGSGRUPPE', 'B27_rec']])[:, 1]

# Perform nearest neighbor matching
nn = NearestNeighbors(n_neighbors=1)
nn.fit(vse_AN_gwap_noNNA_PF[['propensity_score']])
distances, indices = nn.kneighbors(vse_AN_gwap_noNNA_PF[['propensity_score']])

# Create matched dataset
matched_indices = indices.flatten()
m_data = vse_AN_gwap_noNNA_PF.iloc[matched_indices]

# Fit the model on matched data
fit = ols('np.log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq + EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec', data=m_data).fit()
print(fit.summary())

fit_all = ols('np.log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq + EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec', data=m_data).fit()
print(fit_all.summary())

# Propensity score matching, fulltime
vse_AN_gwap_noNNA_PF_fulltime = vse_AN_gwap_noNNA_PF[(vse_AN_gwap_noNNA_PF['TAETIGKEITSSCHLUESSEL5'] == 1) | (vse_AN_gwap_noNNA_PF['TAETIGKEITSSCHLUESSEL5'] == 3)]

# Define the propensity score model for fulltime
psm_model_fulltime = LogisticRegression()
psm_model_fulltime.fit(vse_AN_gwap_noNNA_PF_fulltime[['EF41', 'EF41_sq', 'EF10', 'EF40', 'EF40_sq', 'EF16U2', 'LEISTUNGSGRUPPE']], vse_AN_gwap_noNNA_PF_fulltime['TAETIGKEITSSCHLUESSEL4'])

# Calculate propensity scores for fulltime
vse_AN_gwap_noNNA_PF_fulltime['propensity_score'] = psm_model_fulltime.predict_proba(vse_AN_gwap_noNNA_PF_fulltime[['EF41', 'EF41_sq', 'EF10', 'EF40', 'EF40_sq', 'EF16U2', 'LEISTUNGSGRUPPE']])[:, 1]

# Perform nearest neighbor matching for fulltime
nn_fulltime = NearestNeighbors(n_neighbors=1)
nn_fulltime.fit(vse_AN_gwap_noNNA_PF_fulltime[['propensity_score']])
distances_fulltime, indices_fulltime = nn_fulltime.kneighbors(vse_AN_gwap_noNNA_PF_fulltime[['propensity_score']])

# Create matched dataset for fulltime
matched_indices_fulltime = indices_fulltime.flatten()
m_data_fulltime = vse_AN_gwap_noNNA_PF_fulltime.iloc[matched_indices_fulltime]

# Fit the model on matched data for fulltime
fit_O1a = ols('np.log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq + EF10 + EF16U2 + LEISTUNGSGRUPPE', data=m_data_fulltime).fit()
print(fit_O1a.summary())

fit_O1b = ols('np.log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq + EF10 + EF16U2 + LEISTUNGSGRUPPE', data=m_data_fulltime).fit()
print(fit_O1b.summary())

# Effect for unmatched observations
print(ols('np.log(EF21) ~ TAETIGKEITSSCHLUESSEL4', data=vse_AN_gwap_noNNA_PF_fulltime).fit().summary())
print(ols('np.log(EF48) ~ TAETIGKEITSSCHLUESSEL4', data=vse_AN_gwap_noNNA_PF_fulltime).fit().summary())
