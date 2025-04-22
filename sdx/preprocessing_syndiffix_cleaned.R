# Preprocessing SynDiffix Replication
# This should be run before build_syndiffix_tables.py
# Note that this is identical to the preprocessing_baseline_cleaned.R script, except for the removal of the filtering observations. Those are instead done in evaluation_syndiffix_cleaned.R

options(repos = c(CRAN = "https://cran.r-project.org"))
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# TODO: Edit to read in the real data
vse_AN_gwap <- read.csv("test_data.csv")

# Print the number of rows in vse_AN_gwap
cat("Number of rows in vse_AN_gwap:", nrow(vse_AN_gwap), "\n")

# Assign the default output file name
out_file_name <- "preprocess_syndiffix_data.csv"

# Read in an optional command line argument
args <- commandArgs(trailingOnly = TRUE)
filter <- ifelse(length(args) > 0, args[1], 'post_filter')

# Apply filtering if the filter argument is 'filter'
if (filter == "pre_filter") {
  out_file_name <- "preprocess_syndiffix_data_filtered.csv"
  
  cat('Keep only rows where EF16U2 != 7\n')
  vse_AN_gwap <- vse_AN_gwap[vse_AN_gwap$EF16U2 != 7, ]
  
  cat('Keep only rows where EF41 > 16 and EF41 < 63\n')
  vse_AN_gwap <- vse_AN_gwap[vse_AN_gwap$EF41 > 16 & vse_AN_gwap$EF41 < 63, ]
  
  cat('Keep only rows where TAETIGKEITSSCHLUESSEL5 == 1 or TAETIGKEITSSCHLUESSEL5 == 3\n')
  vse_AN_gwap <- vse_AN_gwap[vse_AN_gwap$TAETIGKEITSSCHLUESSEL5 == 1 | vse_AN_gwap$TAETIGKEITSSCHLUESSEL5 == 3, ]
  
  cat('Keep only rows where PERSONENGRUPPE == 101\n')
  vse_AN_gwap <- vse_AN_gwap[vse_AN_gwap$PERSONENGRUPPE == 101, ]
  cat("Number of rows in vse_AN_gwap after filter:", nrow(vse_AN_gwap), "\n")
}


'1. Employee Dataset'

'    1.1 Baseline replication with original data'
'    1.1.1 Dataset Preparation'
'Removal of fully empty variables'
vse_AN_gwap_noNNA_PF <- vse_AN_gwap %>% dplyr::select(-one_of('RESERVE', 'EF13', 'EF30', 'EF31', 'EF32', 'EF51', 'EF54', 'EF56', 'EF59U2', 'EF60'))
sapply(vse_AN_gwap_noNNA_PF, function(y) sum(is.na(y)))
sum(is.na(vse_AN_gwap_noNNA_PF))

'Recoding missing value in variable "LEISTUNGSGRUPPE" because it equals apprenticeship marginal employment'
vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE[is.na(vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE)] <- 6
sapply(vse_AN_gwap_noNNA_PF, function(y) sum(is.na(y)))
vse_AN_gwap_noNNA_PF$EF9[is.na(vse_AN_gwap_noNNA_PF$EF9)] <- 6

'Imputing value for Verdienstgruppe by replacing the missing value with the statistical mode'
'Creating a function which calculates the mode'
getMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
getMode(vse_AN_gwap_noNNA_PF$VERDIENSTGRUPPE)

vse_AN_gwap_noNNA_PF$VERDIENSTGRUPPE[is.na(vse_AN_gwap_noNNA_PF$VERDIENSTGRUPPE)] <- getMode(vse_AN_gwap_noNNA_PF$VERDIENSTGRUPPE)
vse_AN_gwap_noNNA_PF$EF6[is.na(vse_AN_gwap_noNNA_PF$EF6)] <- getMode(vse_AN_gwap_noNNA_PF$EF6)

'Checking for remaining missing values'
vse_AN_gwap_noNNA_PF$VERDIENSTGRUPPE[is.na(vse_AN_gwap_noNNA_PF$VERDIENSTGRUPPE)] <- getMode(vse_AN_gwap_noNNA_PF$VERDIENSTGRUPPE)
'Result: now there are no missing values left in the onsite employee dataset'

# constraints
'Gross hourly income grouped by (non-)/temporary work'
vse_AN_gwap_noNNA_PF$EF21H <- vse_AN_gwap_noNNA_PF$EF21 / vse_AN_gwap_noNNA_PF$EF19

'Removal of duplet variables (according to dataset description)'
vse_AN_gwap_noNNA_PF <- vse_AN_gwap_noNNA_PF %>% dplyr::select(-one_of('VERDIENSTGRUPPE', 'NUMVERDIENSTREGELUNG', 
                                                                       'GESCHLECHT', 'GEBURTSJAHR', 'B22', 'EINTRITTSMONAT', 'EINTRITTSJAHR', 
                                                                       'WOCHENARBEITSZEIT', 'ARBEITSSTUNDENBEZAHLT', 'B32', 'UEBERSTUNDENBEZAHLT', 'B321',
                                                                       'MVERDIENSTGESAMT', 'B42', 'MVERDIENSTDAVONUEBERSTD', 'B421', 'MVERDIENSTDAVONZUSCHLAEGE',
                                                                       'B422', 'MVERDIENSTDAVONSTEUERSOLI', 'B4232', 'MVERDIENSTDAVONSV', 'B4231',
                                                                       'SVARBEITSTAGEGESAMT', 'B41', 'JVERDIENSTGESAMT', 'JVERDIENSTDAVONSONSTBEZ', 'B411',
                                                                       'URLAUBSANSPRUCH', 'JVERDIENSTDAVONENTGELTUMWANDLUNG', 'B25', 
                                                                       'B26', 'B271', 'B31', 'B33', 'B34'))

# Write the processed data frame to the output file
write.csv(vse_AN_gwap_noNNA_PF, out_file_name, row.names = FALSE)