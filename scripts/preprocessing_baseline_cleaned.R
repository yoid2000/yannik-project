# Preprocessing Baseline Replication

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

'Removal of duplet variables (according to dataset description)'
#vse_AN_gwap_noNNA_PF <- vse_AN_gwap_noNNA_PF %>% dplyr::select(-one_of('VERDIENSTGRUPPE', 'LEISTUNGSGRUPPE', 'NUMVERDIENSTREGELUNG', 
#                                                                 'GESCHLECHT', 'GEBURTSJAHR', 'B22', 'EINTRITTSMONAT', 'EINTRITTSJAHR', 
#                                                                 'WOCHENARBEITSZEIT', 'ARBEITSSTUNDENBEZAHLT', 'B32', 'UEBERSTUNDENBEZAHLT', 'B321',
#                                                                 'MVERDIENSTGESAMT', 'B42', 'MVERDIENSTDAVONUEBERSTD', 'B421', 'MVERDIENSTDAVONZUSCHLAEGE',
#                                                                 'B422', 'MVERDIENSTDAVONSTEUERSOLI', 'B4232', 'MVERDIENSTDAVONSV', 'B4231',
#                                                                 'SVARBEITSTAGEGESAMT', 'B41', 'JVERDIENSTGESAMT', 'JVERDIENSTDAVONSONSTBEZ', 'B411',
#                                                                 'URLAUBSANSPRUCH', 'JVERDIENSTDAVONENTGELTUMWANDLUNG', 'B25', 
#                                                                 'B26', 'B271', 'B31', 'B33', 'B34'))
vse_AN_gwap_noNNA_PF <- vse_AN_gwap_noNNA_PF %>% dplyr::select(-one_of('VERDIENSTGRUPPE', 'NUMVERDIENSTREGELUNG', 
                                                                       'GESCHLECHT', 'GEBURTSJAHR', 'B22', 'EINTRITTSMONAT', 'EINTRITTSJAHR', 
                                                                       'WOCHENARBEITSZEIT', 'ARBEITSSTUNDENBEZAHLT', 'B32', 'UEBERSTUNDENBEZAHLT', 'B321',
                                                                       'MVERDIENSTGESAMT', 'B42', 'MVERDIENSTDAVONUEBERSTD', 'B421', 'MVERDIENSTDAVONZUSCHLAEGE',
                                                                       'B422', 'MVERDIENSTDAVONSTEUERSOLI', 'B4232', 'MVERDIENSTDAVONSV', 'B4231',
                                                                       'SVARBEITSTAGEGESAMT', 'B41', 'JVERDIENSTGESAMT', 'JVERDIENSTDAVONSONSTBEZ', 'B411',
                                                                       'URLAUBSANSPRUCH', 'JVERDIENSTDAVONENTGELTUMWANDLUNG', 'B25', 
                                                                       'B26', 'B271', 'B31', 'B33', 'B34'))


# Filtering observations without employment subject to social security contributions (analogous to Bachmann et al. (2023)
vse_AN_gwap_noNNA_PF <- subset(vse_AN_gwap_noNNA_PF, PERSONENGRUPPE == 101)

# Filtering observations without information on educational level (analogous to Bachmann et al. (2023)
vse_AN_gwap_noNNA_PF <- subset(vse_AN_gwap_noNNA_PF, EF16U2!=7)

# Filtering observations younger than 17 and older than 62(analogous to Bachmann et al. (2023)
vse_AN_gwap_noNNA_PF <- subset(vse_AN_gwap_noNNA_PF, EF41 > 16 & EF41<63)