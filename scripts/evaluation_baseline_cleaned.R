# Baseline evaluations for replication based on original data

vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4 <- ifelse(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4 == 1, 0,1)
vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4 <- as.factor(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4)

# Gross monthly income grouped by (non-)/temporary work
print(aggregate(vse_AN_gwap_noNNA_PF$EF21, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean))

# Gross hourly income grouped by (non-)/temporary work
vse_AN_gwap_noNNA_PF$EF21H <- vse_AN_gwap_noNNA_PF$EF21 / vse_AN_gwap_noNNA_PF$EF19
print(aggregate(vse_AN_gwap_noNNA_PF$EF21H, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean))

print(aggregate(vse_AN_gwap_noNNA_PF$EF48, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean))
(fmean(vse_AN_gwap_noNNA_PF$EF48, vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4, vse_AN_gwap_noNNA_PF$B52))

# Weekly working hours grouped by (non-)/temporary work 
print(aggregate(vse_AN_gwap_noNNA_PF$EF19, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean)/4)

# Age grouped by (non-)/temporary work
aggregate(vse_AN_gwap_noNNA_PF$EF41, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean)

# Gender grouped by (non-)/temporary work
aggregate(vse_AN_gwap_noNNA_PF$EF10, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean)-1

# Full /part time grouped by (non-)/temporary work
table(vse_AN_gwap_noNNA_PF$B27)
vse_AN_gwap_noNNA_PF$B27_rec <- ifelse(vse_AN_gwap_noNNA_PF$B27 == "FT", 1, 0)
table(vse_AN_gwap_noNNA_PF$B27_rec)
aggregate(vse_AN_gwap_noNNA_PF$B27_rec, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean)

# Months of service for the company
aggregate((vse_AN_gwap_noNNA_PF$EF14U2 - vse_AN_gwap_noNNA_PF$EF12U2), list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean)*12

# Level of requirements grouped by (non-)/temporary work
vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE_Helper <- ifelse(vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE == 4 | vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE == 5, 1, 0) 
vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE_Professional <- ifelse(vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE == 3| vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE == 6, 1, 0) 
vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE_Specialist <- ifelse(vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE == 2, 1, 0)
vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE_Expert <- ifelse(vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE == 1, 1, 0)

aggregate(vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE_Helper, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean)
aggregate(vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE_Professional, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean)
aggregate(vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE_Specialist, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean)
aggregate(vse_AN_gwap_noNNA_PF$LEISTUNGSGRUPPE_Expert, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean)



# Level of education grouped by (non-)/temporary work
table(vse_AN_gwap_noNNA_PF$EF16U2)
vse_AN_gwap_noNNA_PF$EF16U2_1 <- ifelse(vse_AN_gwap_noNNA_PF$EF16U2 == 1, 1, 0) 
vse_AN_gwap_noNNA_PF$EF16U2_2 <- ifelse(vse_AN_gwap_noNNA_PF$EF16U2 == 2, 1, 0) 
vse_AN_gwap_noNNA_PF$EF16U2_3 <- ifelse(vse_AN_gwap_noNNA_PF$EF16U2 == 3, 1, 0) 
vse_AN_gwap_noNNA_PF$EF16U2_4 <- ifelse(vse_AN_gwap_noNNA_PF$EF16U2 == 4, 1, 0) 
vse_AN_gwap_noNNA_PF$EF16U2_5 <- ifelse(vse_AN_gwap_noNNA_PF$EF16U2 == 5 | vse_AN_gwap_noNNA_PF$EF16U2 == 6, 1, 0) 

aggregate(vse_AN_gwap_noNNA_PF$EF16U2_1, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean)
aggregate(vse_AN_gwap_noNNA_PF$EF16U2_2, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean)
aggregate(vse_AN_gwap_noNNA_PF$EF16U2_3, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean)
aggregate(vse_AN_gwap_noNNA_PF$EF16U2_4, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean)
aggregate(vse_AN_gwap_noNNA_PF$EF16U2_5, list(vse_AN_gwap_noNNA_PF$TAETIGKEITSSCHLUESSEL4), FUN=mean)

'Replication of Bachmann et al. (2023), table 3'
'Column 1'
lm1_table1 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, data = vse_AN_gwap_noNNA_PF, weights = B52)
summary(lm1_table1)

'Column 2'
vse_AN_gwap_noNNA_PF$EF41_sq <- vse_AN_gwap_noNNA_PF$EF41*vse_AN_gwap_noNNA_PF$EF41
vse_AN_gwap_noNNA_PF$EF40_sq <- vse_AN_gwap_noNNA_PF$EF40*vse_AN_gwap_noNNA_PF$EF40
lm2_table1 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq, data = vse_AN_gwap_noNNA_PF)
summary(lm2_table1)

'Column 3'
lm3_table1 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2, data = vse_AN_gwap_noNNA_PF)
summary(lm3_table1)

'Column 4'
lm4_table1 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2 + LEISTUNGSGRUPPE, data = vse_AN_gwap_noNNA_PF)
summary(lm4_table1)

'=> replication works, thus I refrain from replicating the remaining results as well'

#### Propensity score matching, total
m.out1 <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                    EF40_sq + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                  data = vse_AN_gwap_noNNA_PF,
                  method = "nearest",
                  distance = "glm",
                  link = "probit")
m.data <- match.data(m.out1, drop.unmatched = FALSE)
plot(m.out1, type = "density", interactive = FALSE,
     which.xs = ~ EF41 + EF40)
summary(m.out1, un = FALSE)

fit <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
                            EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
          data = m.data,
          weights = weights)
fit
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = vse_AN_gwap_noNNA_PF))

fit_all <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
            EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
          data = m.data,
          weights = weights)
fit_all

summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = vse_AN_gwap_noNNA_PF))

#### Propensity score matching, fulltime
vse_AN_gwap_noNNA_PF_fulltime <-subset(vse_AN_gwap_noNNA_PF, TAETIGKEITSSCHLUESSEL5 == 1| TAETIGKEITSSCHLUESSEL5 == 3)

'Propensity score matching and comparisons for matched and unmatched observations'
m.out_O1a <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                    EF40_sq + EF16U2 + LEISTUNGSGRUPPE,
                  data = vse_AN_gwap_noNNA_PF_fulltime,
                  method = "nearest",
                  distance = "glm",
                  link = "probit")
m.data_01a <- match.data(m.out_O1a, drop.unmatched = FALSE)
plot(m.out_01a, type = "density", interactive = FALSE,
     which.xs = ~ EF41 + EF40)
summary(m.out_01a, un = FALSE)

'Effect for matched observations'
fit_O1a <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
            EF10 + EF16U2 + LEISTUNGSGRUPPE,
          data = m.data_01a,
          weights = weights)
fit_O1a

fit_O1b <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
                EF10 + EF16U2 + LEISTUNGSGRUPPE,
              data = m.data_01a,
              weights = weights)
fit_O1b

'Effect for unmatched observations'
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = vse_AN_gwap_noNNA_PF_fulltime))

summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = vse_AN_gwap_noNNA_PF_fulltime))

