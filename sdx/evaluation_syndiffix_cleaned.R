options(repos = c(CRAN = "https://cran.r-project.org"))
if (!requireNamespace("MatchIt", quietly = TRUE)) {
  install.packages("MatchIt")
}

library(MatchIt)
# Baseline evaluations for replication based on original data

# Define the subroutine
get_and_process_data <- function(path) {
  df <- read.csv(path)
  df$TAETIGKEITSSCHLUESSEL4 <- ifelse(df$TAETIGKEITSSCHLUESSEL4 == 1, 0, 1)
  df$TAETIGKEITSSCHLUESSEL4 <- as.factor(df$TAETIGKEITSSCHLUESSEL4)
  return(df)
}

# Gross monthly income grouped by (non-)/temporary work
ts4_ef21 <- get_and_process_data("sdx_tables/ts4_ef21.csv")
print(aggregate(ts4_ef21$EF21, list(ts4_ef21$TAETIGKEITSSCHLUESSEL4), FUN=mean))

# Gross hourly income grouped by (non-)/temporary work
ts4_ef21h <- get_and_process_data("sdx_tables/ts4_ef21h.csv")
print(aggregate(ts4_ef21h$EF21H, list(ts4_ef21h$TAETIGKEITSSCHLUESSEL4), FUN=mean))

ts4_ef48 <- get_and_process_data("sdx_tables/ts4_ef48.csv")
print(aggregate(ts4_ef48$EF48, list(ts4_ef48$TAETIGKEITSSCHLUESSEL4), FUN=mean))
ts4_ef48_b52 <- get_and_process_data("sdx_tables/ts4_ef48_b52.csv")
#(fmean(ts4_ef48_b52$EF48, ts4_ef48_b52$TAETIGKEITSSCHLUESSEL4, ts4_ef48_b52$B52))

# Weekly working hours grouped by (non-)/temporary work 
ts4_ef19 <- get_and_process_data("sdx_tables/ts4_ef19.csv")
print(aggregate(ts4_ef19$EF19, list(ts4_ef19$TAETIGKEITSSCHLUESSEL4), FUN=mean)/4)

# Age grouped by (non-)/temporary work
ts4_ef41 <- get_and_process_data("sdx_tables/ts4_ef41.csv")
aggregate(ts4_ef41$EF41, list(ts4_ef41$TAETIGKEITSSCHLUESSEL4), FUN=mean)

# Gender grouped by (non-)/temporary work
ts4_ef10 <- get_and_process_data("sdx_tables/ts4_ef10.csv")
aggregate(ts4_ef10$EF10, list(ts4_ef10$TAETIGKEITSSCHLUESSEL4), FUN=mean)-1

# Full /part time grouped by (non-)/temporary work
ts4_b27 <- get_and_process_data("sdx_tables/ts4_b27.csv")
table(ts4_b27$B27)
ts4_b27$B27_rec <- ifelse(ts4_b27$B27 == "FT", 1, 0)
table(ts4_b27$B27_rec)
aggregate(ts4_b27$B27_rec, list(ts4_b27$TAETIGKEITSSCHLUESSEL4), FUN=mean)

# Months of service for the company
ts4_mos <- get_and_process_data("sdx_tables/ts4_mos.csv")
aggregate(ts4_mos$months_of_service, list(ts4_mos$TAETIGKEITSSCHLUESSEL4), FUN=mean)*12

# Level of requirements grouped by (non-)/temporary work
# Note we are not preprocessing here
ts4_lg <- get_and_process_data("sdx_tables/ts4_lg.csv")
ts4_lg$LEISTUNGSGRUPPE_Helper <- ifelse(ts4_lg$LEISTUNGSGRUPPE == 4 | ts4_lg$LEISTUNGSGRUPPE == 5, 1, 0) 
ts4_lg$LEISTUNGSGRUPPE_Professional <- ifelse(ts4_lg$LEISTUNGSGRUPPE == 3| ts4_lg$LEISTUNGSGRUPPE == 6, 1, 0) 
ts4_lg$LEISTUNGSGRUPPE_Specialist <- ifelse(ts4_lg$LEISTUNGSGRUPPE == 2, 1, 0)
ts4_lg$LEISTUNGSGRUPPE_Expert <- ifelse(ts4_lg$LEISTUNGSGRUPPE == 1, 1, 0)

aggregate(ts4_lg$LEISTUNGSGRUPPE_Helper, list(ts4_lg$TAETIGKEITSSCHLUESSEL4), FUN=mean)
aggregate(ts4_lg$LEISTUNGSGRUPPE_Professional, list(ts4_lg$TAETIGKEITSSCHLUESSEL4), FUN=mean)
aggregate(ts4_lg$LEISTUNGSGRUPPE_Specialist, list(ts4_lg$TAETIGKEITSSCHLUESSEL4), FUN=mean)
aggregate(ts4_lg$LEISTUNGSGRUPPE_Expert, list(ts4_lg$TAETIGKEITSSCHLUESSEL4), FUN=mean)

# Level of education grouped by (non-)/temporary work
# No preprocessing
ts4_ef16u2 <- get_and_process_data("sdx_tables/ts4_ef16u2.csv")
table(ts4_ef16u2$EF16U2)
ts4_ef16u2$EF16U2_1 <- ifelse(ts4_ef16u2$EF16U2 == 1, 1, 0) 
ts4_ef16u2$EF16U2_2 <- ifelse(ts4_ef16u2$EF16U2 == 2, 1, 0) 
ts4_ef16u2$EF16U2_3 <- ifelse(ts4_ef16u2$EF16U2 == 3, 1, 0) 
ts4_ef16u2$EF16U2_4 <- ifelse(ts4_ef16u2$EF16U2 == 4, 1, 0) 
ts4_ef16u2$EF16U2_5 <- ifelse(ts4_ef16u2$EF16U2 == 5 | ts4_ef16u2$EF16U2 == 6, 1, 0) 

aggregate(ts4_ef16u2$EF16U2_1, list(ts4_ef16u2$TAETIGKEITSSCHLUESSEL4), FUN=mean)
aggregate(ts4_ef16u2$EF16U2_2, list(ts4_ef16u2$TAETIGKEITSSCHLUESSEL4), FUN=mean)
aggregate(ts4_ef16u2$EF16U2_3, list(ts4_ef16u2$TAETIGKEITSSCHLUESSEL4), FUN=mean)
aggregate(ts4_ef16u2$EF16U2_4, list(ts4_ef16u2$TAETIGKEITSSCHLUESSEL4), FUN=mean)
aggregate(ts4_ef16u2$EF16U2_5, list(ts4_ef16u2$TAETIGKEITSSCHLUESSEL4), FUN=mean)

'Replication of Bachmann et al. (2023), table 3'
'Column 1'
ts4_ef21_b52 <- get_and_process_data("sdx_tables/ts4_ef21_b52.csv")
lm1_table1 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, data = ts4_ef21_b52, weights = B52)
summary(lm1_table1)

'Column 2'
col2_group <- get_and_process_data("sdx_tables/ts4_ef41_ef10_ef40_b27_tar_ef21.csv")
col2_group$EF41_sq <- col2_group$EF41*col2_group$EF41
col2_group$EF40_sq <- col2_group$EF40*col2_group$EF40
# Note: have to recreate B27_rec here again, because different synthetic table from before
col2_group$B27_rec <- ifelse(col2_group$B27 == "FT", 1, 0)
lm2_table1 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq, data = col2_group)
summary(lm2_table1)

'Column 3'
col3_group <- get_and_process_data("sdx_tables/ts4_ef41_ef10_ef40_b27_ef16u2_tar_ef21.csv")
col3_group$EF41_sq <- col3_group$EF41*col3_group$EF41
col3_group$EF40_sq <- col3_group$EF40*col3_group$EF40
col3_group$B27_rec <- ifelse(col3_group$B27 == "FT", 1, 0)
lm3_table1 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2, data = col3_group)
summary(lm3_table1)

'Column 4'
col4_group <- get_and_process_data("sdx_tables/ts4_ef41_ef10_ef40_b27_ef16u2_lg_tar_ef21.csv")
col4_group$EF41_sq <- col4_group$EF41*col4_group$EF41
col4_group$EF40_sq <- col4_group$EF40*col4_group$EF40
col4_group$B27_rec <- ifelse(col4_group$B27 == "FT", 1, 0)
lm4_table1 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2 + LEISTUNGSGRUPPE, data = col4_group)
summary(lm4_table1)

'=> replication works, thus I refrain from replicating the remaining results as well'

#### Propensity score matching, total
m.out1 <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                    EF40_sq + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                  data = col4_group,
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

# Here we are using the ts4_ef21 synthetic table that we read in earlier
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = ts4_ef21))

ef48_group <- get_and_process_data("sdx_tables/ts4_ef41_ef10_ef40_b27_ef16u2_lg_tar_ef48.csv")
ef48_group$EF41_sq <- ef48_group$EF41*ef48_group$EF41
ef48_group$EF40_sq <- ef48_group$EF40*ef48_group$EF40
ef48_group$B27_rec <- ifelse(ef48_group$B27 == "FT", 1, 0)

m.out2 <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                    EF40_sq + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                  data = ef48_group,
                  method = "nearest",
                  distance = "glm",
                  link = "probit")
m.data2 <- match.data(m.out2, drop.unmatched = FALSE)
fit_all <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
            EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
          data = m.data2,
          weights = weights)
fit_all

# Use the ts4_ef48 synthetic table that we read in earlier
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = ts4_ef48))

#### Propensity score matching, fulltime
ft_group_ef21 <- get_and_process_data("sdx_tables/ts4_ts5_ef41_ef10_ef40_ef16u2_lg_tar_ef21.csv")
# Note that we didn't preprocess TAETIGKEITSSCHLUESSEL5 prior to synthesis 
ft_group_ef21 <-subset(ft_group_ef21, TAETIGKEITSSCHLUESSEL5 == 1| TAETIGKEITSSCHLUESSEL5 == 3)

'Propensity score matching and comparisons for matched and unmatched observations'
ft_group_ef21$EF41_sq <- ft_group_ef21$EF41*ft_group_ef21$EF41
ft_group_ef21$EF40_sq <- ft_group_ef21$EF40*ft_group_ef21$EF40
m.out_01a <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                    EF40_sq + EF16U2 + LEISTUNGSGRUPPE,
                  data = ft_group_ef21,
                  method = "nearest",
                  distance = "glm",
                  link = "probit")
m.data_01a <- match.data(m.out_01a, drop.unmatched = FALSE)
plot(m.out_01a, type = "density", interactive = FALSE,
     which.xs = ~ EF41 + EF40)
summary(m.out_01a, un = FALSE)

'Effect for matched observations'
fit_01a <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
            EF10 + EF16U2 + LEISTUNGSGRUPPE,
          data = m.data_01a,
          weights = weights)
fit_01a

# Because we are going to fit on EF48, we are repeating the above steps with a
# synthetic table targeting EF48 rather tna EF21
ft_group_ef48 <- get_and_process_data("sdx_tables/ts4_ts5_ef41_ef10_ef40_ef16u2_lg_tar_ef48.csv")
ft_group_ef48 <-subset(ft_group_ef48, TAETIGKEITSSCHLUESSEL5 == 1| TAETIGKEITSSCHLUESSEL5 == 3)

'Propensity score matching and comparisons for matched and unmatched observations'
ft_group_ef48$EF41_sq <- ft_group_ef48$EF41*ft_group_ef48$EF41
ft_group_ef48$EF40_sq <- ft_group_ef48$EF40*ft_group_ef48$EF40
m.out_02a <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                    EF40_sq + EF16U2 + LEISTUNGSGRUPPE,
                  data = ft_group_ef48,
                  method = "nearest",
                  distance = "glm",
                  link = "probit")
m.data_02a <- match.data(m.out_02a, drop.unmatched = FALSE)
plot(m.out_02a, type = "density", interactive = FALSE,
     which.xs = ~ EF41 + EF40)
summary(m.out_02a, un = FALSE)
fit_02b <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
                EF10 + EF16U2 + LEISTUNGSGRUPPE,
              data = m.data_02a,
              weights = weights)
fit_02b

'Effect for unmatched observations'
ts4_ts5_ef21 <- get_and_process_data("sdx_tables/ts4_ts5_ef21.csv")
ts4_ts5_ef21_full <-subset(ts4_ts5_ef21, TAETIGKEITSSCHLUESSEL5 == 1| TAETIGKEITSSCHLUESSEL5 == 3)
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = ts4_ts5_ef21_full))

ts4_ts5_ef48 <- get_and_process_data("sdx_tables/ts4_ts5_ef48.csv")
ts4_ts5_ef48_full <-subset(ts4_ts5_ef48, TAETIGKEITSSCHLUESSEL5 == 1| TAETIGKEITSSCHLUESSEL5 == 3)
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = ts4_ts5_ef48_full))

