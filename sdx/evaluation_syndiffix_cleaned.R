options(repos = c(CRAN = "https://cran.r-project.org"))
if (!requireNamespace("MatchIt", quietly = TRUE)) {
  install.packages("MatchIt")
}
if (!requireNamespace("collapse", quietly = TRUE)) {
  install.packages("collapse")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}

library(MatchIt)
library(collapse)
library(jsonlite)

make_file_key <- function(columns, target = NULL) {
  sorted_columns <- sort(columns)
  key <- paste(sorted_columns, collapse = "__")
  if (!is.null(target)) {
    key <- paste0(key, "__tar__", target)
  }
  return(key)
}
# Example usage
# result <- make_file_key(c("banana", "apple", "cherry"), "fruit")

get_path <- function(key) {
  if (key %in% names(filekeys)) {
    return(filekeys[[key]])
  } else {
    stop(paste("Error: Key", key, "not found in filekeys"))
  }
}
# result <- get_path(key)

get_data <- function(columns, target = NULL, mode="post_filter") {
  if (mode == "post_filter") {
    required_columns <- c("PERSONENGRUPPE", "EF16U2", "EF41", "TAETIGKEITSSCHLUESSEL5")
    columns <- unique(c(columns, required_columns))
  }
  key <- make_file_key(columns, target = target)
  path <- get_path(key)
  df <- read.csv(path)
  df <- process_data(df)
  return(df)
}

get_data_fulltime <- function(columns, target = NULL, mode="post_filter") {
  columns <- c(columns, 'TAETIGKEITSSCHLUESSEL5')
  return(get_data(columns, target = target, mode=mode))
}

process_data <- function(df) {
  # Do column transformations
  if ("TAETIGKEITSSCHLUESSEL4" %in% colnames(df)) {
    df$TAETIGKEITSSCHLUESSEL4 <- ifelse(df$TAETIGKEITSSCHLUESSEL4 == 1, 0, 1)
    df$TAETIGKEITSSCHLUESSEL4 <- as.factor(df$TAETIGKEITSSCHLUESSEL4)
  }

  if ("EF40" %in% colnames(df)) {
    df$EF40_sq <- df$EF40*df$EF40
  }

  if ("B27" %in% colnames(df)) {
    df$B27_rec <- ifelse(df$B27 == "FT", 1, 0)
  }

  # EF14U2 is reporting year, EF12U2 is year of entry
  if ("EF14U2" %in% colnames(df) && "EF12U2" %in% colnames(df)) {
    df$MONTHS_OF_SERVICE <- df$EF14U2 - df$EF12U2
  }

  if ("EF41" %in% colnames(df)) {
    df$EF41_sq <- df$EF41*df$EF41
  }

  if ("PERSONENGRUPPE" %in% colnames(df) && "EF16U2" %in% colnames(df) && "EF41" %in% colnames(df) && "TAETIGKEITSSCHLUESSEL5" %in% colnames(df)) {
    cat("Do post filtering")

    # Filtering observations without employment subject to social security contributions (analogous to Bachmann et al. (2023)
    df <- subset(df, PERSONENGRUPPE == 101)

    # Filtering observations without information on educational level (analogous to Bachmann et al. (2023)
    df <- subset(df, EF16U2!=7)

    # Filtering observations younger than 17 and older than 62(analogous to Bachmann et al. (2023)
    df$EF41_sq <- df$EF41*df$EF41

    df <- subset(df, TAETIGKEITSSCHLUESSEL5 == 1| TAETIGKEITSSCHLUESSEL5 == 3)
  }

  return(df)
}


# Assign the default output file name
filekeys_path <- "sdx_tables/filekeys.json"
mode = "post_filter"

# Read in an optional command line argument
args <- commandArgs(trailingOnly = TRUE)
filter <- ifelse(length(args) > 0, args[1], 'post_filter')

if (filter == "pre_filter") {
  filekeys_path <- "sdx_tables_filtered/filekeys.json"
  mode = 'pre_filter'
}

cat("Reading in filekeys from", filekeys_path, "\n")
filekeys <- fromJSON(filekeys_path)

'Replication of Bachmann et al. (2023), table 1'
# Gross monthly income grouped by (non-)/temporary work
df <- get_data(c("EF21", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
'print(aggregate(df$EF21, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean))'
df <- get_data(c("EF21", "TAETIGKEITSSCHLUESSEL4", "B52"), mode=mode)
(fmean(df$EF21, df$TAETIGKEITSSCHLUESSEL4, df$B52))

# Gross hourly income grouped by (non-)/temporary work
df <- get_data(c("EF21H", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
print(aggregate(df$EF21H, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean))

df <- get_data(c("EF48", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
print(aggregate(df$EF48, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean))
df <- get_data(c("EF48", "TAETIGKEITSSCHLUESSEL4", "B52"), mode=mode)
(fmean(df$EF48, df$TAETIGKEITSSCHLUESSEL4, df$B52))

# Weekly working hours grouped by (non-)/temporary work 
df <- get_data(c("EF19", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
'print(aggregate(df$EF19, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean)/4)'
df <- get_data(c("EF19", "TAETIGKEITSSCHLUESSEL4", "B52"), mode=mode)
(fmean(df$EF19, df$TAETIGKEITSSCHLUESSEL4, df$B52)/4)

# Age grouped by (non-)/temporary work
df <- get_data(c("EF41", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
'aggregate(df$EF41, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean)'
df <- get_data(c("EF41", "TAETIGKEITSSCHLUESSEL4", "B52"), mode=mode)
(fmean(df$EF41, df$TAETIGKEITSSCHLUESSEL4, df$B52))

# Gender grouped by (non-)/temporary work
df <- get_data(c("EF10", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
'aggregate(df$EF10, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean)-1'
df <- get_data(c("EF10", "TAETIGKEITSSCHLUESSEL4", "B52"), mode=mode)
(fmean(df$EF10, df$TAETIGKEITSSCHLUESSEL4, df$B52)-1)

# Full /part time grouped by (non-)/temporary work
df <- get_data(c("B27", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
table(df$B27)
table(df$B27_rec)
'aggregate(df$B27_rec, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean)'
df <- get_data(c("B27", "TAETIGKEITSSCHLUESSEL4", "B52"), mode=mode)
table(df$B27_rec)
(fmean(df$B27_rec, df$TAETIGKEITSSCHLUESSEL4, df$B52)-1)

# Months of service for the company
df <- get_data(c("EF14U2", "EF12U2", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
'aggregate(df$MONTHS_OF_SERVICE, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean)*12'
df <- get_data(c("EF14U2", "EF12U2", "TAETIGKEITSSCHLUESSEL4", "B52"), mode=mode)
(fmean(df$MONTHS_OF_SERVICE, df$TAETIGKEITSSCHLUESSEL4, df$B52)*12)

# Level of requirements grouped by (non-)/temporary work
# Note we are not preprocessing here
df <- get_data(c("LEISTUNGSGRUPPE", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
df$LEISTUNGSGRUPPE_Helper <- ifelse(df$LEISTUNGSGRUPPE == 4 | df$LEISTUNGSGRUPPE == 5, 1, 0) 
df$LEISTUNGSGRUPPE_Professional <- ifelse(df$LEISTUNGSGRUPPE == 3| df$LEISTUNGSGRUPPE == 6, 1, 0) 
df$LEISTUNGSGRUPPE_Specialist <- ifelse(df$LEISTUNGSGRUPPE == 2, 1, 0)
df$LEISTUNGSGRUPPE_Expert <- ifelse(df$LEISTUNGSGRUPPE == 1, 1, 0)

df_b <- get_data(c("LEISTUNGSGRUPPE", "TAETIGKEITSSCHLUESSEL4", "B52"), mode=mode)
df_b$LEISTUNGSGRUPPE_Helper <- ifelse(df_b$LEISTUNGSGRUPPE == 4 | df_b$LEISTUNGSGRUPPE == 5, 1, 0) 
df_b$LEISTUNGSGRUPPE_Professional <- ifelse(df_b$LEISTUNGSGRUPPE == 3| df_b$LEISTUNGSGRUPPE == 6, 1, 0) 
df_b$LEISTUNGSGRUPPE_Specialist <- ifelse(df_b$LEISTUNGSGRUPPE == 2, 1, 0)
df_b$LEISTUNGSGRUPPE_Expert <- ifelse(df_b$LEISTUNGSGRUPPE == 1, 1, 0)

'aggregate(df$LEISTUNGSGRUPPE_Helper, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean)'
(fmean(df_b$LEISTUNGSGRUPPE_Helper, df_b$TAETIGKEITSSCHLUESSEL4, df_b$B52))
'aggregate(df$LEISTUNGSGRUPPE_Professional, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean)'
(fmean(df_b$LEISTUNGSGRUPPE_Professional, df_b$TAETIGKEITSSCHLUESSEL4, df_b$B52))
'aggregate(df$LEISTUNGSGRUPPE_Specialist, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean)'
(fmean(df_b$LEISTUNGSGRUPPE_Specialist, df_b$TAETIGKEITSSCHLUESSEL4, df_b$B52))
'aggregate(df$LEISTUNGSGRUPPE_Expert, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean)'
(fmean(df_b$LEISTUNGSGRUPPE_Expert, df_b$TAETIGKEITSSCHLUESSEL4, df_b$B52))

# Level of education grouped by (non-)/temporary work
# No preprocessing
df <- get_data(c("EF16U2", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
table(df$EF16U2)
df$EF16U2_1 <- ifelse(df$EF16U2 == 1, 1, 0) 
df$EF16U2_2 <- ifelse(df$EF16U2 == 2, 1, 0) 
df$EF16U2_3 <- ifelse(df$EF16U2 == 3, 1, 0) 
df$EF16U2_4 <- ifelse(df$EF16U2 == 4, 1, 0) 
df$EF16U2_5 <- ifelse(df$EF16U2 == 5 | df$EF16U2 == 6, 1, 0) 

df_b <- get_data(c("EF16U2", "TAETIGKEITSSCHLUESSEL4", "B52"), mode=mode)
table(df_b$EF16U2)
df_b$EF16U2_1 <- ifelse(df_b$EF16U2 == 1, 1, 0) 
df_b$EF16U2_2 <- ifelse(df_b$EF16U2 == 2, 1, 0) 
df_b$EF16U2_3 <- ifelse(df_b$EF16U2 == 3, 1, 0) 
df_b$EF16U2_4 <- ifelse(df_b$EF16U2 == 4, 1, 0) 
df_b$EF16U2_5 <- ifelse(df_b$EF16U2 == 5 | df_b$EF16U2 == 6, 1, 0) 

'aggregate(df$EF16U2_1, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean)'
(fmean(df_b$EF16U2_1, df_b$TAETIGKEITSSCHLUESSEL4, df_b$B52))
'aggregate(df$EF16U2_2, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean)'
(fmean(df_b$EF16U2_2, df_b$TAETIGKEITSSCHLUESSEL4, df_b$B52))
'aggregate(df$EF16U2_3, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean)'
(fmean(df_b$EF16U2_3, df_b$TAETIGKEITSSCHLUESSEL4, df_b$B52))
'aggregate(df$EF16U2_4, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean)'
(fmean(df_b$EF16U2_4, df_b$TAETIGKEITSSCHLUESSEL4, df_b$B52))
'aggregate(df$EF16U2_5, list(df$TAETIGKEITSSCHLUESSEL4), FUN=mean)'
(fmean(df_b$EF16U2_5, df_b$TAETIGKEITSSCHLUESSEL4, df_b$B52))

'Replication of Bachmann et al. (2023), table 3'
'Column 1'
df <- get_data(c("EF21", "TAETIGKEITSSCHLUESSEL4", "B52"), mode=mode)
lm1_table1 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, data = df, weights = B52)
summary(lm1_table1)

'Column 2'
df <- get_data(c("EF21", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "B27", "EF40"), target="EF21", mode=mode)
lm2_table1 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq, data = df)
summary(lm2_table1)

'Column 3'
df <- get_data(c("EF21", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "B27", "EF40", "EF16U2", "B52"), target="EF21", mode=mode)
lm3_table1 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2, data = df, weights = B52)
summary(lm3_table1)

'Column 4'
df <- get_data(c("EF21", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "B27", "EF40", "EF16U2", "LEISTUNGSGRUPPE", "B52"), target="EF21", mode=mode)
lm4_table1 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2 + LEISTUNGSGRUPPE, data = df, weights = B52)
summary(lm4_table1)

'Replication of Bachmann et al. (2023), table 4'
'Column 1'
df_ft <- get_data_fulltime(c("EF21", "TAETIGKEITSSCHLUESSEL4", "B52"), mode=mode)
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, data = df_ft, weights = B52))

'Column 2'
df_ft <- get_data_fulltime(c("EF21", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "B27", "EF40", "B52"), target="EF21", mode=mode)
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq, data = df_ft, weights = B52))

'Column 3'
df_ft <- get_data_fulltime(c("EF21", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "B27", "EF40", "EF16U2"), target="EF21", mode=mode)
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2, data = df_ft))

'Column 4'
df_ft <- get_data_fulltime(c("EF21", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "B27", "EF40", "EF16U2", "LEISTUNGSGRUPPE"), target="EF21", mode=mode)
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2 + LEISTUNGSGRUPPE, data = df_ft))

'Replication of Bachmann et al. (2023), figure 1'
#### Propensity score matching, total
df <- get_data(c("EF21", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "B27", "EF40", "EF16U2", "LEISTUNGSGRUPPE"), target="EF21", mode=mode)
m.out1 <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                    EF40_sq + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                  data = df,
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

df <- get_data(c("EF21", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = df))

df <- get_data(c("TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "EF40", "EF16U2", "LEISTUNGSGRUPPE", "B27", "EF48"), target="EF48", mode=mode)

m.out2 <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                    EF40_sq + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                  data = df,
                  method = "nearest",
                  distance = "glm",
                  link = "probit")
m.data2 <- match.data(m.out2, drop.unmatched = FALSE)
fit_all <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
            EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
          data = m.data2,
          weights = weights)
fit_all

df <- get_data(c("EF48", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = df))


### Descriptive and econometric analyses on gross hourly wage
'Replication of Bachmann et al. (2023), table 5'
# Gross monthly income grouped by (non-)/temporary work, fulltime
df_ft <- get_data_fulltime(c("EF21", "TAETIGKEITSSCHLUESSEL4", "B52"), mode=mode)
(fmean(df_ft$EF21, df_ft$TAETIGKEITSSCHLUESSEL4, df_ft$B52))

# Gross hourly income grouped by (non-)/temporary work, fulltime
df_ft <- get_data_fulltime(c("EF48", "TAETIGKEITSSCHLUESSEL4", "B52"), mode=mode)
(fmean(df_ft$EF48, df_ft$TAETIGKEITSSCHLUESSEL4, df_ft$B52))

'Replication of Bachmann et al. (2023), table 6'
'Column 1'
df <- get_data(c("EF48", "TAETIGKEITSSCHLUESSEL4", "B52"), mode=mode)
lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, data=df)
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, df, weights = B52))

'Column 2'
df <- get_data(c("EF48", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "B27", "EF40"), target="EF48", mode=mode)
table(df$B27_rec)
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq, data=df))

df <- get_data(c("EF48", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "B27", "EF40", "B52"), target="EF48", mode=mode)
table(df$B27_rec)
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq, df, weights = B52))

'Column 3'
df <- get_data(c("EF48", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "B27", "EF40", "EF16U2"), target="EF48", mode=mode)
table(df$B27_rec)
lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2, data=df)
df <- get_data(c("EF48", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "B27", "EF40", "B52", "EF16U2"), target="EF48", mode=mode)
table(df$B27_rec)
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2, df, weights = B52))

'Column 4'
df <- get_data(c("TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "EF40", "EF16U2", "LEISTUNGSGRUPPE", "B27", "EF48"), target="EF48", mode=mode)
lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2 + LEISTUNGSGRUPPE, data=df)
df <- get_data(c("TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "EF40", "EF16U2", "LEISTUNGSGRUPPE", "B27", "EF48", "B52"), target="EF48", mode=mode)
table(df$B27_rec)
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2 + LEISTUNGSGRUPPE, df, weights = B52))

'Replication of Bachmann et al. (2023), table 7'
'Column 1'
df_ft <- get_data_fulltime(c("EF48", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, data=df_ft)
df_ft <- get_data_fulltime(c("EF48", "TAETIGKEITSSCHLUESSEL4", "B52"), mode=mode)
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, df_ft, weights = B52))

'Column 2'
df_ft <- get_data_fulltime(c("EF48", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "B27", "EF40"), target="EF48", mode=mode)
lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq, data=df_ft)
df_ft <- get_data_fulltime(c("EF48", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "B27", "EF40", "B52"), target="EF48", mode=mode)
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq, df_ft, weights = B52))

'Column 3'
df_ft <- get_data_fulltime(c("EF48", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "B27", "EF40", "EF16U2"), target="EF48", mode=mode)
lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2, data=df_ft)
df_ft <- get_data_fulltime(c("EF48", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "B27", "EF40", "B52", "EF16U2"), target="EF48", mode=mode)
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2, df_ft, weights = B52))

'Column 4'
df_ft <- get_data_fulltime(c("TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "EF40", "EF16U2", "LEISTUNGSGRUPPE", "B27", "EF48"), target="EF48", mode=mode)
lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2 + LEISTUNGSGRUPPE, data=df_ft)
df_ft <- get_data_fulltime(c("TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "EF40", "EF16U2", "LEISTUNGSGRUPPE", "B27", "EF48", "B52"), target="EF48", mode=mode)
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2 + LEISTUNGSGRUPPE, df_ft, weights = B52))


'Replication of Bachmann et al. (2023), figure 2'
#### Propensity score matching, fulltime
df <- get_data(c("EF48", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "EF40", "EF16U2", "LEISTUNGSGRUPPE"), target="EF48", mode=mode)

'Propensity score matching and comparisons for matched and unmatched observations'
m.out_01a <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                    EF40_sq + EF16U2 + LEISTUNGSGRUPPE,
                  data = df,
                  method = "nearest",
                  distance = "glm",
                  link = "probit")
m.data_01a <- match.data(m.out_01a, drop.unmatched = FALSE)
plot(m.out_01a, type = "density", interactive = FALSE,
     which.xs = ~ EF41 + EF40)
summary(m.out_01a, un = FALSE)

'Effect for matched observations'


fit_01a <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
            EF10 + EF16U2 + LEISTUNGSGRUPPE,
          data = m.data_01a,
          weights = weights)
fit_01a

# Because we are going to fit on EF48, we are repeating the above steps with a
# synthetic table targeting EF48 rather tna EF21
df_ft <- get_data_fulltime(c("EF48", "TAETIGKEITSSCHLUESSEL4", "EF41", "EF10", "EF40", "EF16U2", "LEISTUNGSGRUPPE"), target="EF48", mode=mode)

'Propensity score matching and comparisons for matched and unmatched observations'
m.out_02a <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                    EF40_sq + EF16U2 + LEISTUNGSGRUPPE,
                  data = df_ft,
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
'df_ft <- get_data_fulltime(c("EF21", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = df_ft))
'
df <- get_data(c("EF48", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = df))

df_ft <- get_data_fulltime(c("EF48", "TAETIGKEITSSCHLUESSEL4"), mode=mode)
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = df_ft))

