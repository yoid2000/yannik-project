# Evaluations for replication based on multiply partially synthetic data

# Filtering observations without employment subject to social security contributions
for(i in 1:5){
  syn_AN_PF[[3]][[i]] <- subset(syn_AN_PF[[3]][[i]], PERSONENGRUPPE == 101)
}

# Filtering observations without information on educational level
for(i in 1:5){
  syn_AN_PF[[3]][[i]] <- subset(syn_AN_PF[[3]][[i]], EF16U2!=7)
}

# Filtering observations younger than 17 and older than 62
for(i in 1:5){
  syn_AN_PF[[3]][[i]] <- subset(syn_AN_PF[[3]][[i]], EF41 > 16 & EF41<63)
}

# Gross monthly income grouped by (non-)/temporary work
(fmean(syn_AN_PF[[3]][[1]]$EF21, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$EF21, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$EF21, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$EF21, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$EF21, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5

# Gross hourly income grouped by (non-)/temporary work
for(i in 1:5){
  syn_AN_PF[[3]][[i]]$EF21H <- (syn_AN_PF[[3]][[i]]$EF21 / syn_AN_PF[[3]][[i]]$EF19)
}

(fmean(syn_AN_PF[[3]][[1]]$EF21H, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$EF21H, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$EF21H, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$EF21H, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$EF21H, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5

# Weekly working hours grouped by (non-)/temporary work 
(fmean(syn_AN_PF[[3]][[1]]$EF18, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$EF18, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$EF18, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$EF18, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$EF18, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5

# Age grouped by (non-)/temporary work
# Alter nach (Nicht-)/Zeitarbeit
(fmean(syn_AN_PF[[3]][[1]]$EF41, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$EF41, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$EF41, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$EF41, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$EF41, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5

# Gender grouped by (non-)/temporary work
# Geschlecht nach (Nicht-)/Zeitarbeit
(fmean(syn_AN_PF[[3]][[1]]$EF10, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$EF10, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$EF10, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$EF10, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$EF10, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5-1

# Full /part time grouped by (non-)/temporary work
'table(vse_AN_gwap_noNNA_PF$B27)'
for(i in 1:5){
  syn_AN_PF[[3]][[i]]$B27_rec <- ifelse(syn_AN_PF[[3]][[i]]$B27 == "FT", 1, 0)
}

(fmean(syn_AN_PF[[3]][[1]]$B27_rec, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$B27_rec, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$B27_rec, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$B27_rec, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$B27_rec, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5

# Months of service for the company
(fmean((syn_AN_PF[[3]][[1]]$EF14U2 - syn_AN_PF[[3]][[1]]$EF12U2), syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + 
    fmean((syn_AN_PF[[3]][[2]]$EF14U2 - syn_AN_PF[[3]][[1]]$EF12U2), syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + 
    fmean((syn_AN_PF[[3]][[3]]$EF14U2 - syn_AN_PF[[3]][[1]]$EF12U2), syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + 
    fmean((syn_AN_PF[[3]][[4]]$EF14U2 - syn_AN_PF[[3]][[1]]$EF12U2), syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + 
    fmean((syn_AN_PF[[3]][[5]]$EF14U2 - syn_AN_PF[[3]][[1]]$EF12U2), syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5

# Level of requirements grouped by (non-)/temporary work
for(i in 1:5){
  syn_AN_PF[[3]][[i]]$str_schl1 <- as.character(syn_AN_PF[[3]][[i]]$TAETIGKEITSSCHLUESSEL1)
}

for(i in 1:5){
  syn_AN_PF[[3]][[i]]$last_schl1 <- substr(syn_AN_PF[[3]][[i]]$str_schl1, nchar(syn_AN_PF[[3]][[i]]$str_schl1)-1+1, nchar(syn_AN_PF[[3]][[i]]$str_schl1))
}

for(i in 1:5){
  syn_AN_PF[[3]][[i]]$LEISTUNGSGRUPPE_Helper <- ifelse(syn_AN_PF[[3]][[i]]$last_schl1 == 1, 1, 0) 
  syn_AN_PF[[3]][[i]]$LEISTUNGSGRUPPE_Professional <- ifelse(syn_AN_PF[[3]][[i]]$last_schl1 == 2, 1, 0) 
  syn_AN_PF[[3]][[i]]$LEISTUNGSGRUPPE_Specialist <- ifelse(syn_AN_PF[[3]][[i]]$last_schl1 == 3, 1, 0) 
  syn_AN_PF[[3]][[i]]$LEISTUNGSGRUPPE_Expert <- ifelse(syn_AN_PF[[3]][[i]]$last_schl1 == 4, 1, 0) 
}

(fmean(syn_AN_PF[[3]][[1]]$LEISTUNGSGRUPPE_Helper, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$LEISTUNGSGRUPPE_Helper, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$LEISTUNGSGRUPPE_Helper, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$LEISTUNGSGRUPPE_Helper, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$LEISTUNGSGRUPPE_Helper, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5

(fmean(syn_AN_PF[[3]][[1]]$LEISTUNGSGRUPPE_Professional, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$LEISTUNGSGRUPPE_Professional, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$LEISTUNGSGRUPPE_Professional, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$LEISTUNGSGRUPPE_Professional, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$LEISTUNGSGRUPPE_Professional, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5

(fmean(syn_AN_PF[[3]][[1]]$LEISTUNGSGRUPPE_Specialist, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$LEISTUNGSGRUPPE_Specialist, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$LEISTUNGSGRUPPE_Specialist, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$LEISTUNGSGRUPPE_Specialist, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$LEISTUNGSGRUPPE_Specialist, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5

(fmean(syn_AN_PF[[3]][[1]]$LEISTUNGSGRUPPE_Expert, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$LEISTUNGSGRUPPE_Expert, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$LEISTUNGSGRUPPE_Expert, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$LEISTUNGSGRUPPE_Expert, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$LEISTUNGSGRUPPE_Expert, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5

# Level of education grouped by (non-)/temporary work
table(vse_AN_gwap_noNNA_PF$EF16U2)

for(i in 1:5){
  syn_AN_PF[[3]][[i]]$EF16U2_1 <- ifelse(syn_AN_PF[[3]][[i]]$EF16U2 == 1, 1, 0) 
  syn_AN_PF[[3]][[i]]$EF16U2_2 <- ifelse(syn_AN_PF[[3]][[i]]$EF16U2 == 2, 1, 0) 
  syn_AN_PF[[3]][[i]]$EF16U2_3 <- ifelse(syn_AN_PF[[3]][[i]]$EF16U2 == 3, 1, 0) 
  syn_AN_PF[[3]][[i]]$EF16U2_4 <- ifelse(syn_AN_PF[[3]][[i]]$EF16U2 == 4, 1, 0) 
  syn_AN_PF[[3]][[i]]$EF16U2_5 <- ifelse(syn_AN_PF[[3]][[i]]$EF16U2 == 5 | syn_AN_PF[[3]][[i]]$EF16U2 == 6, 1, 0) 
}

(fmean(syn_AN_PF[[3]][[1]]$EF16U2_1, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$EF16U2_1, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$EF16U2_1, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$EF16U2_1, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$EF16U2_1, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5

(fmean(syn_AN_PF[[3]][[1]]$EF16U2_2, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$EF16U2_2, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$EF16U2_2, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$EF16U2_2, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$EF16U2_2, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5

(fmean(syn_AN_PF[[3]][[1]]$EF16U2_3, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$EF16U2_3, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$EF16U2_3, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$EF16U2_3, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$EF16U2_3, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5

(fmean(syn_AN_PF[[3]][[1]]$EF16U2_4, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$EF16U2_4, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$EF16U2_4, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$EF16U2_4, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$EF16U2_4, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5

(fmean(syn_AN_PF[[3]][[1]]$EF16U2_5, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$EF16U2_5, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$EF16U2_5, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$EF16U2_5, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$EF16U2_5, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5

'Creating a replica of the lm.synds function which is also providing the option to weight the results'
lm.synds.weights <- function(formula, dataset, wghts){
  if (!inherits(dataset, "synds")) 
    stop("Data must have class synds\n", call. = FALSE)
  if (is.matrix(dataset$method)) 
    dataset$method <- dataset$method[1, ]
  if (is.matrix(dataset$visit.sequence)) 
    dataset$visit.sequence <- dataset$visit.sequence[1, ]
  if (dataset$m > 1) 
    vars <- names(dataset$syn[[1]])
  else vars <- names(dataset$syn)
  n <- sum(dataset$n)
  if (is.list(dataset$k)) 
    k <- sum(dataset$k[[1]])
  else k <- sum(dataset$k)
  call <- match.call()
  fitting.function <- "lm"
  
  analyses <- as.list(1:dataset$m)
  coef <- 
  if (dataset$m == 1) {
    analyses[[1]] <- summary(lm(formula, data = dataset$syn, weights = dataset$syn$wghts))
  } else {
    for (i in 1:dataset$m){
      analyses[[i]] <- summary(lm(formula, data = dataset$syn[[i]], weights = dataset$syn[[i]]$wghts))
    }
  }
  'incomplete <- checkcomplete(vars, formula, dataset$visit.sequence, 
                              dataset$method)
  allcoefvar <- mcoefvar(analyses = analyses)
  object <- list(call = call, mcoefavg = allcoefvar$mcoefavg, 
                 mvaravg = allcoefvar$mvaravg, analyses = analyses, fitting.function = fitting.function, 
                 n = n, k = k, proper = dataset$proper, m = dataset$m, method = dataset$method, 
                 incomplete = incomplete, mcoef = allcoefvar$mcoef, mvar = allcoefvar$mvar)
  class(object) <- "fit.synds"
  return(object)'
  coef <- coefficients(analyses[[1]])
  for (i in 2:dataset$m){
    coef <- coef + coefficients(analyses[[i]])
  }
  coef <- coef/dataset$m  
  'print(analyses)'
  cat("Call: \n")
  print(call)
  cat("\n")
  cat("Average coefficient estimates from", dataset$m, "syntheses: \n")
  print(coef)
  #message("these are the mean coefficients:", coef)
}

'Replication of Bachmann et al. (2023), table 3'
'Column 1'
lm.synds.weights(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, syn_AN_PF, B52)
lm.synds(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, syn_AN_PF)

'Column 2'
for(i in 1:5){
  syn_AN_PF[[3]][[i]]$EF41_sq <- syn_AN_PF[[3]][[i]]$EF41*syn_AN_PF[[3]][[i]]$EF41
  syn_AN_PF[[3]][[i]]$EF40_sq <- syn_AN_PF[[3]][[i]]$EF40*syn_AN_PF[[3]][[i]]$EF40
  syn_AN_PF[[3]][[i]]$B27_rec <- ifelse(syn_AN_PF[[3]][[i]]$B27 == "FT", 1, 0)
}
lm.synds.weights(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq, syn_AN_PF, B52)

'Column 3'
lm.synds.weights(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2, syn_AN_PF, B52)

'Column 4'
lm.synds.weights(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2 + LEISTUNGSGRUPPE, syn_AN_PF, B52)


################################################################################

syn_AN_PF_fulltime <- syn_AN_PF

for(i in 1:5){
  syn_AN_PF_fulltime[[3]][[i]] <-subset(syn_AN_PF[[3]][[i]], TAETIGKEITSSCHLUESSEL5 == 1| TAETIGKEITSSCHLUESSEL5 == 2)
}

'Replication of Bachmann et al. (2023), table 4'
'Column 1'
lm.synds.weights(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime, B52)
lm.synds(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime)

'Column 2'
for(i in 1:5){
  syn_AN_PF_fulltime[[3]][[i]]$EF41_sq <- syn_AN_PF_fulltime[[3]][[i]]$EF41*syn_AN_PF[[3]][[i]]$EF41
  syn_AN_PF_fulltime[[3]][[i]]$EF40_sq <- syn_AN_PF_fulltime[[3]][[i]]$EF40*syn_AN_PF[[3]][[i]]$EF40
  syn_AN_PF_fulltime[[3]][[i]]$B27_rec <- ifelse(syn_AN_PF_fulltime[[3]][[i]]$B27 == "FT", 1, 0)
}
lm.synds.weights(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq, syn_AN_PF_fulltime, B52)

'Column 3'
lm.synds.weights(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2, syn_AN_PF_fulltime, B52)

'Column 4'
lm.synds.weights(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2 + LEISTUNGSGRUPPE, syn_AN_PF_fulltime, B52)


#Bruttomonatsverdienst (Vollzeit)
(fmean(syn_AN_PF_fulltime[[3]][[1]]$EF21, syn_AN_PF_fulltime[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime[[3]][[1]]$B52) + fmean(syn_AN_PF_fulltime[[3]][[2]]$EF21, syn_AN_PF_fulltime[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime[[3]][[2]]$B52) + fmean(syn_AN_PF_fulltime[[3]][[3]]$EF21, syn_AN_PF_fulltime[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime[[3]][[3]]$B52) + fmean(syn_AN_PF_fulltime[[3]][[4]]$EF21, syn_AN_PF_fulltime[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime[[3]][[4]]$B52) + fmean(syn_AN_PF_fulltime[[3]][[5]]$EF21, syn_AN_PF_fulltime[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime[[3]][[5]]$B52))/5


