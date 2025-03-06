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
(fmean(syn_AN_PF[[3]][[1]]$EF48, syn_AN_PF[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[1]]$B52) + fmean(syn_AN_PF[[3]][[2]]$EF48, syn_AN_PF[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[2]]$B52) + fmean(syn_AN_PF[[3]][[3]]$EF48, syn_AN_PF[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[3]]$B52) + fmean(syn_AN_PF[[3]][[4]]$EF48, syn_AN_PF[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[4]]$B52) + fmean(syn_AN_PF[[3]][[5]]$EF48, syn_AN_PF[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF[[3]][[5]]$B52))/5


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
  syn_AN_PF_fulltime[[3]][[i]] <-subset(syn_AN_PF[[3]][[i]], TAETIGKEITSSCHLUESSEL5 == 1| TAETIGKEITSSCHLUESSEL5 == 3)
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

#### Propensity score matching: Gross monthly income, total
for(i in 1:5){
  syn_AN_PF[[3]][[i]]$TAETIGKEITSSCHLUESSEL4 <- as.factor(ifelse(syn_AN_PF[[3]][[i]]$TAETIGKEITSSCHLUESSEL4 == 1,0,1))
}

'Propensity score matching and comparisons for the 1st synthetic dataset'
m.out1_1 <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                    EF40_sq + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                  data = syn_AN_PF[[3]][[1]],
                  method = "nearest",
                  distance = "glm",
                  link = "probit")


m.data_1 <- match.data(m.out1_1, drop.unmatched = FALSE)
plot(m.out1, type = "density", interactive = FALSE,
     which.xs = ~ EF41 + EF40)
summary(m.out1, un = FALSE)

fit_1 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
            EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
          data = m.data_1,
          weights = weights)
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF[[3]][[1]]))

'Propensity score matching and comparisons for the 2nd synthetic dataset'
m.out1_2 <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                    EF40_sq + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                  data = syn_AN_PF[[3]][[2]],
                  method = "nearest",
                  distance = "glm",
                  link = "probit")

m.data_2 <- match.data(m.out1_2, drop.unmatched = FALSE)
plot(m.out2, type = "density", interactive = FALSE,
     which.xs = ~ EF41 + EF40)
summary(m.out1, un = FALSE)

fit_2 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
            EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
          data = m.data_2,
          weights = weights)
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF[[3]][[2]]))

'Propensity score matching and comparisons for the 3rd synthetic dataset'
m.out1_3 <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                      EF40_sq + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                    data = syn_AN_PF[[3]][[3]],
                    method = "nearest",
                    distance = "glm",
                    link = "probit")

m.data_3 <- match.data(m.out1_3, drop.unmatched = FALSE)
plot(m.out3, type = "density", interactive = FALSE,
     which.xs = ~ EF41 + EF40)
summary(m.out1, un = FALSE)

fit_3 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
              EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
            data = m.data_3,
            weights = weights)
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF[[3]][[3]]))
fit_3

'Propensity score matching and comparisons for the 4th synthetic dataset'
m.out1_4 <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                      EF40_sq + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                    data = syn_AN_PF[[3]][[4]],
                    method = "nearest",
                    distance = "glm",
                    link = "probit")

m.data_4 <- match.data(m.out1_4, drop.unmatched = FALSE)
plot(m.out4, type = "density", interactive = FALSE,
     which.xs = ~ EF41 + EF40)
summary(m.out1, un = FALSE)

fit_4 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
              EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
            data = m.data_4,
            weights = weights)
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF[[3]][[4]]))
fit_4

'Propensity score matching and comparisons for the 5th synthetic dataset'
m.out1_5 <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                      EF40_sq + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                    data = syn_AN_PF[[3]][[5]],
                    method = "nearest",
                    distance = "glm",
                    link = "probit")

m.data_5 <- match.data(m.out1_5, drop.unmatched = FALSE)
plot(m.out5, type = "density", interactive = FALSE,
     which.xs = ~ EF41 + EF40)
summary(m.out1, un = FALSE)

fit_5 <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
              EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
            data = m.data_5,
            weights = weights)
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF[[3]][[5]]))
fit_5

(coef_psm <- (fit_1$coefficients[[2]] + fit_2$coefficients[[2]] + fit_3$coefficients[[2]] + fit_4$coefficients[[2]] + fit_5$coefficients[[2]])/5)
(coef_npsm <- (lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF[[3]][[1]])$coefficients[[2]] + lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF[[3]][[2]])$coefficients[[2]] + lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF[[3]][[3]])$coefficients[[2]] +
  lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF[[3]][[4]])$coefficients[[2]] + lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF[[3]][[5]])$coefficients[[2]])/5)

#### Propensity score matching: Gross monthly income, fulltime
'Subsampling fulltime employees in each of 5 synthetic datasets'
for(i in 1:5){
  syn_AN_PF_fulltime[[3]][[i]]$TAETIGKEITSSCHLUESSEL4 <- as.factor(ifelse(syn_AN_PF_fulltime[[3]][[i]]$TAETIGKEITSSCHLUESSEL4 == 1,0,1))
}

'Propensity score matching and comparisons for the 1st synthetic dataset'
m.out1_1a <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                      EF40_sq + EF16U2 + LEISTUNGSGRUPPE,
                    data = syn_AN_PF_fulltime[[3]][[1]],
                    method = "nearest",
                    distance = "glm",
                    link = "probit")


m.data_1a <- match.data(m.out1_1a, drop.unmatched = FALSE)
plot(m.out1_1a, type = "density", interactive = FALSE,
     which.xs = ~ EF41 + EF40)
summary(m.out1_1a, un = FALSE)

'Effect for matched observations'
fit_1a <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
              EF10 + EF16U2 + LEISTUNGSGRUPPE,
            data = m.data_1a,
            weights = weights)
fit_1a

'Effect for unmatched observations'
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF_fulltime[[3]][[1]]))


'Propensity score matching and comparisons for the 2nd synthetic dataset'
m.out1_2a <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                      EF40_sq + EF16U2 + LEISTUNGSGRUPPE,
                    data = syn_AN_PF_fulltime[[3]][[2]],
                    method = "nearest",
                    distance = "glm",
                    link = "probit")

m.data_2a <- match.data(m.out1_2a, drop.unmatched = FALSE)
plot(m.out1_2a, type = "density", interactive = FALSE,
     which.xs = ~ EF41 + EF40)
summary(m.out1_2a, un = FALSE)

'Effect for matched observations'
fit_2a <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
              EF10 + EF16U2 + LEISTUNGSGRUPPE,
            data = m.data_2a,
            weights = weights)
fit_2a

'Effect for unmatched observations'
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF_fulltime[[3]][[2]]))


'Propensity score matching and comparisons for the 3rd synthetic dataset'
m.out1_3a <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                      EF40_sq + EF16U2 + LEISTUNGSGRUPPE,
                    data = syn_AN_PF_fulltime[[3]][[3]],
                    method = "nearest",
                    distance = "glm",
                    link = "probit")

m.data_3a <- match.data(m.out1_3a, drop.unmatched = FALSE)
plot(m.out1_3a, type = "density", interactive = FALSE,
     which.xs = ~ EF41 + EF40)
summary(m.out1_3a, un = FALSE)

'Effect for matched observations'
fit_3a <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
              EF10 + EF16U2 + LEISTUNGSGRUPPE,
            data = m.data_3a,
            weights = weights)
fit_3a

'Effect for unmatched observations'
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF_fulltime[[3]][[3]]))


'Propensity score matching and comparisons for the 4th synthetic dataset'
m.out1_4a <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                      EF40_sq + EF16U2 + LEISTUNGSGRUPPE,
                    data = syn_AN_PF_fulltime[[3]][[4]],
                    method = "nearest",
                    distance = "glm",
                    link = "probit")

m.data_4a <- match.data(m.out1_4a, drop.unmatched = FALSE)
plot(m.out1_4a, type = "density", interactive = FALSE,
     which.xs = ~ EF41 + EF40)
summary(m.out1_4a, un = FALSE)

'Effect for matched observations'
fit_4a <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
              EF10 + EF16U2 + LEISTUNGSGRUPPE,
            data = m.data_4a,
            weights = weights)
fit_4a

'Effect for unmatched observations'
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF_fulltime[[3]][[4]]))


'Propensity score matching and comparisons for the 5th synthetic dataset'
m.out1_5a <- matchit(TAETIGKEITSSCHLUESSEL4 ~ EF41 + EF41_sq + EF10 + EF40 + 
                      EF40_sq + EF16U2 + LEISTUNGSGRUPPE,
                    data = syn_AN_PF_fulltime[[3]][[5]],
                    method = "nearest",
                    distance = "glm",
                    link = "probit")

m.data_5a <- match.data(m.out1_5a, drop.unmatched = FALSE)
plot(m.out1_5a, type = "density", interactive = FALSE,
     which.xs = ~ EF41 + EF40)
summary(m.out1_5a, un = FALSE)

'Effect for matched observations'
fit_5a <- lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
              EF10 + EF16U2 + LEISTUNGSGRUPPE,
            data = m.data_5a,
            weights = weights)
fit_5a

'Effect for unmatched observations'
summary(lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF_fulltime[[3]][[5]]))


'Estimatng mean results for matched and unmatched observations over m=5 synthetic datasets'
(coef_psm_rec <- (fit_1a$coefficients[[2]] + fit_2a$coefficients[[2]] + fit_3a$coefficients[[2]] + fit_4a$coefficients[[2]] + fit_5a$coefficients[[2]])/5)
(coef_npsm_rec <- (lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF_fulltime[[3]][[1]])$coefficients[[2]] + lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF_fulltime[[3]][[2]])$coefficients[[2]] + lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF_fulltime[[3]][[3]])$coefficients[[2]] +
                 lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF_fulltime[[3]][[4]])$coefficients[[2]] + lm(log(EF21) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF_fulltime[[3]][[5]])$coefficients[[2]])/5)


### Descriptive and econometric analyses on gross hourly wage
'Table 5'
# Gross monthly income grouped by (non-)/temporary work
(fmean(syn_AN_PF_fulltime[[3]][[1]]$EF21, syn_AN_PF_fulltime[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime[[3]][[1]]$B52) + fmean(syn_AN_PF_fulltime[[3]][[2]]$EF21, syn_AN_PF_fulltime[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime[[3]][[2]]$B52) + fmean(syn_AN_PF_fulltime[[3]][[3]]$EF21, syn_AN_PF_fulltime[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime[[3]][[3]]$B52) + fmean(syn_AN_PF_fulltime[[3]][[4]]$EF21, syn_AN_PF_fulltime[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime[[3]][[4]]$B52) + fmean(syn_AN_PF_fulltime[[3]][[5]]$EF21, syn_AN_PF_fulltime[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime[[3]][[5]]$B52))/5

# Gross hourly income grouped by (non-)/temporary work
(fmean(syn_AN_PF_fulltime[[3]][[1]]$EF48, syn_AN_PF_fulltime[[3]][[1]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime[[3]][[1]]$B52) + fmean(syn_AN_PF_fulltime[[3]][[2]]$EF48, syn_AN_PF_fulltime[[3]][[2]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime[[3]][[2]]$B52) + fmean(syn_AN_PF_fulltime[[3]][[3]]$EF48, syn_AN_PF_fulltime[[3]][[3]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime[[3]][[3]]$B52) + fmean(syn_AN_PF_fulltime[[3]][[4]]$EF48, syn_AN_PF_fulltime[[3]][[4]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime[[3]][[4]]$B52) + fmean(syn_AN_PF_fulltime[[3]][[5]]$EF48, syn_AN_PF_fulltime[[3]][[5]]$TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime[[3]][[5]]$B52))/5

# Linear regression model on gross hourly income, total dataset
'table 6'
'Column 1'
lm.synds(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, syn_AN_PF)
lm.synds.weights(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, syn_AN_PF, B52)

'Column 2'
lm.synds(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq, syn_AN_PF)
lm.synds.weights(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq, syn_AN_PF, B52)

'Column 3'
lm.synds(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2, syn_AN_PF)
lm.synds.weights(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2, syn_AN_PF, B52)

'Column 4'
lm.synds(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2 + LEISTUNGSGRUPPE, syn_AN_PF)
lm.synds.weights(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2 + LEISTUNGSGRUPPE, syn_AN_PF, B52)

# Linear regression model on gross hourly income, fulltime employees
'table 6'
'Column 1'
lm.synds(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime)
lm.synds.weights(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, syn_AN_PF_fulltime, B52)

'Column 2'
lm.synds(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq, syn_AN_PF_fulltime)
lm.synds.weights(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq, syn_AN_PF_fulltime, B52)

'Column 3'
lm.synds(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2, syn_AN_PF_fulltime)
lm.synds.weights(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2, syn_AN_PF_fulltime, B52)

'Column 4'
lm.synds(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2 + LEISTUNGSGRUPPE, syn_AN_PF_fulltime)
lm.synds.weights(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF41 + EF41_sq + EF10 + B27_rec + EF40 + EF40_sq + EF16U2 + LEISTUNGSGRUPPE, syn_AN_PF_fulltime, B52)


#### Propensity score matching: Gross hourly income, total
fit_1_hour <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
              EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
            data = m.data_1,
            weights = weights)
fit_1_hour
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF[[3]][[1]]))

fit_2_hour <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
                   EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                 data = m.data_2,
                 weights = weights)
fit_2_hour
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF[[3]][[2]]))

fit_3_hour <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
                   EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                 data = m.data_3,
                 weights = weights)
fit_3_hour
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF[[3]][[3]]))

fit_4_hour <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
                   EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                 data = m.data_4,
                 weights = weights)
fit_4_hour
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF[[3]][[4]]))

fit_5_hour <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
                   EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                 data = m.data_5,
                 weights = weights)
fit_5_hour
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF[[3]][[5]]))

(coef_psm_rec_hourly <- (fit_1_hour$coefficients[[2]] + fit_2_hour$coefficients[[2]] + fit_3_hour$coefficients[[2]] + fit_4_hour$coefficients[[2]] + fit_5_hour$coefficients[[2]])/5)
(coef_npsm_rec_hourly <- (lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF[[3]][[1]])$coefficients[[2]] + lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF[[3]][[2]])$coefficients[[2]] + lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF[[3]][[3]])$coefficients[[2]] +
                                 lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF[[3]][[4]])$coefficients[[2]] + lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF[[3]][[5]])$coefficients[[2]])/5)


#### Propensity score matching: Gross hourly income, fulltime employees
fit_1b_hour <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
                   EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                 data = m.data_1a,
                 weights = weights)
fit_1b_hour
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF_fulltime[[3]][[1]]))

fit_2b_hour <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
                   EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                 data = m.data_2a,
                 weights = weights)
fit_2b_hour
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF_fulltime[[3]][[2]]))

fit_3b_hour <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
                   EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                 data = m.data_3a,
                 weights = weights)
fit_3b_hour
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF_fulltime[[3]][[3]]))

fit_4b_hour <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
                   EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                 data = m.data_4a,
                 weights = weights)
fit_4b_hour
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF_fulltime[[3]][[4]]))

fit_5b_hour <- lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4 + EF40 + EF40_sq + EF41 + EF41_sq +
                   EF10 + EF16U2 + LEISTUNGSGRUPPE + B27_rec,
                 data = m.data_5a,
                 weights = weights)
fit_5b_hour
summary(lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4,
           data = syn_AN_PF_fulltime[[3]][[5]]))

(coef_psm_rec_hourly_full <- (fit_1b_hour$coefficients[[2]] + fit_2b_hour$coefficients[[2]] + fit_3b_hour$coefficients[[2]] + fit_4b_hour$coefficients[[2]] + fit_5b_hour$coefficients[[2]])/5)
(coef_npsm_rec_hourly_full <- (lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF_fulltime[[3]][[1]])$coefficients[[2]] + lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF_fulltime[[3]][[2]])$coefficients[[2]] + lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF_fulltime[[3]][[3]])$coefficients[[2]] +
                     lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF_fulltime[[3]][[4]])$coefficients[[2]] + lm(log(EF48) ~ TAETIGKEITSSCHLUESSEL4, data = syn_AN_PF_fulltime[[3]][[5]])$coefficients[[2]])/5)
