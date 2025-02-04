###########################################################################################
# Title:      Comparing Utility and Risk Measures for the partially synthetic SES 2018    #
#             resulting from R package 'synthpop' and python package 'Data Synthesizer'   #
# Author:     Yannik Garcia Ritz, Paul Francis                                            #
# Datum:      08.11.2024                                                                  #
###########################################################################################

# Masterfile

'Deleting workspace'
'rm(list=ls())'

'Setting number of decimals'
options("scipen" = 999, digits = 7) 

'1. Installing and loading required packages'
'install.packages("collapse")'
'install.packages("dplyr")
install.packages("sdcMicro")
install.packages("Boruta")
install.packages("synthpop")
install.packages("simPop")
install.packages("tidymodels")
install.packages("tidyselect")
install.packages("lubridate")
install.packages("arm")
install.packages("ggplot2")
install.packages("ggplotify")
install.packages("MatchIt")'
library(collapse)
library(dplyr)
library(sdcMicro)
library(Boruta)
library(synthpop)
library(simPop)
library(tidymodels)
library(tidyselect)
library(lubridate)
library(arm)
library(ggplot2)
library(ggplotify)
library(MatchIt)

'2. Loading onsite material of SES 2018'
'Loading employee file of SES 2018 (onsite material)'
vse_AN_gwap <- read.csv("~/Anonymitaet/vse_2018_arbeitnehmer_bund_byanonym.csv")

'3. Calling subscripts'
source("~/Anonymitaet/script/Compare_Synthpop_SynDiffix/preprocessing_baseline_cleaned.R")
source("~/Anonymitaet/script/Compare_Synthpop_SynDiffix/evaluation_baseline_cleaned.R")
source("~/Anonymitaet/script/Compare_Synthpop_SynDiffix/preprocessing_synthetic_cleaned.R")
source("~/Anonymitaet/script/Compare_Synthpop_SynDiffix/synthesis_synthetic_cleaned.R")
source("~/Anonymitaet/script/Compare_Synthpop_SynDiffix/evaluation_synthesic_cleaned.R")