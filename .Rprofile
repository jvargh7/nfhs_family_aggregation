library(haven)
library(tidyverse)
library(survey)
library(srvyr)
library(lubridate)

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

if(Sys.info()["user"] == "JVARGH7"){
  path_dhs_data <- "C:/Cloud/OneDrive - Emory University/data/dhs_program"
  path_family_aggregation_folder <- "C:/Cloud/OneDrive - Emory University/Papers/NFHS Family Concordance"
  path_family_aggregation_repo <- "C:/code/external/nfhs_family_aggregation"
}
if(Sys.info()["user"] == "ksanaka"){
  path_dhs_data <- "/Users/krishnasanaka/Desktop/Public Health Research/nfhs_family_aggregation-main/data"
  path_family_aggregation_folder <- "/Users/krishnasanaka/Library/CloudStorage/OneDrive-SharedLibraries-EmoryUniversity/Varghese, Jithin Sam - NFHS Family Concordance"
  path_family_aggregation_repo <- "/Users/krishnasanaka/Desktop/Public Health Research/nfhs_family_aggregation-main"
}

fasting_time <- 7.9

bmi_max = 6000

bmi_cutoff <- c(1850, 2500, 3000)
bmiasian_cutoff <- c(1850,2300,2750)

# Non-Asians
female_wc_cutoff = 80 
female_whr_cutoff = 0.80
male_wc_cutoff = 94 
male_whr_cutoff = 0.95


# Asians
# female_wc_cutoff = 80 
# female_whr_cutoff = 0.85
# male_wc_cutoff = 90 
# male_whr_cutoff = 0.9

fpg_cutoff <- 126
rpg_cutoff <- 220
# Alternative cutoff for RPG --> used in ncp_preprocessing2.R
rpg_cutoff2 <- 200
# Alternative cutoff for RPG --> used in ncp_preprocessing3.R
rpg_cutoff3 <- 160

sbp_cutoff <- 140
dbp_cutoff <- 90
sbppre_cutoff <- 130
dbppre_cutoff <- 85

# Alternative cutoff for SBP and DBP 
sbp_cutoff2 <- 130
dbp_cutoff2 <- 80
sbppre_cutoff2 <- 120
dbppre_cutoff2 <- 80

fpg_target <- 126
rpg_target <- 180 #Indian DM guidelines
# Indian HTN guidelines (Shah 2020: 130/80 for <= 60y, 140/90 otherwise)
# ICMR 2016 guidelines
# 4.4.1 The current target for control of BP for patients under 80 years of age should
# be systolic blood pressure less than 140 mm and diastolic blood pressure less than 90 mm.
# 4.4.2 The current target for control of BP for patients 80 years or older should be
# less than 150 mm systolic and less than 90 mm diastolic.

sbp_target <- c(140,150) 
agebp_cutoff <- 80
dbp_target <- c(90,90)

fpgpre_cutoff <- 100
rpgpre_cutoff <- 140

