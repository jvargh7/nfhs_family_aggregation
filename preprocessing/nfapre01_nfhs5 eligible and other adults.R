rm(list=ls());gc();source(".Rprofile")


# Based on nfhs5_couples/preprocessing/n5c01_creating couples data.R

source("preprocessing/nfa_preprocessing.R")

variable_list <- readxl::read_excel("data/NFHS Family Aggregation Variable List.xlsx", sheet="7a variables")

iapr7e_female_cleaned <- read_dta(paste0(path_dhs_data,"/IA/IAPR7EDT/IAPR7EFL.dta"),
                      col_select = na.omit(variable_list$iapr7a_women)) %>% 
  rename_with(~ variable_list[!is.na(variable_list$iapr7a_women),]$new_var[which(na.omit(variable_list$iapr7a_women) == .x)], 
              .cols = na.omit(variable_list$iapr7a_women)) %>%
  dplyr::filter(!is.na(age)) %>%
  mutate(sex = "Female") %>% 
  nfa_preprocessing(.,sex = "Female",type = "other")


iair7e_cleaned <- read_dta(paste0(path_dhs_data,"/IA/IAIR7EDT/IAIR7EFL.dta"),
                   col_select = na.omit(variable_list$iair7a)) %>% 
  rename_with(~ variable_list[!is.na(variable_list$iair7a),]$new_var[which(na.omit(variable_list$iair7a) == .x)], 
              .cols = na.omit(variable_list$iair7a))   %>% 
  dplyr::filter(!is.na(age)) %>%
  mutate(sex = "Female") %>% 
  nfa_preprocessing(.,sex = "Female",type = "eligible")


iapr7e_male_cleaned <- read_dta(paste0(path_dhs_data,"/IA/IAPR7EDT/IAPR7EFL.dta"),
                      col_select = na.omit(variable_list$iapr7a_men)) %>% 
  rename_with(~ variable_list[!is.na(variable_list$iapr7a_men),]$new_var[which(na.omit(variable_list$iapr7a_men) == .x)], 
              .cols = na.omit(variable_list$iapr7a_men)) %>%
  dplyr::filter(!is.na(age)) %>%
  mutate(sex = "Male") %>% 
  nfa_preprocessing(.,sex = "Male",type = "other")
  
iamr7e_cleaned <- read_dta(paste0(path_dhs_data,"/IA/IAMR7EDT/IAMR7EFL.dta"),
                     col_select = na.omit(variable_list$iamr7a)) %>% 
    rename_with(~ variable_list[!is.na(variable_list$iamr7a),]$new_var[which(na.omit(variable_list$iamr7a) == .x)], 
                .cols = na.omit(variable_list$iamr7a))   %>% 
    dplyr::filter(!is.na(age)) %>%
  left_join(iapr7e_male_cleaned %>% 
              dplyr::select(cluster,hhid,linenumber,bmi,hb,hb_result,hb_adjusted,anemia),
            by = c("cluster","hhid","linenumber")) %>% 
    mutate(sex = "Male") %>% 
    nfa_preprocessing(.,sex = "Male",type = "eligible")


saveRDS(iair7e_cleaned,paste0(path_family_aggregation_folder,"/working/cleaned/iair7e_cleaned.RDS"))
saveRDS(iamr7e_cleaned,paste0(path_family_aggregation_folder,"/working/cleaned/iamr7e_cleaned.RDS"))
saveRDS(iapr7e_female_cleaned,paste0(path_family_aggregation_folder,"/working/cleaned/iapr7e_female_cleaned.RDS"))
saveRDS(iapr7e_male_cleaned,paste0(path_family_aggregation_folder,"/working/cleaned/iapr7e_male_cleaned.RDS"))