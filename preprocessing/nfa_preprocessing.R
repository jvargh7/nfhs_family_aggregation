source("functions/bp_processing.R")
source("functions/self_report_processing.R")

nfa_preprocessing <- function(df, sex = "Female", type = "eligible"){
  
  df %>% 
    mutate(sampleweight = sampleweight/(10^6),
           day_interview = case_when(is.na(day_interview) | day_interview == 98 ~ 15,
                                     month_interview == 2 & year_interview %in% c(2008,2012,2016,2020) & day_interview > 29 ~ 29,
                                     month_interview == 2 & day_interview > 28 ~ 28,
                                     month_interview %in% c(4,6,9,11) & day_interview > 30 ~ 30,
                                     TRUE ~ as.numeric(day_interview))) %>% 
    mutate(interview = as_date(paste0(year_interview,"-",month_interview,"-",day_interview)),
           phase = case_when(interview <= "2020-03-23" ~ 1,
                             interview > "2020-03-23" ~ 2,
                             TRUE ~ NA_real_)
    )  %>% 
    bp_processing(.) %>% 
    
    self_report_processing(.,type = type) %>% 
    
    
    mutate(bmi_underweight = case_when(bmi > bmi_max ~ NA_real_,
                                       bmi < bmi_cutoff[1] ~ 1,
                                       bmi >= bmi_cutoff[1] ~ 0,
                                       TRUE ~ NA_real_),
           
           
           bmi_overweight = case_when(bmi > bmi_max ~ NA_real_,
                                      bmi >= bmi_cutoff[2] & bmi < bmi_cutoff[3] ~ 1,
                                      bmi < bmi_cutoff[2] | bmi >= bmi_cutoff[3] ~ 0,
                                      TRUE ~ NA_real_),
           
           
           bmi_obese = case_when(bmi > bmi_max ~ NA_real_,
                                 bmi >= bmi_cutoff[3] ~ 1,
                                 bmi < bmi_cutoff[3] ~ 0,
                                 TRUE ~ NA_real_)) %>% 
    
    
    # Option 2: Should this be average of last 2 measurements?
    # Option 3: ICMR suggests take 2 measurements 1 min apart, if difference in SBP > 5mmHg, take 3rd. Take lowest among closest.
    mutate(
      # sbp = rowMeans(.[,c("sbp1","sbp2","sbp3")],na.rm=TRUE),
      # https://stackoverflow.com/questions/53084598/row-wise-min-on-right-hand-when-using-dplyrcase-when
      sbp = case_when((abs(sbp1-sbp2) <= 5) ~ pmin(sbp1,sbp2,na.rm=TRUE),
                      TRUE ~ pmin(sbp1,sbp2,sbp3,na.rm=TRUE)),
      
      # "sb18d" has 108 everywhere
      # dbp = rowMeans(.[,c("dbp1","dbp2","dbp3")],na.rm=TRUE),
      
      dbp = case_when(sbp == sbp1 ~ dbp1,
                      sbp == sbp2 ~ dbp2,
                      sbp == sbp3 ~ dbp3),
      
      dbp = case_when(is.na(dbp) ~ pmin(dbp1,dbp2,dbp3,na.rm=TRUE),
                      TRUE ~ dbp),
      
      htn = case_when(diagnosed_bp == 1 ~ 1,
                      is.na(sbp) | is.na(dbp) ~ NA_real_,
                      sbp >= sbp_cutoff ~ 1,
                      dbp >= dbp_cutoff ~ 1,
                      sbp < sbp_cutoff ~ 0,
                      dbp < dbp_cutoff ~ 0,
                      TRUE ~ NA_real_),
      highbp = case_when(
        is.na(sbp) | is.na(dbp) ~ NA_real_,
        sbp >= sbp_cutoff ~ 1,
        dbp >= dbp_cutoff ~ 1,
        sbp < sbp_cutoff ~ 0,
        dbp < dbp_cutoff ~ 0,
        TRUE ~ NA_real_),
      
      invhighbp = 1 - highbp,
      
      # Among those diagnosed, indicator of hypertension control status
      diaghtn = case_when(
        diagnosed_bp == 0 ~ NA_real_,
        is.na(sbp) | is.na(dbp) ~ NA_real_,
        diagnosed_bp == 1 & age < agebp_cutoff & sbp >= sbp_target[1] ~ 1,
        diagnosed_bp == 1 & age < agebp_cutoff & dbp >= dbp_target[1] ~ 1,
        diagnosed_bp == 1 & age < agebp_cutoff & sbp < sbp_target[1] ~ 0,
        diagnosed_bp == 1 & age < agebp_cutoff & dbp < dbp_target[1] ~ 0,
        
        diagnosed_bp == 1 & age >= agebp_cutoff & sbp >= sbp_target[2] ~ 1,
        diagnosed_bp == 1 & age >= agebp_cutoff & dbp >= dbp_target[2] ~ 1,
        diagnosed_bp == 1 & age >= agebp_cutoff & sbp < sbp_target[2] ~ 0,
        diagnosed_bp == 1 & age >= agebp_cutoff & dbp < dbp_target[2] ~ 0,
        
        TRUE ~ NA_real_)
    ) %>% 
    
    
    # Hypertension cascade -----
  mutate(# Need both BP measurements
    htn_sample = case_when(!is.na(sbp) & !is.na(dbp) ~ 1,
                           is.na(sbp) | is.na(dbp) ~ 0,
                           TRUE ~ 1),
    # Diagnosis: No/DK, Blood pressure: in range
    htn_free = case_when(
      # Need BP measurements AND self-report
      is.na(htn) | is.na(sbp) | is.na(dbp) ~ NA_real_,
      htn == 1 ~ 0,
      htn == 0 ~ 1,
      TRUE ~ NA_real_),
    htn_unscreened  = case_when(htn == 0 ~ NA_real_,
                                screened_bp == 1 ~ 0,
                                screened_bp == 0 ~ 1,
                                TRUE ~ NA_real_),
    
    # Diagnosis: No/DK + Blood pressure: hypertension
    htn_screened_undiag = case_when(screened_bp == 0 | is.na(screened_bp) ~ NA_real_,
                                    diagnosed_bp == 1 ~ 0,
                                    diagnosed_bp == 0 ~ 1,
                                    TRUE ~ NA_real_),
    
    htn_undiag_htn = case_when(diagnosed_bp == 1 | is.na(diagnosed_bp) ~ NA_real_,
                               htn == 1 ~ 1,
                               htn == 0 ~ 0,
                               TRUE ~ NA_real_),
    
    # Diagnosis: Yes + Treated: No, Blood pressure: <NA>
    htn_diag_untreat = case_when(diagnosed_bp == 1 & treated_bp == 1 ~ 0,
                                 diagnosed_bp == 1 & treated_bp == 0 ~ 1,
                                 TRUE ~ NA_real_),
    
    # Dignosis: Yes, Treated: Yes, Blood pressure: out of control range
    htn_treat_uncontr = case_when(treated_bp == 0 | is.na(treated_bp)  ~ NA_real_,
                                  treated_bp == 1 & diaghtn == 1 ~ 1,
                                  treated_bp == 1 & diaghtn == 0 ~ 0,
                                  TRUE ~ NA_real_),
    # Dignosis: Yes, Treated: Yes, Blood pressure: in range
    htn_treat_contr = 1 - htn_treat_uncontr,
    
    # Dignosis: Yes, Treated: Yes or No, Blood pressure: out of control range
    htn_diag_uncontr = case_when(diagnosed_bp == 0 | is.na(diagnosed_bp)  ~ NA_real_,
                                 diaghtn == 1 ~ 1,
                                 diaghtn == 0 ~ 0,
                                 TRUE ~ NA_real_),
    # Dignosis: Yes, Treated: Yes, Blood pressure: in control range
    htn_diag_contr = 1 - htn_diag_uncontr
    
  ) %>%
    
    # Prehypertension ------
  mutate(
    prehypertension = case_when(diagnosed_bp == 1 ~ NA_real_,
                                is.na(sbp) | is.na(dbp) ~ NA_real_,
                                htn == 1 ~ 0,
                                sbp >= sbppre_cutoff & sbp < sbp_cutoff ~ 1,
                                dbp >= dbppre_cutoff & dbp < dbp_cutoff~ 1,
                                sbp < sbppre_cutoff ~ 0,
                                dbp < dbppre_cutoff ~ 0,
                                TRUE ~ NA_real_)
  ) %>% 
    # BMI
    mutate_at(vars(bmi),function(x) case_when(x > 6000 ~ NA_real_,
                                              TRUE ~ as.numeric(x))) %>%
    # Circumferences
    mutate_at(vars(waistcircumference,hipcircumference),function(x) case_when(x > 240 ~ NA_real_,
                                                                              TRUE ~ as.numeric(x))) %>% 
    
    
    # Glucose
    mutate_at(vars(glucose), function(x) case_when(is.na(x) | x > 498 ~ NA_real_,
                                                   TRUE ~ as.numeric(x))) %>% 
    # Caste
    mutate(na_caste = case_when(is.na(caste) | caste == 8 ~ 1,
                                TRUE ~ 0)) %>% 
    mutate_at(vars(caste),function(x) case_when(x == 1 ~ "Schedule Caste",
                                                x == 2 ~ "Schedule Tribe",
                                                x == 3 ~ "OBC",
                                                x == 4 ~ "General",
                                                x == 8 ~ "General",
                                                TRUE ~ "General")) %>% 
    # Education
    mutate(na_education = case_when(is.na(education) | education == 9 ~ 1,
                                    TRUE ~ 0)) %>% 
    mutate_at(vars(education),function(x) case_when(x == 0 ~ "No education",
                                                    x == 1 ~ "Primary",
                                                    x == 2 ~ "Secondary",
                                                    x == 3 ~ "Higher",
                                                    x == 9 ~ "No education",
                                                    TRUE ~ "No education")) %>% 
    # Religion
    mutate_at(vars(religion),function(x) case_when(x == 1 ~ "Hindu",
                                                   x == 2 ~ "Muslim",
                                                   TRUE ~ "Other")) %>% 
    # insurance, alcohol
    mutate_at(vars(
      alcohol,insurance), function(x) case_when(x == 0 ~ 0,
                                                x == 1 ~ 1,
                                                TRUE ~ NA_real_)) %>% 
    # Smoking
    mutate_at(vars(smokecurr), function(x) case_when(x %in% c(1,2) ~ 1, # SM604 has every day (==1), some days (==2)
                                                     x == 0 ~ 0,
                                                     TRUE ~ NA_real_)) %>% 
    
    mutate(smokecount = case_when(smokecount > 80 ~ NA_real_,
                                  TRUE ~ as.numeric(smokecount))) %>% 
    
    mutate(
      eduyr = case_when(education == "No education" ~ 0,
                        TRUE ~ as.numeric(eduyr))
    ) %>% 
    # Diagnosed, Treated in Diagnosed, Controlled in Treated
    mutate(htn_disease = case_when(is.na(htn_free) ~ NA_real_,
                                   htn_free == 1 ~ 0,
                                   htn_undiag_htn == 1 ~ 1,
                                   htn_diag_untreat == 1 ~ 1,
                                   htn_treat_uncontr == 1 ~ 1,
                                   htn_treat_contr == 1 ~ 1,
                                   TRUE ~ 0),
           htn_screened = case_when(
             screened_bp == 1 ~ 1,
             htn_undiag_htn == 1 ~ 0,
             htn_diag_untreat == 1 ~ 1,
             htn_treat_uncontr == 1 ~ 1,
             htn_treat_contr == 1 ~ 1,
             TRUE ~ 0),
           
           htn_diagnosed = case_when(is.na(htn_free) ~ NA_real_,
                                     htn_free == 1 ~ 0,
                                     htn_undiag_htn == 1 ~ 0,
                                     htn_diag_untreat == 1 ~ 1,
                                     htn_treat_uncontr == 1 ~ 1,
                                     htn_treat_contr == 1 ~ 1,
                                     TRUE ~ 0
           ),
           htn_treated = case_when(is.na(htn_free) ~ NA_real_,
                                   htn_free == 1 ~ 0,
                                   htn_undiag_htn == 1 ~ 0,
                                   htn_diag_untreat == 1 ~ 0,
                                   htn_treat_uncontr == 1 ~ 1,
                                   htn_treat_contr == 1 ~ 1,
                                   TRUE ~ 0
           ),
           htn_controlled = case_when(is.na(htn_free) ~ NA_real_,
                                      htn_free == 1 ~ 0,
                                      htn_undiag_htn == 1 ~ 0,
                                      # EDIT 30-Mar-2023 -------
                                      # htn_diag_contr == 1 ~ 1,
                                      htn_treat_contr == 1 ~ 1,
                                      htn_diag_untreat == 1 ~ 0,
                                      htn_diag_uncontr == 1 ~ 0,
                                      TRUE ~ 0
           ),
           htn_screened_in_dis = case_when(
             is.na(htn_free) ~ NA_real_,
             htn_free == 1 ~ NA_real_,
             screened_bp == 1 ~ 1,
             htn_undiag_htn == 1 ~ 0,
             htn_diag_untreat == 1 ~ 1,
             htn_treat_uncontr == 1 ~ 1,
             htn_treat_contr == 1 ~ 1,
             TRUE ~ 0),
           htn_diagnosed_in_dis = case_when(is.na(htn_free) ~ NA_real_,
                                            htn_free == 1 ~ NA_real_,
                                            htn_undiag_htn == 1 ~ 0,
                                            htn_diag_untreat == 1 ~ 1,
                                            htn_treat_uncontr == 1 ~ 1,
                                            htn_treat_contr == 1 ~ 1,
                                            TRUE ~ 0
           ),
           htn_treated_in_dis = case_when(is.na(htn_free) ~ NA_real_,
                                          htn_free == 1 ~ NA_real_,
                                          htn_undiag_htn == 1 ~ 0,
                                          htn_diag_untreat == 1 ~ 0,
                                          htn_treat_uncontr == 1 ~ 1,
                                          htn_treat_contr == 1 ~ 1,
                                          TRUE ~ 0
           ),
           htn_controlled_in_dis = case_when(is.na(htn_free) ~ NA_real_,
                                             htn_free == 1 ~ NA_real_,
                                             htn_undiag_htn == 1 ~ 0,
                                             # EDIT 30-Mar-2023 -------
                                             # htn_diag_contr == 1 ~ 1,
                                             htn_treat_contr == 1 ~ 1,
                                             htn_diag_untreat == 1 ~ 0,
                                             htn_diag_uncontr == 1 ~ 0,
                                             TRUE ~ 0
           )) %>% 
    
    mutate(bmi_category = case_when(bmi > bmi_max ~ NA_real_,
                                    bmi >= bmi_cutoff[3] ~ 4,
                                    bmi >= bmi_cutoff[2] ~ 3,
                                    bmi >= bmi_cutoff[1] ~ 2,
                                    bmi < bmi_cutoff[1] ~ 1,
                                    TRUE ~ NA_real_),
           
           highwc = case_when(sex == "Female" & waistcircumference >= female_wc_cutoff ~ 1,
                              sex == "Female" & waistcircumference < female_wc_cutoff ~ 0,
                              sex == "Male" & waistcircumference >= male_wc_cutoff ~ 1,
                              sex == "Male" & waistcircumference < male_wc_cutoff ~ 0,
                              TRUE ~ NA_real_
           ),
           waist_hip = case_when(!is.na(hipcircumference) ~ waistcircumference/hipcircumference,
                                 TRUE ~ NA_real_)
    ) %>% 
    
    mutate(bmi_category = factor(bmi_category,levels=c(1:4),labels=c("Underweight","Normal","Overweight","Obese")),
           highwhr = case_when(sex == "Female" & waist_hip >= female_whr_cutoff ~ 1,
                               sex == "Female" & waist_hip < female_whr_cutoff ~ 0,
                               sex == "Male" & waist_hip >= male_whr_cutoff ~ 1,
                               sex == "Male" & waist_hip < male_whr_cutoff ~ 0,
                               TRUE ~ NA_real_),
           age_category = case_when(age %in% c(18:39) ~ 1,
                                    age %in% c(40:64) ~ 2,
                                    age >= 65 ~ 3,
                                    TRUE ~ NA_real_),
           dm_at_risk = case_when(age_category %in% c(2,3) & 
                                    # bmi_category %in% c("Overweight","Obese") &
                                    htn_disease == 1 ~ 1,
                                  TRUE ~ 0)) %>% 
    mutate(age_category = factor(age_category,levels=c(1:3),labels=c("18-39","40-64","65 plus")),
           
           # State wealth quintile - urban/rural
           swealthq_ur = case_when(!is.na(suwealthq) ~ suwealthq,
                                   TRUE ~ srwealthq),
           # State wealth factor score - urban/rural
           swealths_ur = case_when(!is.na(suwealths) ~ suwealths,
                                   TRUE ~ srwealths)
    ) %>% 
    mutate(bmi = bmi/100)  %>% 
    mutate(age_category10 = cut(age,breaks=c(18,30,40,50,60,70,80,100),include.lowest=TRUE,right=FALSE),
           age_category5 = cut(age,breaks=seq(15,100,by=5),include.lowest=TRUE,right=FALSE)) %>% 
    
    # Marital status
    mutate(across(one_of(c("marital","marital2")),.fns=function(x) case_when(x %in% c(0,9) ~ "Unmarried or Unknown", # V501 has 9 = Missing, # Both have 0 = Never in Union
                                                                             x %in% c(1,2) ~ "Currently Married", # 1 = Married, 2 = Living with partner
                                                                             x %in% c(3:6) ~ "Previously Married",# 3 = Widowed, 4 = Divorced, 5 = Separated, 6 = Deserted
                                                                             TRUE ~ "Unmarried or Unknown"))) %>%  
    mutate(across(one_of(c("marital3")),.fns = function(x) case_when(x %in% c(1,9) ~ "Never or Unknown",
                                                                     x == 2 ~ "Current or Former",
                                                                     TRUE ~ "Never or Unknown"))) %>% 
    return(.)
}

