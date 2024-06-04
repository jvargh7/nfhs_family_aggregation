ind_covariates = "+ sex + age + education + smokecurr + alcohol"
hh_covariates = "+ swealthq_ur + nmembers + residence + factor(state) + glucose"

# 1. Disease predicting disease -----------
m0 = paste0("htn_disease ~ I(o_htn >= 1)")
m1 = paste0("htn_disease ~ I(o_htn >= 1)",ind_covariates,hh_covariates)
m2 = paste0("htn_disease ~ I(o_htn >= 1)*sex",ind_covariates,hh_covariates)  %>% str_replace(.,"\\+ sex ","")
m3 = paste0("htn_disease ~ I(o_htn >= 1)*age_category",ind_covariates,hh_covariates)  %>% str_replace(.,"\\+ age ","")
m4 = paste0("htn_disease ~ I(o_htn >= 1)*residence",ind_covariates,hh_covariates)  %>% str_replace(.,"\\+ residence ","")

# Disease predicted by blood relation
s0 = paste0("htn_disease ~ I(o_htn_blood_related >= 1)")
s1 = paste0("htn_disease ~ I(o_htn_blood_related >= 1)",ind_covariates,hh_covariates)

# Disease predicted by non-blood relation
t0 = paste0("htn_disease ~ I(o_htn_not_blood_related >= 1)")
t1 = paste0("htn_disease ~ I(o_htn_not_blood_related >= 1)",ind_covariates,hh_covariates)

# Disease predicted by blood relation and non-blood relation
u0 = paste0("htn_disease ~ I(o_htn_blood_related >= 1) + I(o_htn_not_blood_related >= 1)")
u1 = paste0("htn_disease ~ I(o_htn_blood_related >= 1) + I(o_htn_not_blood_related >= 1)",ind_covariates,hh_covariates)
u2 = paste0("htn_disease ~ I(o_htn_blood_related >= 1)*sex + I(o_htn_not_blood_related >= 1)*sex",ind_covariates,hh_covariates)
u3 = paste0("htn_disease ~ I(o_htn_blood_related >= 1)*age_category + I(o_htn_not_blood_related >= 1)*age_category",ind_covariates,hh_covariates)
u4 = paste0("htn_disease ~ I(o_htn_blood_related >= 1)*residence + I(o_htn_not_blood_related >= 1)*residence",ind_covariates,hh_covariates)


v0 = paste0("htn_disease ~ I(o_htn_blood_related >= 1)*I(o_htn_not_blood_related >= 1)")
v1 = paste0("htn_disease ~ I(o_htn_blood_related >= 1)*I(o_htn_not_blood_related >= 1)",ind_covariates,hh_covariates)


# 2. Clustering of diagnosis and health equity ------------
# Clustering of diagnosis: Diagnosis predicting diagnosis among 'hypertension (diagnosed + undiagnosed)'
p0 = paste0("htn_diagnosed ~ I(o_diagnosedhtn >= 1)")
p1 = paste0("htn_diagnosed ~ I(o_diagnosedhtn >= 1)",ind_covariates,hh_covariates)

# Clustering of diagnosis: Undiagnosed predicting undiagnosed among 'non-diagnosed (undiagnosed + no disease)'
q0 = paste0("htn_disease ~ I(o_undiagnosedhtn >= 1)")
q1 = paste0("htn_disease ~ I(o_undiagnosedhtn >= 1)",ind_covariates,hh_covariates)


# 3. Family aggregation as a screening strategy -------
# Diagnosis predicting disease among 'non-diagnosed (undiagnosed + no disease)'
n0 = paste0("htn_disease ~ I(o_diagnosedhtn >= 1)")
n1 = paste0("htn_disease ~ I(o_diagnosedhtn >= 1)",ind_covariates,hh_covariates)

