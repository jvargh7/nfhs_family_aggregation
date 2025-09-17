# Define function to fit, tidy, and calculate contrasts
fit_tidy_and_contrast_model <- function(formula, design, current_model_name, contrast_list = NULL,design_name = NA) {
  m <- svyglm(formula = as.formula(formula), design = design, family = quasipoisson())
  tidy_m <- broom::tidy(m) %>% mutate(model = current_model_name,design = design_name)
  
  contrast = contrast_list %>% 
    dplyr::filter(model_name == current_model_name)
  
  model_contrasts = data.frame()
  
  if(nrow(contrast) != 0){
    
    
    for (i in 1:nrow(contrast)){
      df <- contrasts_svyglm(svymodel = m, 
                                          modifier = contrast[i,]$modifier, 
                                          exposure = contrast[i,]$exposure) %>% 
        mutate(contrast = contrast[i,]$contrast_name) 
      
      model_contrasts = bind_rows(model_contrasts,
                                  df)
    }
    
  }

  
  rm(m)
  gc()
  return(list(tidy_model = tidy_m, contrasts = model_contrasts))
}