self_report_processing <- function(df, type = "eligible"){
  
  if(type == "eligible"){
    df_update <- df %>% 
      mutate(screened_dm = case_when(screened_dm == 1 ~ 1,
                                     TRUE ~ 0),
             diagnosed_dm = case_when(current_dm == 1 | toldhigh_dm == 1 ~ 1,
                                      TRUE ~ 0),
             treated_dm = case_when(soughttx_dm == 1 | medication_dm == 1 ~ 1,
                                    TRUE ~ 0),
             
             
             screened_bp = case_when(screened_bp == 1 ~ 1,
                                     TRUE ~ 0),
             diagnosed_bp = case_when(current_bp == 1 | toldhigh_bp == 1 ~ 1,
                                      TRUE ~ 0),
             treated_bp = case_when(soughttx_bp == 1 | medication_bp == 1 ~ 1,
                                    TRUE ~ 0))
    
    
  } else{
    
    df_update <- df %>% 
      mutate(screened_dm = case_when(screened_dm == 1 ~ 1,
                                     TRUE ~ 0),
             diagnosed_dm = case_when(toldhigh_dm == 1 ~ 1,
                                      TRUE ~ 0),
             treated_dm = case_when(medication_dm == 1 ~ 1,
                                    TRUE ~ 0),
             
             
             screened_bp = case_when(screened_bp == 1 ~ 1,
                                     TRUE ~ 0),
             diagnosed_bp = case_when(toldhigh_bp == 1 ~ 1,
                                      TRUE ~ 0),
             treated_bp = case_when(medication_bp == 1 ~ 1,
                                    TRUE ~ 0))
  }
  
  return(df_update)
}