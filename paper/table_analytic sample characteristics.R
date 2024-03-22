analytic_characteristics <- read_csv("analysis/nfaan01_analytic sample characteristics.csv") %>% 
  dplyr::select(strata,variable,group,est_ci)

analytic_characteristics %>% 
  pivot_wider(names_from=c("strata"),values_from="est_ci") %>% 
  write_csv(.,"paper/table_analytic sample characteristics.csv")
