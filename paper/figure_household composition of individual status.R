rm(list=ls());gc();source(".Rprofile")


composition <- read_csv("analysis/nfaan04_household type by valid number of members.csv") %>% 
  # All members --> htype
  rename(htype = group)
proportions <- read_csv("analysis/nfaan04_proportions of households by valid number of members.csv") %>% 
  rename(valid_size_cat = group)
composition_individual <- read_csv("analysis/nfaan04_household type by hypertension status of individual.csv") %>% 
  # Others members --> o_htype
  rename(o_htype = group)
composition_individual_by_size_cat <- read_csv("analysis/nfaan04_household type by hypertension status of individual and valid number of members.csv") %>% 
  rename(o_htype = group)
proportions_individual_by_size_cat <- read_csv("analysis/nfaan04_proportions of individuals by valid number of members.csv") %>% 
  rename(valid_size_cat = group)
proportions_individual <- read_csv("analysis/nfaan04_proportions of individuals.csv") %>% 
  rename(htn_status = group) %>% 
  mutate(htn_status = case_when(is.na(htn_status) ~ "Total",
                                TRUE ~ htn_status))



(fig_hh_comp = (hh_comp_df = composition %>% 
  left_join(proportions %>% 
              dplyr::select(-variable,-type) %>% 
              rename(prop_valid_size_cat = estimate,
                     lci_valid_size_cat = lci,
                     uci_valid_size_cat = uci),
              by = c("valid_size_cat")) %>% 
  mutate(prop_valid_size_cat = case_when(is.na(prop_valid_size_cat) ~ 100,
                                         TRUE ~ prop_valid_size_cat)) %>% 
  mutate(plot_estimate = (estimate*prop_valid_size_cat)/100,
         valid_size_cat = factor(valid_size_cat,levels=c("Total","2","3-4","5"),labels=c("Overall","2","3 to 4","5 or more")),
         htype = factor(htype, levels = c("No Hypertension","All Diagnosed","All Undiagnosed","Discordance"))) %>%
    arrange(desc(valid_size_cat)) %>% 
    # KRISHNA Please reformat the value labels for the values inside the block ---- 
    group_by(valid_size_cat) %>% 
    mutate(label_pos = cumsum(plot_estimate)) %>% 
    ungroup() %>% 
    mutate(label_pos = case_when(plot_estimate < 5 & htype == "Discordance" ~ label_pos + 2,
                                 TRUE ~ label_pos - 1))) %>% 
  ggplot(data=.,aes(x=valid_size_cat,y=plot_estimate,
                    # ymax=uci_valid_size_cat,ymin=lci_valid_size_cat,
                    # https://stackoverflow.com/questions/42710056/reverse-stacked-bar-order
                    fill=forcats::fct_rev(htype),label=sprintf("%.01f",estimate))) +
  geom_col() +
  geom_text(aes(y=label_pos),size = 3)+
  xlab("Number of adults") +
    theme_bw() +
    ylab("Distribution of households") +
    scale_fill_manual(name="",values=c("orange","#F8C291","#A084CA","lightblue")) +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          axis.title = element_text(size = 12)) +
    guides(fill=guide_legend(nrow=2))
  ) 



(fig_ind_dist =
    (ind_dist_df = 
       composition_individual %>% 
       left_join(proportions_individual %>% 
                   dplyr::select(-variable,-type) %>% 
                   rename(prop_htn_status = estimate,
                          lci_htn_status = lci,
                          uci_htn_status = uci),
                 by=c("htn_status")) %>% 
  mutate(plot_estimate = (estimate*prop_htn_status)/100,
         htn_status = factor(htn_status,levels=c("Total","d","u","n"),labels=c("Total \nHypertension","Diagnosed \nHypertension","Undiagnosed \nHypertension","Normal \nBlood Pressure")),
         o_htype = factor(o_htype, levels = c("No Hypertension","All Diagnosed","All Undiagnosed","Discordance"),
                          labels=c("No Hypertension","Others Diagnosed","Others Undiagnosed","Discordance")))%>%
    arrange(desc(htn_status)) %>% 
    # KRISHNA Please reformat the value labels for the values inside the block ---- 
    group_by(htn_status) %>% 
    mutate(label_pos = cumsum(plot_estimate)) %>% 
    ungroup() %>% 
    mutate(label_pos = case_when(plot_estimate < 5 & o_htype == "Discordance" ~ label_pos + 1,
                                  TRUE ~ label_pos - 1))
    ) %>% 
    dplyr::filter(htn_status %in% c("Total \nHypertension","Diagnosed \nHypertension","Undiagnosed \nHypertension")) %>% 
    ggplot(data=.,aes(x=htn_status,y=plot_estimate,
                      # ymax=uci_valid_size_cat,ymin=lci_valid_size_cat,
                      # https://stackoverflow.com/questions/42710056/reverse-stacked-bar-order
                      fill=forcats::fct_rev(o_htype),label=sprintf("%.01f",estimate))) +
    geom_col() +
    geom_text(aes(y=label_pos),size = 3)+
    xlab("") +
    theme_bw() +
    ylab("Prevalence of Hypertension (%)") +
    scale_fill_manual(name="",values=c("orange","#F8C291","#A084CA","lightblue")) +
    theme(legend.position = "bottom",
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          axis.title = element_text(size = 12)) +
    guides(fill=guide_legend(nrow=2))
) 


# fig_ind_dist %>% 
#   ggsave(.,filename=paste0(path_family_aggregation_folder,"/figures/individual distribution for household types.jpg"),width = 8, height =6)


library(ggpubr)

ggarrange(fig_hh_comp,
          fig_ind_dist,
          nrow = 1,
          ncol = 2,
          labels = c("A","B")) %>% 

  ggsave(.,filename=paste0(path_family_aggregation_folder,"/figures/household composition of individual status.jpg"),width = 12, height =6)



