
# AUTHOR:  USAID/Namibia
# PURPOSE:  KP Target Allocations
# LICENSE:  MIT
# DATE:     2024-06-24
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
library(tidyverse)
library(readxl)
library(gagglr)
library(stringr)



# IMPORT ------------------------------------------------------------------
  
  #tst
  tst_path<-return_latest("Data",pattern="Target Setting Tool")
  
  df_kp<-read_excel(tst_path,
                    sheet="KP",
                    skip=13) %>% 
    select(PSNU,KeyPop,ID,TX_NEW.KP.T_81837,
           TX_NEW.KP.T_87514,TX_NEW.KP.T_87515) %>% 
    filter(KeyPop %in% c("FSW","MSM","TG"))
  
  #pxnu x im
  psnu_path<-return_latest("Data",pattern="PSNUxIM")
  
  psnu_im<-read_excel(psnu_path,
                      sheet="PSNUxIM",
                      skip=13)

# MUNGE -------------------------------------------------------------------

  df_total_target<-psnu_im %>% 
    filter(Age %in% c("15-24","25-34","35-49","50+")) %>% #filter to 15+
    group_by(PSNU,indicator_code,Sex) %>% #aggregate by indicator per psnu by sex
    summarize_at(vars(DataPackTarget),sum,na.rm=TRUE) %>%  #summarize the DataPackTarget column using the sum function
    ungroup() %>% 
    filter(indicator_code=="TX_NEW.T") %>% 
    unite(join,PSNU,indicator_code,Sex,remove = FALSE) #making a column to join on
  
  
  
  df_kp_2<-df_kp %>% 
    mutate(indicator_code="TX_NEW.T") %>%  #create a new column and populate it with the indicator code
    rename_with(~str_remove(.,".*_")) %>%  #fix this to apply only to certain columns; this is removing all column name text including and before an underscore
    mutate(sex=case_when(
      KeyPop=="FSW" ~ "Female",
      KeyPop=="MSM" ~ "Male",
      TRUE ~ "Male")) %>% 
    group_by(PSNU,code,sex) %>% 
    summarise(across(where(is.numeric), # what variable to sum on if we are looking at results? 
                     \(x) sum(x, na.rm = TRUE)), #\(x) is shorthand for function
              .groups = "drop") %>% 
    ungroup() %>% 
    unite(join,PSNU,code,sex)
  
  

  new_rows<- psnu_im %>% 
    filter(indicator_code=="TX_NEW.T") %>% 
    select(PSNU,indicator_code,Age,Sex,KeyPop,ID)
  
  
  allocations<-df_total_target %>% 
    left_join(df_kp_2,by="join") %>% 
    mutate('81837_DSD'=`81837`/DataPackTarget,
           '87514_DSD'= `87514`/DataPackTarget,
           '87515_DSD'=`87515`/DataPackTarget) %>% 
    bind_rows(new_rows) %>% 
    select(PSNU,indicator_code,Age,Sex,KeyPop,ID,
           `81837_DSD`,`87514_DSD`,`87515_DSD`) %>% 
    mutate(type=case_when(
      is.na(Age) ~ "Total",
      TRUE ~ "disagg"
    )) %>% 
    group_by(PSNU,indicator_code,Sex) %>% 
    tidyr::fill(`81837_DSD`, .direction="down") %>% 
    tidyr::fill(`87514_DSD`, .direction="down") %>% 
    tidyr::fill(`87515_DSD`, .direction="down") %>%
    dplyr::ungroup()
