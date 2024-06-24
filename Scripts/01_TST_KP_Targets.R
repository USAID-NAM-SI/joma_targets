
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
library(openxlsx)



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
          #"TX_CURR.T","TX_PVLS.D.T","TX_PVLS.N.T")) add these later!!!!!!!
    unite(join,PSNU,indicator_code,Sex,remove = FALSE) #making a column to join on
  
  
  
  df_kp_2<-df_kp %>% 
    pivot_longer(cols=contains("_"),
                 names_to = "ind", #naming our new variable column
                 values_to = "value") %>% 
    separate(ind, c("indicator_code", "mech_code"), sep = "_(?=[0-9]+$)") %>% 
    pivot_wider(names_from = mech_code, 
                values_from = value) %>% 
    mutate(sex=case_when(
      KeyPop=="FSW" ~ "Female",
      KeyPop=="MSM" ~ "Male",
      TRUE ~ "Male")) %>% 
    mutate(indicator_code=str_remove(indicator_code,"KP.")) %>% 
    group_by(PSNU,indicator_code,sex) %>% 
    summarise(across(where(is.numeric), # what variable to sum on if we are looking at results? 
                     \(x) sum(x, na.rm = TRUE)), #\(x) is shorthand for function
              .groups = "drop") %>% 
    ungroup() %>% 
    unite(join,PSNU,indicator_code,sex) #making a column to join on;new column called join - combining psnu + indicator, sex
  
  
  #create rows that include age columns in order to create allocations for age 
  new_rows<- psnu_im %>% 
    filter(indicator_code=="TX_NEW.T", #add other indicators later!!!!!
           Age %in% c("15-24","25-34","35-49","50+")) #filter to 15+

  
  #create allocations %s
  allocations<-df_total_target %>% 
    left_join(df_kp_2,by="join") %>% #join KP targets to total targets
    mutate('81837_DSD'=`81837`/DataPackTarget, #create allocation % for each mech
           '87514_DSD'= `87514`/DataPackTarget,
           '87515_DSD'=`87515`/DataPackTarget) %>% 
    bind_rows(new_rows) %>% #bind the blank age rows to the dataset
    group_by(PSNU,indicator_code,Sex) %>% #group by psnu indicator and sex so that the KP % allocation can be filled for the new age rows
    tidyr::fill(c(`81837_DSD`,`87514_DSD`,`87515_DSD`), .direction="down") %>%  #fill in % KP allocation for this mech to all age rows
    dplyr::ungroup() %>% 
    filter(!is.na(ID)) %>% #filter out our total rows
    select(-join,-`81837`,-`87514`,-`87515`,
           -`81837_DSD...13`,-`87514_DSD...19`,-`87515_DSD...20`) %>% 
    relocate(`81837_DSD`, .after=`70247_DSD...12`) %>% 
    relocate(`87514_DSD`,`87515_DSD`, .after=`86927_DSD...18`)

  
  
  #bind the allocations to the rest of the psnu x im
  psnu_im_output <- psnu_im %>%
    filter(!(Age %in% c("15-24", "25-34", "35-49", "50+") & 
               indicator_code %in% c("TX_NEW.T"))) %>% #filter out rows that I am replacing; add other indicators!!!
    bind_rows(allocations) #add replacement rows
    
  # EXPORT -------------------------------------------------------------------
  
  filename_xlsx<-paste(Sys.Date(),"PSNUxIM_USAID_KP.xlsx",sep ="_")
  
  write.xlsx(psnu_im_output,file.path("Dataout",filename_xlsx), na="")
  

