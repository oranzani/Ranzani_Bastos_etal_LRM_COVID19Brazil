#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
### Creation of the in-hospital mortality tables stratified by number of comorbidities                   ## 
### and its sensitivity analysis                                                                         ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidylog)
library(gtsummary)
library(writexl)

# Input data --------------------------------------------------------------

### SRAG Data - Hospitalizations
df_srag_adults_pcr = vroom::vroom("Data/srag_adults_pcr_12_10.csv") %>%
  mutate(EVOLUCAO = case_when(EVOLUCAO == "Discharge" ~ 0,
                              EVOLUCAO == "Death" ~ 1)) %>% 
  filter(EVOLUCAO == 0 | EVOLUCAO == 1) %>% 
  filter(!is.na(REGIAO)) %>% 
  mutate(COMORBIDADES_AUX = case_when((n_comorb_mreal == "0" & EVOLUCAO == 0) ~ "No Comorbidity/Discharge",
                                      (n_comorb_mreal == "0" & EVOLUCAO == 1) ~ "No Comorbidity/Death",
                                      (n_comorb_mreal == "1" & EVOLUCAO == 0) ~ "1-2 Comorbidity/Discharge",
                                      (n_comorb_mreal == "1" & EVOLUCAO == 1) ~ "1-2 Comorbidity/Death",
                                      (n_comorb_mreal == "2" & EVOLUCAO == 0) ~ "3+ Comorbidities/Discharge",
                                      (n_comorb_mreal == "2" & EVOLUCAO == 1) ~ "3+ Comorbidities/Death"))

srag_comorbidades = df_srag_adults_pcr[complete.cases(df_srag_adults_pcr %>% select(FAIXA_IDADE, COMORBIDADES_AUX)),]%>% select(HOSPITAL, FAIXA_IDADE, COMORBIDADES_AUX)

lista_labels = list(HOSPITAL ~ "Total", FAIXA_IDADE ~ "Age groups")

table1 = srag_comorbidades %>% 
  tbl_summary(by = COMORBIDADES_AUX,
              missing = "no",
              label = lista_labels,
              statistic = list(all_categorical() ~ "{n}")) %>% 
  add_overall()
table1

descritiva_comorbidades = table1$table_body %>% 
    select(-c(variable, row_type)) %>% 
    mutate(ThreeMoreC = ifelse(!is.na(stat_0), paste0(stat_3,"/",as.character((as.numeric(stat_3)+as.numeric(stat_4)))," (",round((as.numeric(stat_3)/(as.numeric(stat_3)+as.numeric(stat_4)))*100,0),"%)"), stat_0),
           NoC = ifelse(!is.na(stat_0), paste0(stat_5,"/",as.character((as.numeric(stat_5)+as.numeric(stat_6)))," (",round((as.numeric(stat_5)/(as.numeric(stat_5)+as.numeric(stat_6)))*100,0),"%)"), stat_0),
           OneTwoC = ifelse(!is.na(stat_0), paste0(stat_1,"/",as.character((as.numeric(stat_1)+as.numeric(stat_2)))," (",round((as.numeric(stat_1)/(as.numeric(stat_1)+as.numeric(stat_2)))*100,0),"%)"), stat_0)) %>% 
    select(label, stat_0, NoC, OneTwoC, ThreeMoreC) %>% 
    rename("Admission Characteristic" = "label",
           "Total" = "stat_0",
           "0 Comorbidity" = "NoC",
           "1-2 Comorbidity" = "OneTwoC",
           "3+ Comorbidities" = "ThreeMoreC")

# exporting files

write_xlsx(descritiva_comorbidades, "Outputs/Tables/ihm_comorbidity_table.xlsx", format_headers = FALSE)

# finished

### Sensitivity Analysis
### SRAG Data - Hospitalizations
df_srag_adults_covid = vroom::vroom("Data/srag_adults_covid_12_10.csv") %>%
  mutate(EVOLUCAO = case_when(EVOLUCAO == "Discharge" ~ 0,
                              EVOLUCAO == "Death" ~ 1)) %>% 
  filter(EVOLUCAO == 0 | EVOLUCAO == 1) %>% 
  filter(!is.na(REGIAO)) %>% 
  mutate(COMORBIDADES_AUX = case_when((n_comorb_mreal == "0" & EVOLUCAO == 0) ~ "No Comorbidity/Discharge",
                                      (n_comorb_mreal == "0" & EVOLUCAO == 1) ~ "No Comorbidity/Death",
                                      (n_comorb_mreal == "1" & EVOLUCAO == 0) ~ "1-2 Comorbidity/Discharge",
                                      (n_comorb_mreal == "1" & EVOLUCAO == 1) ~ "1-2 Comorbidity/Death",
                                      (n_comorb_mreal == "2" & EVOLUCAO == 0) ~ "3+ Comorbidities/Discharge",
                                      (n_comorb_mreal == "2" & EVOLUCAO == 1) ~ "3+ Comorbidities/Death"))

srag_comorbidades_sensibilidade = df_srag_adults_covid[complete.cases(df_srag_adults_covid %>% select(FAIXA_IDADE, COMORBIDADES_AUX)),]%>% select(HOSPITAL, FAIXA_IDADE, COMORBIDADES_AUX)

lista_labels = list(HOSPITAL ~ "Total", FAIXA_IDADE ~ "Age groups")

table2 = srag_comorbidades_sensibilidade %>% 
  tbl_summary(by = COMORBIDADES_AUX,
              missing = "no",
              label = lista_labels,
              statistic = list(all_categorical() ~ "{n}")) %>% 
  add_overall()
table2

descritiva_comorbidades_sensibilidade = table2$table_body %>% 
  select(-c(variable, row_type)) %>% 
  mutate(ThreeMoreC = ifelse(!is.na(stat_0), paste0(stat_3,"/",as.character((as.numeric(stat_3)+as.numeric(stat_4)))," (",round((as.numeric(stat_3)/(as.numeric(stat_3)+as.numeric(stat_4)))*100,0),"%)"), stat_0),
         NoC = ifelse(!is.na(stat_0), paste0(stat_5,"/",as.character((as.numeric(stat_5)+as.numeric(stat_6)))," (",round((as.numeric(stat_5)/(as.numeric(stat_5)+as.numeric(stat_6)))*100,0),"%)"), stat_0),
         OneTwoC = ifelse(!is.na(stat_0), paste0(stat_1,"/",as.character((as.numeric(stat_1)+as.numeric(stat_2)))," (",round((as.numeric(stat_1)/(as.numeric(stat_1)+as.numeric(stat_2)))*100,0),"%)"), stat_0)) %>% 
  select(label, stat_0, NoC, OneTwoC, ThreeMoreC) %>% 
  rename("Admission Characteristic" = "label",
         "Total" = "stat_0",
         "0 Comorbidity" = "NoC",
         "1-2 Comorbidity" = "OneTwoC",
         "3+ Comorbidities" = "ThreeMoreC")

# exporting files

write_xlsx(descritiva_comorbidades_sensibilidade, "Outputs/Tables/Sensitivity Analysis/sensitivity_ihm_comorbidity_table.xlsx", format_headers = FALSE)

# finished