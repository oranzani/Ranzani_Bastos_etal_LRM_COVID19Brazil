#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 200,000 hospitalizations for COVID-19 in Brazil:an analysis of      ## 
###             nationwide data                                                                          ##
### Coding Otavio Ranzani, Leonardo Bastos, João Gabriel Gelli                                           ##
### October 2020                                                                                         ##
###                                                                                                      ##
### Creation of the in-hospital mortality tables stratified by ICU admission                             ##
###     and its sensitivity analysis                                                                     ## 
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
    filter(!is.na(REGIAO)) %>% 
    mutate(UTI = case_when(UTI == "No" ~ 0, UTI == "Yes" ~ 1)) %>% 
    mutate(EVOLUCAO = case_when(EVOLUCAO == "Discharge" ~ 0,
                                EVOLUCAO == "Death" ~ 1)) %>% 
    filter(EVOLUCAO == 0 | EVOLUCAO == 1) %>% 
    mutate(UTI_AUX = case_when((UTI == 0 & EVOLUCAO == 0) ~ "No ICU/Discharge",
                               (UTI == 0 & EVOLUCAO == 1) ~ "No ICU/Death",
                               (UTI == 1 & EVOLUCAO == 0) ~ "ICU/Discharge",
                               (UTI == 1 & EVOLUCAO == 1) ~ "ICU/Death")) 

srag_uti = df_srag_adults_pcr[complete.cases(df_srag_adults_pcr %>% select(FAIXA_IDADE, UTI_AUX)),] %>% select(HOSPITAL, FAIXA_IDADE, UTI_AUX)

lista_labels = list(HOSPITAL ~ "Total", FAIXA_IDADE ~ "Age groups")

table1 = srag_uti %>% 
  tbl_summary(by = UTI_AUX,
              missing = "no",
              label = lista_labels,
              statistic = list(all_categorical() ~ "{n}")) %>% 
  add_overall()
table1

descritiva_uti = table1$table_body %>% 
    select(-c(variable, row_type)) %>% 
    mutate(NoICU = ifelse(!is.na(stat_0), paste0(stat_3, "/", as.character((as.numeric(stat_3) + as.numeric(stat_4)))," (",round((as.numeric(stat_3)/(as.numeric(stat_3) + as.numeric(stat_4)))*100,0),"%)"), stat_0),
           ICU = ifelse(!is.na(stat_0), paste0(stat_1, "/", as.character((as.numeric(stat_1) + as.numeric(stat_2)))," (",round((as.numeric(stat_1)/(as.numeric(stat_1) + as.numeric(stat_2)))*100,0),"%)"), stat_0)) %>% 
    select(label, stat_0, NoICU, ICU) %>% 
    rename("Admission Characteristic" = "label",
           "Total" = "stat_0",
           "No ICU" = "NoICU",
           "ICU" = "ICU")

# exporting files

write_xlsx(descritiva_uti, "Outputs/Tables/ihm_icu_table.xlsx", format_headers = FALSE)

# finished

### Sensitivity Analysis
### SRAG Data - Hospitalizations
df_srag_adults_covid = vroom::vroom("Data/srag_adults_covid_12_10.csv") %>%
  filter(!is.na(REGIAO)) %>% 
  mutate(UTI = case_when(UTI == "No" ~ 0, UTI == "Yes" ~ 1)) %>% 
  mutate(EVOLUCAO = case_when(EVOLUCAO == "Discharge" ~ 0,
                              EVOLUCAO == "Death" ~ 1)) %>% 
  filter(EVOLUCAO == 0 | EVOLUCAO == 1) %>% 
  mutate(UTI_AUX = case_when((UTI == 0 & EVOLUCAO == 0) ~ "No ICU/Discharge",
                             (UTI == 0 & EVOLUCAO == 1) ~ "No ICU/Death",
                             (UTI == 1 & EVOLUCAO == 0) ~ "ICU/Discharge",
                             (UTI == 1 & EVOLUCAO == 1) ~ "ICU/Death")) 

srag_uti_sensibilidade = df_srag_adults_covid[complete.cases(df_srag_adults_covid %>% select(FAIXA_IDADE, UTI_AUX)),] %>% select(HOSPITAL, FAIXA_IDADE, UTI_AUX)

lista_labels = list(HOSPITAL ~ "Total", FAIXA_IDADE ~ "Age groups")

table2 = srag_uti_sensibilidade %>% 
  tbl_summary(by = UTI_AUX,
              missing = "no",
              label = lista_labels,
              statistic = list(all_categorical() ~ "{n}")) %>% 
  add_overall()
table2

descritiva_uti_sensibilidade = table2$table_body %>% 
  select(-c(variable, row_type)) %>% 
  mutate(NoICU = ifelse(!is.na(stat_0), paste0(stat_3, "/", as.character((as.numeric(stat_3) + as.numeric(stat_4))), " (", round((as.numeric(stat_3)/(as.numeric(stat_3) + as.numeric(stat_4)))*100,0),"%)"), stat_0),
         ICU = ifelse(!is.na(stat_0), paste0(stat_1, "/", as.character((as.numeric(stat_1) + as.numeric(stat_2))),"  (", round((as.numeric(stat_1)/(as.numeric(stat_1) + as.numeric(stat_2)))*100,0),"%)"), stat_0)) %>% 
  select(label, stat_0, NoICU, ICU) %>% 
  rename("Admission Characteristic" = "label",
         "Total" = "stat_0",
         "No ICU" = "NoICU",
         "ICU" = "ICU")

# exporting files

write_xlsx(descritiva_uti_sensibilidade, "Outputs/Tables/Sensitivity Analysis/sensitivity_ihm_icu_table.xlsx", format_headers = FALSE)

# finished