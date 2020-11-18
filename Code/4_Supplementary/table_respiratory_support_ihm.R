#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
### Creation of the in-hospital mortality tables stratified by use of respiratory support                ##
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
  filter(!is.na(REGIAO)) %>% 
  mutate(SUPORT_VEN = case_when(SUPORT_VEN == "No" ~ 0,
                                SUPORT_VEN == "Yes, non-invasive" ~ 1,
                                SUPORT_VEN == "Yes, invasive" ~ 2)) %>% 
  mutate(EVOLUCAO = case_when(EVOLUCAO == "Discharge" ~ 0,
                                EVOLUCAO == "Death" ~ 1)) %>% 
  filter(EVOLUCAO == 0 | EVOLUCAO == 1) %>% 
  mutate(SUPORT_VEN_AUX = case_when((SUPORT_VEN == 0 & EVOLUCAO == 0) ~ "No Ventilatory Support/Discharge",
                                    (SUPORT_VEN == 0 & EVOLUCAO == 1) ~ "No Ventilatory Support/Death",
                                    (SUPORT_VEN == 1 & EVOLUCAO == 0) ~ "Non-invasive Ventilatory Support/Discharge",
                                    (SUPORT_VEN == 1 & EVOLUCAO == 1) ~ "Non-invasive Ventilatory Support/Death",
                                    (SUPORT_VEN == 2 & EVOLUCAO == 0) ~ "Invasive Ventilatory Support/Discharge",
                                    (SUPORT_VEN == 2 & EVOLUCAO == 1) ~ "Invasive Ventilatory Support/Death"))

srag_ventilacao = df_srag_adults_pcr[complete.cases(df_srag_adults_pcr %>% select(FAIXA_IDADE, SUPORT_VEN_AUX)),]%>% select(HOSPITAL, FAIXA_IDADE, SUPORT_VEN_AUX)

lista_labels = list(HOSPITAL ~ "Total", FAIXA_IDADE ~ "Age groups")

table1 = srag_ventilacao %>% 
  tbl_summary(by = SUPORT_VEN_AUX,
              missing = "no",
              label = lista_labels,
              statistic = list(all_categorical() ~ "{n}")) %>% 
  add_overall()
table1

descritiva_ventilacao = table1$table_body %>% 
    select(-c(variable, row_type)) %>% 
    mutate(NoVS = ifelse(!is.na(stat_0), paste0(stat_3,"/",as.character((as.numeric(stat_3)+as.numeric(stat_4)))," (",round((as.numeric(stat_3)/(as.numeric(stat_3)+as.numeric(stat_4)))*100,0),"%)"), stat_0),
           NIVS = ifelse(!is.na(stat_0), paste0(stat_5,"/",as.character((as.numeric(stat_5)+as.numeric(stat_6)))," (",round((as.numeric(stat_5)/(as.numeric(stat_5)+as.numeric(stat_6)))*100,0),"%)"), stat_0),
           IVS = ifelse(!is.na(stat_0), paste0(stat_1,"/",as.character((as.numeric(stat_1)+as.numeric(stat_2)))," (",round((as.numeric(stat_1)/(as.numeric(stat_1)+as.numeric(stat_2)))*100,0),"%)"), stat_0)) %>% 
    select(label, stat_0, NoVS, NIVS, IVS) %>% 
    rename("Admission Characteristic" = "label",
           "Total" = "stat_0",
           "No Ventilatory Support" = "NoVS",
           "Non-invasive Ventilatory Support" = "NIVS",
           "Invasive Ventilatory Support" = "IVS")

# exporting files

write_xlsx(descritiva_ventilacao, "Outputs/Tables/ihm_respiratory_support_table.xlsx", format_headers = FALSE)

# finished

### Sensitivity Analysis
### SRAG Data - Hospitalizations
df_srag_adults_covid = vroom::vroom("Data/srag_adults_covid_12_10.csv")%>%
  filter(!is.na(REGIAO)) %>% 
  mutate(SUPORT_VEN = case_when(SUPORT_VEN == "No" ~ 0,
                                SUPORT_VEN == "Yes, non-invasive" ~ 1,
                                SUPORT_VEN == "Yes, invasive" ~ 2)) %>% 
  mutate(EVOLUCAO = case_when(EVOLUCAO == "Discharge" ~ 0,
                              EVOLUCAO == "Death" ~ 1)) %>% 
  filter(EVOLUCAO == 0 | EVOLUCAO == 1) %>% 
  mutate(SUPORT_VEN_AUX = case_when((SUPORT_VEN == 0 & EVOLUCAO == 0) ~ "No Ventilatory Support/Discharge",
                                    (SUPORT_VEN == 0 & EVOLUCAO == 1) ~ "No Ventilatory Support/Death",
                                    (SUPORT_VEN == 1 & EVOLUCAO == 0) ~ "Non-invasive Ventilatory Support/Discharge",
                                    (SUPORT_VEN == 1 & EVOLUCAO == 1) ~ "Non-invasive Ventilatory Support/Death",
                                    (SUPORT_VEN == 2 & EVOLUCAO == 0) ~ "Invasive Ventilatory Support/Discharge",
                                    (SUPORT_VEN == 2 & EVOLUCAO == 1) ~ "Invasive Ventilatory Support/Death"))

srag_ventilacao_sensibilidade = df_srag_adults_covid[complete.cases(df_srag_adults_covid %>% select(FAIXA_IDADE, SUPORT_VEN_AUX)),]%>% select(HOSPITAL, FAIXA_IDADE, SUPORT_VEN_AUX)

lista_labels = list(HOSPITAL ~ "Total", FAIXA_IDADE ~ "Age groups")

table2 = srag_ventilacao_sensibilidade %>% 
  tbl_summary(by = SUPORT_VEN_AUX,
              missing = "no",
              label = lista_labels,
              statistic = list(all_categorical() ~ "{n}")) %>% 
  add_overall()
table2

descritiva_ventilacao_sensibilidade = table2$table_body %>% 
  select(-c(variable, row_type)) %>% 
  mutate(NoVS = ifelse(!is.na(stat_0), paste0(stat_3,"/",as.character((as.numeric(stat_3)+as.numeric(stat_4)))," (",round((as.numeric(stat_3)/(as.numeric(stat_3)+as.numeric(stat_4)))*100,0),"%)"), stat_0),
         NIVS = ifelse(!is.na(stat_0), paste0(stat_5,"/",as.character((as.numeric(stat_5)+as.numeric(stat_6)))," (",round((as.numeric(stat_5)/(as.numeric(stat_5)+as.numeric(stat_6)))*100,0),"%)"), stat_0),
         IVS = ifelse(!is.na(stat_0), paste0(stat_1,"/",as.character((as.numeric(stat_1)+as.numeric(stat_2)))," (",round((as.numeric(stat_1)/(as.numeric(stat_1)+as.numeric(stat_2)))*100,0),"%)"), stat_0)) %>% 
  select(label, stat_0, NoVS, NIVS, IVS) %>% 
  rename("Admission Characteristic" = "label",
         "Total" = "stat_0",
         "No Ventilatory Support" = "NoVS",
         "Non-invasive Ventilatory Support" = "NIVS",
         "Invasive Ventilatory Support" = "IVS")

# exporting files

write_xlsx(descritiva_ventilacao_sensibilidade, "Outputs/Tables/Sensitivity Analysis/sensitivity_ihm_respiratory_support_table.xlsx", format_headers = FALSE)

# finished