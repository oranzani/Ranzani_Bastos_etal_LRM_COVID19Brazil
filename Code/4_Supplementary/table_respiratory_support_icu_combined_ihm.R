#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
### Creation of the in-hospital mortality tables stratified by ICU admission                             ##
### and respiratory support combined and its sensitivity analysis                                        ##
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
    mutate(EVOLUCAO = case_when(EVOLUCAO == "Discharge" ~ 0,
                                EVOLUCAO == "Death" ~ 1)) %>% 
    mutate(SUPORT_VEN = case_when(SUPORT_VEN == "No" ~ 0,
                                  SUPORT_VEN == "Yes, non-invasive" ~ 1,
                                  SUPORT_VEN == "Yes, invasive" ~ 2)) %>% 
    mutate(UTI = case_when(UTI == "No" ~ 0, UTI == "Yes" ~ 1)) %>% 
    filter(EVOLUCAO == 0 | EVOLUCAO == 1) %>% 
    mutate(VEN_UTI = case_when((SUPORT_VEN == 0 & UTI == 0 & EVOLUCAO == 0) ~ "No VS/No ICU/Discharge",
                               (SUPORT_VEN == 0 & UTI == 0 & EVOLUCAO == 1) ~ "No VS/No ICU/Death",
                               (SUPORT_VEN == 0 & UTI == 1 & EVOLUCAO == 0) ~ "No VS/ICU/Discharge",
                               (SUPORT_VEN == 0 & UTI == 1 & EVOLUCAO == 1) ~ "No VS/ICU/Death",
                               (SUPORT_VEN == 1 & UTI == 0 & EVOLUCAO == 0) ~ "NIVS/No ICU/Discharge",
                               (SUPORT_VEN == 1 & UTI == 0 & EVOLUCAO == 1) ~ "NIVS/No ICU/Death",
                               (SUPORT_VEN == 1 & UTI == 1 & EVOLUCAO == 0) ~ "NIVS/ICU/Discharge",
                               (SUPORT_VEN == 1 & UTI == 1 & EVOLUCAO == 1) ~ "NIVS/ICU/Death",
                               (SUPORT_VEN == 2 & UTI == 0 & EVOLUCAO == 0) ~ "IVS/No ICU/Discharge",
                               (SUPORT_VEN == 2 & UTI == 0 & EVOLUCAO == 1) ~ "IVS/No ICU/Death",
                               (SUPORT_VEN == 2 & UTI == 1 & EVOLUCAO == 0) ~ "IVS/ICU/Discharge",
                               (SUPORT_VEN == 2 & UTI == 1 & EVOLUCAO == 1) ~ "IVS/ICU/Death"))

srag_ventilacao_uti = df_srag_adults_pcr[complete.cases(df_srag_adults_pcr %>% select(FAIXA_IDADE, VEN_UTI)),] %>% select(HOSPITAL, FAIXA_IDADE, VEN_UTI)

lista_labels = list(HOSPITAL ~ "Total", FAIXA_IDADE ~ "Age groups")

table1 = srag_ventilacao_uti %>% 
    tbl_summary(by = VEN_UTI,
                missing = "no",
                label = lista_labels,
                statistic = list(all_categorical() ~ "{n}")) %>% 
    add_overall()
table1

descritiva_ventilacao_uti = table1$table_body %>% 
    select(-c(variable, row_type)) %>% 
    mutate(NoVS_NoICU = ifelse(!is.na(stat_0), paste0(stat_11,"/",as.character((as.numeric(stat_11)+as.numeric(stat_12)))," (",round((as.numeric(stat_11)/(as.numeric(stat_11)+as.numeric(stat_12)))*100,0),"%)"), stat_0),
           NoVS_ICU = ifelse(!is.na(stat_0), paste0(stat_9,"/",as.character((as.numeric(stat_9)+as.numeric(stat_10)))," (",round((as.numeric(stat_9)/(as.numeric(stat_9)+as.numeric(stat_10)))*100,0),"%)"), stat_0),
           NIVS_NoICU = ifelse(!is.na(stat_0), paste0(stat_7,"/",as.character((as.numeric(stat_7)+as.numeric(stat_8)))," (",round((as.numeric(stat_7)/(as.numeric(stat_7)+as.numeric(stat_8)))*100,0),"%)"), stat_0),
           NIVS_ICU = ifelse(!is.na(stat_0), paste0(stat_5,"/",as.character((as.numeric(stat_5)+as.numeric(stat_6)))," (",round((as.numeric(stat_5)/(as.numeric(stat_5)+as.numeric(stat_6)))*100,0),"%)"), stat_0),
           IVS_NoICU = ifelse(!is.na(stat_0), paste0(stat_3,"/",as.character((as.numeric(stat_3)+as.numeric(stat_4)))," (",round((as.numeric(stat_3)/(as.numeric(stat_3)+as.numeric(stat_4)))*100,0),"%)"), stat_0),
           IVS_ICU = ifelse(!is.na(stat_0), paste0(stat_1,"/",as.character((as.numeric(stat_1)+as.numeric(stat_2)))," (",round((as.numeric(stat_1)/(as.numeric(stat_1)+as.numeric(stat_2)))*100,0),"%)"), stat_0)) %>% 
    select(label, stat_0, NoVS_NoICU, NoVS_ICU, NIVS_NoICU, NIVS_ICU, IVS_NoICU, IVS_ICU) %>% 
    rename("Admission Characteristic" = "label",
           "Total" = "stat_0")

# exporting files

write_xlsx(descritiva_ventilacao_uti, "Outputs/Tables/ihm_respiratory_support_icu_combined_table.xlsx", format_headers = FALSE)

# finished

### Sensitivity Analysis
### SRAG Data - Hospitalizations
df_srag_adults_covid = vroom::vroom("Data/srag_adults_covid_12_10.csv") %>%
    filter(!is.na(REGIAO)) %>% 
    mutate(EVOLUCAO = case_when(EVOLUCAO == "Discharge" ~ 0,
                                EVOLUCAO == "Death" ~ 1)) %>% 
    mutate(SUPORT_VEN = case_when(SUPORT_VEN == "No" ~ 0,
                                  SUPORT_VEN == "Yes, non-invasive" ~ 1,
                                  SUPORT_VEN == "Yes, invasive" ~ 2)) %>% 
    mutate(UTI = case_when(UTI == "No" ~ 0, UTI == "Yes" ~ 1)) %>% 
    filter(EVOLUCAO == 0 | EVOLUCAO == 1) %>% 
    mutate(VEN_UTI = case_when((SUPORT_VEN == 0 & UTI == 0 & EVOLUCAO == 0) ~ "No VS/No ICU/Discharge",
                               (SUPORT_VEN == 0 & UTI == 0 & EVOLUCAO == 1) ~ "No VS/No ICU/Death",
                               (SUPORT_VEN == 0 & UTI == 1 & EVOLUCAO == 0) ~ "No VS/ICU/Discharge",
                               (SUPORT_VEN == 0 & UTI == 1 & EVOLUCAO == 1) ~ "No VS/ICU/Death",
                               (SUPORT_VEN == 1 & UTI == 0 & EVOLUCAO == 0) ~ "NIVS/No ICU/Discharge",
                               (SUPORT_VEN == 1 & UTI == 0 & EVOLUCAO == 1) ~ "NIVS/No ICU/Death",
                               (SUPORT_VEN == 1 & UTI == 1 & EVOLUCAO == 0) ~ "NIVS/ICU/Discharge",
                               (SUPORT_VEN == 1 & UTI == 1 & EVOLUCAO == 1) ~ "NIVS/ICU/Death",
                               (SUPORT_VEN == 2 & UTI == 0 & EVOLUCAO == 0) ~ "IVS/No ICU/Discharge",
                               (SUPORT_VEN == 2 & UTI == 0 & EVOLUCAO == 1) ~ "IVS/No ICU/Death",
                               (SUPORT_VEN == 2 & UTI == 1 & EVOLUCAO == 0) ~ "IVS/ICU/Discharge",
                               (SUPORT_VEN == 2 & UTI == 1 & EVOLUCAO == 1) ~ "IVS/ICU/Death"))

srag_ventilacao_uti_sensibilidade = df_srag_adults_covid[complete.cases(df_srag_adults_covid %>% select(FAIXA_IDADE, VEN_UTI)),] %>% select(HOSPITAL, FAIXA_IDADE, VEN_UTI)

lista_labels = list(HOSPITAL ~ "Total", FAIXA_IDADE ~ "Age groups")

table2 = srag_ventilacao_uti_sensibilidade %>% 
    tbl_summary(by = VEN_UTI,
                missing = "no",
                label = lista_labels,
                statistic = list(all_categorical() ~ "{n}")) %>% 
    add_overall()
table2

descritiva_ventilacao_uti_sensibilidade = table2$table_body %>% 
    select(-c(variable, row_type)) %>% 
    mutate(NoVS_NoICU = ifelse(!is.na(stat_0), paste0(stat_11,"/",as.character((as.numeric(stat_11)+as.numeric(stat_12)))," (",round((as.numeric(stat_11)/(as.numeric(stat_11)+as.numeric(stat_12)))*100,0),"%)"), stat_0),
           NoVS_ICU = ifelse(!is.na(stat_0), paste0(stat_9,"/",as.character((as.numeric(stat_9)+as.numeric(stat_10)))," (",round((as.numeric(stat_9)/(as.numeric(stat_9)+as.numeric(stat_10)))*100,0),"%)"), stat_0),
           NIVS_NoICU = ifelse(!is.na(stat_0), paste0(stat_7,"/",as.character((as.numeric(stat_7)+as.numeric(stat_8)))," (",round((as.numeric(stat_7)/(as.numeric(stat_7)+as.numeric(stat_8)))*100,0),"%)"), stat_0),
           NIVS_ICU = ifelse(!is.na(stat_0), paste0(stat_5,"/",as.character((as.numeric(stat_5)+as.numeric(stat_6)))," (",round((as.numeric(stat_5)/(as.numeric(stat_5)+as.numeric(stat_6)))*100,0),"%)"), stat_0),
           IVS_NoICU = ifelse(!is.na(stat_0), paste0(stat_3,"/",as.character((as.numeric(stat_3)+as.numeric(stat_4)))," (",round((as.numeric(stat_3)/(as.numeric(stat_3)+as.numeric(stat_4)))*100,0),"%)"), stat_0),
           IVS_ICU = ifelse(!is.na(stat_0), paste0(stat_1,"/",as.character((as.numeric(stat_1)+as.numeric(stat_2)))," (",round((as.numeric(stat_1)/(as.numeric(stat_1)+as.numeric(stat_2)))*100,0),"%)"), stat_0)) %>% 
    select(label, stat_0, NoVS_NoICU, NoVS_ICU, NIVS_NoICU, NIVS_ICU, IVS_NoICU, IVS_ICU) %>% 
    rename("Admission Characteristic" = "label",
           "Total" = "stat_0")

# exporting files

write_xlsx(descritiva_ventilacao_uti_sensibilidade, "Outputs/Tables/Sensitivity Analysis/sensitivity_ihm_respiratory_support_icu_combined_table.xlsx", format_headers = FALSE)

# finished