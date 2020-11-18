#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
### Creation of the in-hospital mortality tables stratified by age and sex and its sensitivity analysis  ##
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
df_srag_adults_pcr <- vroom::vroom("Data/srag_adults_pcr_12_10.csv") %>%
  filter(!is.na(REGIAO)) %>% 
  mutate( EVOLUCAO = case_when(EVOLUCAO == "Discharge" ~ 0,
                               EVOLUCAO == "Death" ~ 1)) %>% 
  filter(EVOLUCAO == 0 | EVOLUCAO == 1)

### SRAG Data - Sensitivity Analysis
df_srag_adults_covid = vroom::vroom("Data/srag_adults_covid_12_10.csv") %>%
  filter(!is.na(REGIAO)) %>% 
  mutate( EVOLUCAO = case_when(EVOLUCAO == "Discharge" ~ 0,
                               EVOLUCAO == "Death" ~ 1)) %>% 
  filter(EVOLUCAO == 0 | EVOLUCAO == 1)

lista_labels = list(HOSPITAL ~ "Total", FAIXA_IDADE ~ "Age groups")

### Stratified by Age

### Main analysis
srag_idade <- df_srag_adults_pcr[complete.cases(df_srag_adults_pcr %>% select(FAIXA_IDADE, EVOLUCAO)),] %>% 
  select(HOSPITAL, FAIXA_IDADE, EVOLUCAO)

table1 = srag_idade %>% 
  tbl_summary(by = EVOLUCAO,
              missing = "no",
              label = lista_labels,
              statistic = list(all_categorical() ~ "{n}")) %>% 
  add_overall()
table1

descritiva_idade <- table1$table_body %>% 
  select(-c(variable, row_type)) %>% 
  mutate(stat_0 = gsub(pattern = ",", replacement =  "", x = stat_0),
         stat_1 = gsub(pattern = ",", replacement =  "", x = stat_1),
         stat_2 = gsub(pattern = ",", replacement =  "", x = stat_2))

descritiva_idade <- descritiva_idade %>% 
  mutate(Deaths = ifelse(!is.na(stat_0), paste0(stat_2,"/",as.character((as.numeric(stat_2)+as.numeric(stat_1)))," (",round((as.numeric(stat_2)/(as.numeric(stat_1)+as.numeric(stat_2)))*100,0),"%)"), stat_0)) %>% 
  select(label, stat_0, Deaths) %>% 
  rename("Admission Characteristic" = "label",
         "Total"  = "stat_0",
         "By Age" = "Deaths")

# exporting files

write_xlsx(descritiva_idade, "Outputs/Tables/ihm_age_table.xlsx", format_headers = FALSE)

# finished

### Sensitivity Analysis
srag_idade_sensibilidade <- df_srag_adults_covid[complete.cases(df_srag_adults_covid %>% select(FAIXA_IDADE, EVOLUCAO)),] %>% 
  select(HOSPITAL, FAIXA_IDADE, EVOLUCAO)

table2 = srag_idade_sensibilidade %>% 
  tbl_summary(by = EVOLUCAO,
              missing = "no",
              label = lista_labels,
              statistic = list(all_categorical() ~ "{n}")) %>% 
  add_overall()
table2

descritiva_idade_sensibilidade <- table2$table_body %>% 
  select(-c(variable, row_type)) %>% 
  mutate(stat_0 = gsub(pattern = ",", replacement =  "", x = stat_0),
         stat_1 = gsub(pattern = ",", replacement =  "", x = stat_1),
         stat_2 = gsub(pattern = ",", replacement =  "", x = stat_2))

descritiva_idade_sensibilidade <- descritiva_idade_sensibilidade %>% 
  mutate(Deaths = ifelse(!is.na(stat_0), paste0(stat_2,"/",as.character((as.numeric(stat_2)+as.numeric(stat_1)))," (",round((as.numeric(stat_2)/(as.numeric(stat_1)+as.numeric(stat_2)))*100,0),"%)"), stat_0)) %>% 
  select(label, stat_0, Deaths) %>% 
  rename("Admission Characteristic" = "label",
         "Total"  = "stat_0",
         "By Age" = "Deaths")

# exporting files

write_xlsx(descritiva_idade_sensibilidade, "Outputs/Tables/Sensitivity Analysis/sensitivity_ihm_age_table.xlsx", format_headers = FALSE)

# finished



### Stratified by Sex

### Main analysis
srag_sexo = df_srag_adults_pcr %>%
  filter(!is.na(REGIAO)) %>% 
  mutate(CS_SEXO = case_when(CS_SEXO == "Female" ~ 0, CS_SEXO == "Male" ~ 1)) %>% 
  mutate(SEXO_AUX = case_when((CS_SEXO == 0 & EVOLUCAO == 0) ~ "Female/Discharge",
                              (CS_SEXO  == 0 & EVOLUCAO == 1) ~ "Female/Death",
                              (CS_SEXO  == 1 & EVOLUCAO == 0) ~ "Male/Discharge",
                              (CS_SEXO  == 1 & EVOLUCAO == 1) ~ "Male/Death")) 

srag_sexo = srag_sexo[complete.cases(srag_sexo %>% select(FAIXA_IDADE, SEXO_AUX)),] %>% select(HOSPITAL, FAIXA_IDADE, SEXO_AUX)

table3 = srag_sexo %>% 
  tbl_summary(by = SEXO_AUX,
              missing = "no",
              label = lista_labels,
              statistic = list(all_categorical() ~ "{n}")) %>% 
  add_overall()
table3

descritiva_sexo <- table3$table_body %>% 
  select(-c(variable, row_type)) %>% 
  mutate(stat_0 = gsub(pattern = ",", replacement =  "", x = stat_0),
         stat_1 = gsub(pattern = ",", replacement =  "", x = stat_1),
         stat_2 = gsub(pattern = ",", replacement =  "", x = stat_2),
         stat_3 = gsub(pattern = ",", replacement =  "", x = stat_3),
         stat_4 = gsub(pattern = ",", replacement =  "", x = stat_4))

descritiva_sexo <- descritiva_sexo %>% 
  mutate(Male = ifelse(!is.na(stat_0), paste0(stat_3,"/",as.character((as.numeric(stat_3)+as.numeric(stat_4)))," (",round((as.numeric(stat_3)/(as.numeric(stat_3)+as.numeric(stat_4)))*100,0),"%)"), stat_0),
         Female = ifelse(!is.na(stat_0), paste0(stat_1,"/",as.character((as.numeric(stat_1)+as.numeric(stat_2)))," (",round((as.numeric(stat_1)/(as.numeric(stat_1)+as.numeric(stat_2)))*100,0),"%)"), stat_0)) %>% 
  select(label, stat_0, Female, Male) %>% 
  rename("Admission Characteristic" = "label",
         "Total"  = "stat_0")

# exporting files

write_xlsx(descritiva_sexo, "Outputs/Tables/ihm_sex_table.xlsx", format_headers = FALSE)

# finished

### Sensitivity Analysis
srag_sexo_sensibilidade = df_srag_adults_covid %>%
  filter(!is.na(REGIAO)) %>% 
  mutate(CS_SEXO = case_when(CS_SEXO == "Female" ~ 0, CS_SEXO == "Male" ~ 1)) %>% 
  mutate(SEXO_AUX = case_when((CS_SEXO == 0 & EVOLUCAO == 0) ~ "Female/Discharge",
                              (CS_SEXO  == 0 & EVOLUCAO == 1) ~ "Female/Death",
                              (CS_SEXO  == 1 & EVOLUCAO == 0) ~ "Male/Discharge",
                              (CS_SEXO  == 1 & EVOLUCAO == 1) ~ "Male/Death")) 

srag_sexo_sensibilidade = srag_sexo_sensibilidade[complete.cases(srag_sexo_sensibilidade %>% select(FAIXA_IDADE, SEXO_AUX)),] %>% select(HOSPITAL, FAIXA_IDADE, SEXO_AUX)

table4 = srag_sexo_sensibilidade %>% 
  tbl_summary(by = SEXO_AUX,
              missing = "no",
              label = lista_labels,
              statistic = list(all_categorical() ~ "{n}")) %>% 
  add_overall()
table4

descritiva_sexo_sensibilidade <- table4$table_body %>% 
  select(-c(variable, row_type)) %>% 
  mutate(stat_0 = gsub(pattern = ",", replacement =  "", x = stat_0),
         stat_1 = gsub(pattern = ",", replacement =  "", x = stat_1),
         stat_2 = gsub(pattern = ",", replacement =  "", x = stat_2),
         stat_3 = gsub(pattern = ",", replacement =  "", x = stat_3),
         stat_4 = gsub(pattern = ",", replacement =  "", x = stat_4))

descritiva_sexo_sensibilidade <- descritiva_sexo_sensibilidade %>% 
  mutate(Male = ifelse(!is.na(stat_0), paste0(stat_3,"/",as.character((as.numeric(stat_3)+as.numeric(stat_4)))," (",round((as.numeric(stat_3)/(as.numeric(stat_3)+as.numeric(stat_4)))*100,0),"%)"), stat_0),
         Female = ifelse(!is.na(stat_0), paste0(stat_1,"/",as.character((as.numeric(stat_1)+as.numeric(stat_2)))," (",round((as.numeric(stat_1)/(as.numeric(stat_1)+as.numeric(stat_2)))*100,0),"%)"), stat_0)) %>% 
  select(label, stat_0, Female, Male) %>% 
  rename("Admission Characteristic" = "label",
         "Total"  = "stat_0")

# exporting files

write_xlsx(descritiva_sexo_sensibilidade, "Outputs/Tables/Sensitivity Analysis/sensitivity_ihm_sex_table.xlsx", format_headers = FALSE)

# finished
