#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, João Gabriel Gelli                                           ##
### October 2020                                                                                         ##
###                                                                                                      ##
### Table 1. Patient characteristics stratified by region                                                ##    
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
  mutate(HOSPITAL = "Yes") %>%
  mutate(NU_IDADE_N_2 = NU_IDADE_N) %>% 
  select(HOSPITAL, NU_IDADE_N, NU_IDADE_N_2,
         FAIXA_IDADE, CS_SEXO, CS_RACA, CS_ESCOL_N,
         REGIAO, n_comorb_mreal, SATURACAO, DISPNEIA, DESC_RESP, 
         SRAG_original, SRAG_sfebre, IS_CAPITAL) %>% 
  mutate(CS_SEXO = factor(CS_SEXO, levels = c("Male", "Female"))) %>%
  mutate(CS_RACA = factor(CS_RACA, levels = c("White", "Black/Mixed", "Asian", "Indigenous"))) %>% 
  mutate(CS_ESCOL_N = factor(CS_ESCOL_N, levels = c("Illiterate", "Up to high school", "High school", "College/University"))) %>%
  mutate(REGIAO = factor(REGIAO, levels = c("North", "Northeast", "Central-West", "Southeast", "South"))) %>%
  mutate(n_comorb_mreal = factor(n_comorb_mreal, levels = c(0, 1, 2), labels = c("No comorbidites", "1-2",">=3"))) %>% 
  filter(!is.na(REGIAO))


# Descriptive Table
lista_labels = list(HOSPITAL ~ "Total, No (%)",
                    CS_SEXO ~ "Male sex, No. (%)",
                    NU_IDADE_N ~ "Age, mean (sd)",
                    NU_IDADE_N_2 ~ "median (IQR)",
                    FAIXA_IDADE ~ "Age group, No. (%)",
                    CS_RACA ~ "Self-reported skin colour, No. (%)",
                    CS_ESCOL_N ~ "Level of education, No. (%)",
                    IS_CAPITAL ~ "Hospitalization in state capital, No. (%)",
                    SATURACAO ~ "Oxygen saturation \u2264 95%, No. (%)",
                    DISPNEIA ~ "Dyspnea, No. (%)",
                    DESC_RESP ~ "Respiratory distress, No. (%)",
                    SRAG_original ~ "SARI criteria, No. (%)",
                    SRAG_sfebre ~ "SARI without fever criteria, No. (%)",
                    n_comorb_mreal ~ "Number of comorbidities, No. (%)")

table1 = df_srag_adults_pcr %>% 
  tbl_summary(by = REGIAO, 
              label = lista_labels,
              missing = "no",
              statistic = list(all_categorical() ~ "{n} ({p}%)",
                               NU_IDADE_N ~ "{mean} ({sd})",
                               HOSPITAL ~ "{n}")) %>%
  add_overall() %>% 
  add_n()

total = as.numeric(table1$table_body[1,4])

descritiva = table1$table_body %>% 
  select(-c(variable)) %>% 
  mutate(label = ifelse(row_type == "label", paste0(label, " [n = ", n, " (", round((as.numeric(n)/total)*100, 0),"%)]"), label)) %>%  
  rename("Admission Characteristics" = "label",
           "N" = "n",
           "Brazil" = "stat_0",
           "South" = "stat_5",
           "Central-West" = "stat_3",
           "Southeast" = "stat_4",
           "North" = "stat_1",
           "Northeast" = "stat_2") %>%
  select(-c(row_type, N))

descritiva[1,1] = "Total, No"
descritiva[1,2:7] = descritiva[2,2:7]
descritiva = descritiva[-c(2),]
descritiva[11,2:7] = descritiva[12,2:7]
descritiva = descritiva[-c(12,13),]

# exporting files

write_xlsx(descritiva, "Outputs/Tables/descriptive_table.xlsx", format_headers = FALSE)

# finished


# Creates descriptive table for the sensitivity analysis
# Patients with laboratorial and clinical diagnosis of COVID-19

# Input data --------------------------------------------------------------

### SRAG Data - Hospitalizations
df_srag_adults_covid = vroom::vroom("Data/srag_adults_covid_12_10.csv") %>% 
  mutate(HOSPITAL = "Yes") %>%
  mutate(NU_IDADE_N_2 = NU_IDADE_N) %>% 
  select(HOSPITAL, NU_IDADE_N, NU_IDADE_N_2, 
         FAIXA_IDADE, CS_SEXO, CS_RACA, CS_ESCOL_N,
         REGIAO, n_comorb_mreal, SATURACAO, DISPNEIA, DESC_RESP, 
         SRAG_original, SRAG_sfebre, IS_CAPITAL) %>% 
  mutate(CS_SEXO = factor(CS_SEXO, levels = c("Male", "Female"))) %>%
  mutate(CS_RACA = factor(CS_RACA, levels = c("White", "Black/Mixed", "Asian", "Indigenous"))) %>% 
  mutate(CS_ESCOL_N = factor(CS_ESCOL_N, levels = c("Illiterate", "Up to high school", "High school", "College/University"))) %>%
  mutate(REGIAO = factor(REGIAO, levels = c("North", "Northeast", "Central-West", "Southeast", "South"))) %>%
  mutate(n_comorb_mreal = factor(n_comorb_mreal, levels = c(0, 1, 2), labels = c("No comorbidites", "1-2",">=3"))) %>% 
  filter(!is.na(REGIAO))


# Descriptive Table
table2 = df_srag_adults_covid %>% 
  tbl_summary(by = REGIAO, 
              label = lista_labels,
              missing = "no",
              statistic = list(all_categorical() ~ "{n} ({p}%)",
                               NU_IDADE_N ~ "{mean} ({sd})",
                               HOSPITAL ~ "{n}")) %>%
  add_overall() %>% 
  add_n()

total = as.numeric(table2$table_body[1,4])

descritiva_sensibilidade = table2$table_body %>% 
  select(-c(variable)) %>% 
  mutate(label = ifelse(row_type == "label", paste0(label, " [n = ", n, " (", round((as.numeric(n)/total)*100, 0),"%)]"), label)) %>%  
  rename("Admission Characteristics" = "label",
         "N" = "n",
         "Brazil" = "stat_0",
         "South" = "stat_5",
         "Central-West" = "stat_3",
         "Southeast" = "stat_4",
         "North" = "stat_1",
         "Northeast" = "stat_2") %>%
  select(-c(row_type, N))

descritiva_sensibilidade[1,1] = "Total, No"
descritiva_sensibilidade[1,2:7] = descritiva_sensibilidade[2,2:7]
descritiva_sensibilidade = descritiva_sensibilidade[-c(2),]
descritiva_sensibilidade[11,2:7] = descritiva_sensibilidade[12,2:7]
descritiva_sensibilidade = descritiva_sensibilidade[-c(12,13),]

# exporting files

write_xlsx(descritiva_sensibilidade, "Outputs/Tables/Sensitivity Analysis/sensitivity_analysis_descriptive_table.xlsx", format_headers = FALSE)

# finished

