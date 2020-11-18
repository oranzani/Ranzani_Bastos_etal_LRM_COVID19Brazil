#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
### Creation of the descriptive table about the comorbidities.                                           ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidylog)
library(gtsummary)
library(writexl)

# Input data --------------------------------------------------------------

### Lists variables that represent the comorbidities with missing values
comorbidades = c("CARDIOPATI_m", "DIABETES_m", "RENAL_m", "OBESIDADE_m", 
                 "NEUROLOGIC_m", "PNEUMOPATI_m", "IMUNODEPRE_m", "ASMA_m", 
                 "HEMATOLOGI_m",  "HEPATICA_m", "SIND_DOWN_m", "OUT_MORBI_m")

### SRAG Data - Hospitalizations
df_srag_adults_pcr = vroom::vroom("Data/srag_adults_pcr_12_10.csv") %>%
  select(comorbidades, REGIAO) %>% 
  mutate(REGIAO = factor(REGIAO, levels = c("North", "Northeast", "Central-West",
                                              "Southeast","South"))) %>% 
  mutate_at(all_of(comorbidades), function(x){ case_when(x == "Yes" ~ "Yes",
                                                         x == "No" ~ "No",
                                                         TRUE ~ "Missing")}) %>% 
  mutate_at(all_of(comorbidades), function(x){ factor(x, levels = c("No", "Yes",
                                                                    "Missing"))}) %>% 
  filter(!is.na(REGIAO))

lista_labels = list(CARDIOPATI_m ~ "Cardiovascular disease, No. (%)",
                    DIABETES_m ~ "Diabetes, No. (%)",
                    ASMA_m ~ "Asthma, No. (%)",
                    PNEUMOPATI_m ~ "COPD, No. (%)",
                    RENAL_m ~ "Kidney disease, No. (%)",
                    NEUROLOGIC_m ~ "Neurological disease, No. (%)",
                    OBESIDADE_m ~ "Obesity, No. (%)",
                    IMUNODEPRE_m ~ "Immunodepression, No. (%)",
                    HEMATOLOGI_m ~ "Hematological disease, No. (%)",
                    HEPATICA_m ~ "Hepatic disease, No. (%)",
                    SIND_DOWN_m ~ "Down syndrome, No. (%)",
                    OUT_MORBI_m ~ "Other comorbidities, No. (%)")

table1 = df_srag_adults_pcr %>% 
  tbl_summary(by = REGIAO,
              label = lista_labels,
              missing = "no",
              statistic = list(all_categorical() ~ "{n} / {N} ({p}%)")) %>% 
  add_overall()
table1

descritiva = table1$table_body %>% 
  select(-c(variable, row_type)) %>% 
  rename("Comorbidities" = "label",
         "Brazil" = "stat_0",
         "South" = "stat_5",
         "Central-West" = "stat_3",
         "Southeast" = "stat_4",
         "North" = "stat_1",
         "Northeast" = "stat_2")

### As only women can be puerperal, the dataframe has to be filtered only for women.

df_srag_puerperal = vroom::vroom("Data/srag_adults_pcr_12_10.csv") %>% 
  filter(CS_SEXO == "Female") %>% 
  select(PUERPERA_m, REGIAO) %>% 
  mutate(REGIAO = factor(REGIAO, levels = c("North", "Northeast", "Central-West",
                                            "Southeast","South"))) %>% 
  mutate(PUERPERA_m = case_when(PUERPERA_m == "Yes" ~ "Yes",
                                PUERPERA_m == "No" ~ "No",
                                TRUE ~ "Missing")) %>% 
  mutate(PUERPERA_m = factor(PUERPERA_m, levels = c("No", "Yes", "Missing"))) %>% 
  filter(!is.na(REGIAO))

table2 = df_srag_puerperal %>% 
  tbl_summary(by = REGIAO,
              label = list(PUERPERA_m ~ "Puerperal, No. (%)"),
              missing = "no",
              statistic = list(all_categorical() ~ "{n} / {N} ({p}%)")) %>% 
  add_overall()
table2

descritiva_puerperal = table2$table_body %>% 
  select(-c(variable, row_type)) %>% 
  rename("Comorbidities" = "label",
         "Brazil" = "stat_0",
         "South" = "stat_5",
         "Central-West" = "stat_3",
         "Southeast" = "stat_4",
         "North" = "stat_1",
         "Northeast" = "stat_2")

### Joins both descriptive tables
descritiva = rbind(descritiva[1:40,], descritiva_puerperal, descritiva[-(1:40),])

# exporting files

write_xlsx(descritiva, "Outputs/Tables/comorbidity_descriptive_table.xlsx", format_headers = FALSE)

# finished