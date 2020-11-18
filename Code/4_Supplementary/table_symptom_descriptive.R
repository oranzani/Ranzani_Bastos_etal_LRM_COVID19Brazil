#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
### Creation of the descriptive table about the symptoms                                                 ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(gtsummary)
library(writexl)

# Input data --------------------------------------------------------------

### SRAG Data - Hospitalizations
df_srag_adults_pcr = vroom::vroom("Data/srag_adults_pcr_12_10.csv") %>% 
  mutate(HOSPITAL = "Yes") %>% 
  select(HOSPITAL, REGIAO, TOSSE, FEBRE, DISPNEIA, SATURACAO,
         DESC_RESP, GARGANTA, DIARREIA, VOMITO, OUTRO_SIN) %>% 
  mutate(REGIAO = factor(REGIAO, levels = c("North", "Northeast", "Central-West", "Southeast", "South"))) %>%
  filter(!is.na(REGIAO))

### Creates descriptive table
lista_labels = list(HOSPITAL ~ "Total, No (%)",
                    TOSSE ~ "Cough, No. (%)",
                    FEBRE ~ "Fever, No. (%)",
                    DISPNEIA ~ "Dyspnea, No. (%)",
                    DESC_RESP ~ "Respiratory distress, No. (%)",
                    SATURACAO ~ "Oxygen saturation < 95%, No. (%)",
                    GARGANTA ~ "Sore throat, No. (%)",
                    DIARREIA ~ "Diarrhea, No. (%)",
                    VOMITO ~ "Vomit, No. (%)",
                    OUTRO_SIN ~ "Other symptoms, No. (%)")

table1 = df_srag_adults_pcr %>% 
  tbl_summary(by = REGIAO,
              label = lista_labels,
              missing = "no",
              statistic = list(all_categorical() ~ "{n} / {N} ({p}%)",
                               HOSPITAL ~ "{n}")) %>% 
  add_overall() %>% 
  add_n()
table1

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

# exporting files

write_xlsx(descritiva, "Outputs/Tables/symptom_descriptive_table.xlsx", format_headers = FALSE)

# finished