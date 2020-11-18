#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
###                                                                                                      ##
### Deriving ICU mortality                                                                               ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################


library(tidyverse)
library(tidylog)

#### importing previous cleanned database
srag_adults_pcr_12_10 <- read_csv("Data/srag_adults_pcr_12_10.csv")

#### filtering those with defined outcome
srag_adults_pcr_12_10 <- srag_adults_pcr_12_10 %>% filter(EVOLUCAO != "Ongoing")

#### we did not have ICU outcome variable in the database
#### but we have date of ICU discharge/outcome and date of hospital outcome
#### for some patients


## creating a variable for when we have both ICU and hospital dates
## only patients with ICU Yes have these dates
srag_adults_pcr_12_10 <- srag_adults_pcr_12_10 %>% 
  mutate(icu_sameday = 
           ifelse(as.numeric(date_desf - date_said_uti) == 0, "Same", "No"))

#### counting numbers for tables

## Overall hospital mortality for those on the ICU
srag_adults_pcr_12_10 %>% 
  filter(UTI == "Yes") %>% 
  count(EVOLUCAO) %>% 
  pivot_wider(names_from = EVOLUCAO, values_from = n) %>% 
  mutate(total = Discharge + Death, prop = Death / total)

## patients we could or not get data and their hospital mortality
srag_adults_pcr_12_10 %>% 
  filter(UTI == "Yes") %>% 
  count(is.na(icu_sameday), EVOLUCAO) %>% 
  pivot_wider(names_from = EVOLUCAO, values_from = n) %>% 
  mutate(total = Discharge + Death, prop = Death / total)


#### Now on only for those patients we can get both dates and define ICU outcome
## proportion of deaths that occurred in the ICU among those who died
srag_adults_pcr_12_10 %>% 
  filter(UTI == "Yes" & EVOLUCAO == "Death" & !is.na(icu_sameday)) %>% 
  count(icu_sameday) %>% 
  pivot_wider(names_from = icu_sameday, values_from = n) %>% 
  mutate(total = Same + No, prop = Same / total)

## propotion of pts we could get data for Brazil and Regions
srag_adults_pcr_12_10 %>% 
  filter(UTI == "Yes") %>% 
  count(is.na(icu_sameday)) %>% 
  pivot_wider(names_from = `is.na(icu_sameday)`, values_from = n) %>% 
  mutate(total = `FALSE` + `TRUE`,
    prop = `FALSE` / total)

srag_adults_pcr_12_10 %>% 
  group_by(REGIAO) %>% 
  filter(UTI == "Yes") %>% 
  count(is.na(icu_sameday)) %>% 
  pivot_wider(names_from = `is.na(icu_sameday)`, values_from = n) %>% 
  mutate(total = `FALSE` + `TRUE`,
         prop = `FALSE` / total)

## ICU mortality for Brazil
srag_adults_pcr_12_10 %>% mutate(icu_out = case_when(
  UTI == "Yes" & icu_sameday == "Same" & EVOLUCAO == "Death" ~ "Yes",
  UTI == "Yes" & !is.na(icu_sameday)                         ~ "No")) %>% 
  filter(!is.na(icu_out)) %>% 
  count(icu_out)  %>% 
  pivot_wider(names_from = icu_out, values_from = n) %>% 
  mutate(total = No + Yes, prop = Yes / total) 

## ICU mortality for Regions
srag_adults_pcr_12_10 %>% mutate(icu_out = case_when(
  UTI == "Yes" & icu_sameday == "Same" & EVOLUCAO == "Death" ~ "Yes",
  UTI == "Yes" & !is.na(icu_sameday)                         ~ "No")) %>% 
  filter(!is.na(icu_out)) %>% 
  group_by(REGIAO) %>% 
  count(icu_out)  %>% 
  pivot_wider(names_from = icu_out, values_from = n) %>% 
  mutate(total = No + Yes, prop = Yes / total) 

## Hospital mortality for those observations we have both dates Brazil and Regions

srag_adults_pcr_12_10 %>% 
  filter(UTI == "Yes" & !is.na(icu_sameday)) %>% 
  count(EVOLUCAO)  %>% 
  pivot_wider(names_from = EVOLUCAO, values_from = n) %>% 
  mutate(total = Death + Discharge, prop = Death / total) 



srag_adults_pcr_12_10 %>% 
  filter(UTI == "Yes" & !is.na(icu_sameday)) %>% 
  group_by(REGIAO) %>% 
  count(EVOLUCAO)  %>% 
  pivot_wider(names_from = EVOLUCAO, values_from = n) %>% 
  mutate(total = Death + Discharge, prop = Death / total) 

# finished