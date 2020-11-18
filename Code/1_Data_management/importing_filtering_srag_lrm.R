#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
###                                                                                                      ##
### Importing data, filters, and data preparation                                                        ##    
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################


### libraries
library(tidyverse)
library(tidylog)

## importing csv file from Ministry of Health on day 14/10/2020, when released the 12/10/2020 database


file_SRAG <- "Data/INFLUD-12-10-2020.csv"
# 
name_file_output <- "srag_adults_pcr_12_10"
# 
srag <- vroom::vroom(file_SRAG)
#
end_date = as.Date("2020-08-15")
#

text = paste0("Filtered on: ", Sys.time(), "\n","File: ", file_SRAG, "\n")

# Patients after epidemiological week 8 (COVID period) and until 10/08/2020 (week 33) (timeframe used for the original submission, now updating)
srag <- srag %>% 
  filter(between(SEM_NOT, 8, 33))

text = paste0(text, "Total: ", nrow(srag), "\n")

# Hospitalized patients 
srag_hospitalized <- srag %>% 
  filter(HOSPITAL == 1) %>% 
  mutate(index = 1:n()) # unique index

text = paste0(text, "Hospitalisations: ", nrow(srag_hospitalized), "\n")

# Adjusting CLASSIFIN==5 (COVID) for those cases with PCR+ for SARS-COV2 but not CLASSIFIN==5,as by Ministry of Health official recommendation
# MoH has been correcting it over time, so less frequent than beggining of the pandemic
srag_hospitalized <- srag_hospitalized %>% 
  mutate(CLASSI_FIN = case_when(
  PCR_SARS2 == 1 ~ 5, # column Sars-CoV-2
  str_detect(DS_PCR_OUT, "SARS|COVID|COV|CORONA|CIVID") & !str_detect(DS_PCR_OUT,"63|43|229|HK|RINO|SINCI|PARE") == 1 ~ 5, #other PCRs, Sars-CoV2
  TRUE ~ CLASSI_FIN))


text <- paste0(text, "(-) Other respiratory viruses (influenza + others): ", nrow(srag_hospitalized %>%
                                                                   filter(CLASSI_FIN == 1 | CLASSI_FIN == 2)), "\n")
text <- paste0(text, "(-) Other causes (e.g, tuberculosis, bacterial pneumonia, etc): ", nrow(srag_hospitalized %>%
                                                                   filter(CLASSI_FIN == 3)), "\n")
text <- paste0(text, "(-) Without etiologic definition: ", nrow(srag_hospitalized %>%
                                                                   filter(CLASSI_FIN == 4)), "\n")
text <- paste0(text, "(-) Without classification: ", nrow(srag_hospitalized %>%
                                                               filter(is.na(CLASSI_FIN))), "\n")

srag_hospitalized %>% count(CLASSI_FIN)

# Filter pagients with COVID-19
srag_covid <- srag_hospitalized %>%
  filter(CLASSI_FIN == 5)
  

text <- paste0(text,"SARS-CoV-2 Classification: ", nrow(srag_covid), "\n")

#Filtra pacientes com PCR positivo para COVID
srag_pcr <- srag_covid %>% 
  filter((PCR_SARS2 == 1) |
         (str_detect(DS_PCR_OUT, "SARS|COVID|COV|CORONA|CIVID") & !str_detect(DS_PCR_OUT,"63|43|229|HK|RINO|SINCI|PARE")) |
         (PCR_RESUL == 1 & CRITERIO == 1 & is.na(DS_PCR_OUT) &  ## Adicional filter: among those cases already defined as COVID-19 by laboratory criterion,
          (PCR_RINO  != 1 | is.na(PCR_RINO)) &                  ## those with a positive PCR result and not being positive for other PCRs
          (POS_PCRFLU!= 1 | is.na(POS_PCRFLU)) & 
          (PCR_OUTRO != 1 | is.na(PCR_OUTRO)) & 
          (POS_PCROUT!= 1 | is.na(POS_PCROUT)) & 
          (                 is.na(PCR_VSR)) & 
          (                 is.na(PCR_METAP)) & 
          (                 is.na(PCR_PARA1))))

## Those COVID-19 defined with other laboratory tests
text <- paste0(text, "(-) SARS-CoV-2 Serological/Antigen tests: ", 
               nrow(srag_covid %>% filter(!(index %in% srag_pcr$index)) %>% 
               filter(CRITERIO == 1 | 
                     (is.na(CRITERIO) & 
                     (RES_AN == 1 | RES_IGG == 1 | RES_IGM == 1 | RES_IGA == 1 | AN_SARS2 == TRUE)))), "\n")

## other definitions (clinical-epi, clinical+tomography, etc)
text <- paste0(text, "(-) Clinico-epidemiological: ", 
               nrow(srag_covid) - nrow(srag_pcr) - 
                 nrow(srag_covid %>% filter(!(index %in% srag_pcr$index)) %>% 
                                                          filter(CRITERIO == 1 | 
                                                                   (is.na(CRITERIO) & 
                                                                      (RES_AN == 1 | RES_IGG == 1 | RES_IGM == 1 | RES_IGA == 1 | AN_SARS2 == TRUE)))),"\n")


text <- paste0(text, "SARS-CoV-2 PCR Positive: ", nrow(srag_pcr), "\n")


####### Main analysis on RT-qPCR positive

## Adult patients (>= 20 yo)
srag_adults_pcr <- srag_pcr %>%
    mutate(NU_IDADE_N = ifelse(NU_IDADE_N > 120, 36, NU_IDADE_N)) %>% #typo
    filter(NU_IDADE_N >= 20 & TP_IDADE == 3) 

text <- paste0(text, "(-) Age < 20 years: ", nrow(srag_pcr) - nrow(srag_adults_pcr), "\n")
text <- paste0(text, "Final Sample: ", nrow(srag_adults_pcr), "\n")


#### initial cleanning and recoding

srag_adults_pcr <- srag_adults_pcr %>% 
  mutate(date_int      = as.Date(DT_INTERNA, format = "%d/%m/%Y"),
         date_not      = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
         date_sint     = as.Date(DT_SIN_PRI, format = "%d/%m/%Y"),
         date_desf     = as.Date(DT_EVOLUCA, format = "%d/%m/%Y"),
         date_enc      = as.Date(DT_ENCERRA, format = "%d/%m/%Y"),
         date_uti      = as.Date(DT_ENTUTI,  format = "%d/%m/%Y"),
         date_said_uti = as.Date(DT_SAIDUTI, format = "%d/%m/%Y"),
         date_pcr      = as.Date(DT_PCR,     format = "%d/%m/%Y")) %>% 
  mutate(date_int = as.Date(case_when(
    DT_INTERNA == "31/10/7202" ~ "31/07/2020", # typo
    date_int > end_date & lubridate::year(date_int)  > lubridate::year(end_date)  ~ paste0(str_sub(DT_INTERNA,1, 6), "2020"),
    TRUE ~ as.character(DT_INTERNA)), format = "%d/%m/%Y")) %>% 
  mutate(date_not = as.Date(case_when(
    date_not > end_date & lubridate::year(date_not) > lubridate::year(end_date) ~ paste0(str_sub(DT_NOTIFIC,1, 6), "2020"),
    TRUE ~ as.character(DT_NOTIFIC)), format = "%d/%m/%Y")) %>% 
  mutate(date_sint = as.Date(case_when(
    date_sint > end_date & lubridate::year(date_sint) > lubridate::year(end_date) ~ paste0(str_sub(DT_SIN_PRI,1, 6), "2020"),
    TRUE ~ as.character(DT_SIN_PRI)), format = "%d/%m/%Y")) %>% 
  mutate(date_enc = as.Date(case_when(
    date_enc > end_date & lubridate::year(date_enc) > lubridate::year(end_date) ~ paste0(str_sub(DT_ENCERRA,1, 6), "2020"),
    TRUE ~ as.character(DT_ENCERRA)), format = "%d/%m/%Y")) %>% 
  mutate(date_uti = as.Date(case_when(
    date_uti > end_date & lubridate::year(date_uti) > lubridate::year(end_date) ~ paste0(str_sub(DT_ENTUTI,1, 6), "2020"),
    TRUE ~ as.character(DT_ENTUTI)), format = "%d/%m/%Y")) %>% 
  mutate(date_said_uti = as.Date(case_when(
    date_said_uti > end_date & lubridate::year(date_said_uti) > lubridate::year(end_date) ~ paste0(str_sub(DT_SAIDUTI,1, 6), "2020"),
    TRUE ~ as.character(DT_SAIDUTI)), format = "%d/%m/%Y")) %>% 
  mutate(date_pcr = as.Date(case_when(
    date_pcr > end_date & lubridate::year(date_pcr) > lubridate::year(end_date) ~ paste0(str_sub(DT_PCR,1, 6), "2020"),
    TRUE ~ as.character(DT_PCR)), format = "%d/%m/%Y"))

## Columns with same options, Yes, No, Ignored, NA

columns <- c("FEBRE", "TOSSE", "GARGANTA", "DISPNEIA", "DESC_RESP",
            "SATURACAO", "DIARREIA", "VOMITO", "OUTRO_SIN", "PUERPERA",
            "CARDIOPATI", "HEMATOLOGI", "SIND_DOWN", "HEPATICA", "ASMA",
            "DIABETES", "NEUROLOGIC", "PNEUMOPATI", "IMUNODEPRE",
            "RENAL", "OBESIDADE", "OUT_MORBI")

## other symptoms appeared latter on the database, such as abdomnal cramps, anosmia, etc.
## We did not use them, because they were missing for the majority of our analysis period

## Creating, recoding, age, regions, times
## Adapting for descriptive tables

srag_adults_pcr <- srag_adults_pcr %>%
  mutate(CS_SEXO = case_when(CS_SEXO == "M" ~ "Male",
                             CS_SEXO == "F" ~ "Female")) %>% 
  mutate(FAIXA_IDADE = case_when(NU_IDADE_N <= 39 ~ "20-39",
                                 NU_IDADE_N <= 49 ~ "40-49",
                                 NU_IDADE_N <= 59 ~ "50-59",
                                 NU_IDADE_N <= 69 ~ "60-69",
                                 NU_IDADE_N <= 79 ~ "70-79",
                                 TRUE ~ "80+")) %>%
  mutate(CS_RACA = case_when(CS_RACA == "1" ~ "White",
                             CS_RACA == "2" ~ "Black/Brown",
                             CS_RACA == "3" ~ "Asian",
                             CS_RACA == "4" ~ "Black/Brown",
                             CS_RACA == "5" ~ "Indigenous")) %>% 
  mutate(CS_ESCOL_N = case_when(CS_ESCOL_N == "0" ~ "Illiterate",   # Not applicable not present in this filtered adults dataset
                                CS_ESCOL_N == "1" ~ "Up to high school",
                                CS_ESCOL_N == "2" ~ "Up to high school",
                                CS_ESCOL_N == "3" ~ "High school",
                                CS_ESCOL_N == "4" ~ "College/University")) %>% 
  mutate(REGIAO = case_when(SG_UF_INTE %in% c("SP", "RJ", "ES", "MG") ~ "Southeast",
                            SG_UF_INTE %in% c("SC", "RS", "PR") ~ "South",
                            SG_UF_INTE %in% c("MT", "MS", "GO", "DF") ~ "Central-West",
                            SG_UF_INTE %in% c("AM", "AP", "TO", "PA", "RO", "RR", "AC") ~ "North",
                            SG_UF_INTE %in% c("BA", "AL", "SE", "PE", "MA", "RN", "PB", "CE", "PI") ~ "Northeast")) %>% 
  mutate(EVOLUCAO = case_when(EVOLUCAO == 1 ~ "Discharge",
                              EVOLUCAO == 2 ~ "Death",
                              EVOLUCAO == 3 ~ "Death",
                              TRUE ~ "Ongoing")) %>%
  mutate(SUPORT_VEN = case_when(SUPORT_VEN == 1 ~ "Yes, invasive",
                                SUPORT_VEN == 2 ~ "Yes, non-invasive",
                                SUPORT_VEN == 3 ~ "No")) %>% 
  mutate(UTI = case_when(UTI == 1 ~ "Yes",
                         UTI == 2 ~ "No")) %>%
  mutate_at(all_of(columns), function(x) {case_when(x == 1 ~ "Yes",
                                                     x == 2 ~ "No")}) %>% 
  mutate(
    SRAG_original = case_when(
      FEBRE == "Yes" &
        (TOSSE == "Yes" | GARGANTA == "Yes") &
        (DESC_RESP == "Yes" | DISPNEIA == "Yes" | SATURACAO == "Yes") ~ "Yes"),
    SRAG_original_total = case_when(
      !is.na(FEBRE) &
        (!is.na(TOSSE) | !is.na(GARGANTA)) &
        (!is.na(DESC_RESP) | !is.na(DISPNEIA) | !is.na(SATURACAO)) ~ "Not missing"),
    SRAG_sfebre = case_when(
      (TOSSE == "Yes" | GARGANTA == "Yes") &
        (DESC_RESP == "Yes" | DISPNEIA == "Yes" | SATURACAO == "Yes") ~ "Yes"),
    SRAG_sfebre_total = case_when(
      (!is.na(TOSSE) | !is.na(GARGANTA)) &
        (!is.na(DESC_RESP) | !is.na(DISPNEIA) | !is.na(SATURACAO)) ~ "Not missing")) %>% 
  mutate(
    SRAG_original = ifelse((is.na(SRAG_original) & SRAG_original_total == "Not missing"),  "No", SRAG_original),
    SRAG_sfebre   = ifelse((is.na(SRAG_sfebre)  & SRAG_sfebre_total == "Not missing"),  "No", SRAG_sfebre)) %>% 
  select(-c(SRAG_original_total, SRAG_sfebre_total))



## comorbidities / symptons

# keeping the original with missing values, generating new with real missing sufix _m
db_temp <- srag_adults_pcr %>% select(index, columns)
db_temp <- db_temp %>% rename_at(all_of(columns), function(x) paste0(x, "_m"))

srag_adults_pcr <-  srag_adults_pcr %>% left_join(db_temp, by = "index")

# now considering missing as No for commorbidities only

comorbidades <-  c("PUERPERA", "CARDIOPATI", "HEMATOLOGI",
                   "SIND_DOWN", "HEPATICA", "ASMA",
                   "DIABETES", "NEUROLOGIC", "PNEUMOPATI",
                   "IMUNODEPRE", "RENAL", "OBESIDADE", "OUT_MORBI")

srag_adults_pcr <-  srag_adults_pcr %>% 
  mutate(PUERPERA = case_when(CS_SEXO == "Male" ~ NA_character_, TRUE ~ PUERPERA), # few missing considered as entered for puerpera
         CS_SEXO  = case_when(PUERPERA == "Yes" ~ "Female", TRUE ~ CS_SEXO)) %>%  # 1 case puerpera with missing sex
  mutate_at(all_of(comorbidades), function(x){case_when(x == "Yes" ~ 1,
                                                        TRUE ~ 0)}) %>%
  mutate(
      CONT_COMORB = CARDIOPATI + HEMATOLOGI +  HEPATICA + DIABETES +
          NEUROLOGIC + PNEUMOPATI + IMUNODEPRE + RENAL +
          OBESIDADE,
      n_comorb = case_when(
          CONT_COMORB == 0 ~ 0,
          CONT_COMORB == 1 ~ 1,
          CONT_COMORB == 2 ~ 1,
          CONT_COMORB >  2 ~ 2)
      ) %>% 
    mutate(
        CONT_COMORB_m = case_when(
            is.na(CARDIOPATI_m) & is.na(HEMATOLOGI_m) & is.na(HEPATICA_m) &
                is.na(DIABETES_m) & is.na(NEUROLOGIC_m) & is.na(PNEUMOPATI_m) &
                is.na(IMUNODEPRE_m) & is.na(RENAL_m) & is.na(OBESIDADE_m) ~ NA_real_,
            TRUE ~ CONT_COMORB
        ),
        n_comorb_m = case_when(
            CONT_COMORB_m == 0 ~ 0,
            CONT_COMORB_m == 1 ~ 1,
            CONT_COMORB_m == 2 ~ 1,
            CONT_COMORB_m >  2 ~ 2),
        
    ) %>% 
  mutate(CONT_COMORB_mreal = case_when(!is.na(CARDIOPATI_m) & !is.na(HEMATOLOGI_m) & !is.na(HEPATICA_m) &
           !is.na(DIABETES_m) & !is.na(NEUROLOGIC_m) & !is.na(PNEUMOPATI_m) &
           !is.na(IMUNODEPRE_m) & !is.na(RENAL_m) & !is.na(OBESIDADE_m) ~ CONT_COMORB,
         TRUE ~ NA_real_
  ),
n_comorb_mreal = case_when(
  CONT_COMORB_mreal == 0 ~ 0,
  CONT_COMORB_mreal == 1 ~ 1,
  CONT_COMORB_mreal == 2 ~ 1,
  CONT_COMORB_mreal >  2 ~ 2))

# to fill in the flowchart and text
srag_adults_pcr %>% select(EVOLUCAO) %>% Hmisc::describe()

srag_adults_pcr %>% filter(EVOLUCAO %in% c("Death", "Discharge")) %>% count(is.na(date_enc)) ## >99% closed notifications



srag_adults_pcr <- tibble(cbind(ID = rownames(srag_adults_pcr), srag_adults_pcr))


# adding time variables and information of capital
srag_adults_pcr <- srag_adults_pcr %>% 
  mutate(
    TIME_SYMPTOMS_HOSP = as.numeric(date_int - date_sint),
    TIME_SYMPTOMS_NOTIF = as.numeric(date_not - date_sint)
    ) %>% 
  mutate(
    TIME_SYMPTOMS_HOSP = ifelse(TIME_SYMPTOMS_HOSP < 0, NA, TIME_SYMPTOMS_HOSP),
    TIME_SYMPTOMS_NOTIF = ifelse(TIME_SYMPTOMS_NOTIF < 0, NA, TIME_SYMPTOMS_NOTIF)
    ) %>% 
  # adding information about cities using data from github.com/kelvins
  left_join(
    read_csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/main/csv/municipios.csv") %>% 
      mutate(codigo_ibge_6dig = as.numeric(str_sub(codigo_ibge, 1, 6))) %>%
      select(capital, codigo_ibge_6dig), 
    by = c("CO_MU_INTE" = "codigo_ibge_6dig")
  ) %>% 
  mutate(IS_CAPITAL = ifelse(capital == 1, "Yes", "No"))
  
  
# exporting files
text <- paste0(text, "Saved in: ", name_file_output, "\n")

write_csv(srag_adults_pcr, paste0("Data/", name_file_output,".csv"))
write.table(text, paste0("Data/filters_steps_", name_file_output ,".txt"), row.names = F)


# checking potential duplicates -> unlikely influence if true duplicates
# birth date, notification date, hospital, municipality of residence, sex, and onset of symptoms
srag_adults_pcr %>% 
  distinct(DT_NASC, date_not, ID_UNIDADE, ID_MN_RESI, CS_SEXO, SEM_PRI) %>% 
  count() # 254020

# birth date, notification date, onset of symptoms date, hospital, municipality of residence, sex, and date of sample for diagnosis
srag_adults_pcr %>% 
  distinct(DT_NASC, date_not, date_sint, ID_MUNICIP, CO_MUN_RES, DT_COLETA) %>% 
  count() # 254089  



# finished

#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################

### Creating database for sensitivity analysis: all definitions for hospitalized COVID-19

# Filter patients with COVID-19

## Adult patients (>= 20 yo)
srag_adults_covid <- srag_covid %>%
    mutate(NU_IDADE_N = ifelse(NU_IDADE_N > 120, 36, NU_IDADE_N)) %>% #typo
    filter(NU_IDADE_N >= 20 & TP_IDADE == 3) 


#### initial cleanning and recoding

srag_adults_covid <- srag_adults_covid %>% 
  mutate(date_int      = as.Date(DT_INTERNA, format = "%d/%m/%Y"),
         date_not      = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
         date_sint     = as.Date(DT_SIN_PRI, format = "%d/%m/%Y"),
         date_desf     = as.Date(DT_EVOLUCA, format = "%d/%m/%Y"),
         date_enc      = as.Date(DT_ENCERRA, format = "%d/%m/%Y"),
         date_uti      = as.Date(DT_ENTUTI,  format = "%d/%m/%Y"),
         date_said_uti = as.Date(DT_SAIDUTI, format = "%d/%m/%Y"),
         date_pcr      = as.Date(DT_PCR,     format = "%d/%m/%Y")) %>% 
  mutate(date_int = as.Date(case_when(
    DT_INTERNA == "31/10/7202" ~ "31/07/2020", # typo
    date_int > end_date & lubridate::year(date_int)  > lubridate::year(end_date)  ~ paste0(str_sub(DT_INTERNA,1, 6), "2020"),
    TRUE ~ as.character(DT_INTERNA)), format = "%d/%m/%Y")) %>% 
  mutate(date_not = as.Date(case_when(
    date_not > end_date & lubridate::year(date_not) > lubridate::year(end_date) ~ paste0(str_sub(DT_NOTIFIC,1, 6), "2020"),
    TRUE ~ as.character(DT_NOTIFIC)), format = "%d/%m/%Y")) %>% 
  mutate(date_sint = as.Date(case_when(
    date_sint > end_date & lubridate::year(date_sint) > lubridate::year(end_date) ~ paste0(str_sub(DT_SIN_PRI,1, 6), "2020"),
    TRUE ~ as.character(DT_SIN_PRI)), format = "%d/%m/%Y")) %>% 
  mutate(date_enc = as.Date(case_when(
    date_enc > end_date & lubridate::year(date_enc) > lubridate::year(end_date) ~ paste0(str_sub(DT_ENCERRA,1, 6), "2020"),
    TRUE ~ as.character(DT_ENCERRA)), format = "%d/%m/%Y")) %>% 
  mutate(date_uti = as.Date(case_when(
    date_uti > end_date & lubridate::year(date_uti) > lubridate::year(end_date) ~ paste0(str_sub(DT_ENTUTI,1, 6), "2020"),
    TRUE ~ as.character(DT_ENTUTI)), format = "%d/%m/%Y")) %>% 
  mutate(date_said_uti = as.Date(case_when(
    date_said_uti > end_date & lubridate::year(date_said_uti) > lubridate::year(end_date) ~ paste0(str_sub(DT_SAIDUTI,1, 6), "2020"),
    TRUE ~ as.character(DT_SAIDUTI)), format = "%d/%m/%Y")) %>% 
  mutate(date_pcr = as.Date(case_when(
    date_pcr > end_date & lubridate::year(date_pcr) > lubridate::year(end_date) ~ paste0(str_sub(DT_PCR,1, 6), "2020"),
    TRUE ~ as.character(DT_PCR)), format = "%d/%m/%Y"))

## Columns with same options, Yes, No, Ignored, NA

## Creating, recoding, age, regions, times
## Adapting for descriptive tables

srag_adults_covid <- srag_adults_covid %>%
  mutate(CS_SEXO = case_when(CS_SEXO == "M" ~ "Male",
                             CS_SEXO == "F" ~ "Female")) %>% 
  mutate(FAIXA_IDADE = case_when(NU_IDADE_N <= 39 ~ "20-39",
                                 NU_IDADE_N <= 49 ~ "40-49",
                                 NU_IDADE_N <= 59 ~ "50-59",
                                 NU_IDADE_N <= 69 ~ "60-69",
                                 NU_IDADE_N <= 79 ~ "70-79",
                                 TRUE ~ "80+")) %>%
  mutate(CS_RACA = case_when(CS_RACA == "1" ~ "White",
                             CS_RACA == "2" ~ "Black/Brown",
                             CS_RACA == "3" ~ "Asian",
                             CS_RACA == "4" ~ "Black/Brown",
                             CS_RACA == "5" ~ "Indigenous")) %>% 
  mutate(CS_ESCOL_N = case_when(CS_ESCOL_N == "0" ~ "Illiterate",   # Not applicable not present in this filtered adults dataset
                                CS_ESCOL_N == "1" ~ "Up to high school",
                                CS_ESCOL_N == "2" ~ "Up to high school",
                                CS_ESCOL_N == "3" ~ "High school",
                                CS_ESCOL_N == "4" ~ "College/University")) %>% 
  mutate(REGIAO = case_when(SG_UF_INTE %in% c("SP", "RJ", "ES", "MG") ~ "Southeast",
                            SG_UF_INTE %in% c("SC", "RS", "PR") ~ "South",
                            SG_UF_INTE %in% c("MT", "MS", "GO", "DF") ~ "Central-West",
                            SG_UF_INTE %in% c("AM", "AP", "TO", "PA", "RO", "RR", "AC") ~ "North",
                            SG_UF_INTE %in% c("BA", "AL", "SE", "PE", "MA", "RN", "PB", "CE", "PI") ~ "Northeast")) %>% 
  mutate(EVOLUCAO = case_when(EVOLUCAO == 1 ~ "Discharge",
                              EVOLUCAO == 2 ~ "Death",
                              EVOLUCAO == 3 ~ "Death",
                              TRUE ~ "Ongoing")) %>%
  mutate(SUPORT_VEN = case_when(SUPORT_VEN == 1 ~ "Yes, invasive",
                                SUPORT_VEN == 2 ~ "Yes, non-invasive",
                                SUPORT_VEN == 3 ~ "No")) %>% 
  mutate(UTI = case_when(UTI == 1 ~ "Yes",
                         UTI == 2 ~ "No")) %>%
  mutate_at(all_of(columns), function(x) {case_when(x == 1 ~ "Yes",
                                                    x == 2 ~ "No")}) %>% 
  mutate(
    SRAG_original = case_when(
      FEBRE == "Yes" &
        (TOSSE == "Yes" | GARGANTA == "Yes") &
        (DESC_RESP == "Yes" | DISPNEIA == "Yes" | SATURACAO == "Yes") ~ "Yes"),
    SRAG_original_total = case_when(
      !is.na(FEBRE) &
        (!is.na(TOSSE) | !is.na(GARGANTA)) &
        (!is.na(DESC_RESP) | !is.na(DISPNEIA) | !is.na(SATURACAO)) ~ "Not missing"),
    SRAG_sfebre = case_when(
      (TOSSE == "Yes" | GARGANTA == "Yes") &
        (DESC_RESP == "Yes" | DISPNEIA == "Yes" | SATURACAO == "Yes") ~ "Yes"),
    SRAG_sfebre_total = case_when(
      (!is.na(TOSSE) | !is.na(GARGANTA)) &
        (!is.na(DESC_RESP) | !is.na(DISPNEIA) | !is.na(SATURACAO)) ~ "Not missing")) %>% 
  mutate(
    SRAG_original = ifelse((is.na(SRAG_original) & SRAG_original_total == "Not missing"),  "No", SRAG_original),
    SRAG_sfebre   = ifelse((is.na(SRAG_sfebre)  & SRAG_sfebre_total == "Not missing"),  "No", SRAG_sfebre)) %>% 
  select(-c(SRAG_original_total, SRAG_sfebre_total))



## comorbidities / symptons

# keeping the original with missing values, generating new with real missing sufix _m
db_temp2 <- srag_adults_covid %>% select(index, columns)
db_temp2 <- db_temp2 %>% rename_at(all_of(columns), function(x) paste0(x, "_m"))

srag_adults_covid <-  srag_adults_covid %>% left_join(db_temp2, by = "index")

# now considering missing as No for commorbidities only


srag_adults_covid <-  srag_adults_covid %>%
  mutate(PUERPERA = case_when(CS_SEXO == "Male" ~ NA_character_, TRUE ~ PUERPERA), # few missing considered as entered for puerpera
         CS_SEXO  = case_when(PUERPERA == "Yes" ~ "Female", TRUE ~ CS_SEXO)) %>%  # 1 case puerpera with missing sex
  mutate_at(all_of(comorbidades), function(x){case_when(x == "Yes" ~ 1,
                                                        TRUE ~ 0)}) %>%
  mutate(
    CONT_COMORB = CARDIOPATI + HEMATOLOGI +  HEPATICA + DIABETES +
      NEUROLOGIC + PNEUMOPATI + IMUNODEPRE + RENAL +
      OBESIDADE,
    n_comorb = case_when(
      CONT_COMORB == 0 ~ 0,
      CONT_COMORB == 1 ~ 1,
      CONT_COMORB == 2 ~ 1,
      CONT_COMORB >  2 ~ 2)
  ) %>% 
  mutate(
    CONT_COMORB_m = case_when(
      is.na(CARDIOPATI_m) & is.na(HEMATOLOGI_m) & is.na(HEPATICA_m) &
        is.na(DIABETES_m) & is.na(NEUROLOGIC_m) & is.na(PNEUMOPATI_m) &
        is.na(IMUNODEPRE_m) & is.na(RENAL_m) & is.na(OBESIDADE_m) ~ NA_real_,
      TRUE ~ CONT_COMORB
    ),
    n_comorb_m = case_when(
      CONT_COMORB_m == 0 ~ 0,
      CONT_COMORB_m == 1 ~ 1,
      CONT_COMORB_m == 2 ~ 1,
      CONT_COMORB_m >  2 ~ 2),
    
  ) %>% 
  mutate(CONT_COMORB_mreal = case_when(!is.na(CARDIOPATI_m) & !is.na(HEMATOLOGI_m) & !is.na(HEPATICA_m) &
                                         !is.na(DIABETES_m) & !is.na(NEUROLOGIC_m) & !is.na(PNEUMOPATI_m) &
                                         !is.na(IMUNODEPRE_m) & !is.na(RENAL_m) & !is.na(OBESIDADE_m) ~ CONT_COMORB,
                                       TRUE ~ NA_real_
  ),
  n_comorb_mreal = case_when(
    CONT_COMORB_mreal == 0 ~ 0,
    CONT_COMORB_mreal == 1 ~ 1,
    CONT_COMORB_mreal == 2 ~ 1,
    CONT_COMORB_mreal >  2 ~ 2))



# to fill in the flowchart and text
srag_adults_covid %>% select(EVOLUCAO) %>% Hmisc::describe()

srag_adults_covid %>% filter(EVOLUCAO %in% c("Death", "Discharge")) %>% count(is.na(date_enc)) ## 99% closed notifications

srag_adults_covid <- tibble(cbind(ID = rownames(srag_adults_covid), srag_adults_covid))

srag_adults_covid <- srag_adults_covid %>% 
  mutate(
    TIME_SYMPTOMS_HOSP = as.numeric(date_int - date_sint),
    TIME_SYMPTOMS_NOTIF = as.numeric(date_not - date_sint)
  ) %>% 
  mutate(
    TIME_SYMPTOMS_HOSP = ifelse(TIME_SYMPTOMS_HOSP < 0, NA, TIME_SYMPTOMS_HOSP),
    TIME_SYMPTOMS_NOTIF = ifelse(TIME_SYMPTOMS_NOTIF < 0, NA, TIME_SYMPTOMS_NOTIF)
  ) %>% 
  # adding information about cities using data from github.com/kelvins
  left_join(
    read_csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/main/csv/municipios.csv") %>% 
      mutate(codigo_ibge_6dig = as.numeric(str_sub(codigo_ibge, 1, 6))) %>%
      select(capital, codigo_ibge_6dig), 
    by = c("CO_MU_INTE" = "codigo_ibge_6dig")
  ) %>% 
  mutate(IS_CAPITAL = ifelse(capital == 1, "Yes", "No"))

  


# exporting files

write_csv(srag_adults_covid, "Data/srag_adults_covid_12_10.csv")

# finished
