#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
### Missing pattern table                                                                                ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################

### libraries
library(tidyverse)
library(tidylog)
library(gtsummary)

# input covid data
srag_covid_pcr <- vroom::vroom("Data/srag_adults_pcr_12_10.csv") %>% 
    filter(EVOLUCAO != "Ongoing")


# descriptive table of comorbidities and missing
list_labels <- list(
    HOSPITAL ~ "Hospitaliaztions, No.(%)",
    SEX_MISSING ~ "Missing sex info., No. (%)",
    CONT_COMORB_MISSING ~ "Missing comorbidities info., No. (%)",
    EVOLUCAO ~ "Hospital outcomes, No. (%)"
)


df_srag_covid_comorb <- 
    srag_covid_pcr %>% 
    mutate(
        SEX_MISSING = is.na(CS_SEXO),
        CONT_COMORB_MISSING = is.na(CARDIOPATI_m) + is.na(HEMATOLOGI_m) + is.na(HEPATICA_m) +
                    is.na(DIABETES_m) + is.na(NEUROLOGIC_m) + is.na(PNEUMOPATI_m) +
                    is.na(IMUNODEPRE_m) + is.na(RENAL_m) + is.na(OBESIDADE_m) 
    ) %>% 
    mutate(
        CONT_COMORB_MISSING = case_when(
            CONT_COMORB_MISSING == 0 ~ "Complete cases",
            CONT_COMORB_MISSING == 9 ~ "All missing",
            TRUE ~ as.character(CONT_COMORB_MISSING)
        ) 
    ) %>% 
    select(
        REGIAO,
        HOSPITAL,
        SEX_MISSING,
        FAIXA_IDADE,
        CONT_COMORB_MISSING,
        EVOLUCAO
        ) %>% 
    tbl_summary(
        by = "REGIAO",
        label = list_labels
    ) %>% 
    add_overall()
    

# descriptive table of resp support and icu (only hospitalizations with an outcome)
list_labels <- list(
    HOSPITAL ~ "Hospitaliaztions with an outcome, No.(%)",
    SUPORT_VEN ~ "Respiratory support, No. (%)",
    UTI ~ "ICU admissions, No. (%)"
    )




df_srag_covid_resources <- 
    srag_covid_pcr %>%
    filter(EVOLUCAO != "Ongoing") %>%
    mutate(
        SUPORT_VEN = ifelse(is.na(SUPORT_VEN), "Missing", SUPORT_VEN),
        UTI = ifelse(is.na(UTI), "Missing", UTI)
    ) %>% 
    select(
        REGIAO,
        HOSPITAL,
        SUPORT_VEN,
        UTI
    ) %>% 
    tbl_summary(
        by = "REGIAO",
        label = list_labels
    ) %>% 
    add_overall()


# exporting table

write_csv(
    bind_rows(
        df_srag_covid_comorb$table_body,
        df_srag_covid_resources$table_body
        ) %>% 
        mutate(
            brazil = stat_0,
            central_west = stat_1,
            north = stat_2,
            northeast = stat_3,
            south = stat_4,
            southeast = stat_5
        ) %>% 
    select(variable = label, brazil, north, northeast, central_west, southeast, south)
    , "Outputs/Tables/missing_pattern_comorb_resource.csv"
    )




# finished