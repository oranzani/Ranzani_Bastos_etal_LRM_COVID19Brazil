#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
### Analysis of missing data from ICU, Respiratory Support and comorbidities                             ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################

# Libraries ---------------------------------------------------------------
library(tidyverse)

# Input data --------------------------------------------------------------

### SRAG Data - Hospitalizations
srag_adults_pcr <- vroom::vroom("Data/srag_adults_pcr_12_10.csv") %>% 
    filter(EVOLUCAO != "Ongoing") %>% 
    # adding information from Brazilian cities
    mutate(
        ICU_missing = if_else(is.na(UTI), "Missing", "Not missing"),
        resp_support_missing = if_else(is.na(SUPORT_VEN), "Missing", "Not missing"),
        comorb_missing = if_else(is.na(n_comorb_mreal), "Missing", "Not missing")
        ) %>% 
    mutate_at(
        c("NU_IDADE_N", "FAIXA_IDADE", "CS_SEXO", "SRAG_original",
          "n_comorb_m", "CS_RACA", "CS_ESCOL_N", "SUPORT_VEN",
          "UTI", "REGIAO", "IS_CAPITAL", "EVOLUCAO"),
        function(x){ifelse(is.na(x), "Missing", x)}
    ) %>% 
    mutate(n_comorb_m = factor(n_comorb_m, levels = c("0", "1", "2", "Missing"), 
                             labels = c("No comorbidites", "1-2",">=3", "Missing"))) %>%
    mutate(SUPORT_VEN = factor(SUPORT_VEN,
                               levels = c("No", "Yes, non-invasive", "Yes, invasive", "Missing"))) %>% 
    mutate(UTI = factor(UTI,
                        levels = c("No", "Yes", "Missing"))) %>%
    mutate(CS_SEXO = factor(CS_SEXO,
                            levels = c("Female", "Male", "Missing"))) %>% 
    mutate(CS_RACA = factor(CS_RACA,
                            levels = c("Black/Mixed", "White", "Asian", "Indigenous", "Missing"))) %>% 
    mutate(CS_ESCOL_N = factor(CS_ESCOL_N,
                            levels = c("College/University", "High school", "Up to high school", "Illiterate", "Missing"))) %>%
    mutate(REGIAO = factor(REGIAO, 
                           levels = c("North", "Northeast", "Central-West", "Southeast", "South"))) %>% 
    mutate(SRAG_original = factor(SRAG_original, 
                           levels = c("No", "Yes", "Missing")))
    
    
    
    







# Tables with missing x no missing data comparison ------------------------

### Missing values in UTI (ICU admission) variable
# labels for table variables
lista_labels_icu <-  list(
    NU_IDADE_N ~ "Age, median (IQR)",
    FAIXA_IDADE ~ "Age group, No. (%)",
    CS_SEXO ~ "Gender, No. (%)",
    SRAG_original ~ "SARS hospitalizations, No. (%)",
    n_comorb_m ~ "Number of comorbidities, No. (%)",
    CS_RACA ~ "Self-reported skin colour, No. (%)",
    CS_ESCOL_N ~ "Level of education, No. (%)",
    SUPORT_VEN ~ "Respiratory support, No. (%)",
    # UTI ~ "ICU admission, No. (%)",
    REGIAO ~ "Region, No. (%)",
    IS_CAPITAL ~ "Hospitalization in capital city, No. (%)",
    EVOLUCAO ~ "Outcome, No. (%)"
)


# Table for ICU missing x no missing comparison in Brazil
df_covid_icu_missing_br <- 
    srag_adults_pcr %>% 
    select(ICU_missing, NU_IDADE_N, FAIXA_IDADE, CS_SEXO, SRAG_original, n_comorb_m, 
           CS_RACA, CS_ESCOL_N, SUPORT_VEN, REGIAO, IS_CAPITAL, EVOLUCAO) %>%
    gtsummary::tbl_summary(
        by = "ICU_missing",
        label = lista_labels_icu,
        missing_text = "(Missing)"
        # missing = "no"
        ) %>% 
    gtsummary::modify_spanning_header(c("stat_1", "stat_2") ~ "**Brazil**")


# List of tables for missing x no missing comparison in each region
ls_covid_icu_missing_region <- 
    srag_adults_pcr %>% 
    select(ICU_missing, NU_IDADE_N, FAIXA_IDADE, CS_SEXO, SRAG_original, n_comorb_m, 
           CS_RACA, CS_ESCOL_N, SUPORT_VEN, REGIAO, IS_CAPITAL, EVOLUCAO) %>%
    split(.$REGIAO) %>% 
    map(
        ~gtsummary::tbl_summary(., 
                                by = "ICU_missing",
                                label = lista_labels_icu,
                                missing_text = "(Missing)"
                                # missing = "no"
        ) 
    ) %>% 
    imap(
        ~gtsummary::modify_spanning_header(.x, c("stat_1", "stat_2") ~ glue::glue("**{.y}**"))
    )


# Creating a large table with data from Brazil + Regions
df_covid_icu_missing_all <-
    gtsummary::tbl_merge(
        tbls = list(
            df_covid_icu_missing_br,
            ls_covid_icu_missing_region[[1]],
            ls_covid_icu_missing_region[[2]],
            ls_covid_icu_missing_region[[3]],
            ls_covid_icu_missing_region[[4]],
            ls_covid_icu_missing_region[[5]]
            ),
    tab_spanner = c("**Brazil**", 
                    paste0("**", names(ls_covid_icu_missing_region), "**")
                    )
    )


# Exporting table
write_csv(
    df_covid_icu_missing_all$table_body %>% 
        select(label, starts_with("stat_")) %>% 
        set_names(c("variable", 
                    "brazil_missing",    "brazil_no_missing",
                    "north_missing",     "north_no_missing",
                    "northest_missing",  "northeast_no_missing",
                    "central_missing",   "central_no_missing",
                    "southeast_missing", "southeast_no_missing",
                    "south_missing",     "south_no_missing")
                  )
    , "Outputs/Tables/Missing_Comparison/ICU_missing_comparison.csv"
    )
    








### Missing values in SUPORT_VEN (Respiratory support) variable
# labels for table variables
lista_labels_resp <-  list(
    NU_IDADE_N ~ "Age, median (IQR)",
    FAIXA_IDADE ~ "Age group, No. (%)",
    CS_SEXO ~ "Gender, No. (%)",
    SRAG_original ~ "SARS hospitalizations, No. (%)",
    n_comorb_m ~ "Number of comorbidities, No. (%)",
    CS_RACA ~ "Self-reported skin colour, No. (%)",
    CS_ESCOL_N ~ "Level of education, No. (%)",
    # SUPORT_VEN ~ "Respiratory support, No. (%)",
    UTI ~ "ICU admission, No. (%)",
    REGIAO ~ "Region, No. (%)",
    IS_CAPITAL ~ "Hospitalization in capital city, No. (%)",
    EVOLUCAO ~ "Outcome, No. (%)"
)


# Table for Respiratory Support missing x no missing comparison in Brazil
df_covid_resp_supp_missing_br <- 
    srag_adults_pcr %>% 
    select(resp_support_missing, NU_IDADE_N, FAIXA_IDADE, CS_SEXO, SRAG_original, n_comorb_m, 
           CS_RACA, CS_ESCOL_N, UTI, REGIAO, IS_CAPITAL, EVOLUCAO) %>%
    gtsummary::tbl_summary(
        by = "resp_support_missing",
        label = lista_labels_resp,
        missing_text = "(Missing)"
        # missing = "no"
    ) %>% 
    gtsummary::modify_spanning_header(c("stat_1", "stat_2") ~ "**Brazil**")


# List of tables for missing x no missing comparison in each region
ls_covid_resp_supp_missing_region <- 
    srag_adults_pcr %>% 
    select(resp_support_missing, NU_IDADE_N, FAIXA_IDADE, CS_SEXO, SRAG_original, n_comorb_m, 
           CS_RACA, CS_ESCOL_N, UTI, REGIAO, IS_CAPITAL, EVOLUCAO) %>%
    split(.$REGIAO) %>% 
    map(
        ~gtsummary::tbl_summary(., 
                                by = "resp_support_missing",
                                label = lista_labels_resp,
                                missing_text = "(Missing)"
                                # missing = "no"
        ) 
    ) %>% 
    imap(
        ~gtsummary::modify_spanning_header(.x, c("stat_1", "stat_2") ~ glue::glue("**{.y}**"))
    )


# Creating a large table with data from Brazil + Regions
df_covid_resp_supp_missing_all <-
    gtsummary::tbl_merge(
        tbls = list(
            df_covid_resp_supp_missing_br,
            ls_covid_resp_supp_missing_region[[1]],
            ls_covid_resp_supp_missing_region[[2]],
            ls_covid_resp_supp_missing_region[[3]],
            ls_covid_resp_supp_missing_region[[4]],
            ls_covid_resp_supp_missing_region[[5]]
        ),
        tab_spanner = c("**Brazil**", 
                        paste0("**", names(ls_covid_resp_supp_missing_region), "**")
        )
    )



write_csv(
    df_covid_resp_supp_missing_all$table_body %>% 
        select(label, starts_with("stat_")) %>% 
        set_names(c("variable", 
                    "brazil_missing",    "brazil_no_missing",
                    "north_missing",     "north_no_missing",
                    "northest_missing",  "northeast_no_missing",
                    "central_missing",   "central_no_missing",
                    "southeast_missing", "southeast_no_missing",
                    "south_missing",     "south_no_missing")
        )
    , "Outputs/Tables/Missing_Comparison/Resp_Support_missing_comparison.csv"
    )











### Missing values in n_comorb (Number of Comorbidities) variable
# labels for table variables
lista_labels_comorb <-  list(
    NU_IDADE_N ~ "Age, median (IQR)",
    FAIXA_IDADE ~ "Age group, No. (%)",
    CS_SEXO ~ "Gender, No. (%)",
    SRAG_original ~ "SARS hospitalizations, No. (%)",
    CS_RACA ~ "Self-reported skin colour, No. (%)",
    CS_ESCOL_N ~ "Level of education, No. (%)",
    SUPORT_VEN ~ "Respiratory support, No. (%)",
    UTI ~ "ICU admission, No. (%)",
    REGIAO ~ "Region, No. (%)",
    IS_CAPITAL ~ "Hospitalization in capital city, No. (%)",
    EVOLUCAO ~ "Outcome, No. (%)"
)


# Table for Comorbidities missing x no missing comparison in Brazil
df_covid_comorb_missing <- 
    srag_adults_pcr %>% 
    select(comorb_missing, NU_IDADE_N, FAIXA_IDADE, CS_SEXO, SRAG_original,  
           CS_RACA, CS_ESCOL_N, SUPORT_VEN, UTI, REGIAO, IS_CAPITAL, EVOLUCAO) %>%
    gtsummary::tbl_summary(
        by = "comorb_missing",
        label = lista_labels_comorb,
        missing_text = "(Missing)"
        # missing = "no"
    ) %>% 
    gtsummary::modify_spanning_header(c("stat_1", "stat_2") ~ "**Brazil**")

 
# List of tables for missing x no missing comparison in each region
ls_covid_comorb_missing_region <- 
    srag_adults_pcr %>% 
    select(comorb_missing, NU_IDADE_N, FAIXA_IDADE, CS_SEXO, SRAG_original,  
           CS_RACA, CS_ESCOL_N, SUPORT_VEN, UTI, REGIAO, IS_CAPITAL, EVOLUCAO) %>%
    split(.$REGIAO) %>% 
    map(
        ~gtsummary::tbl_summary(., 
                                by = "comorb_missing",
                                label = lista_labels_comorb,
                                missing_text = "(Missing)"
                                # missing = "no"
        ) 
    ) %>% 
    imap(
        ~gtsummary::modify_spanning_header(.x, c("stat_1", "stat_2") ~ glue::glue("**{.y}**"))
    )


# Creating a large table with data from Brazil + Regions
df_covid_comorb_missing_all <-
    gtsummary::tbl_merge(
        tbls = list(
            df_covid_comorb_missing,
            ls_covid_comorb_missing_region[[1]],
            ls_covid_comorb_missing_region[[2]],
            ls_covid_comorb_missing_region[[3]],
            ls_covid_comorb_missing_region[[4]],
            ls_covid_comorb_missing_region[[5]]
        ),
        tab_spanner = c("**Brazil**", 
                        paste0("**", names(ls_covid_comorb_missing_region), "**")
        )
    )




write_csv(
    df_covid_comorb_missing_all$table_body %>% 
        select(label, starts_with("stat_")) %>% 
        set_names(c("variable", 
                    "brazil_missing",    "brazil_no_missing",
                    "north_missing",     "north_no_missing",
                    "northest_missing",  "northeast_no_missing",
                    "central_missing",   "central_no_missing",
                    "southeast_missing", "southeast_no_missing",
                    "south_missing",     "south_no_missing")
        )
    , "Outputs/Tables/Missing_Comparison/Comorbidities_missing_comparison.csv"
)



# Summary -----------------------------------------------------------------

# Summary of main tables for Missing x No Missing comparison - Brasil and Regions
# Tables are printed in markdown and showed in the "Viewer" tab at RStudio

# ICU admission information)
print(df_covid_icu_missing_all)

# Respiratory Support information
print(df_covid_resp_supp_missing_all)

# Comorbidities information
print(df_covid_comorb_missing_all)

# CSV files are exported in the 'Tables/Missing_Comparison/' folder
