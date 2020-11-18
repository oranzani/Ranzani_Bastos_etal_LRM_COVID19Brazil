#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
###                                                                                                      ##
### Table 2. Intensive care admission, need of respiratory support, ICU and in-hospital mortality        ## 
###     among patients with a defined hospital outcome (n=232,036)                                       ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################



# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidylog)





# (main analysis)  --------------------------------------------------------
### SRAG Data - Hospitalizations
srag_adults_pcr <- vroom::vroom("Data/srag_adults_pcr_12_10.csv") 


srag_adults_pcr_outcomes <- 
    srag_adults_pcr %>% 
    filter(EVOLUCAO %in% c("Death", "Discharge")) 




# Calculating table of outcomes, resource use and times (main analysis) ---
df_outcomes <- 
    srag_adults_pcr_outcomes %>%
    mutate(
        TIME_SYMPTOMS_ICU = ifelse(UTI == "Yes", as.numeric(date_uti - date_sint), NA),
        LOS_ICU = ifelse(UTI == "Yes", as.numeric(date_said_uti - date_uti), NA),
        TIME_SYMPTOMS_DEATH = ifelse(EVOLUCAO == "Death", as.numeric(date_desf - date_sint), NA),
        TIME_HOSPITAL_DEATH = ifelse(EVOLUCAO == "Death", as.numeric(date_desf - date_int), NA),
        LOS_HOSP = as.numeric(date_desf - date_int)
        ) %>% 
    mutate(
        TIME_SYMPTOMS_HOSP = ifelse(TIME_SYMPTOMS_HOSP   <= 0 , NA, TIME_SYMPTOMS_HOSP),
        TIME_SYMPTOMS_ICU = ifelse(TIME_SYMPTOMS_ICU     <= 0 , NA, TIME_SYMPTOMS_ICU),
        TIME_SYMPTOMS_DEATH = ifelse(TIME_SYMPTOMS_DEATH <= 0 , NA, TIME_SYMPTOMS_DEATH),
        TIME_HOSPITAL_DEATH = ifelse(TIME_HOSPITAL_DEATH <= 0 , NA, TIME_HOSPITAL_DEATH)
        ) %>% 
    mutate(
        TIME_SYMPTOMS_HOSP = ifelse(TIME_SYMPTOMS_HOSP   > 30, 30, TIME_SYMPTOMS_HOSP),
        TIME_SYMPTOMS_ICU = ifelse(TIME_SYMPTOMS_ICU     > 30, 30, TIME_SYMPTOMS_ICU),
        TIME_SYMPTOMS_DEATH = ifelse(TIME_SYMPTOMS_DEATH > 30, 30, TIME_SYMPTOMS_DEATH),
        TIME_HOSPITAL_DEATH = ifelse(TIME_HOSPITAL_DEATH > 30, 30, TIME_HOSPITAL_DEATH),
    ) %>% 
    mutate(
        IMV_ICU = case_when(
            UTI == "Yes" & SUPORT_VEN == "Yes, invasive" ~ "MV in the ICU",
            UTI == "No" & SUPORT_VEN == "Yes, invasive" ~ "MV out of the ICU"
            ),
        NIV_ICU = case_when(
            UTI == "Yes" & SUPORT_VEN == "Yes, non-invasive" ~ "NIV in the ICU",
            UTI == "No" & SUPORT_VEN == "Yes, non-invasive" ~ "NIV out of the ICU"
        ),
        
        ) %>% 
    select(
        REGIAO, 
        UTI,  
        SUPORT_VEN,
        IMV_ICU,
        NIV_ICU,
        EVOLUCAO,
        LOS_HOSP,
        LOS_ICU,
        TIME_SYMPTOMS_HOSP, 
        TIME_SYMPTOMS_ICU,
        TIME_SYMPTOMS_DEATH,
        TIME_HOSPITAL_DEATH
        )


tb_outcomes <- 
    df_outcomes %>% 
    gtsummary::tbl_summary(
        by = "REGIAO",
        missing = "no",
        # missing_text = "Missing", 
        type = list(
            UTI ~ "categorical"),
        # statistic = list(
        #     all_categorical() ~ "{n} / {N} ({p}%)"
        #     )
        ) %>% 
    gtsummary::add_overall() %>% 
    gtsummary::add_n()




total_n <- tb_outcomes$table_body %>% 
    filter(variable == "EVOLUCAO") %>% 
    slice(1) %>%
    pull(n) %>% as.numeric()

total_icu <- tb_outcomes$table_body %>% 
    filter(variable == "UTI", label == "Yes") %>% 
    pull(stat_0) %>% 
    str_extract(".+?(?= )") %>% as.numeric()
    # str_extract(".+?(?= /)") %>% as.numeric()

total_niv <- tb_outcomes$table_body %>% 
    filter(variable == "SUPORT_VEN", label == "Yes, non-invasive") %>% 
    pull(stat_0) %>% 
    str_extract(".+?(?= )")  %>% as.numeric()

total_imv <- tb_outcomes$table_body %>% 
    filter(variable == "SUPORT_VEN", label == "Yes, invasive") %>% 
    pull(stat_0) %>% 
    str_extract(".+?(?= )")  %>% as.numeric()


total_death <-  tb_outcomes$table_body %>% 
    filter(variable == "EVOLUCAO", label == "Death") %>% 
    pull(stat_0) %>% 
    str_extract(".+?(?= )") %>% as.numeric()

    # str_extract(".+?(?= /)") %>% as.numeric()



tb_desc_outcomes <-  
    tb_outcomes$table_body %>% 
    mutate(
        variable = factor(
            variable, 
            levels = 
                c("TIME_SYMPTOMS_HOSP",
                  "UTI",
                  "TIME_SYMPTOMS_ICU",
                  "LOS_ICU",
                  "SUPORT_VEN",
                  "IMV_ICU",
                  "NIV_ICU",
                  "EVOLUCAO",
                  "TIME_SYMPTOMS_DEATH",
                  "TIME_HOSPITAL_DEATH",
                  "LOS_HOSP"
                  ),
            labels = 
                c("Symptoms to hospital admission",
                  "ICU admission",
                  "Symptoms to ICU admission",
                  "ICU LOS",
                  "Respiratory support",
                  "Place of IMV",
                  "Place of NIV",
                  "Hospital outcome/mortality",
                  "Symptoms to death",
                  "Hospital admission to death",
                  "Hospital LOS"
                  )
            )
        ) %>% 
    # select(-c(variable)) %>% 
    mutate(label2 = ifelse(row_type == "label" ,
                          paste0(variable, " [n = ", n, " (", round((as.numeric(n)/total_n)*100, 0),"%)]"), 
                          label)
           ) %>% 
    mutate(label2 = ifelse(row_type == "label" & variable %in% c("Symptoms to death", "Hospital admission to death", "ICU LOS", "Symptoms to ICU admission"), 
                           paste0(variable, " [n = ", n, " (", round((as.numeric(n)/total_icu)*100, 0),"%)]"), 
                           label2)
    ) %>% 
    mutate(label2 = ifelse(row_type == "label" & variable %in%  c("Place of IMV"), 
                           paste0(variable, " [n = ", n, " (", round((as.numeric(n)/total_imv)*100, 0),"%)]"), 
                           label2)
    ) %>% 
    mutate(label2 = ifelse(row_type == "label" & variable %in%  c("Place of NIV"), 
                           paste0(variable, " [n = ", n, " (", round((as.numeric(n)/total_niv)*100, 0),"%)]"), 
                           label2)
    ) %>% 
    mutate(label2 = ifelse(row_type == "label" & variable %in% c("Symptoms to death", "Hospital admission to death"), 
                           paste0(variable, " [n = ", n, " (", round((as.numeric(n)/total_death)*100, 0),"%)]"), 
                           label2)
    ) %>% 
    filter(row_type != "missing") %>% 
    rename(
        Brazil = stat_0,
        Central_West = stat_1,
        North = stat_2,
        Northeast = stat_3,
        South = stat_4,
        Southeast = stat_5
    ) %>% 
    select(-c(row_type, n, label, variable)) %>% 
    select(variable = label2, Brazil, North, Northeast, Central_West, Southeast, South) 



write_csv(tb_desc_outcomes,
          "Outputs/Tables/Outcomes Resources and Times/table2_outcomes_resource_times.csv")

# finished





# Sensitivity analysis ----------------------------------------------------

### SRAG Data - Hospitalizations (sensitivity)
srag_adults_covid <- vroom::vroom("Data/srag_adults_covid_12_10.csv") 


srag_adults_covid_outcomes <- 
    srag_adults_covid %>% 
    filter(EVOLUCAO %in% c("Death", "Discharge")) 




# Calculating table of outcomes, resource use and times (main analysis) ---
df_outcomes <- 
    srag_adults_covid_outcomes %>%
    mutate(
        TIME_SYMPTOMS_ICU = ifelse(UTI == "Yes", as.numeric(date_uti - date_sint), NA),
        LOS_ICU = ifelse(UTI == "Yes", as.numeric(date_said_uti - date_uti), NA),
        TIME_SYMPTOMS_DEATH = ifelse(EVOLUCAO == "Death", as.numeric(date_desf - date_sint), NA),
        TIME_HOSPITAL_DEATH = ifelse(EVOLUCAO == "Death", as.numeric(date_desf - date_int), NA),
        LOS_HOSP = as.numeric(date_desf - date_int)
    ) %>% 
    mutate(
        TIME_SYMPTOMS_HOSP = ifelse(TIME_SYMPTOMS_HOSP   <= 0 , NA, TIME_SYMPTOMS_HOSP),
        TIME_SYMPTOMS_ICU = ifelse(TIME_SYMPTOMS_ICU     <= 0 , NA, TIME_SYMPTOMS_ICU),
        TIME_SYMPTOMS_DEATH = ifelse(TIME_SYMPTOMS_DEATH <= 0 , NA, TIME_SYMPTOMS_DEATH),
        TIME_HOSPITAL_DEATH = ifelse(TIME_HOSPITAL_DEATH <= 0 , NA, TIME_HOSPITAL_DEATH)
    ) %>% 
    mutate(
        TIME_SYMPTOMS_HOSP = ifelse(TIME_SYMPTOMS_HOSP   > 30, 30, TIME_SYMPTOMS_HOSP),
        TIME_SYMPTOMS_ICU = ifelse(TIME_SYMPTOMS_ICU     > 30, 30, TIME_SYMPTOMS_ICU),
        TIME_SYMPTOMS_DEATH = ifelse(TIME_SYMPTOMS_DEATH > 30, 30, TIME_SYMPTOMS_DEATH),
        TIME_HOSPITAL_DEATH = ifelse(TIME_HOSPITAL_DEATH > 30, 30, TIME_HOSPITAL_DEATH),
    ) %>% 
    mutate(
        IMV_ICU = case_when(
            UTI == "Yes" & SUPORT_VEN == "Yes, invasive" ~ "MV in the ICU",
            UTI == "No" & SUPORT_VEN == "Yes, invasive" ~ "MV out of the ICU"
        ),
        NIV_ICU = case_when(
            UTI == "Yes" & SUPORT_VEN == "Yes, non-invasive" ~ "NIV in the ICU",
            UTI == "No" & SUPORT_VEN == "Yes, non-invasive" ~ "NIV out of the ICU"
        ),
        
    ) %>% 
    select(
        REGIAO, 
        UTI,  
        SUPORT_VEN,
        IMV_ICU,
        NIV_ICU,
        EVOLUCAO,
        LOS_HOSP,
        LOS_ICU,
        TIME_SYMPTOMS_HOSP, 
        TIME_SYMPTOMS_ICU,
        TIME_SYMPTOMS_DEATH,
        TIME_HOSPITAL_DEATH
    )


tb_outcomes <- 
    df_outcomes %>% 
    gtsummary::tbl_summary(
        by = "REGIAO",
        missing = "no",
        # missing_text = "Missing", 
        type = list(
            UTI ~ "categorical"),
        # statistic = list(
        #     all_categorical() ~ "{n} / {N} ({p}%)"
        #     )
    ) %>% 
    gtsummary::add_overall() %>% 
    gtsummary::add_n()




total_n <- tb_outcomes$table_body %>% 
    filter(variable == "EVOLUCAO") %>% 
    slice(1) %>%
    pull(n) %>% as.numeric()

total_icu <- tb_outcomes$table_body %>% 
    filter(variable == "UTI", label == "Yes") %>% 
    pull(stat_0) %>% 
    str_extract(".+?(?= )") %>% as.numeric()
# str_extract(".+?(?= /)") %>% as.numeric()

total_niv <- tb_outcomes$table_body %>% 
    filter(variable == "SUPORT_VEN", label == "Yes, non-invasive") %>% 
    pull(stat_0) %>% 
    str_extract(".+?(?= )")  %>% as.numeric()

total_imv <- tb_outcomes$table_body %>% 
    filter(variable == "SUPORT_VEN", label == "Yes, invasive") %>% 
    pull(stat_0) %>% 
    str_extract(".+?(?= )")  %>% as.numeric()


total_death <-  tb_outcomes$table_body %>% 
    filter(variable == "EVOLUCAO", label == "Death") %>% 
    pull(stat_0) %>% 
    str_extract(".+?(?= )") %>% as.numeric()

# str_extract(".+?(?= /)") %>% as.numeric()



tb_desc_outcomes <-  
    tb_outcomes$table_body %>% 
    mutate(
        variable = factor(
            variable, 
            levels = 
                c("TIME_SYMPTOMS_HOSP",
                  "UTI",
                  "TIME_SYMPTOMS_ICU",
                  "LOS_ICU",
                  "SUPORT_VEN",
                  "IMV_ICU",
                  "NIV_ICU",
                  "EVOLUCAO",
                  "TIME_SYMPTOMS_DEATH",
                  "TIME_HOSPITAL_DEATH",
                  "LOS_HOSP"
                ),
            labels = 
                c("Symptoms to hospital admission",
                  "ICU admission",
                  "Symptoms to ICU admission",
                  "ICU LOS",
                  "Respiratory support",
                  "Place of IMV",
                  "Place of NIV",
                  "Hospital outcome/mortality",
                  "Symptoms to death",
                  "Hospital admission to death",
                  "Hospital LOS"
                )
        )
    ) %>% 
    # select(-c(variable)) %>% 
    mutate(label2 = ifelse(row_type == "label" ,
                           paste0(variable, " [n = ", n, " (", round((as.numeric(n)/total_n)*100, 0),"%)]"), 
                           label)
    ) %>% 
    mutate(label2 = ifelse(row_type == "label" & variable %in% c("Symptoms to death", "Hospital admission to death", "ICU LOS", "Symptoms to ICU admission"), 
                           paste0(variable, " [n = ", n, " (", round((as.numeric(n)/total_icu)*100, 0),"%)]"), 
                           label2)
    ) %>% 
    mutate(label2 = ifelse(row_type == "label" & variable %in%  c("Place of IMV"), 
                           paste0(variable, " [n = ", n, " (", round((as.numeric(n)/total_imv)*100, 0),"%)]"), 
                           label2)
    ) %>% 
    mutate(label2 = ifelse(row_type == "label" & variable %in%  c("Place of NIV"), 
                           paste0(variable, " [n = ", n, " (", round((as.numeric(n)/total_niv)*100, 0),"%)]"), 
                           label2)
    ) %>% 
    mutate(label2 = ifelse(row_type == "label" & variable %in% c("Symptoms to death", "Hospital admission to death"), 
                           paste0(variable, " [n = ", n, " (", round((as.numeric(n)/total_death)*100, 0),"%)]"), 
                           label2)
    ) %>% 
    filter(row_type != "missing") %>% 
    rename(
        Brazil = stat_0,
        Central_West = stat_1,
        North = stat_2,
        Northeast = stat_3,
        South = stat_4,
        Southeast = stat_5
    ) %>% 
    select(-c(row_type, n, label, variable)) %>% 
    select(variable = label2, Brazil, North, Northeast, Central_West, Southeast, South) 



write_csv(tb_desc_outcomes,
          "Outputs/Tables/Sensitivity Analysis/sensitvity_table_outcomes_resource_times.csv")

# finished