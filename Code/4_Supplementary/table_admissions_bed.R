#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
### Creation of the in-hospital mortality tables stratified by number of comorbidities                   ## 
### and its sensitivity analysis                                                                         ##
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
df_srag_adults_pcr_count <- 
    vroom::vroom("Data/srag_adults_pcr_12_10.csv") %>% 
    filter(EVOLUCAO != "Ongoing") %>% 
    bind_rows(
        vroom::vroom("Data/srag_adults_pcr_12_10.csv") %>% 
            filter(EVOLUCAO != "Ongoing") %>% 
            mutate(REGIAO = "Brazil")
    ) %>% 
    group_by(REGIAO) %>% 
    summarise(
        total_adm = n(),
        total_adm_icu = sum(UTI == "Yes", na.rm = TRUE)
    ) %>% 
    ungroup()

v_color_regions <- c("#42B540FF","#FDAF91FF", "#AD002AFF", 
                     "#00468BFF", "#925E9FFF", "#ADB6B6FF")

### SRAG Hosp and ICU beds per region and capital
df_leitos_reg_adultos_capital <- 
    read_csv("Outputs/Tables/df_ICU_beds_CNES_capital.csv") %>% 
    group_by(REGIAO) %>%
    summarise(
        total_hosp_bed = sum(adult_beds),
        total_icu_bed = sum(adult_beds_ICU)
    ) %>% 
    ungroup()


df_hosp_beds <- 
    df_srag_adults_pcr_count %>% 
    left_join(
        df_leitos_reg_adultos_capital
    ) %>% 
    mutate(
        adm_bed_total_10k = (total_adm / total_hosp_bed) * 10000,
        adm_bed_icu_1k = (total_adm_icu / total_icu_bed) * 1000
    ) %>% 
    select(REGIAO, adm_bed_total_10k, adm_bed_icu_1k) %>%
    pivot_longer(-REGIAO, names_to = "var", values_to = "val") %>% 
    filter(REGIAO != "Brazil") %>% 
    mutate(REGIAO = factor(REGIAO, levels = c("North", "Northeast", 
                                              "Central-West", "Southeast",
                                              "South"))) %>% 
    mutate(
        var = factor(var, levels = c("adm_bed_total_10k", 
                                     "adm_bed_icu_1k"),
                     labels = c("Hospital admissions per 10,000 hospital beds",
                                "ICU admissions per 1,000 ICU beds")
                     )
        )


df_hosp_beds %>% 
    ggplot() +
    geom_col(aes(x = REGIAO, y = val, fill = REGIAO),
             position = "dodge") +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_fill_manual(name = "", values = v_color_regions, guide = FALSE) +
    labs(x = "", title = "", y = "") +
    facet_wrap(. ~ var, nrow = 1, ncol = 2, scales = "free_y") +
    theme_bw()
