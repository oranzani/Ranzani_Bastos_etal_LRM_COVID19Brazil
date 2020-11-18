#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
###                                                                                                      ##
### Figure 5. Health system burden and in-hospital mortality stratified by age in hospitalised COVID-19  ##
###     patients in the five regions of Brazil.                                                          ## 
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidylog)

# Input data --------------------------------------------------------------

### SRAG Data - hospitalisations
srag_adults_pcr <- vroom::vroom("Data/srag_adults_pcr_12_10.csv")


srag_adults_pcr_outcome <- 
    srag_adults_pcr %>% 
    filter(EVOLUCAO %in% c("Death", "Discharge"))



df_pop_ibge_2020 <- read_csv2("Data/pop_ibge_proj_2020.csv") %>%
    filter(IDADE >= 20) %>% 
    mutate(FAIXA_IDADE = 
               case_when(
                   IDADE <= 39 ~ "20-39",
                   IDADE <= 49 ~ "40-49",
                   IDADE <= 59 ~ "50-59",
                   IDADE <= 69 ~ "60-69",
                   IDADE <= 79 ~ "70-79",
                   TRUE ~ "80+")
           ) %>% 
    group_by(REGIAO, FAIXA_IDADE) %>% 
    summarise(POP_ADULTS = sum(POPULACAO)) %>% 
    ungroup()






# v_color_regions <- c("#00BF7D","#A3A500", "#F8766D", "#00B0F6", "#E76BF3", "gray50")
v_color_regions <- c("#42B540FF","#FDAF91FF", "#AD002AFF", 
                     "#00468BFF", "#925E9FFF", "#ADB6B6FF")
v_color_regions_alpha <- c("#42B5407F","#FDAF917F", "#AD002A7F", 
                           "#00468B7F", "#925E9F7F", "#ADB6B67F")











#################################### #################################### #################################
#### Main analysis - Rates per 100,000 population and In-hospital Mortality per age and region         ####
####    Overall, ICU and Invasive Mechanical Ventilation                                               ####
#### Hospitalizations with a defined outcome (discharge or death)                                      ####
#################################### #################################### #################################


# Calculates rates per 100,000 population ---------------------------------
### Calculates rates per 100,000 population - Regions
# Overall - Regions
df_hosp_100k_region <- 
    srag_adults_pcr_outcome %>%
    count(REGIAO, FAIXA_IDADE) %>% 
    rename(total = n) %>% 
    left_join(
        srag_adults_pcr_outcome %>% 
            filter(UTI == "Yes") %>%
            count(REGIAO, FAIXA_IDADE) %>%
            rename(icu = n)
    ) %>%
    left_join(
        srag_adults_pcr_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>%
            count(REGIAO, FAIXA_IDADE) %>%
            rename(mv = n)
    ) %>% 
    filter(!is.na(FAIXA_IDADE) & !is.na(REGIAO)) %>%
    left_join(
        df_pop_ibge_2020 %>%
            group_by(REGIAO, FAIXA_IDADE) %>% 
            summarise(POP_ADULTS = sum(POP_ADULTS))
    ) %>%
    group_by(REGIAO) %>% 
    summarise(
        total_100k = (sum(total) / sum(POP_ADULTS)) * 100000,
        icu_100k = (sum(icu) / sum(POP_ADULTS)) * 100000,
        mv_100k = (sum(mv) / sum(POP_ADULTS)) * 100000,
        total = paste0(sum(total), "/", sum(POP_ADULTS)),
        icu = paste0(sum(icu), "/", sum(POP_ADULTS)),
        mv = paste0(sum(mv), "/", sum(POP_ADULTS))
    ) %>% 
    ungroup() %>% 
    mutate(FAIXA_IDADE = "Overall") %>% 
    select(REGIAO, everything()) 

# By age group - Region
df_hosp_100k_age_region <- 
    srag_adults_pcr_outcome %>%
    count(REGIAO, FAIXA_IDADE) %>% 
    rename(total = n) %>% 
    left_join(
        srag_adults_pcr_outcome %>% 
            filter(UTI == "Yes") %>%
            count(REGIAO, FAIXA_IDADE) %>%
            rename(icu = n)
        ) %>%
    left_join(
        srag_adults_pcr_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>%
            count(REGIAO, FAIXA_IDADE) %>%
            rename(mv = n)
        ) %>% 
    filter(!is.na(FAIXA_IDADE) & !is.na(REGIAO)) %>%
    left_join(
        df_pop_ibge_2020
        ) %>% 
    mutate(
        total_100k = (total / POP_ADULTS) * 100000,
        icu_100k = (icu / POP_ADULTS) * 100000,
        mv_100k = (mv / POP_ADULTS) * 100000
        ) %>% 
    mutate(
        total = paste0(total, "/", POP_ADULTS),
        icu = paste0(icu, "/", POP_ADULTS),
        mv = paste0(mv, "/", POP_ADULTS)
    ) %>% 
    bind_rows(
        
    ) %>% 
    mutate(REGIAO = factor(REGIAO,
                           levels = c("North",
                                      "Northeast",
                                      "Central-West",
                                      "Southeast",
                                      "South"
                                      )
                           )
           ) %>% 
    arrange(REGIAO) %>% 
    select(-POP_ADULTS)




## Calculates rates per 100k population - Brazil
# Overall - Brazil
df_hosp_100k_brazil <- 
    srag_adults_pcr_outcome %>%
    count(FAIXA_IDADE) %>% 
    rename(total = n) %>% 
    left_join(
        srag_adults_pcr_outcome %>% 
            filter(UTI == "Yes") %>%
            count(FAIXA_IDADE) %>%
            rename(icu = n)
    ) %>%
    left_join(
        srag_adults_pcr_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>%
            count(FAIXA_IDADE) %>%
            rename(mv = n)
    ) %>% 
    mutate(REGIAO = "Brazil") %>% 
    filter(!is.na(FAIXA_IDADE) & !is.na(REGIAO)) %>%
    left_join(
        df_pop_ibge_2020 %>%
            group_by(FAIXA_IDADE) %>% 
            summarise(POP_ADULTS = sum(POP_ADULTS))
    ) %>%
    group_by(REGIAO) %>% 
    summarise(
        total_100k = (sum(total) / sum(POP_ADULTS)) * 100000,
        icu_100k = (sum(icu) / sum(POP_ADULTS)) * 100000,
        mv_100k = (sum(mv) / sum(POP_ADULTS)) * 100000,
        total = paste0(sum(total), "/", sum(POP_ADULTS)),
        icu = paste0(sum(icu), "/", sum(POP_ADULTS)),
        mv = paste0(sum(mv), "/", sum(POP_ADULTS))
    ) %>% 
    ungroup() %>% 
    mutate(FAIXA_IDADE = "Overall") %>% 
    select(REGIAO, everything()) 

# By Age Group - Brazil
df_hosp_100k_brazil_age <- 
    srag_adults_pcr_outcome %>%
    count(FAIXA_IDADE) %>% 
    rename(total = n) %>% 
    left_join(
        srag_adults_pcr_outcome %>% 
            filter(UTI == "Yes") %>%
            count(FAIXA_IDADE) %>%
            rename(icu = n)
    ) %>%
    left_join(
        srag_adults_pcr_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>%
            count(FAIXA_IDADE) %>%
            rename(mv = n)
    ) %>% 
    mutate(REGIAO = "Brazil") %>% 
    filter(!is.na(FAIXA_IDADE) & !is.na(REGIAO)) %>%
    left_join(
        df_pop_ibge_2020 %>%
            group_by(FAIXA_IDADE) %>% 
            summarise(POP_ADULTS = sum(POP_ADULTS))
    ) %>% 
    mutate(
        total_100k = (total / POP_ADULTS) * 100000,
        icu_100k = (icu / POP_ADULTS) * 100000,
        mv_100k = (mv / POP_ADULTS) * 100000
    ) %>% 
    mutate(
        total = paste0(total, "/", POP_ADULTS),
        icu = paste0(icu, "/", POP_ADULTS),
        mv = paste0(mv, "/", POP_ADULTS)
    ) %>% 
    select(REGIAO, everything()) %>% 
    select(-POP_ADULTS)





# Exporting values of rats per 100,000 population for Brazil and Regions
write_csv(
    bind_rows(
        df_hosp_100k_brazil,
        df_hosp_100k_brazil_age,
        df_hosp_100k_region,
        df_hosp_100k_age_region
        ) %>%
        mutate(REGIAO = factor(REGIAO,
                               levels = c("Brazil",
                                          "North",
                                          "Northeast",
                                          "Central-West",
                                          "Southeast",
                                          "South"
                                          )
                               )
               ) %>% 
        arrange(REGIAO) %>% 
        select(REGIAO, FAIXA_IDADE, everything())
    , "Outputs/Tables/Rates and In-hospital Mortality/Hosp_rate_100k_population.csv")
    



### Plots - Rates of hosp, ICU and mechanical ventilation per 100,000 pop - Regions
## Line 1 of Figure 3

## Plot Total 100k
plot_reg_idade_total_100k <- 
    df_hosp_100k_age_region %>% 
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = total_100k, fill = REGIAO),
             position = "dodge") +
    scale_y_continuous(labels = scales::comma_format(), 
                       breaks = seq(0, 900, 150),
                       limits = c(0, 950)) +
    scale_fill_manual(name = "", values = v_color_regions) +
    labs(x = "", title = "Hospitalisations", y = "per 100,000 population") +
    theme_bw()


## Plot ICU 100k
plot_reg_idade_icu_100k <- 
    df_hosp_100k_age_region %>% 
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = icu_100k, fill = REGIAO),
             position = "dodge") +
    scale_y_continuous(
        labels = scales::comma_format(), 
                       breaks = seq(0, 450, 75),
                       limits = c(0, 450)) +
    scale_fill_manual(name = "", values = v_color_regions) +
    labs(x = "", title = "ICU admissions", y = "") +
    theme_bw()


## Plot ICU 100k
plot_reg_idade_mv_100k <- 
    df_hosp_100k_age_region %>% 
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = mv_100k, fill = REGIAO),
             position = "dodge") +
    scale_y_continuous(labels = scales::comma_format(), 
                       breaks = seq(0, 300, 50),
                       limits = c(0, 300)) +
    scale_fill_manual(name = "", values = v_color_regions) +
    labs(x = "", title = "Invasive ventilation", y = "") +
    theme_bw()






# In-hospital mortality ---------------------------------------------------

## Calculates In-hospital mortality (deaths/hospitalisations) - Regions
# Overall - Regions
df_ihm_reg <-
    srag_adults_pcr_outcome %>%
    count(REGIAO, FAIXA_IDADE, EVOLUCAO) %>% 
    rename(deaths = n) %>% 
    group_by(REGIAO, FAIXA_IDADE) %>% 
    mutate(total = sum(deaths)) %>%
    ungroup() %>% 
    filter(EVOLUCAO == "Death") %>% 
    left_join(
        srag_adults_pcr_outcome %>%
            filter(UTI == "Yes") %>% 
            count(REGIAO, FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_icu = n) %>% 
            group_by(REGIAO, FAIXA_IDADE) %>% 
            mutate(total_icu = sum(deaths_icu)) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    left_join(
        srag_adults_pcr_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>% 
            count(REGIAO, FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_mv = n) %>% 
            group_by(REGIAO, FAIXA_IDADE) %>% 
            mutate(total_mv = sum(deaths_mv)) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    select(-EVOLUCAO) %>% 
    filter(!is.na(REGIAO), !is.na(FAIXA_IDADE)) %>% 
    group_by(REGIAO) %>% 
    summarise(
        ihm = sum(deaths) / sum(total),
        ihm_icu = sum(deaths_icu) / sum(total_icu),
        ihm_mv = sum(deaths_mv) / sum(total_mv),
        total = paste0(sum(deaths), "/", sum(total), " (", round(100 * ihm, 1), "%)"),
        icu = paste0(sum(deaths_icu), "/", sum(total_icu), " (", round(100 * ihm_icu, 1), "%)"),
        mv = paste0(sum(deaths_mv), "/", sum(total_mv), " (", round(100 * ihm_mv, 1), "%)")
    ) %>% 
    mutate(FAIXA_IDADE = "Overall") %>% 
    select(REGIAO, FAIXA_IDADE, total, icu, mv, ihm, ihm_icu, ihm_mv) %>% 
    arrange(REGIAO)


# By age group - Regions
df_ihm_reg_age <- 
    srag_adults_pcr_outcome %>%
    count(REGIAO, FAIXA_IDADE, EVOLUCAO) %>% 
    rename(deaths = n) %>% 
    group_by(REGIAO, FAIXA_IDADE) %>% 
    mutate(total = sum(deaths), 
           ihm = deaths / total) %>%
    ungroup() %>% 
    filter(EVOLUCAO == "Death") %>% 
    left_join(
        srag_adults_pcr_outcome %>%
            filter(UTI == "Yes") %>% 
            count(REGIAO, FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_icu = n) %>% 
            group_by(REGIAO, FAIXA_IDADE) %>% 
            mutate(total_icu = sum(deaths_icu),
                   ihm_icu = deaths_icu / total_icu) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    left_join(
        srag_adults_pcr_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>% 
            count(REGIAO, FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_mv = n) %>% 
            group_by(REGIAO, FAIXA_IDADE) %>% 
            mutate(total_mv = sum(deaths_mv), 
                   ihm_mv = deaths_mv / total_mv) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    select(-EVOLUCAO) %>% 
    filter(!is.na(REGIAO), !is.na(FAIXA_IDADE)) %>% 
    mutate(
        total = paste0(deaths, "/", total, " (", round(100 * (deaths / total), 1), "%)"),
        icu = paste0(deaths_icu, "/", total_icu, " (", round(100 * (deaths_icu / total_icu), 1), "%)"),
        mv = paste0(deaths_mv, "/", total_mv, " (", round(100 * (deaths_mv / total_mv), 1), "%)")
    ) %>% 
    mutate(REGIAO = factor(REGIAO,
                           levels = c("North",
                                      "Northeast",
                                      "Central-West",
                                      "Southeast",
                                      "South"
                           ),
    )
    ) %>% 
    select(REGIAO, FAIXA_IDADE, total, ihm, icu, ihm_icu, 
           mv, ihm_mv) %>% 
    arrange(REGIAO)




## Calculates In-hospital mortality (deaths/hospitalisations) - Brazil
# Overall - Brazil
df_ihm_brazil <- 
    srag_adults_pcr_outcome %>%
    count(FAIXA_IDADE, EVOLUCAO) %>% 
    rename(deaths = n) %>% 
    group_by(FAIXA_IDADE) %>% 
    mutate(total = sum(deaths)) %>%
    ungroup() %>% 
    filter(EVOLUCAO == "Death") %>% 
    left_join(
        srag_adults_pcr_outcome %>%
            filter(UTI == "Yes") %>% 
            count(FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_icu = n) %>% 
            group_by(FAIXA_IDADE) %>% 
            mutate(total_icu = sum(deaths_icu)) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    left_join(
        srag_adults_pcr_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>% 
            count(FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_mv = n) %>% 
            group_by(FAIXA_IDADE) %>% 
            mutate(total_mv = sum(deaths_mv)) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    mutate(REGIAO = "Brazil") %>%
    select(-EVOLUCAO) %>% 
    filter(!is.na(REGIAO), !is.na(FAIXA_IDADE)) %>% 
    group_by(REGIAO) %>% 
    summarise(
        ihm = sum(deaths) / sum(total),
        ihm_icu = sum(deaths_icu) / sum(total_icu),
        ihm_mv = sum(deaths_mv) / sum(total_mv),
        total = paste0(sum(deaths), "/", sum(total), " (", round(100 * ihm, 1), "%)"),
        icu = paste0(sum(deaths_icu), "/", sum(total_icu), " (", round(100 * ihm_icu, 1), "%)"),
        mv = paste0(sum(deaths_mv), "/", sum(total_mv), " (", round(100 * ihm_mv, 1), "%)")
    ) %>% 
    mutate(FAIXA_IDADE = "Overall") %>% 
    select(REGIAO, FAIXA_IDADE, total, icu, mv, ihm, ihm_icu, ihm_mv) %>% 
    arrange(REGIAO)


# By age group - Brazil
df_ihm_brazil_age <- 
    srag_adults_pcr_outcome %>%
    count(FAIXA_IDADE, EVOLUCAO) %>% 
    rename(deaths = n) %>% 
    group_by(FAIXA_IDADE) %>% 
    mutate(total = sum(deaths), 
           ihm = deaths / total) %>%
    ungroup() %>% 
    filter(EVOLUCAO == "Death") %>% 
    left_join(
        srag_adults_pcr_outcome %>%
            filter(UTI == "Yes") %>% 
            count(FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_icu = n) %>% 
            group_by(FAIXA_IDADE) %>% 
            mutate(total_icu = sum(deaths_icu),
                   ihm_icu = deaths_icu / total_icu) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    left_join(
        srag_adults_pcr_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>% 
            count(FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_mv = n) %>% 
            group_by(FAIXA_IDADE) %>% 
            mutate(total_mv = sum(deaths_mv), 
                   ihm_mv = deaths_mv / total_mv) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    mutate(REGIAO = "Brazil") %>% 
    select(-EVOLUCAO) %>% 
    filter(!is.na(REGIAO), !is.na(FAIXA_IDADE)) %>% 
    mutate(
        total = paste0(deaths, "/", total, " (", round(100 * (deaths / total), 1), "%)"),
        icu = paste0(deaths_icu, "/", total_icu, " (", round(100 * (deaths_icu / total_icu), 1), "%)"),
        mv = paste0(deaths_mv, "/", total_mv, " (", round(100 * (deaths_mv / total_mv), 1), "%)")
    ) %>% 
    select(REGIAO, FAIXA_IDADE, total, ihm, icu, ihm_icu, 
           mv, ihm_mv) %>% 
    arrange(REGIAO)


# Exporting values of in-hospital mortality for Brazil and Regions
write_csv(
    bind_rows(
        df_ihm_brazil,
        df_ihm_brazil_age,
        df_ihm_reg,
        df_ihm_reg_age,
        ) %>%
        mutate(REGIAO = factor(REGIAO,
                               levels = c("Brazil",
                                          "North",
                                          "Northeast",
                                          "Central-West",
                                          "Southeast",
                                          "South"
                                          )
                               )
               ) %>% 
        arrange(REGIAO) %>% 
        select(REGIAO, FAIXA_IDADE, everything())
    , "Outputs/Tables/Rates and In-hospital Mortality/In-hospital_mortality_regions.csv")





## Plot IHM Total
plot_ihm_df_ihm_reg_idade_total <- 
    df_ihm_reg_age %>% 
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = ihm, fill = REGIAO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(), 
                       limits = c(0, 1)) +
    scale_fill_manual(name = "", values = v_color_regions) +
    labs(x = "", title = "", y = "In-hospital mortality") +
    theme_bw()



## Plot IHM ICU
plot_ihm_df_ihm_reg_idade_icu <- 
    df_ihm_reg_age %>% 
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = ihm_icu, fill = REGIAO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(), 
                       limits = c(0, 1)) +
    scale_fill_manual(name = "", values = v_color_regions) +
    labs(x = "Age (years)", title = "", y = "") +
    theme_bw()



## Plot IHM Total
plot_ihm_df_ihm_reg_idade_vm <- 
    df_ihm_reg_age %>% 
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = ihm_mv, fill = REGIAO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(), 
                       limits = c(0, 1)) +
    scale_fill_manual(name = "", values = v_color_regions) +
    labs(x = "", title = "", y = "") +
    theme_bw()



## Combining plots 

# Creating Figure 3
combined_plot_region <-
    ggpubr::ggarrange(plot_reg_idade_total_100k,
                      plot_reg_idade_icu_100k,
                      plot_reg_idade_mv_100k,
                      plot_ihm_df_ihm_reg_idade_total,
                      plot_ihm_df_ihm_reg_idade_icu,
                      plot_ihm_df_ihm_reg_idade_vm,
                      label.x = "Age (years)",
                      nrow = 2,
                      ncol = 3,
                      align = "hv",
                      common.legend = TRUE,
                      legend = "bottom", font.label = list(size = 8))





ggsave("Outputs/Figures/Figure5_IHM_Rates_100k.pdf",
       combined_plot_region,
       units = "in", dpi = 800, height = 6, width = 13)

# finished









#################################### #################################### #################################
#### SENSITIVITY ANALYSIS - Rates per 100,000 population and In-hospital Mortality per age and region  ####
####    Overall, ICU and Invasive Mechanical Ventilation                                               ####
#### CLASSI_FIN == 5                                                                                   ####
#### Hospitalizations with a defined outcome (discharge or death)                                      ####
#################################### #################################### #################################





# data from covid patients
srag_adults_covid <- vroom::vroom("Data/srag_adults_covid_12_10.csv")


srag_adults_covid_outcome <- 
    srag_adults_covid %>% 
    filter(EVOLUCAO %in% c("Death", "Discharge"))



## Sensitivity - Rates per 100k population and IHM per 

## Total/100mil hab 
## Calculates rates per 100k population - Regions

# Overall - Regions
df_hosp_100k_region <- 
    srag_adults_covid_outcome %>%
    count(REGIAO, FAIXA_IDADE) %>% 
    rename(total = n) %>% 
    left_join(
        srag_adults_covid_outcome %>% 
            filter(UTI == "Yes") %>%
            count(REGIAO, FAIXA_IDADE) %>%
            rename(icu = n)
    ) %>%
    left_join(
        srag_adults_covid_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>%
            count(REGIAO, FAIXA_IDADE) %>%
            rename(mv = n)
    ) %>% 
    filter(!is.na(FAIXA_IDADE) & !is.na(REGIAO)) %>%
    left_join(
        df_pop_ibge_2020 %>%
            group_by(REGIAO, FAIXA_IDADE) %>% 
            summarise(POP_ADULTS = sum(POP_ADULTS))
    ) %>%
    group_by(REGIAO) %>% 
    summarise(
        total_100k = (sum(total) / sum(POP_ADULTS)) * 100000,
        icu_100k = (sum(icu) / sum(POP_ADULTS)) * 100000,
        mv_100k = (sum(mv) / sum(POP_ADULTS)) * 100000,
        total = paste0(sum(total), "/", sum(POP_ADULTS)),
        icu = paste0(sum(icu), "/", sum(POP_ADULTS)),
        mv = paste0(sum(mv), "/", sum(POP_ADULTS))
    ) %>% 
    ungroup() %>% 
    mutate(FAIXA_IDADE = "Overall") %>% 
    select(REGIAO, everything()) 

# By age group - Region
df_hosp_100k_age_region <- 
    srag_adults_covid_outcome %>%
    count(REGIAO, FAIXA_IDADE) %>% 
    rename(total = n) %>% 
    left_join(
        srag_adults_covid_outcome %>% 
            filter(UTI == "Yes") %>%
            count(REGIAO, FAIXA_IDADE) %>%
            rename(icu = n)
    ) %>%
    left_join(
        srag_adults_covid_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>%
            count(REGIAO, FAIXA_IDADE) %>%
            rename(mv = n)
    ) %>% 
    filter(!is.na(FAIXA_IDADE) & !is.na(REGIAO)) %>%
    left_join(
        df_pop_ibge_2020
    ) %>% 
    mutate(
        total_100k = (total / POP_ADULTS) * 100000,
        icu_100k = (icu / POP_ADULTS) * 100000,
        mv_100k = (mv / POP_ADULTS) * 100000
    ) %>% 
    mutate(
        total = paste0(total, "/", POP_ADULTS),
        icu = paste0(icu, "/", POP_ADULTS),
        mv = paste0(mv, "/", POP_ADULTS)
    ) %>% 
    bind_rows(
        
    ) %>% 
    mutate(REGIAO = factor(REGIAO,
                           levels = c("North",
                                      "Northeast",
                                      "Central-West",
                                      "Southeast",
                                      "South"
                           )
    )
    ) %>% 
    arrange(REGIAO) %>% 
    select(-POP_ADULTS)




## Calculates rates per 100k population - Brazil
# Overall - Brazil
df_hosp_100k_brazil <- 
    srag_adults_covid_outcome %>%
    count(FAIXA_IDADE) %>% 
    rename(total = n) %>% 
    left_join(
        srag_adults_covid_outcome %>% 
            filter(UTI == "Yes") %>%
            count(FAIXA_IDADE) %>%
            rename(icu = n)
    ) %>%
    left_join(
        srag_adults_covid_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>%
            count(FAIXA_IDADE) %>%
            rename(mv = n)
    ) %>% 
    mutate(REGIAO = "Brazil") %>% 
    filter(!is.na(FAIXA_IDADE) & !is.na(REGIAO)) %>%
    left_join(
        df_pop_ibge_2020 %>%
            group_by(FAIXA_IDADE) %>% 
            summarise(POP_ADULTS = sum(POP_ADULTS))
    ) %>%
    group_by(REGIAO) %>% 
    summarise(
        total_100k = (sum(total) / sum(POP_ADULTS)) * 100000,
        icu_100k = (sum(icu) / sum(POP_ADULTS)) * 100000,
        mv_100k = (sum(mv) / sum(POP_ADULTS)) * 100000,
        total = paste0(sum(total), "/", sum(POP_ADULTS)),
        icu = paste0(sum(icu), "/", sum(POP_ADULTS)),
        mv = paste0(sum(mv), "/", sum(POP_ADULTS))
    ) %>% 
    ungroup() %>% 
    mutate(FAIXA_IDADE = "Overall") %>% 
    select(REGIAO, everything()) 

# By Age Group - Brazil
df_hosp_100k_brazil_age <- 
    srag_adults_covid_outcome %>%
    count(FAIXA_IDADE) %>% 
    rename(total = n) %>% 
    left_join(
        srag_adults_covid_outcome %>% 
            filter(UTI == "Yes") %>%
            count(FAIXA_IDADE) %>%
            rename(icu = n)
    ) %>%
    left_join(
        srag_adults_covid_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>%
            count(FAIXA_IDADE) %>%
            rename(mv = n)
    ) %>% 
    mutate(REGIAO = "Brazil") %>% 
    filter(!is.na(FAIXA_IDADE) & !is.na(REGIAO)) %>%
    left_join(
        df_pop_ibge_2020 %>%
            group_by(FAIXA_IDADE) %>% 
            summarise(POP_ADULTS = sum(POP_ADULTS))
    ) %>% 
    mutate(
        total_100k = (total / POP_ADULTS) * 100000,
        icu_100k = (icu / POP_ADULTS) * 100000,
        mv_100k = (mv / POP_ADULTS) * 100000
    ) %>% 
    mutate(
        total = paste0(total, "/", POP_ADULTS),
        icu = paste0(icu, "/", POP_ADULTS),
        mv = paste0(mv, "/", POP_ADULTS)
    ) %>% 
    select(REGIAO, everything()) %>% 
    select(-POP_ADULTS)





# Exporting values of rats per 100,000 population for Brazil and Regions
write_csv(
    bind_rows(
        df_hosp_100k_brazil,
        df_hosp_100k_brazil_age,
        df_hosp_100k_region,
        df_hosp_100k_age_region
    ) %>%
        mutate(REGIAO = factor(REGIAO,
                               levels = c("Brazil",
                                          "North",
                                          "Northeast",
                                          "Central-West",
                                          "Southeast",
                                          "South"
                               )
        )
        ) %>% 
        arrange(REGIAO) %>% 
        select(REGIAO, FAIXA_IDADE, everything())
    , "Outputs/Tables/Sensitivity Analysis/sensitivity_hosp_rate_100k_population.csv")




### Plots - Rates of hosp, ICU and mechanical ventilation per 100,000 pop - Regions

## Plot Total 100k
plot_reg_idade_total_100k <- 
    df_hosp_100k_age_region %>% 
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = total_100k, fill = REGIAO),
             position = "dodge") +
    scale_y_continuous(labels = scales::comma_format(), 
                       breaks = seq(0, 1800, 300),
                       limits = c(0, 1800)) +
    scale_fill_manual(name = "", values = v_color_regions) +
    labs(x = "", title = "Hospitalisations", y = "per 100,000 population") +
    theme_bw()


## Plot ICU 100k
plot_reg_idade_icu_100k <- 
    df_hosp_100k_age_region %>% 
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = icu_100k, fill = REGIAO),
             position = "dodge") +
    scale_y_continuous(
        labels = scales::comma_format(), 
        breaks = seq(0, 480, 80),
        limits = c(0, 480)) +
    scale_fill_manual(name = "", values = v_color_regions) +
    labs(x = "", title = "ICU admissions", y = "") +
    theme_bw()


## Plot ICU 100k
plot_reg_idade_mv_100k <- 
    df_hosp_100k_age_region %>% 
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = mv_100k, fill = REGIAO),
             position = "dodge") +
    scale_y_continuous(labels = scales::comma_format(), 
                       breaks = seq(0, 480, 80),
                       limits = c(0, 480)) +
    scale_fill_manual(name = "", values = v_color_regions) +
    labs(x = "", title = "Invasive ventilation", y = "") +
    theme_bw()






## Sensitivity - In-hospital mortality 

## Calculates In-hospital mortality (deaths/hospitalisations) - Regions
# Overall - Regions
df_ihm_reg <-
    srag_adults_covid_outcome %>%
    count(REGIAO, FAIXA_IDADE, EVOLUCAO) %>% 
    rename(deaths = n) %>% 
    group_by(REGIAO, FAIXA_IDADE) %>% 
    mutate(total = sum(deaths)) %>%
    ungroup() %>% 
    filter(EVOLUCAO == "Death") %>% 
    left_join(
        srag_adults_covid_outcome %>%
            filter(UTI == "Yes") %>% 
            count(REGIAO, FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_icu = n) %>% 
            group_by(REGIAO, FAIXA_IDADE) %>% 
            mutate(total_icu = sum(deaths_icu)) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    left_join(
        srag_adults_covid_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>% 
            count(REGIAO, FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_mv = n) %>% 
            group_by(REGIAO, FAIXA_IDADE) %>% 
            mutate(total_mv = sum(deaths_mv)) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    select(-EVOLUCAO) %>% 
    filter(!is.na(REGIAO), !is.na(FAIXA_IDADE)) %>% 
    group_by(REGIAO) %>% 
    summarise(
        ihm = sum(deaths) / sum(total),
        ihm_icu = sum(deaths_icu) / sum(total_icu),
        ihm_mv = sum(deaths_mv) / sum(total_mv),
        total = paste0(sum(deaths), "/", sum(total), " (", round(100 * ihm, 1), "%)"),
        icu = paste0(sum(deaths_icu), "/", sum(total_icu), " (", round(100 * ihm_icu, 1), "%)"),
        mv = paste0(sum(deaths_mv), "/", sum(total_mv), " (", round(100 * ihm_mv, 1), "%)")
    ) %>% 
    mutate(FAIXA_IDADE = "Overall") %>% 
    select(REGIAO, FAIXA_IDADE, total, icu, mv, ihm, ihm_icu, ihm_mv) %>% 
    arrange(REGIAO)


# By age group - Regions
df_ihm_reg_age <- 
    srag_adults_covid_outcome %>%
    count(REGIAO, FAIXA_IDADE, EVOLUCAO) %>% 
    rename(deaths = n) %>% 
    group_by(REGIAO, FAIXA_IDADE) %>% 
    mutate(total = sum(deaths), 
           ihm = deaths / total) %>%
    ungroup() %>% 
    filter(EVOLUCAO == "Death") %>% 
    left_join(
        srag_adults_covid_outcome %>%
            filter(UTI == "Yes") %>% 
            count(REGIAO, FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_icu = n) %>% 
            group_by(REGIAO, FAIXA_IDADE) %>% 
            mutate(total_icu = sum(deaths_icu),
                   ihm_icu = deaths_icu / total_icu) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    left_join(
        srag_adults_covid_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>% 
            count(REGIAO, FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_mv = n) %>% 
            group_by(REGIAO, FAIXA_IDADE) %>% 
            mutate(total_mv = sum(deaths_mv), 
                   ihm_mv = deaths_mv / total_mv) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    select(-EVOLUCAO) %>% 
    filter(!is.na(REGIAO), !is.na(FAIXA_IDADE)) %>% 
    mutate(
        total = paste0(deaths, "/", total, " (", round(100 * (deaths / total), 1), "%)"),
        icu = paste0(deaths_icu, "/", total_icu, " (", round(100 * (deaths_icu / total_icu), 1), "%)"),
        mv = paste0(deaths_mv, "/", total_mv, " (", round(100 * (deaths_mv / total_mv), 1), "%)")
    ) %>% 
    mutate(REGIAO = factor(REGIAO,
                           levels = c("North",
                                      "Northeast",
                                      "Central-West",
                                      "Southeast",
                                      "South"
                           ),
    )
    ) %>% 
    select(REGIAO, FAIXA_IDADE, total, ihm, icu, ihm_icu, 
           mv, ihm_mv) %>% 
    arrange(REGIAO)




## Calculates In-hospital mortality (deaths/hospitalisations) - Brazil
# Overall - Brazil
df_ihm_brazil <- 
    srag_adults_covid_outcome %>%
    count(FAIXA_IDADE, EVOLUCAO) %>% 
    rename(deaths = n) %>% 
    group_by(FAIXA_IDADE) %>% 
    mutate(total = sum(deaths)) %>%
    ungroup() %>% 
    filter(EVOLUCAO == "Death") %>% 
    left_join(
        srag_adults_covid_outcome %>%
            filter(UTI == "Yes") %>% 
            count(FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_icu = n) %>% 
            group_by(FAIXA_IDADE) %>% 
            mutate(total_icu = sum(deaths_icu)) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    left_join(
        srag_adults_covid_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>% 
            count(FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_mv = n) %>% 
            group_by(FAIXA_IDADE) %>% 
            mutate(total_mv = sum(deaths_mv)) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    mutate(REGIAO = "Brazil") %>%
    select(-EVOLUCAO) %>% 
    filter(!is.na(REGIAO), !is.na(FAIXA_IDADE)) %>% 
    group_by(REGIAO) %>% 
    summarise(
        ihm = sum(deaths) / sum(total),
        ihm_icu = sum(deaths_icu) / sum(total_icu),
        ihm_mv = sum(deaths_mv) / sum(total_mv),
        total = paste0(sum(deaths), "/", sum(total), " (", round(100 * ihm, 1), "%)"),
        icu = paste0(sum(deaths_icu), "/", sum(total_icu), " (", round(100 * ihm_icu, 1), "%)"),
        mv = paste0(sum(deaths_mv), "/", sum(total_mv), " (", round(100 * ihm_mv, 1), "%)")
    ) %>% 
    mutate(FAIXA_IDADE = "Overall") %>% 
    select(REGIAO, FAIXA_IDADE, total, icu, mv, ihm, ihm_icu, ihm_mv) %>% 
    arrange(REGIAO)


# By age group - Brazil
df_ihm_brazil_age <- 
    srag_adults_covid_outcome %>%
    count(FAIXA_IDADE, EVOLUCAO) %>% 
    rename(deaths = n) %>% 
    group_by(FAIXA_IDADE) %>% 
    mutate(total = sum(deaths), 
           ihm = deaths / total) %>%
    ungroup() %>% 
    filter(EVOLUCAO == "Death") %>% 
    left_join(
        srag_adults_covid_outcome %>%
            filter(UTI == "Yes") %>% 
            count(FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_icu = n) %>% 
            group_by(FAIXA_IDADE) %>% 
            mutate(total_icu = sum(deaths_icu),
                   ihm_icu = deaths_icu / total_icu) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    left_join(
        srag_adults_covid_outcome %>%
            filter(SUPORT_VEN == "Yes, invasive") %>% 
            count(FAIXA_IDADE, EVOLUCAO) %>% 
            rename(deaths_mv = n) %>% 
            group_by(FAIXA_IDADE) %>% 
            mutate(total_mv = sum(deaths_mv), 
                   ihm_mv = deaths_mv / total_mv) %>%
            ungroup() %>% 
            filter(EVOLUCAO == "Death")
    ) %>% 
    mutate(REGIAO = "Brazil") %>% 
    select(-EVOLUCAO) %>% 
    filter(!is.na(REGIAO), !is.na(FAIXA_IDADE)) %>% 
    mutate(
        total = paste0(deaths, "/", total, " (", round(100 * (deaths / total), 1), "%)"),
        icu = paste0(deaths_icu, "/", total_icu, " (", round(100 * (deaths_icu / total_icu), 1), "%)"),
        mv = paste0(deaths_mv, "/", total_mv, " (", round(100 * (deaths_mv / total_mv), 1), "%)")
    ) %>% 
    select(REGIAO, FAIXA_IDADE, total, ihm, icu, ihm_icu, 
           mv, ihm_mv) %>% 
    arrange(REGIAO)


# Exporting values of in-hospital mortality for Brazil and Regions
write_csv(
    bind_rows(
        df_ihm_brazil,
        df_ihm_brazil_age,
        df_ihm_reg,
        df_ihm_reg_age,
    ) %>%
        mutate(REGIAO = factor(REGIAO,
                               levels = c("Brazil",
                                          "North",
                                          "Northeast",
                                          "Central-West",
                                          "Southeast",
                                          "South"
                               )
        )
        ) %>% 
        arrange(REGIAO) %>% 
        select(REGIAO, FAIXA_IDADE, everything())
    , "Outputs/Tables/Sensitivity Analysis/sensitivity_in-hospital_mortality_regions.csv")





## Plot IHM Total
plot_ihm_df_ihm_reg_idade_total <- 
    df_ihm_reg_age %>% 
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = ihm, fill = REGIAO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(), 
                       limits = c(0, 1)) +
    scale_fill_manual(name = "", values = v_color_regions) +
    labs(x = "", title = "", y = "In-hospital mortality") +
    theme_bw()



## Plot IHM ICU
plot_ihm_df_ihm_reg_idade_icu <- 
    df_ihm_reg_age %>% 
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = ihm_icu, fill = REGIAO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(), 
                       limits = c(0, 1)) +
    scale_fill_manual(name = "", values = v_color_regions) +
    labs(x = "Age (years)", title = "", y = "") +
    theme_bw()



## Plot IHM Total
plot_ihm_df_ihm_reg_idade_vm <- 
    df_ihm_reg_age %>% 
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = ihm_mv, fill = REGIAO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(), 
                       limits = c(0, 1)) +
    scale_fill_manual(name = "", values = v_color_regions) +
    labs(x = "", title = "", y = "") +
    theme_bw()



# Combining plots ---------------------------------------------------------


# Creating Figure 3

combined_plot_region <-
    ggpubr::ggarrange(plot_reg_idade_total_100k,
                      plot_reg_idade_icu_100k,
                      plot_reg_idade_mv_100k,
                      plot_ihm_df_ihm_reg_idade_total,
                      plot_ihm_df_ihm_reg_idade_icu,
                      plot_ihm_df_ihm_reg_idade_vm,
                      label.x = "Age (years)",
                      nrow = 2,
                      ncol = 3,
                      align = "hv",
                      common.legend = TRUE,
                      legend = "bottom", font.label = list(size = 8))





ggsave("Outputs/Figures/Sensitivity Analysis/sensitivity_figure_IHM_Rate100k.png",
       combined_plot_region,
       units = "in", dpi = 800, height = 6, width = 13)






# srag_adults_pcr_outcome %>% 
#     # filter(!is.na(n_comorb_mreal)) %>%
#     mutate(TIME_SYMPTOMS_HOSP = ifelse(TIME_SYMPTOMS_HOSP   <= 0 , NA, TIME_SYMPTOMS_HOSP)) %>% 
#     mutate(TIME_SYMPTOMS_HOSP = ifelse(TIME_SYMPTOMS_HOSP   > 30, 30, TIME_SYMPTOMS_HOSP)) %>% 
#     mutate(
#         TEMPO_SE = case_when(
#             TIME_SYMPTOMS_HOSP <= 3 ~ "<=3",
#             TIME_SYMPTOMS_HOSP <= 6 ~ "<=6",
#             TIME_SYMPTOMS_HOSP <= 9 ~ "<=9",
#             TIME_SYMPTOMS_HOSP <= 12 ~ "<=12",
#             TIME_SYMPTOMS_HOSP <= 15 ~ "<=15",
#             TIME_SYMPTOMS_HOSP > 15 ~ ">15"
# 
#             )
#         ) %>% 
#     group_by(TEMPO_SE) %>% 
#     summarise(
#         mortes = sum(EVOLUCAO == "Death"),
#         total = n()
#     ) %>% 
#     ungroup() %>% 
#     mutate(
#         ihm_ratio = paste0(mortes, "/", total, " (", round(100*mortes/total),"%)"),
#     ) %>% 
#     select(-mortes, -total)
# 
# 
# 
# 
# 
# 
# srag_adults_pcr_outcome %>% 
#     # filter(!is.na(n_comorb_mreal)) %>%
#     mutate(TIME_SYMPTOMS_HOSP = ifelse(TIME_SYMPTOMS_HOSP   <= 0 , NA, TIME_SYMPTOMS_HOSP)) %>% 
#     mutate(TIME_SYMPTOMS_HOSP = ifelse(TIME_SYMPTOMS_HOSP   > 30, 30, TIME_SYMPTOMS_HOSP)) %>% 
#     mutate(
#         TEMPO_SE = case_when(
#             TIME_SYMPTOMS_HOSP <= 3 ~ "<=3",
#             TIME_SYMPTOMS_HOSP <= 6 ~ "<=6",
#             TIME_SYMPTOMS_HOSP <= 9 ~ "<=9",
#             TIME_SYMPTOMS_HOSP <= 12 ~ "<=12",
#             TIME_SYMPTOMS_HOSP <= 15 ~ "<=15",
#             TIME_SYMPTOMS_HOSP > 15 ~ ">15"
#             
#         )
#     ) %>% 
#     group_by(TEMPO_SE) %>% 
#     summarise(
#         # mortes = sum(EVOLUCAO == "Death"),
#         total = n(),
#         
#     ) %>% 
#     ungroup() %>% 
#     mutate(
#         total_todos = sum(total),
#         ihm_ratio = paste0(total, "/", total_todos, " (", round(100*total/total_todos),"%)"),
#     ) %>% 
#     select(-total_todos, -total)
