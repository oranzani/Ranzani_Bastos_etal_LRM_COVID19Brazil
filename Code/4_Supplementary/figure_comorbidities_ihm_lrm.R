#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
### Analysis of comorbidities                                                                            ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidylog)

# Input data --------------------------------------------------------------

### SRAG Data - Hospitalizations
srag_adults_pcr <- vroom::vroom("Data/srag_adults_pcr_12_10.csv")

srag_adults_pcr_outcomes <- 
    srag_adults_pcr %>% 
    filter(EVOLUCAO %in% c("Death", "Discharge")) %>% 
    mutate(n_comorb = factor(n_comorb, levels = c(0, 1, 2), labels = c("No comorbidities", "1-2", ">=3"))) %>% 
    mutate(n_comorb_m = factor(n_comorb_m, levels = c(0, 1, 2), labels = c("No comorbidities", "1-2", ">=3")),
           n_comorb_mreal = factor(n_comorb_mreal, levels = c(0, 1, 2), labels = c("No comorbidities", "1-2", ">=3")))



v_color_regions <- c("#1B1919FF", "#0099B4FF", 
                     "#ED0000FF", "#AD002AFF")

v_color_regions_alpha <- c("#1B19197F", "#0099B47F", 
                           "#ED00007F", "#AD002A7F")

v_color_regions_alpha_75 <- c("#1B1919BF", "#0099B4BF", 
                           "#ED0000BF", "#AD002ABF")


# In-hospital mortality - comorbidities -----------------------------------

## IHM No. of comorbidities
df_ihm_comorb <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(n_comorb) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(n_comorb) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(n_comorb)) %>% 
    mutate(FAIXA_IDADE = "Overall")


## IHM No. of comorbidities Age
df_ihm_comorb_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(n_comorb, FAIXA_IDADE ) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(n_comorb, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    left_join(
        srag_adults_pcr_outcomes %>%
            count(n_comorb_m, FAIXA_IDADE ) %>%
            filter(!is.na(n_comorb_m)) %>% 
            rename(total_m = n,
                   n_comorb = n_comorb_m), 
    ) %>% 
    left_join(
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(n_comorb_m, FAIXA_IDADE) %>%
            filter(!is.na(n_comorb_m)) %>% 
            rename(deaths_m = n,
                   n_comorb = n_comorb_m)
    ) %>%
    left_join(
        srag_adults_pcr_outcomes %>%
            count(n_comorb_mreal, FAIXA_IDADE ) %>%
            filter(!is.na(n_comorb_mreal)) %>% 
            rename(total_mreal = n,
                   n_comorb = n_comorb_mreal), 
    ) %>% 
    left_join(
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(n_comorb_mreal, FAIXA_IDADE) %>%
            filter(!is.na(n_comorb_mreal)) %>% 
            rename(deaths_mreal = n,
                   n_comorb = n_comorb_mreal)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        in_hosp_mortal_m = deaths_m / total_m,
        in_hosp_mortal_mreal = deaths_mreal / total_mreal,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
        ) %>%
    filter(!is.na(n_comorb)) %>%
    select(n_comorb, FAIXA_IDADE, in_hosp_mortal, in_hosp_mortal_m, in_hosp_mortal_mreal) %>%
    pivot_longer(c(-n_comorb, -FAIXA_IDADE), names_to = "ihm", values_to = "values")


# Export IHM - No of Comorbidities
# write_csv(
#     bind_rows(
#         df_ihm_comorb,
#         df_ihm_comorb_age
#     ) %>%
#         select(n_comorb, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
#         arrange(n_comorb)
#     , "Outputs/Tables/In-hospital mortality - Figure 2/IHM_Gender_Comorbidities.csv"
# )


## Plot - IHM per No. of comorbidities and age
plot_ihm_comorb_age <-
    df_ihm_comorb_age %>%
    mutate(ihm = case_when(ihm == "in_hosp_mortal" ~ "Complete-case", TRUE ~ "Imputed No")) %>% 
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = values, fill = factor(ihm)),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(
        values = c(v_color_regions_alpha[2],
                   v_color_regions_alpha_75[2],
                   v_color_regions[2]),
        # labels = c("Imputado", "N?o Imputado"),
        # labels = c("No comorbidities", "1-2", "\u2265 3"),
        name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    facet_wrap(. ~ n_comorb) + 
    theme_bw() +
    theme(legend.position = "bottom")


#### Adding inputed
comorb3_outcome_agecat <- comorb3_outcome_agecat %>% 
    filter(str_detect(name, "death")) %>% 
    separate(name, into = c("FAIXA_IDADE", "n_comorb", "outcome")) %>% 
    mutate(n_comorb = case_when(
        n_comorb == "nocomorb" ~ 0,
        n_comorb == "12" ~ 1,
        TRUE ~ 2),
        n_comorb = factor(n_comorb, levels = c(0, 1, 2), labels = c("No comorbidities", "1-2", ">=3")),
        FAIXA_IDADE = case_when(
            FAIXA_IDADE == "age1" ~ "20-39",
            FAIXA_IDADE == "age2" ~ "40-49",
            FAIXA_IDADE == "age3" ~ "50-59",
            FAIXA_IDADE == "age4" ~ "60-69",
            FAIXA_IDADE == "age5" ~ "70-79",
            FAIXA_IDADE == "age6" ~ "80+"),
        b = as.numeric(b),
        ihm = "Multiple Imputation") %>% 
    rename(values = b)

plot_ihm_comorb_agem <-
    bind_rows(df_ihm_comorb_age,comorb3_outcome_agecat) %>%
    mutate(ihm = case_when(ihm == "in_hosp_mortal" ~ "Imputed No", 
                           ihm == "in_hosp_mortal_m" ~ "Complete-case",
                           ihm == "Multiple Imputation" ~ "Multiple Imputation",
                           TRUE ~ "Real - All answered")) %>% 
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = values, fill = factor(ihm)),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(
        values = c(v_color_regions_alpha[2],
                   v_color_regions_alpha_75[2],
                   v_color_regions[2], "red"),
        # labels = c("Imputado", "N?o Imputado"),
        # labels = c("No comorbidities", "1-2", "\u2265 3"),
        name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    facet_wrap(. ~ n_comorb) + 
    theme_bw() +
    theme(legend.position = "bottom")




plot_ihm_comorb_agem1 <-
    bind_rows(df_ihm_comorb_age,comorb3_outcome_agecat) %>%
    mutate(ihm = case_when(ihm == "in_hosp_mortal" ~ "Imputed as No", 
                           ihm == "in_hosp_mortal_m" ~ "Complete-case",
                           ihm == "Multiple Imputation" ~ "Multiple Imputation",
                           TRUE ~ "Real - All answered")) %>% 
    filter(ihm %in% c("Imputed as No", "Multiple Imputation")) %>% 
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = values, fill = factor(ihm)),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(
        values = c(v_color_regions_alpha[2],
                   v_color_regions_alpha_75[2],
                   v_color_regions[2], "red"),
        # labels = c("Imputado", "N?o Imputado"),
        # labels = c("No comorbidities", "1-2", "\u2265 3"),
        name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    facet_wrap(. ~ n_comorb) + 
    theme_bw() +
    theme(legend.position = "bottom")

srag_adults_pcr_outcomes %>% select(n_comorb_m) %>% Hmisc::describe()

