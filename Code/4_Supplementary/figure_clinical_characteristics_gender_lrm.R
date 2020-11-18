#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### Extra information of In-Hospital mortality per clinical characteristics and Gender                   ##
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
    mutate(n_comorb = factor(n_comorb, levels = c(0, 1, 2), 
                             labels = c("No comorbidities", "1-2", ">=3"))) %>% 
    mutate(SRAG_original = ifelse(SRAG_original == "Yes", "SARI", "Not SARI"))


v_color_regions <- c("#1B1919FF", "#0099B4FF", 
                     "#ED0000FF", "#AD002AFF")

v_color_regions_alpha <- c("#1B19197F", "#0099B47F", 
                           "#ED00007F", "#AD002A7F")

v_color_regions_alpha_75 <- c("#1B1919BF", "#0099B4BF", 
                           "#ED0000BF", "#AD002ABF")




# In-hospital mortality - Gender ------------------------------------------
## IHM Gender
df_ihm_gender_saturation <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(SATURACAO) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(SATURACAO) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(SATURACAO)) %>% 
    mutate(FAIXA_IDADE = "Overall")


## IHM Gender and Age
df_ihm_saturation_gender_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(CS_SEXO, SATURACAO, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_SEXO, SATURACAO, FAIXA_IDADE) %>%
            rename(deaths = n)
        ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
        ) %>%
    filter(!is.na(CS_SEXO), !is.na(SATURACAO)) %>% 
    mutate(SATURACAO = ifelse(SATURACAO=="No", "Oxygen Saturation ≥95%", "Oxygen Satuation <95%"),
           SATURACAO = as_factor(SATURACAO),
           SATURACAO = fct_relevel(SATURACAO, "Oxygen Saturation ≥95%", "Oxygen Satuation <95%"))




## Plot - IHM per gender and age
plot_ihm_saturation_gender_age <-
    df_ihm_saturation_gender_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = CS_SEXO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha[1], v_color_regions[1]), name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    facet_wrap(. ~ SATURACAO) +
    theme_bw() +
    theme(legend.position = "bottom")





# In-hospital mortality - comorbidities -----------------------------------

## IHM No. of comorbidities
df_ihm_comorb_gender <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(CS_SEXO, n_comorb) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_SEXO, n_comorb) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(n_comorb), !is.na(CS_SEXO)) %>% 
    mutate(FAIXA_IDADE = "Overall")


## IHM No. of comorbidities Age
df_ihm_comorb_gender_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(CS_SEXO, FAIXA_IDADE, n_comorb) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter( EVOLUCAO == "Death") %>%
            count(CS_SEXO, FAIXA_IDADE, n_comorb) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(n_comorb), !is.na(CS_SEXO)) 


## Plot - IHM per No. of comorbidities and age
plot_ihm_comorb_gender_age <-
    df_ihm_comorb_gender_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = factor(CS_SEXO)),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(
        values = c(v_color_regions_alpha[1],
                   v_color_regions[1]),
        # labels = c("No comorbidities", "1-2", "\u2265 3"),
        name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    facet_wrap(. ~ n_comorb) +
    theme_bw() +
    theme(legend.position = "bottom")







# In-hospital mortality - ICU admission -----------------------------------


## IHM ICU admission 
df_ihm_gender_icu <-
    left_join(
        srag_adults_pcr_outcomes %>%
            count(CS_SEXO, UTI) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_SEXO, UTI) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(UTI), !is.na(CS_SEXO)) %>%
    mutate(UTI = factor(UTI,
                        levels = c("No", "Yes"),
                        labels = c("No ICU", "ICU")
                        )
           ) %>%
    mutate(FAIXA_IDADE = "Overall")


## IHM ICU admission and age
df_ihm_icu_gender_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(CS_SEXO, UTI, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_SEXO, UTI, FAIXA_IDADE) %>%
            rename(deaths = n)
        ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>% 
    filter(!is.na(UTI), !is.na(CS_SEXO)) %>%
    mutate(UTI = factor(UTI,
                        levels = c("No", "Yes"),
                        labels = c("No ICU", "ICU")
                        )
           )

## Plot - IHM per ICU admission and age
plot_ihm_icu_gender_age <-
    df_ihm_icu_gender_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = CS_SEXO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha[1], 
                                  v_color_regions[1]), name = "") +
    
    labs(x = "Age (years)", y = "In-hospital mortality") +
    facet_wrap(. ~ UTI) +
    theme_bw() +
    theme(legend.position = "bottom")







# In-hospital mortality - Respiratory Support -----------------------------

## IHM Respiratory Support
df_ihm_mv_gender_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            mutate(SUPORT_VEN = case_when(
                SUPORT_VEN == "No" ~ "None or \n non-invasive",
                SUPORT_VEN == "Yes, non-invasive" ~ "None or \n non-invasive",
                SUPORT_VEN == "Yes, invasive" ~ "Invasive"
                )
                ) %>%
            count(CS_SEXO, SUPORT_VEN, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            mutate(SUPORT_VEN = case_when(
                SUPORT_VEN == "No" ~ "None or \n non-invasive",
                SUPORT_VEN == "Yes, non-invasive" ~ "None or \n non-invasive",
                SUPORT_VEN == "Yes, invasive" ~ "Invasive"
                )
                ) %>%
            count(CS_SEXO, SUPORT_VEN, FAIXA_IDADE) %>%
            rename(deaths = n)
        ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>% 
    filter(!is.na(SUPORT_VEN), !is.na(CS_SEXO)) %>%
    mutate(SUPORT_VEN = factor(SUPORT_VEN,
                               levels = c("None or \n non-invasive",
                                          # "Non-invasive",
                                          "Invasive"
                                          ),
                               labels = c("None or Non-invasive\nrespiratory support",
                                          # "Non-invasive\nrespiratory support",
                                          "Invasive\nrespiratory support"),
                           ordered = TRUE
                           )
       )

## Plot - IHM per Respiratory support and age
plot_ihm_mv_gender_age <-
    df_ihm_mv_gender_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = CS_SEXO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c(v_color_regions_alpha[1], 
                                 v_color_regions[1]
                                 ), name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    facet_wrap(. ~ SUPORT_VEN) +
    theme_bw() +
    theme(legend.position = "bottom")





### Combining plots
combined_plot_ihm_gender_vars <- 
    ggpubr::ggarrange(plot_ihm_saturation_gender_age,
                      plot_ihm_comorb_gender_age,
                      plot_ihm_icu_gender_age,
                      plot_ihm_mv_gender_age,
                      nrow = 2,
                      ncol = 2,
                      align = "hv",
                      common.legend = TRUE,
                      legend = "bottom", font.label = list(size = 8))


ggsave("Outputs/Figures/Others/Figure_IHM_SARI_Comorb_ICU_Resp_byGender.png",
       combined_plot_ihm_gender_vars,
       units = "in", dpi = 800, height = 7, width = 15)
