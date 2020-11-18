#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
### In-hospital mortality stratified by Oxygen Saturation <95%, Dyspnoea, Respiratory distress,          ##
### and SARI for hospitalized COVID-19 patients in Brazil                                                ## 
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidylog)


## Colour palletes for plots (based on ggsci::pal_lancet()) 
v_color_regions_alpha_30 <- c("#1B19194C", "#0099B44C", 
                              "#ED00004C", "#AD002A4C",
                              "#00468B4C", "#925E9F4C")

v_color_regions_alpha_50 <- c("#1B19197F", "#0099B47F", 
                              "#ED00007F", "#AD002A7F",
                              "#00468B7F", "#925E9F7F")

v_color_regions_alpha_75 <- c("#1B1919BF", "#0099B4BF", 
                              "#ED0000BF", "#AD002ABF",
                              "#00468BBF", "#925E9FBF")

v_color_regions_alpha <- c("#1B1919FF", "#0099B4FF", 
                           "#ED0000FF", "#AD002AFF",
                           "#00468BFF", "#925E9FFF")





# Data Input --------------------------------------------------------------
# COVID Data
srag_adults_pcr <- vroom::vroom("Data/srag_adults_pcr_12_10.csv")

# COVID Data - hospitalizations with a defined outcome
srag_adults_pcr_outcomes <- 
    srag_adults_pcr %>% 
    filter(EVOLUCAO %in% c("Death", "Discharge")) %>% 
    mutate(n_comorb_mreal = factor(n_comorb_mreal, levels = c(0, 1, 2), 
                                   labels = c("No comorbidities", "1-2", ">=3")))







# In-hospital mortality - Oxygen Saturation <95% --------------------------

### IMH - Oxygen saturation < 95% - Overall
df_ihm_sat <- 
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
    ) %>%
    filter(!is.na(SATURACAO)) %>% 
    mutate(FAIXA_IDADE = "Overall")



### IMH - Oxygen saturation < 95% - per age group
df_ihm_sat_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(SATURACAO, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(SATURACAO, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
    ) %>%
    filter(!is.na(SATURACAO)) 



# Export IMH - Oxygen saturation < 95%
write_csv(
    bind_rows(
        df_ihm_sat,
        df_ihm_sat_age
        ) %>%
        select(SATURACAO, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(SATURACAO)
    , "Outputs/Tables/In-hospital mortality and Age/IHM_O2_Sat.csv")


## Plot - IHM oxygen saturation <= 95% 
plot_ihm_sat_age <-
    df_ihm_sat_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = SATURACAO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha_50[1], v_color_regions_alpha[1]), name = "") +
    labs(title = "Oxygen Saturation <95%", x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")





# In-hospital mortality - Dyspnoea ----------------------------------------

### IMH - Dyspnoea - Overall
df_ihm_dys <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(DISPNEIA) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(DISPNEIA) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
    ) %>%
    filter(!is.na(DISPNEIA)) %>% 
    mutate(FAIXA_IDADE = "Overall")


### IMH - Dyspnea - per age group
df_ihm_dys_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(DISPNEIA, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(DISPNEIA, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
    ) %>%
    filter(!is.na(DISPNEIA)) 


# Export IMH - Dyspnea
write_csv(
    bind_rows(
        df_ihm_dys,
        df_ihm_dys_age
    ) %>%
        select(DISPNEIA, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(DISPNEIA)
    , "Outputs/Tables/In-hospital mortality and Age/IHM_Dyspnoea.csv"
    )





## Plot - IHM Dyspnea and Age
plot_ihm_dys_age <-
    df_ihm_dys_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = DISPNEIA),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha_50[1], v_color_regions_alpha[1]), name = "") +
    labs(title = "Dyspnoea", x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")










# In-hospital mortality - Respiratory distress ----------------------------

## IMH - Respiratory distress - Overall
df_ihm_dist <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(DESC_RESP) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(DESC_RESP) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(DESC_RESP)) %>% 
    mutate(FAIXA_IDADE = "Overall")


## IMH - Respiratory distress - per age group
df_ihm_dist_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(DESC_RESP, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(DESC_RESP, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(DESC_RESP)) 


# Export IMH - Respiratory distress
write_csv(
    bind_rows(
        df_ihm_dist,
        df_ihm_dist_age
    ) %>%
        select(DESC_RESP, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(DESC_RESP)
    , "Outputs/Tables/In-hospital mortality and Age/IHM_RespiratoryDist.csv"
)



## Plot - IHM Respiratory distress and Age
plot_ihm_dist_age <-
    df_ihm_dist_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = DESC_RESP),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha_50[1], v_color_regions_alpha[1]), name = "") +
    labs(title = "Respiratory Distress", x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")






# In-hospital mortality - SARI criteria -----------------------------------

### IMH - SARI - Overall
df_ihm_sari <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(SRAG_original) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(SRAG_original) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(SRAG_original)) %>% 
    mutate(FAIXA_IDADE = "Overall")



### IMH - SARI - per group
df_ihm_sari_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(SRAG_original, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(SRAG_original, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(SRAG_original)) 

# Export IMH - SARI
write_csv(
    bind_rows(
        df_ihm_sari,
        df_ihm_sari_age
        ) %>%
        select(SRAG_original, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(SRAG_original)
    , "Outputs/Tables/In-hospital mortality and Age/IHM_SARI.csv"
    )


## Plot - IHM Dyspnea and Age
plot_ihm_sari_age <-
    df_ihm_sari_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = SRAG_original),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha_50[1], v_color_regions_alpha[1]), name = "") +
    labs(title = "SARI", x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")





### Combining plots
combined_plot_ihm_sympt <- 
    ggpubr::ggarrange(plot_ihm_sat_age,
                      plot_ihm_dys_age,
                      plot_ihm_dist_age,
                      plot_ihm_sari_age,
                      nrow = 2,
                      ncol = 2,
                      align = "hv",
                      common.legend = TRUE,
                      legend = "bottom", font.label = list(size = 8))


ggsave("Outputs/Figures/Others/Figures_IHM_symptoms.png",
       combined_plot_ihm_sympt,
       units = "in", dpi = 800, height = 7, width = 8)

