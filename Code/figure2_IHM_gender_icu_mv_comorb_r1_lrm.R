#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 200,000 hospitalizations for COVID-19 in Brazil: a nationwide study ##
### Coding Otavio Ranzani, Leonardo Bastos, João Gabriel Gelli                                           ##
### October 2020                                                                                         ##
###                                                                                                      ##
### Figure 3. In-hospital mortality stratified by age, sex, comorbidities, intensive care admission      ##
###    and invasive mechanical ventilation for hospitalized COVID-19 patients in Brazil                  ## 
###                                                                                                      ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidylog)


# In-hospital mortality - Gender ------------------------------------------
### SRAG Data - Hospitalizations
srag_adults_pcr <- vroom::vroom("Data/srag_adults_pcr_12_10.csv")

srag_adults_pcr_outcomes <- 
    srag_adults_pcr %>% 
    filter(EVOLUCAO %in% c("Death", "Discharge")) %>% 
    mutate(n_comorb_mreal = factor(n_comorb_mreal, levels = c(0, 1, 2), 
                             labels = c("No comorbidities", "1-2", ">=3")))


v_color_regions <- c("#1B1919FF", "#0099B4FF", 
                     "#ED0000FF", "#AD002AFF")

v_color_regions_alpha <- c("#1B19197F", "#0099B47F", 
                           "#ED00007F", "#AD002A7F")

v_color_regions_alpha_75 <- c("#1B1919BF", "#0099B4BF", 
                              "#ED0000BF", "#AD002ABF")

## IHM Gender
df_ihm_gender <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(CS_SEXO) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_SEXO) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(CS_SEXO)) %>% 
    mutate(FAIXA_IDADE = "Overall") 


## IHM Gender and Age
df_ihm_gender_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(CS_SEXO, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_SEXO, FAIXA_IDADE) %>%
            rename(deaths = n)
        ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
        ) %>%
    filter(!is.na(CS_SEXO))


# Export data - IHM Gender
write_csv(
    bind_rows(
        df_ihm_gender,
        df_ihm_gender_age
    ) %>%
        select(CS_SEXO, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(CS_SEXO)
    , "Outputs/Tables/In-hospital mortality and Age/IHM_Gender_Age.csv"
)

## Plot - IHM per gender and age
plot_ihm_gender_age <-
    df_ihm_gender_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = CS_SEXO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha[1], v_color_regions[1]), name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")









# In-hospital mortality - comorbidities -----------------------------------

## IHM No. of comorbidities
df_ihm_comorb <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(n_comorb_mreal) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(n_comorb_mreal) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(n_comorb_mreal)) %>% 
    mutate(FAIXA_IDADE = "Overall")


## IHM No. of comorbidities Age
df_ihm_comorb_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(n_comorb_mreal, FAIXA_IDADE ) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(n_comorb_mreal, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(n_comorb_mreal)) 


# Export IHM - No of Comorbidities
write_csv(
    bind_rows(
        df_ihm_comorb,
        df_ihm_comorb_age
    ) %>%
        select(n_comorb_mreal, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(n_comorb_mreal)
    , "Outputs/Tables/In-hospital mortality and Age/IHM_Gender_Comorbidities.csv"
)


## Plot - IHM per No. of comorbidities and age
plot_ihm_comorb_age <-
    df_ihm_comorb_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = factor(n_comorb_mreal)),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(
        values = c(v_color_regions_alpha[2],
                   v_color_regions_alpha_75[2],
                   v_color_regions[2]),
        labels = c("No comorbidities", "1-2", "\u2265 3"),
        name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")










# In-hospital mortality - ICU admission -----------------------------------
## IHM ICU admission 
df_ihm_icu <-
    left_join(
        srag_adults_pcr_outcomes %>%
            count(UTI) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(UTI) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>% 
    filter(!is.na(UTI)) %>%
    mutate(UTI = factor(UTI,
                        levels = c("No", "Yes"),
                        labels = c("No ICU", "ICU")
                        )
           ) %>% 
    mutate(FAIXA_IDADE = "Overall")


## IHM ICU admission and age
df_ihm_icu_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(UTI, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(UTI, FAIXA_IDADE) %>%
            rename(deaths = n)
        ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>% 
    filter(!is.na(UTI)) %>%
    mutate(UTI = factor(UTI,
                        levels = c("No", "Yes"),
                        labels = c("No ICU", "ICU")
                        )
           )

# Export IHM - ICU
write_csv(
    bind_rows(
        df_ihm_icu,
        df_ihm_icu_age
    ) %>%
        select(UTI, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(UTI)
    , "Outputs/Tables/In-hospital mortality and Age/IHM_Gender_ICU.csv"
)



## Plot - IHM per ICU admission and age
plot_ihm_icu_age <-
    df_ihm_icu_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = UTI),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha[3], v_color_regions[3]), name = "") +
    
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")







# In-hospital mortality - Respiratory Support -----------------------------
## IHM Respiratory Support
df_ihm_mv <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            mutate(SUPORT_VEN = case_when(
                # SUPORT_VEN == "No" ~ "None or \n non-invasive",
                # SUPORT_VEN == "Yes, non-invasive" ~ "None or \n non-invasive",
                SUPORT_VEN == "No" ~ "None",
                SUPORT_VEN == "Yes, non-invasive" ~ "Non-invasive",
                SUPORT_VEN == "Yes, invasive" ~ "Invasive"
            )
            ) %>%
            count(SUPORT_VEN) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            mutate(SUPORT_VEN = case_when(
                # SUPORT_VEN == "No" ~ "None or \n non-invasive",
                # SUPORT_VEN == "Yes, non-invasive" ~ "None or \n non-invasive",
                SUPORT_VEN == "No" ~ "None",
                SUPORT_VEN == "Yes, non-invasive" ~ "Non-invasive",
                SUPORT_VEN == "Yes, invasive" ~ "Invasive"
            )
            ) %>%
            count(SUPORT_VEN) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>% 
    filter(!is.na(SUPORT_VEN)) %>%
    mutate(SUPORT_VEN = factor(SUPORT_VEN,
                               levels = c(
                                   # "None or \n non-invasive",
                                   "None", 
                                   "Non-invasive",
                                   "Invasive"
                               ),
                               labels = c(
                                   # "None or Non-invasive\nrespiratory support",
                                   "None",
                                   "Non-invasive\nrespiratory support",
                                   "Invasive\nrespiratory support"),
                               ordered = TRUE
                               )
           )

## IHM Respiratory Support
df_ihm_mv_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            mutate(SUPORT_VEN = case_when(
                # SUPORT_VEN == "No" ~ "None or \n non-invasive",
                # SUPORT_VEN == "Yes, non-invasive" ~ "None or \n non-invasive",
                SUPORT_VEN == "No" ~ "None",
                SUPORT_VEN == "Yes, non-invasive" ~ "Non-invasive",
                SUPORT_VEN == "Yes, invasive" ~ "Invasive"
            )
            ) %>%
            count(SUPORT_VEN, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            mutate(SUPORT_VEN = case_when(
                # SUPORT_VEN == "No" ~ "None or \n non-invasive",
                # SUPORT_VEN == "Yes, non-invasive" ~ "None or \n non-invasive",
                SUPORT_VEN == "No" ~ "None",
                SUPORT_VEN == "Yes, non-invasive" ~ "Non-invasive",
                SUPORT_VEN == "Yes, invasive" ~ "Invasive"
            )
            ) %>%
            count(SUPORT_VEN, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>% 
    filter(!is.na(SUPORT_VEN)) %>%
    mutate(SUPORT_VEN = factor(SUPORT_VEN,
                               levels = c(
                                   # "None or \n non-invasive",
                                   "None", 
                                   "Non-invasive",
                                   "Invasive"
                               ),
                               labels = c(
                                   # "None or Non-invasive\nrespiratory support",
                                   "None",
                                   "Non-invasive\nrespiratory support",
                                   "Invasive\nrespiratory support"),
                               ordered = TRUE
                               )
           )

# Export IHM - Respiratory support
write_csv(
    bind_rows(
        df_ihm_mv,
        df_ihm_mv_age
    ) %>%
        select(SUPORT_VEN, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(SUPORT_VEN)
    , "Outputs/Tables/In-hospital mortality and Age/IHM_Gender_Resp_Support.csv"
)


## Plot - IHM per Respiratory support and age
plot_ihm_mv_age <-
    df_ihm_mv_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = SUPORT_VEN),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    # scale_fill_manual(values = c(v_color_regions_alpha[4], 
    #                              v_color_regions[4]
    #                              ), name = "") +
    scale_fill_manual(values = c(v_color_regions_alpha[4], 
                                 v_color_regions_alpha_75[4],
                                 v_color_regions[4]
    ), name = "") +
    
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")





### Combining plots
combined_plot_ihm_age_vars <- 
    ggpubr::ggarrange(plot_ihm_gender_age,
                      plot_ihm_comorb_age,
                      plot_ihm_icu_age,
                      plot_ihm_mv_age,
                      nrow = 2,
                      ncol = 2,
                      align = "hv",
                      # common.legend = TRUE, 
                      legend = "bottom", font.label = list(size = 8))


ggsave("Outputs/Figures/Figure2_IHM_Gender_Comorb_ICU_RespSupport_2.pdf",
       combined_plot_ihm_age_vars,
       units = "in", dpi = 800, height = 8, width = 9)


















# Other plots for complementary analysis ----------------------------------


### Plots for severe sypmtoms - Oxygen saturation <= 95, Dyspnea and 

### OXYGEN SATURATION
## IMH - Oxygen saturation < 95% - Overall
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(SATURACAO)) %>% 
    mutate(FAIXA_IDADE = "Overall")

## IMH - Oxygen saturation < 95% and Age
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
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
    , "Outputs/Tables/In-hospital mortality and Age/IHM_O2_saturation.csv"
)


## Plot - IHM oxygen saturation <= 95% and Age
plot_ihm_sat_age <-
    df_ihm_sat_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = SATURACAO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha[1], v_color_regions[1]), name = "") +
    labs(title = "Oxygen Saturation <95%", x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")




### DYSPNEA
## IMH - Dyspnea - Overall
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(DISPNEIA)) %>% 
    mutate(FAIXA_IDADE = "Overall")


## IMH - Dyspnea and Age
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
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
    , "Outputs/Tables/In-hospital mortality and Age/IHM_dyspnea.csv"
)





## Plot - IHM Dyspnea and Age
plot_ihm_dys_age <-
    df_ihm_dys_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = DISPNEIA),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha[1], v_color_regions[1]), name = "") +
    labs(title = "Dyspnoea", x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")






### RESPIRATORY DISTRESS
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


## IMH - Respiratory distress and Age
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
    , "Outputs/Tables/In-hospital mortality and Age/IHM_Respiratory_distress.csv"
)



## Plot - IHM Respiratory distress and Age
plot_ihm_dist_age <-
    df_ihm_dist_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = DESC_RESP),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha[1], v_color_regions[1]), name = "") +
    labs(title = "Respiratory Distress", x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")





### SARI
## IMH - SARI - Overall
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



## IMH - SARI and Age
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
    scale_fill_manual(values = c( v_color_regions_alpha[1], v_color_regions[1]), name = "") +
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




ggsave("Outputs/Figures/Others/Figures_IHM_SATURACAO.png",
       plot_ihm_sat_age,
       units = "in", dpi = 800, height = 4, width = 5)





### saturation by region

### OXYGEN SATURATION
## IMH - Oxygen saturation <= 95% and Age
df_ihm_sat_age_region <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(REGIAO, SATURACAO, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(REGIAO, SATURACAO, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(SATURACAO)) 

## Plot - IHM oxygen saturation <= 95% and Age
plot_ihm_sat_age_region <-
    df_ihm_sat_age_region %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = SATURACAO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha[1], v_color_regions[1]), name = "") +
    labs(title = "Oxygen Saturation <95%", x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom") +
    facet_wrap(~REGIAO)






############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################



#### Sensitivity analysis - Hospitalizations classified as COVID-19 (CLASSI_FIN == 5)


### SRAG Data (CLASSI_FIN == 5)
srag_adults_covid <- vroom::vroom("Data/srag_adults_covid_12_10.csv")

srag_adults_covid_outcomes <- 
    srag_adults_covid %>% 
    filter(EVOLUCAO %in% c("Death", "Discharge")) %>% 
    mutate(n_comorb_mreal = factor(n_comorb_mreal, levels = c(0, 1, 2), 
                                   labels = c("No comorbidities", "1-2", ">=3")))



## IHM Gender
df_ihm_gender <- 
    left_join(
        srag_adults_covid_outcomes %>%
            count(CS_SEXO) %>%
            rename(total = n),
        srag_adults_covid_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_SEXO) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(CS_SEXO)) %>% 
    mutate(FAIXA_IDADE = "Overall") 


## IHM Gender and Age
df_ihm_gender_age <- 
    left_join(
        srag_adults_covid_outcomes %>%
            count(CS_SEXO, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_covid_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_SEXO, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(CS_SEXO))


# Export data - IHM Gender
write_csv(
    bind_rows(
        df_ihm_gender,
        df_ihm_gender_age
    ) %>%
        select(CS_SEXO, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(CS_SEXO)
    , "Outputs/Tables/Sensitivity Analysis/sensitivity_IHM_Gender_Age.csv"
)

## Plot - IHM per gender and age
plot_ihm_gender_age <-
    df_ihm_gender_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = CS_SEXO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha[1], v_color_regions[1]), name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")









# In-hospital mortality - comorbidities -----------------------------------

## IHM No. of comorbidities
df_ihm_comorb <- 
    left_join(
        srag_adults_covid_outcomes %>%
            count(n_comorb_mreal) %>%
            rename(total = n),
        srag_adults_covid_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(n_comorb_mreal) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(n_comorb_mreal)) %>% 
    mutate(FAIXA_IDADE = "Overall")


## IHM No. of comorbidities Age
df_ihm_comorb_age <- 
    left_join(
        srag_adults_covid_outcomes %>%
            count(n_comorb_mreal, FAIXA_IDADE ) %>%
            rename(total = n),
        srag_adults_covid_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(n_comorb_mreal, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>%
    filter(!is.na(n_comorb_mreal)) 


# Export IHM - No of Comorbidities
write_csv(
    bind_rows(
        df_ihm_comorb,
        df_ihm_comorb_age
    ) %>%
        select(n_comorb_mreal, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(n_comorb_mreal)
    , "Outputs/Tables/Sensitivity Analysis/sensitivity_IHM_Gender_Comorbidities.csv"
)


## Plot - IHM per No. of comorbidities and age
plot_ihm_comorb_age <-
    df_ihm_comorb_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = factor(n_comorb_mreal)),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(
        values = c(v_color_regions_alpha[2],
                   v_color_regions_alpha_75[2],
                   v_color_regions[2]),
        labels = c("No comorbidities", "1-2", "\u2265 3"),
        name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")










# In-hospital mortality - ICU admission -----------------------------------
## IHM ICU admission 
df_ihm_icu <-
    left_join(
        srag_adults_covid_outcomes %>%
            count(UTI) %>%
            rename(total = n),
        srag_adults_covid_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(UTI) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>% 
    filter(!is.na(UTI)) %>%
    mutate(UTI = factor(UTI,
                        levels = c("No", "Yes"),
                        labels = c("No ICU", "ICU")
    )
    ) %>% 
    mutate(FAIXA_IDADE = "Overall")


## IHM ICU admission and age
df_ihm_icu_age <- 
    left_join(
        srag_adults_covid_outcomes %>%
            count(UTI, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_covid_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(UTI, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>% 
    filter(!is.na(UTI)) %>%
    mutate(UTI = factor(UTI,
                        levels = c("No", "Yes"),
                        labels = c("No ICU", "ICU")
    )
    )

# Export IHM - ICU
write_csv(
    bind_rows(
        df_ihm_icu,
        df_ihm_icu_age
    ) %>%
        select(UTI, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(UTI)
    , "Outputs/Tables/Sensitivity Analysis/sensitivity_IHM_Gender_ICU.csv"
)



## Plot - IHM per ICU admission and age
plot_ihm_icu_age <-
    df_ihm_icu_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = UTI),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha[3], v_color_regions[3]), name = "") +
    
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")







# In-hospital mortality - Respiratory Support -----------------------------
## IHM Respiratory Support
df_ihm_mv <- 
    left_join(
        srag_adults_covid_outcomes %>%
            mutate(SUPORT_VEN = case_when(
                # SUPORT_VEN == "No" ~ "None or \n non-invasive",
                # SUPORT_VEN == "Yes, non-invasive" ~ "None or \n non-invasive",
                SUPORT_VEN == "No" ~ "None",
                SUPORT_VEN == "Yes, non-invasive" ~ "Non-invasive",
                SUPORT_VEN == "Yes, invasive" ~ "Invasive"
            )
            ) %>%
            count(SUPORT_VEN) %>%
            rename(total = n),
        srag_adults_covid_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            mutate(SUPORT_VEN = case_when(
                # SUPORT_VEN == "No" ~ "None or \n non-invasive",
                # SUPORT_VEN == "Yes, non-invasive" ~ "None or \n non-invasive",
                SUPORT_VEN == "No" ~ "None",
                SUPORT_VEN == "Yes, non-invasive" ~ "Non-invasive",
                SUPORT_VEN == "Yes, invasive" ~ "Invasive"
            )
            ) %>%
            count(SUPORT_VEN) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>% 
    filter(!is.na(SUPORT_VEN)) %>%
    mutate(SUPORT_VEN = factor(SUPORT_VEN,
                               levels = c(
                                          # "None or \n non-invasive",
                                          "None", 
                                          "Non-invasive",
                                          "Invasive"
                               ),
                               labels = c(
                                          # "None or Non-invasive\nrespiratory support",
                                          "None",
                                          "Non-invasive\nrespiratory support",
                                          "Invasive\nrespiratory support"),
                               ordered = TRUE
                               )
           )

## IHM Respiratory Support
df_ihm_mv_age <- 
    left_join(
        srag_adults_covid_outcomes %>%
            mutate(SUPORT_VEN = case_when(
                # SUPORT_VEN == "No" ~ "None or \n non-invasive",
                # SUPORT_VEN == "Yes, non-invasive" ~ "None or \n non-invasive",
                SUPORT_VEN == "No" ~ "None",
                SUPORT_VEN == "Yes, non-invasive" ~ "Non-invasive",
                SUPORT_VEN == "Yes, invasive" ~ "Invasive"
            )
            ) %>%
            count(SUPORT_VEN, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_covid_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            mutate(SUPORT_VEN = case_when(
                # SUPORT_VEN == "No" ~ "None or \n non-invasive",
                # SUPORT_VEN == "Yes, non-invasive" ~ "None or \n non-invasive",
                SUPORT_VEN == "No" ~ "None",
                SUPORT_VEN == "Yes, non-invasive" ~ "Non-invasive",
                SUPORT_VEN == "Yes, invasive" ~ "Invasive"
            )
            ) %>%
            count(SUPORT_VEN, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 1), ")")
    ) %>% 
    filter(!is.na(SUPORT_VEN)) %>%
    mutate(SUPORT_VEN = factor(SUPORT_VEN,
                               levels = c(
                                   # "None or \n non-invasive",
                                   "None", 
                                   "Non-invasive",
                                   "Invasive"
                               ),
                               labels = c(
                                   # "None or Non-invasive\nrespiratory support",
                                   "None",
                                   "Non-invasive\nrespiratory support",
                                   "Invasive\nrespiratory support"),
                               ordered = TRUE
                               )
           )

# Export IHM - Respiratory support
write_csv(
    bind_rows(
        df_ihm_mv,
        df_ihm_mv_age
    ) %>%
        select(SUPORT_VEN, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(SUPORT_VEN)
    , "Outputs/Tables/Sensitivity Analysis/sensitivity_IHM_Gender_Resp_Support.csv"
)


## Plot - IHM per Respiratory support and age
plot_ihm_mv_age <-
    df_ihm_mv_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = SUPORT_VEN),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    # scale_fill_manual(values = c(v_color_regions_alpha[4], 
    #                              v_color_regions[4]
    # ), name = "") +
    scale_fill_manual(values = c(v_color_regions_alpha[4],
                                 v_color_regions_alpha_75[4],
                                 v_color_regions[4]
    ), name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")





### Combining plots
combined_plot_ihm_age_vars <- 
    ggpubr::ggarrange(plot_ihm_gender_age,
                      plot_ihm_comorb_age,
                      plot_ihm_icu_age,
                      plot_ihm_mv_age,
                      nrow = 2,
                      ncol = 2,
                      align = "hv",
                      # common.legend = TRUE, 
                      legend = "bottom", font.label = list(size = 8))


ggsave("Outputs/Figures/sensitivity_IHM_Gender_Comorb_ICU_RespSupport.png",
       combined_plot_ihm_age_vars,
       units = "in", dpi = 800, height = 8, width = 9)



srag_adults_pcr %>% 
    filter(!is.na(n_comorb_mreal)) %>% 
    count(HEMATOLOGI) %>% 
    mutate(prop = round(100*n/sum(n), 0), 
           total = sum(n))
