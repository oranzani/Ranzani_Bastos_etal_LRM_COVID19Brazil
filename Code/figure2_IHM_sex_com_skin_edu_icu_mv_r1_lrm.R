#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 200,000 hospitalizations for COVID-19 in Brazil:an analysis of      ## 
###             nationwide data                                                                          ##
### Coding Otavio Ranzani, Leonardo Bastos, João Gabriel Gelli                                           ##
### October 2020                                                                                         ##
###                                                                                                      ##
### Figure 2. In-hospital mortality stratified by age, sex, comorbidities, level of education,           ##
###             self-reported skin colour, intensive care admission                                      ##
###             and invasive mechanical ventilation for hospitalized COVID-19 patients in Brazil         ## 
###                                                                                                      ##
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







#################################### #################################### #################################
#### Main analysis - In-hospital Mortality - IHM (hospitalizations with a defined outcome)             ####
#################################### #################################### #################################






# In-hospital mortality - Sex ---------------------------------------------

### IHM Sex - Overall
df_ihm_sex <- 
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
    ) %>%
    filter(!is.na(CS_SEXO)) %>% 
    mutate(FAIXA_IDADE = "Overall") 


### IHM Sex - per Age group
df_ihm_sex_age <- 
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
        ) %>%
    filter(!is.na(CS_SEXO))


# Export data - IHM Sex
write_csv(
    bind_rows(
        df_ihm_sex,
        df_ihm_sex_age
        ) %>%
        select(CS_SEXO, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>%
        arrange(CS_SEXO)
    , "Outputs/Tables/In-hospital mortality and Age/IHM_Sex_Age.csv"
    )



### Plot - IHM per gender and age
plot_ihm_sex_age <-
    df_ihm_sex_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = CS_SEXO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c(v_color_regions_alpha_50[1], v_color_regions[1]), name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")









# In-hospital mortality - No. of comorbidities ----------------------------

### IHM No. of comorbidities - Overall
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
    ) %>%
    filter(!is.na(n_comorb_mreal)) %>% 
    mutate(FAIXA_IDADE = "Overall")



### IHM No. of comorbidities - per age group
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
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
    , "Outputs/Tables/In-hospital mortality and Age/IHM_Comorbidities.csv"
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
        values = c(v_color_regions_alpha_50[2],
                   v_color_regions_alpha_75[2],
                   v_color_regions_alpha[2]),
        labels = c("No comorbidities", "1-2", ">= 3"),
        name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")







# In-hospital mortality - ICU admission -----------------------------------

### IHM ICU admission - Overall 
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
    ) %>% 
    filter(!is.na(UTI)) %>%
    mutate(UTI = factor(UTI,
                        levels = c("No", "Yes"),
                        labels = c("No ICU", "ICU")
                        )
           ) %>% 
    mutate(FAIXA_IDADE = "Overall")


## IHM ICU admission - per age group
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
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
    , "Outputs/Tables/In-hospital mortality and Age/IHM_ICU.csv"
    )



## Plot - IHM per ICU admission and age
plot_ihm_icu_age <-
    df_ihm_icu_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = UTI),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha_50[3], 
                                  v_color_regions_alpha[3]), name = "") +
    
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")







# In-hospital mortality - Respiratory Support -----------------------------

## IHM Respiratory Support - Overall
df_ihm_mv <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            mutate(SUPORT_VEN = case_when(
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
    ) %>% 
    filter(!is.na(SUPORT_VEN)) %>%
    mutate(SUPORT_VEN = factor(SUPORT_VEN,
                               levels = c(
                                   "None", 
                                   "Non-invasive",
                                   "Invasive"
                               ),
                               labels = c(
                                   "None",
                                   "Non-invasive\nrespiratory support",
                                   "Invasive\nrespiratory support"),
                               ordered = TRUE
                               )
           )

## IHM Respiratory Support - per age group
df_ihm_mv_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            mutate(SUPORT_VEN = case_when(
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
    ) %>% 
    filter(!is.na(SUPORT_VEN)) %>%
    mutate(SUPORT_VEN = factor(SUPORT_VEN,
                               levels = c(
                                   "None", 
                                   "Non-invasive",
                                   "Invasive"
                               ),
                               labels = c(
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
    , "Outputs/Tables/In-hospital mortality and Age/IHM_Resp_Support.csv"
    )


## Plot - IHM per Respiratory support and age
plot_ihm_mv_age <-
    df_ihm_mv_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = SUPORT_VEN),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c(v_color_regions_alpha_50[4], 
                                 v_color_regions_alpha_75[4],
                                 v_color_regions_alpha[4]
    ), name = "") +
    
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")








# In-hospital mortality - Self-reported skin colour -----------------------

### IHM Skin color - Overall
df_ihm_skin <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(CS_RACA) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_RACA) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), "%)")
    ) %>% 
    filter(!is.na(CS_RACA)) %>%
    mutate(CS_RACA = factor(CS_RACA,
                            levels = c(
                                "White",
                                "Black/Mixed",
                                "Asian",
                                "Indigenous"
                                )
                            )
           )


### IHM Skin color - per age group
df_ihm_skin_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(CS_RACA, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_RACA, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), "%)")
    ) %>% 
    filter(!is.na(CS_RACA)) %>%
    mutate(CS_RACA = factor(CS_RACA,
                            levels = c(
                                "Indigenous",
                                "Asian",
                                "Black/Mixed",
                                "White"
                                )
                            )
           )

# Export IHM - Respiratory support
write_csv(
    bind_rows(
        df_ihm_skin %>% 
            mutate(FAIXA_IDADE = "Overall"),
        df_ihm_skin_age
    ) %>%
        select(CS_RACA, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(CS_RACA)
    , "Outputs/Tables/In-hospital mortality and Age/IHM_Skin_colour.csv"
)


## Plot - IHM per Respiratory support and age
plot_ihm_skin_age <-
    df_ihm_skin_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = CS_RACA),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c(
        v_color_regions_alpha_30[5],
        v_color_regions_alpha_50[5],
        v_color_regions_alpha_75[5],
        v_color_regions_alpha[5]
    ), name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom") 








# In-hospital mortality - Level of education ------------------------------

### IHM level of education - Overall
df_ihm_edu <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(CS_ESCOL_N) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_ESCOL_N) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), "%)")
    ) %>% 
    filter(!is.na(CS_ESCOL_N)) %>%
    mutate(CS_ESCOL_N = factor(CS_ESCOL_N,
                               levels = c(
                                   "Illiterate",
                                   "Up to high school",
                                   "High school",
                                   "College/University"
                                   )
                               )
           )


### IHM level of education - per age group
df_ihm_edu_age <- 
    left_join(
        srag_adults_pcr_outcomes %>%
            count(CS_ESCOL_N, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_pcr_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_ESCOL_N, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), "%)")
    ) %>% 
    filter(!is.na(CS_ESCOL_N)) %>%
    mutate(CS_ESCOL_N = factor(CS_ESCOL_N,
                               levels = c(
                                   "Illiterate",
                                   "Up to high school",
                                   "High school",
                                   "College/University"
                                   )
                               )
           )

# Export IHM - Respiratory support
write_csv(
    bind_rows(
        df_ihm_edu %>% 
            mutate(FAIXA_IDADE = "Overall"),
        df_ihm_edu_age
    ) %>%
        select(CS_ESCOL_N, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(CS_ESCOL_N)
    , "Outputs/Tables/In-hospital mortality and Age/IHM_Education_level.csv"
)


## Plot - IHM per Respiratory support and age
plot_ihm_edu_age <-
    df_ihm_edu_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = CS_ESCOL_N),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c(
        v_color_regions_alpha_30[6],
        v_color_regions_alpha_50[6],
        v_color_regions_alpha_75[6],
        v_color_regions_alpha[6]
        ),
    name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(
        legend.position = "bottom",
    ) 




#### Combining plots
combined_plot_ihm_age <- 
    ggpubr::ggarrange(plot_ihm_sex_age,
                      plot_ihm_comorb_age,
                      plot_ihm_edu_age,
                      plot_ihm_skin_age,
                      plot_ihm_icu_age,
                      plot_ihm_mv_age,
                      nrow = 3,
                      ncol = 2,
                      align = "hv",
                      # common.legend = TRUE, 
                      legend = "bottom", font.label = list(size = 8))


ggsave("Outputs/Figures/Figure2_IHM_Gender_Comorb_ICU_RespSupport_Education_SkinColor.pdf",
       combined_plot_ihm_age,
       units = "in", dpi = 800, height = 13, width = 11)




# finished











#################################### #################################### #################################
#### SENSITIVITY ANALYSIS - In-hospital Mortality - IHM (hospitalizations with a defined outcome)      ####
#### CLASSI_FIN == 5                                                                                   ####
#################################### #################################### #################################


#### Sensitivity analysis - Hospitalizations classified as COVID-19 (CLASSI_FIN == 5)

### SRAG Data (CLASSI_FIN == 5)
# Input data
srag_adults_covid <- vroom::vroom("Data/srag_adults_covid_12_10.csv")

# Hospitalizations with an outcome
srag_adults_covid_outcomes <- 
    srag_adults_covid %>% 
    filter(EVOLUCAO %in% c("Death", "Discharge")) %>% 
    mutate(n_comorb_mreal = factor(n_comorb_mreal, levels = c(0, 1, 2), 
                                   labels = c("No comorbidities", "1-2", ">=3")))




# In-hospital mortality - Sex ---------------------------------------------

### IHM Sex - Overall
df_ihm_sex <- 
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
    ) %>%
    filter(!is.na(CS_SEXO)) %>% 
    mutate(FAIXA_IDADE = "Overall") 


### IHM Sex - per Age group
df_ihm_sex_age <- 
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
    ) %>%
    filter(!is.na(CS_SEXO))


# Export data - IHM Sex
write_csv(
    bind_rows(
        df_ihm_sex,
        df_ihm_sex_age
    ) %>%
        select(CS_SEXO, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>%
        arrange(CS_SEXO)
    , "Outputs/Tables/Sensitivity Analysis/sensitivity_IHM_Sex_Age.csv"
    )



### Plot - IHM per gender and age
plot_ihm_sex_age <-
    df_ihm_sex_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = CS_SEXO),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c(v_color_regions_alpha_50[1], v_color_regions[1]), name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")









# In-hospital mortality - No. of comorbidities ----------------------------

### IHM No. of comorbidities - Overall
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
    ) %>%
    filter(!is.na(n_comorb_mreal)) %>% 
    mutate(FAIXA_IDADE = "Overall")



### IHM No. of comorbidities - per age group
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
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
    , "Outputs/Tables/Sensitivity Analysis/sensitivity_IHM_Comorbidities.csv"
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
        values = c(v_color_regions_alpha_50[2],
                   v_color_regions_alpha_75[2],
                   v_color_regions_alpha[2]),
        labels = c("No comorbidities", "1-2", ">= 3"),
        name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")







# In-hospital mortality - ICU admission -----------------------------------

### IHM ICU admission - Overall 
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
    ) %>% 
    filter(!is.na(UTI)) %>%
    mutate(UTI = factor(UTI,
                        levels = c("No", "Yes"),
                        labels = c("No ICU", "ICU")
    )
    ) %>% 
    mutate(FAIXA_IDADE = "Overall")


## IHM ICU admission - per age group
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
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
    , "Outputs/Tables/Sensitivity Analysis/sensitivity_IHM_ICU.csv"
)



## Plot - IHM per ICU admission and age
plot_ihm_icu_age <-
    df_ihm_icu_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = UTI),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c( v_color_regions_alpha_50[3], 
                                  v_color_regions_alpha[3]), name = "") +
    
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")







# In-hospital mortality - Respiratory Support -----------------------------

## IHM Respiratory Support - Overall
df_ihm_mv <- 
    left_join(
        srag_adults_covid_outcomes %>%
            mutate(SUPORT_VEN = case_when(
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
    ) %>% 
    filter(!is.na(SUPORT_VEN)) %>%
    mutate(SUPORT_VEN = factor(SUPORT_VEN,
                               levels = c(
                                   "None", 
                                   "Non-invasive",
                                   "Invasive"
                               ),
                               labels = c(
                                   "None",
                                   "Non-invasive\nrespiratory support",
                                   "Invasive\nrespiratory support"),
                               ordered = TRUE
    )
    )

## IHM Respiratory Support - per age group
df_ihm_mv_age <- 
    left_join(
        srag_adults_covid_outcomes %>%
            mutate(SUPORT_VEN = case_when(
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
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), ")")
    ) %>% 
    filter(!is.na(SUPORT_VEN)) %>%
    mutate(SUPORT_VEN = factor(SUPORT_VEN,
                               levels = c(
                                   "None", 
                                   "Non-invasive",
                                   "Invasive"
                               ),
                               labels = c(
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
    , "Outputs/Tables/Sensitivity Analysis/sensitivity_IHM_Resp_Support.csv"
    )


## Plot - IHM per Respiratory support and age
plot_ihm_mv_age <-
    df_ihm_mv_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = SUPORT_VEN),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c(v_color_regions_alpha_50[4], 
                                 v_color_regions_alpha_75[4],
                                 v_color_regions_alpha[4]
    ), name = "") +
    
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom")








# In-hospital mortality - Self-reported skin colour -----------------------

### IHM Skin color - Overall
df_ihm_skin <- 
    left_join(
        srag_adults_covid_outcomes %>%
            count(CS_RACA) %>%
            rename(total = n),
        srag_adults_covid_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_RACA) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), "%)")
    ) %>% 
    filter(!is.na(CS_RACA)) %>%
    mutate(CS_RACA = factor(CS_RACA,
                            levels = c(
                                "White",
                                "Black/Mixed",
                                "Asian",
                                "Indigenous"
                                )
                            )
           ) %>% 
    mutate(FAIXA_IDADE = "Overall")


### IHM Skin color - per age group
df_ihm_skin_age <- 
    left_join(
        srag_adults_covid_outcomes %>%
            count(CS_RACA, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_covid_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_RACA, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), "%)")
    ) %>% 
    filter(!is.na(CS_RACA)) %>%
    mutate(CS_RACA = factor(CS_RACA,
                            levels = c(
                                "Indigenous",
                                "Asian",
                                "Black/Mixed",
                                "White"
                                )
                            )
           )

# Export IHM - Respiratory support
write_csv(
    bind_rows(
        df_ihm_skin,
        df_ihm_skin_age
    ) %>%
        select(CS_RACA, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(CS_RACA)
    , "Outputs/Tables/Sensitivity Analysis/sensitivity_IHM_Skin_colour.csv"
    )


## Plot - IHM per Respiratory support and age
plot_ihm_skin_age <-
    df_ihm_skin_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = CS_RACA),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c(
        v_color_regions_alpha_30[5],
        v_color_regions_alpha_50[5],
        v_color_regions_alpha_75[5],
        v_color_regions_alpha[5]
    ), name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom") 








# In-hospital mortality - Level of education ------------------------------

### IHM level of education - Overall
df_ihm_edu <- 
    left_join(
        srag_adults_covid_outcomes %>%
            count(CS_ESCOL_N) %>%
            rename(total = n),
        srag_adults_covid_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_ESCOL_N) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), "%)")
    ) %>% 
    filter(!is.na(CS_ESCOL_N)) %>%
    mutate(CS_ESCOL_N = factor(CS_ESCOL_N,
                               levels = c(
                                   "Illiterate",
                                   "Up to high school",
                                   "High school",
                                   "College/University"
                                   )
                               )
           ) %>% 
    mutate(FAIXA_IDADE = "Overall")


### IHM level of education - per age group
df_ihm_edu_age <- 
    left_join(
        srag_adults_covid_outcomes %>%
            count(CS_ESCOL_N, FAIXA_IDADE) %>%
            rename(total = n),
        srag_adults_covid_outcomes %>%
            filter(EVOLUCAO == "Death") %>%
            count(CS_ESCOL_N, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), "%)")
    ) %>% 
    filter(!is.na(CS_ESCOL_N)) %>%
    mutate(CS_ESCOL_N = factor(CS_ESCOL_N,
                               levels = c(
                                   "Illiterate",
                                   "Up to high school",
                                   "High school",
                                   "College/University"
                               )
    )
    )

# Export IHM - Respiratory support
write_csv(
    bind_rows(
        df_ihm_edu,
        df_ihm_edu_age
    ) %>%
        select(CS_ESCOL_N, FAIXA_IDADE, total, deaths, in_hosp_mortal, ihm_ratio) %>% 
        arrange(CS_ESCOL_N)
    , "Outputs/Tables/Sensitivity Analysis/sensitivity_IHM_Education_level.csv"
    )


## Plot - IHM per Respiratory support and age
plot_ihm_edu_age <-
    df_ihm_edu_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = CS_ESCOL_N),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_manual(values = c(
        v_color_regions_alpha_30[6],
        v_color_regions_alpha_50[6],
        v_color_regions_alpha_75[6],
        v_color_regions_alpha[6]
    ),
    name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() +
    theme(
        legend.position = "bottom",
    ) 




#### Combining plots
combined_plot_ihm_age <- 
    ggpubr::ggarrange(plot_ihm_sex_age,
                      plot_ihm_comorb_age,
                      plot_ihm_edu_age,
                      plot_ihm_skin_age,
                      plot_ihm_icu_age,
                      plot_ihm_mv_age,
                      nrow = 3,
                      ncol = 2,
                      align = "hv",
                      # common.legend = TRUE, 
                      legend = "bottom", font.label = list(size = 8))



ggsave("Outputs/Figures/sensitivity_HM_Gender_Comorb_ICU_RespSupport_Education_SkinColor.png",
       combined_plot_ihm_age,
       units = "in", dpi = 800, height = 13, width = 11)



# finished

