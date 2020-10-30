#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 200,000 hospitalizations for COVID-19 in Brazil: a nationwide study ##
### Coding Otavio Ranzani, Leonardo Bastos, João Gabriel Gelli                                           ##
### October 2020                                                                                         ##
###                                                                                                      ##
## Figure 4. Proportion of intensive care unit admission and use of mechanical ventilation              ##
### stratified by age in hospitalized COVID-19 patients in the five regions of Brazil                    ##    
###                                                                                                      ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidylog)



# Proportion of ICU and MV per region and age (main analysis) -------------
### SRAG Data - Hospitalizations
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




# Proportions of ICU admissions and MV ------------------------------------
df_proportion_total <- 
  bind_rows(
    srag_adults_pcr_outcome %>%
      bind_rows(
        srag_adults_pcr_outcome %>% 
          mutate(REGIAO = "Brazil")
      ) %>% 
      filter(!is.na(REGIAO), !is.na(SUPORT_VEN)) %>%
      count(REGIAO, SUPORT_VEN) %>%
      group_by(REGIAO) %>%
      mutate(total = sum(n), 
             prop = n / total
      ) %>%
      ungroup() %>% 
      filter(SUPORT_VEN == "Yes, invasive") %>% 
      mutate(type = "Mechanical Ventilation") %>% 
      select(REGIAO, type, n, total, prop),
    
    srag_adults_pcr_outcome %>%
      bind_rows(
        srag_adults_pcr_outcome %>% 
          mutate(REGIAO = "Brazil")
      ) %>%
      filter(!is.na(REGIAO), !is.na(UTI)) %>%
      count(REGIAO, UTI) %>%
      group_by(REGIAO) %>%
      mutate(total = sum(n), 
             prop = n / total
      ) %>%
      ungroup() %>% 
      filter(UTI == "Yes") %>% 
      mutate(type = "ICU") %>% 
      select(REGIAO, type, n, total, prop)
  ) %>% 
  mutate(
    ratio = paste0(n, "/", total, " (", round(100 * (n / total), 1), "%)")
  ) %>% 
  mutate(FAIXA_IDADE = "Overall")




df_proportion_age <- 
  bind_rows(
    srag_adults_pcr_outcome %>%
      bind_rows(
        srag_adults_pcr_outcome %>% 
          mutate(REGIAO = "Brazil")
      ) %>% 
      filter(!is.na(FAIXA_IDADE), !is.na(REGIAO), !is.na(SUPORT_VEN)) %>%
      count(REGIAO, FAIXA_IDADE, SUPORT_VEN) %>%
      group_by(REGIAO, FAIXA_IDADE) %>%
      mutate(total = sum(n), 
             prop = n / total
      ) %>%
      ungroup() %>% 
      filter(SUPORT_VEN == "Yes, invasive") %>% 
      mutate(type = "Mechanical Ventilation") %>% 
      select(REGIAO, FAIXA_IDADE, type, n, total, prop),
    
    srag_adults_pcr_outcome %>%
      bind_rows(
        srag_adults_pcr_outcome %>% 
          mutate(REGIAO = "Brazil")
      ) %>%
      filter(!is.na(FAIXA_IDADE), !is.na(REGIAO), !is.na(UTI)) %>%
      count(REGIAO, FAIXA_IDADE, UTI) %>%
      group_by(REGIAO, FAIXA_IDADE) %>%
      mutate(total = sum(n), 
             prop = n / total
      ) %>%
      ungroup() %>% 
      filter(UTI == "Yes") %>% 
      mutate(type = "ICU") %>% 
      select(REGIAO, FAIXA_IDADE, type, n, total, prop)
  ) %>% 
  mutate(
    ratio = paste0(n, "/", total, " (", round(100 * (n / total), 1), "%)")
  ) 

write_csv(
  bind_rows(  
    df_proportion_total,
    df_proportion_age
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
    arrange(type, REGIAO)
    
  , "Outputs/Tables/Proportions of ICU and MV/Proportion_ICU_MV_regions.csv"
)









## Plot IHM ICU
plot_proportion_age_icu_mv_north <-
  df_proportion_age %>%
  filter(REGIAO == "North") %>% 
  ggplot() +
  geom_col(aes(x = FAIXA_IDADE, y = prop, fill = type), position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(NA, 0.6),
                     breaks = seq(0, 0.6, 0.1)
                     ) +
  scale_fill_manual(name = "", 
                    values = c(v_color_regions[1],
                               v_color_regions_alpha[1]),
                    labels = c("ICU", "Invasive ventilation"),
                    guide = FALSE
                    ) +
  labs(x = "Age (years)", title = "North", y = "Proportion") +
  theme_bw()



plot_proportion_age_icu_mv_northeast <-
  df_proportion_age %>%
  filter(REGIAO == "Northeast") %>% 
  ggplot() +
  geom_col(aes(x = FAIXA_IDADE, y = prop, fill = type), position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(NA, 0.6),
                     breaks = seq(0, 0.6, 0.1)
                     ) +
  scale_fill_manual(name = "", 
                    values = c(v_color_regions[2],
                               v_color_regions_alpha[2]),
                    labels = c("ICU", "Invasive ventilation"),
                    guide = FALSE
                    ) +
  labs(x = "Age (years)", title = "Northeast", y = "Proportion") +
  theme_bw()



plot_proportion_age_icu_mv_central_west <-
  df_proportion_age %>%
  filter(REGIAO == "Central-West") %>% 
  ggplot() +
  geom_col(aes(x = FAIXA_IDADE, y = prop, fill = type), position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(NA, 0.6),
                     breaks = seq(0, 0.6, 0.1)
                     ) +
  scale_fill_manual(name = "", 
                    values = c(v_color_regions[3],
                               v_color_regions_alpha[3]),
                    labels = c("ICU", "Invasive ventilation"),
                    guide = FALSE
                    ) +
  labs(x = "Age (years)", title = "Central-West", y = "Proportion") +
  theme_bw()



plot_proportion_age_icu_mv_southeast <-
  df_proportion_age %>%
  filter(REGIAO == "Southeast") %>% 
  ggplot() +
  geom_col(aes(x = FAIXA_IDADE, y = prop, fill = type), position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(NA, 0.6),
                     breaks = seq(0, 0.6, 0.1)
                     ) +
  scale_fill_manual(name = "", 
                    values = c(v_color_regions[4],
                               v_color_regions_alpha[4]),
                    labels = c("ICU", "Invasive ventilation"),
                    guide = FALSE
                    ) +
  labs(x = "Age (years)", title = "Southeast", y = "Proportion") +
  theme_bw()



plot_proportion_age_icu_mv_south <-
  df_proportion_age %>%
  filter(REGIAO == "South") %>% 
  ggplot() +
  geom_col(aes(x = FAIXA_IDADE, y = prop, fill = type), position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(NA, 0.6), 
                     breaks = seq(0, 0.6, 0.1)
                     ) +
  scale_fill_manual(name = "", 
                    values = c(v_color_regions[5],
                               v_color_regions_alpha[5]),
                    labels = c("ICU", "Invasive ventilation"),
                    guide = FALSE
                    ) +
  labs(x = "Age (years)", title = "South", y = "Proportion") +
  theme_bw()





combined_plot_A <- ggpubr::ggarrange(plot_proportion_age_icu_mv_north,
                                     plot_proportion_age_icu_mv_northeast,
                                     plot_proportion_age_icu_mv_central_west,
                                     # plot_proportion_age_icu_mv_southeast,
                                     # plot_proportion_age_icu_mv_south,
                                     nrow = 1,
                                     ncol = 3,
                                     align = "v",
                                     # common.legend = TRUE, 
                                     legend = "top", 
                                     font.label = list(size = 8))



combined_plot_B <- ggpubr::ggarrange(plot_proportion_age_icu_mv_southeast,
                                     plot_proportion_age_icu_mv_south,
                                     nrow = 1,
                                     ncol = 2,
                                     align = "v",
                                     # common.legend = TRUE,
                                     legend = "top",
                                     font.label = list(size = 8)) +
  theme(plot.margin = unit(c(0,110,0,110), "pt"))


###### Auxiliary - Example plot with gray legend (Auxiliary plot variable)
plot_prop_legend <-
  df_proportion_age %>%
  filter(REGIAO == "South") %>% 
  ggplot() +
  geom_col(aes(x = FAIXA_IDADE, y = prop, fill = type), position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(NA, 0.6), 
                     breaks = seq(0, 0.6, 0.1)
  ) +
  scale_fill_manual(name = "", 
                    values = c("#1B1919B2",
                               "#1B191966"),
                    labels = c("ICU", "Invasive ventilation")
  ) +
  labs(x = "Age (years)", title = "South", y = "Proportion") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14))

# get legend model
legend <- cowplot::get_legend(plot_prop_legend)


# combine plots and legend
combined_plot_region_prop <-
  ggpubr::ggarrange(
    combined_plot_A,
    combined_plot_B,
    legend,
    nrow = 3,
    heights = c(1, 1, 0.05),
    legend.grob = legend,
    legend = "bottom",
    font.label = list(size = 8)
    )



ggsave("Outputs/Figures/Figure5_ICU_MV_Proportion.pdf",
       combined_plot_region_prop,
       units = "in", dpi = 800, height = 9, width = 13)








###########################################################################
###########################################################################
###########################################################################
###########################################################################

# Sensitivity analysis ----------------------------------------------------


### SRAG Data - Hospitalizations
srag_adults_covid <- vroom::vroom("Data/srag_adults_covid_12_10.csv")


srag_adults_covid_outcome <- 
  srag_adults_covid %>% 
  filter(EVOLUCAO %in% c("Death", "Discharge"))





# Proportions of ICU admissions and MV ------------------------------------
df_proportion_total <- 
  bind_rows(
    srag_adults_covid_outcome %>%
      bind_rows(
        srag_adults_covid_outcome %>% 
          mutate(REGIAO = "Brazil")
      ) %>% 
      filter(!is.na(REGIAO), !is.na(SUPORT_VEN)) %>%
      count(REGIAO, SUPORT_VEN) %>%
      group_by(REGIAO) %>%
      mutate(total = sum(n), 
             prop = n / total
      ) %>%
      ungroup() %>% 
      filter(SUPORT_VEN == "Yes, invasive") %>% 
      mutate(type = "Mechanical Ventilation") %>% 
      select(REGIAO, type, n, total, prop),
    
    srag_adults_covid_outcome %>%
      bind_rows(
        srag_adults_covid_outcome %>% 
          mutate(REGIAO = "Brazil")
      ) %>%
      filter(!is.na(REGIAO), !is.na(UTI)) %>%
      count(REGIAO, UTI) %>%
      group_by(REGIAO) %>%
      mutate(total = sum(n), 
             prop = n / total
      ) %>%
      ungroup() %>% 
      filter(UTI == "Yes") %>% 
      mutate(type = "ICU") %>% 
      select(REGIAO, type, n, total, prop)
  ) %>% 
  mutate(
    ratio = paste0(n, "/", total, " (", round(100 * (n / total), 1), "%)")
  ) %>% 
  mutate(FAIXA_IDADE = "Overall")

# per age group
df_proportion_age <- 
  bind_rows(
    srag_adults_covid_outcome %>%
      bind_rows(
        srag_adults_covid_outcome %>% 
          mutate(REGIAO = "Brazil")
      ) %>% 
      filter(!is.na(FAIXA_IDADE), !is.na(REGIAO), !is.na(SUPORT_VEN)) %>%
      count(REGIAO, FAIXA_IDADE, SUPORT_VEN) %>%
      group_by(REGIAO, FAIXA_IDADE) %>%
      mutate(total = sum(n), 
             prop = n / total
      ) %>%
      ungroup() %>% 
      filter(SUPORT_VEN == "Yes, invasive") %>% 
      mutate(type = "Mechanical Ventilation") %>% 
      select(REGIAO, FAIXA_IDADE, type, n, total, prop),
    
    srag_adults_covid_outcome %>%
      bind_rows(
        srag_adults_covid_outcome %>% 
          mutate(REGIAO = "Brazil")
      ) %>%
      filter(!is.na(FAIXA_IDADE), !is.na(REGIAO), !is.na(UTI)) %>%
      count(REGIAO, FAIXA_IDADE, UTI) %>%
      group_by(REGIAO, FAIXA_IDADE) %>%
      mutate(total = sum(n), 
             prop = n / total
      ) %>%
      ungroup() %>% 
      filter(UTI == "Yes") %>% 
      mutate(type = "ICU") %>% 
      select(REGIAO, FAIXA_IDADE, type, n, total, prop)
  ) %>% 
  mutate(
    ratio = paste0(n, "/", total, " (", round(100 * (n / total), 1), "%)")
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
  )

write_csv(
  bind_rows(  
    df_proportion_total,
    df_proportion_age
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
    arrange(type, REGIAO)
  , "Outputs/Tables/Sensitivity Analysis/sensitivity_proportion_ICU_MV_regions.csv"
)









## Plot IHM ICU
plot_proportion_age_icu_mv_north <-
  df_proportion_age %>%
  filter(REGIAO == "North") %>% 
  ggplot() +
  geom_col(aes(x = FAIXA_IDADE, y = prop, fill = type), position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(NA, 0.6),
                     breaks = seq(0, 0.6, 0.1)
  ) +
  scale_fill_manual(name = "", 
                    values = c(v_color_regions[1],
                               v_color_regions_alpha[1]),
                    labels = c("ICU", "Invasive ventilation"),
                    guide = FALSE
  ) +
  labs(x = "Age (years)", title = "North", y = "Proportion") +
  theme_bw()



plot_proportion_age_icu_mv_northeast <-
  df_proportion_age %>%
  filter(REGIAO == "Northeast") %>% 
  ggplot() +
  geom_col(aes(x = FAIXA_IDADE, y = prop, fill = type), position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(NA, 0.6),
                     breaks = seq(0, 0.6, 0.1)
  ) +
  scale_fill_manual(name = "", 
                    values = c(v_color_regions[2],
                               v_color_regions_alpha[2]),
                    labels = c("ICU", "Invasive ventilation"),
                    guide = FALSE
  ) +
  labs(x = "Age (years)", title = "Northeast", y = "Proportion") +
  theme_bw()



plot_proportion_age_icu_mv_central_west <-
  df_proportion_age %>%
  filter(REGIAO == "Central-West") %>% 
  ggplot() +
  geom_col(aes(x = FAIXA_IDADE, y = prop, fill = type), position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(NA, 0.6),
                     breaks = seq(0, 0.6, 0.1)
  ) +
  scale_fill_manual(name = "", 
                    values = c(v_color_regions[3],
                               v_color_regions_alpha[3]),
                    labels = c("ICU", "Invasive ventilation"),
                    guide = FALSE
  ) +
  labs(x = "Age (years)", title = "Central-West", y = "Proportion") +
  theme_bw()



plot_proportion_age_icu_mv_southeast <-
  df_proportion_age %>%
  filter(REGIAO == "Southeast") %>% 
  ggplot() +
  geom_col(aes(x = FAIXA_IDADE, y = prop, fill = type), position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(NA, 0.6),
                     breaks = seq(0, 0.6, 0.1)
  ) +
  scale_fill_manual(name = "", 
                    values = c(v_color_regions[4],
                               v_color_regions_alpha[4]),
                    labels = c("ICU", "Invasive ventilation"),
                    guide = FALSE
  ) +
  labs(x = "Age (years)", title = "Southeast", y = "Proportion") +
  theme_bw()



plot_proportion_age_icu_mv_south <-
  df_proportion_age %>%
  filter(REGIAO == "South") %>% 
  ggplot() +
  geom_col(aes(x = FAIXA_IDADE, y = prop, fill = type), position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(NA, 0.6), 
                     breaks = seq(0, 0.6, 0.1)
  ) +
  scale_fill_manual(name = "", 
                    values = c(v_color_regions[5],
                               v_color_regions_alpha[5]),
                    labels = c("ICU", "Invasive ventilation"),
                    guide = FALSE
  ) +
  labs(x = "Age (years)", title = "South", y = "Proportion") +
  theme_bw()






combined_plot_region_prop <- 
  ggpubr::ggarrange(plot_proportion_age_icu_mv_north,
                    plot_proportion_age_icu_mv_northeast,
                    plot_proportion_age_icu_mv_central_west,
                    plot_proportion_age_icu_mv_southeast,
                    plot_proportion_age_icu_mv_south,
                    nrow = 2,
                    ncol = 3,
                    align = "v",
                    # common.legend = TRUE, 
                    legend = "top", 
                    font.label = list(size = 8))



ggsave("Outputs/Figures/sensitivity_ICU_MV_Proportion.png",
       combined_plot_region_prop,
       units = "in", dpi = 800, height = 9, width = 13)

# finished









# finished
