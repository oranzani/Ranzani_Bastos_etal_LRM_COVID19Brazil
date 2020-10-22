#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 200,000 hospitalizations for COVID-19 in Brazil: a nationwide study ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
### Figure and DATA mortality overtime                                                                   ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################


library(tidyverse)
library(tidylog)

#### importing previous cleanned database
srag_adults_pcr_12_10 <- read_csv("Data/srag_adults_pcr_12_10.csv")


#### generating db to fill in the figure, by week of symptoms onset
week_cases_outcome <- srag_adults_pcr_12_10 %>% 
  filter(EVOLUCAO != "Ongoing") %>% 
  mutate(sem_4 = floor(SEM_PRI/4)) %>% 
  group_by(REGIAO, sem_4) %>% 
  summarise(hospitalizations = n())

week_cases_outcome_br <- srag_adults_pcr_12_10 %>% 
  filter(EVOLUCAO != "Ongoing") %>% 
  mutate(sem_4 = floor(SEM_PRI/4)) %>% 
  group_by(sem_4) %>% 
  summarise(hospitalizations = n()) %>% 
  mutate(REGIAO = "Brazil")

week_cases_outcome <- bind_rows(week_cases_outcome, week_cases_outcome_br) %>% 
  ungroup() %>% 
  mutate(REGIAO = as_factor(REGIAO),
         REGIAO = fct_relevel(REGIAO, "Brazil", "North", "Northeast", "Central-West",
                              "Southeast", "South"))


####### calculating  hospitalization rates
df_pop_ibge_2020 <- read_csv2("Data/pop_ibge_proj_2020.csv") %>%
  filter(IDADE >= 20) %>% 
  group_by(REGIAO) %>% 
  summarise(POP_ADULTS = sum(POPULACAO)) %>% 
  ungroup()


df_pop_ibge_2020_br <- read_csv2("Data/pop_ibge_proj_2020.csv") %>%
  filter(IDADE >= 20) %>% 
  summarise(POP_ADULTS = sum(POPULACAO)) %>% 
  ungroup() %>% mutate(REGIAO = "Brazil")


df_pop_ibge_2020 <- bind_rows(df_pop_ibge_2020, df_pop_ibge_2020_br) %>% 
  mutate(REGIAO = as_factor(REGIAO),
         REGIAO = fct_relevel(REGIAO, "Brazil", "North", "Northeast", "Central-West",
                              "Southeast", "South"))

week_cases_outcome <- week_cases_outcome %>% left_join(df_pop_ibge_2020, by = "REGIAO")
week_cases_outcome <- week_cases_outcome %>% 
  mutate(hosp_rate = hospitalizations/POP_ADULTS*100000)

#### deaths
week_deaths_outcome <- srag_adults_pcr_12_10 %>% 
                       filter(EVOLUCAO != "Ongoing") %>% 
                       mutate(sem_4 = floor(SEM_PRI/4)) %>% 
                       group_by(REGIAO, sem_4) %>% 
                       count(EVOLUCAO) %>% 
                       pivot_wider(names_from = "EVOLUCAO", 
                                   values_from = n) %>% 
                       replace(is.na(.), 0) %>% 
                       mutate(total = Discharge + Death)

week_deaths_outcome_br <- srag_adults_pcr_12_10 %>% 
  filter(EVOLUCAO != "Ongoing") %>% 
  mutate(sem_4 = floor(SEM_PRI/4)) %>% 
  group_by(sem_4) %>% 
  count(EVOLUCAO) %>% 
  pivot_wider(names_from = "EVOLUCAO", 
              values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total = Discharge + Death,
         REGIAO = "Brazil")
  

week_deaths_outcome <- bind_rows(week_deaths_outcome, week_deaths_outcome_br)
week_deaths_outcome <- week_deaths_outcome %>% 
  mutate(REGIAO = as_factor(REGIAO),
         REGIAO = fct_relevel(REGIAO, "Brazil", "North", "Northeast", "Central-West",
                              "Southeast", "South"))
  


week_deaths_outcome <- week_deaths_outcome %>% 
                       ungroup() %>% 
                       mutate(prop  = binom::binom.agresti.coull(week_deaths_outcome$Death, week_deaths_outcome$total,conf.level = 0.95)$mean,
                              lower = binom::binom.agresti.coull(week_deaths_outcome$Death, week_deaths_outcome$total,conf.level = 0.95)$lower,
                              upper = binom::binom.agresti.coull(week_deaths_outcome$Death, week_deaths_outcome$total,conf.level = 0.95)$upper) %>% 
  mutate(REGIAO = factor(REGIAO, levels =  c("Brazil", "North", "Northeast", "Central-West",
                              "Southeast", "South"), ordered = T)
  )



v_color_regions <- c("#42B540FF","#FDAF91FF", "#AD002AFF", 
                     "#00468BFF", "#925E9FFF")

A <- week_deaths_outcome %>% 
  filter(REGIAO != "Brazil") %>% 
  ggplot(aes(x = sem_4)) +
  geom_line(aes(y = prop, col = REGIAO), size = 1.05) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = REGIAO), alpha = 0.5) +
  facet_wrap(~REGIAO, ncol = 1) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Epidemiological weeks", y = "In-hospital mortality (%)") +
  scale_y_continuous(breaks = seq(0,0.7,0.1), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 2:8, 
                     labels = c("8-12", "13-16", "17-20",
                                "21-23", "24-27", "28-31",
                                "32-33")) +
  scale_fill_manual(values = v_color_regions) +
  scale_color_manual(values = v_color_regions)
A


B <- week_cases_outcome %>% 
  filter(REGIAO != "Brazil") %>% 
  ggplot(aes(x = sem_4, y = hosp_rate, fill = REGIAO)) +
  stat_smooth(geom = "area", span = 0.3, method = "loess", alpha = 0.8) +
  facet_wrap(~REGIAO, ncol = 1, scales = "fixed") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Epidemiological weeks", y = "Hospitalizations (per 100.000)") +
  scale_y_continuous(breaks = seq(0,50, 10)) +
  scale_x_continuous(breaks = 2:8, 
                     labels = c("8-12", "13-16", "17-20",
                                "21-23", "24-27", "28-31",
                                "32-33")) +
  scale_fill_manual(values = v_color_regions) +
  scale_color_manual(values = v_color_regions)

cowplot::plot_grid(B,A, ncol = 2)




### Combining plots
combined_plot_figure2 <- 
  ggpubr::ggarrange(B,
                    A,
                    nrow = 1,
                    ncol = 2,
                    align = "hv",
                    legend = "none", font.label = list(size = 8))


ggsave("Outputs/Figures/Figure_3.pdf",
       combined_plot_figure2,
       units = "in", dpi = 800, height = 8, width = 6)


### age
srag_adults_pcr_12_10 %>% 
  filter(EVOLUCAO != "Ongoing") %>% 
  mutate(sem_4 = floor(SEM_PRI/4),
         REGIAO = as_factor(REGIAO),
         REGIAO = fct_relevel(REGIAO, "North", "Northeast", "Central-West",
                              "Southeast", "South"),
         NU_IDADE_N = ifelse(NU_IDADE_N > 120, 36, NU_IDADE_N)) %>% #typo
  ggplot(aes(x = factor(sem_4), y = NU_IDADE_N, fill = REGIAO), alpha = 0.6) +
  geom_boxplot() +
  facet_wrap(~REGIAO, ncol = 1) +
  scale_fill_manual(values = v_color_regions) +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Epidemiological weeks", y = "Age (years)") +
  scale_x_discrete(labels = c("8-12", "13-16", "17-20",
                              "21-23", "24-27", "28-31",
                              "32-33"))
