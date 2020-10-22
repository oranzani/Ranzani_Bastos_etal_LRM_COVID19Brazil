# Sys.setlocale("LC_TIME", "English")

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(tidylog)


# Population data - Brazil and Regions ------------------------------------

# Input data from brazilian population and for each region
# data is from IBGE - Brazilian Institute of Geography and Statisics
# and corresponds to the population projection for 2020 performed by the institute
# in 2018 ()
# Data was pre-processed in a single csv containing used information from Brazil
# and its five Regions

df_pop_ibge_2020 <- 
    read_csv2("Data/pop_ibge_proj_2020_gender.csv") %>%
    mutate(FAIXA_IDADE =
               case_when(
                   IDADE <= 19 ~ "<20",
                   IDADE <= 39 ~ "20-39",
                   IDADE <= 49 ~ "40-49",
                   IDADE <= 59 ~ "50-59",
                   IDADE <= 69 ~ "60-69",
                   IDADE <= 79 ~ "70-79",
                   TRUE ~ "80+")
    )





### Total population per age group + Proportion of females -> Brazil and Regions 

## Total projected population and adult population - Brazil and Regions
df_pop_ibge_2020_population <- 
    bind_rows(
        df_pop_ibge_2020 %>%
            bind_rows(
                df_pop_ibge_2020 %>%
                    mutate(REGIAO = "Brazil")
                ) %>%
            group_by(REGIAO) %>% 
            summarise(population = sum(POPULACAO)) %>% 
            mutate(variable = "population"),
        df_pop_ibge_2020 %>%
            bind_rows(
                df_pop_ibge_2020 %>%
                    mutate(REGIAO = "Brazil")
            ) %>%
            filter(IDADE >= 20) %>% 
            group_by(REGIAO) %>% 
            summarise(population = sum(POPULACAO)) %>% 
            mutate(variable = "population_adult")
        ) %>% 
    pivot_wider(names_from = "REGIAO", values_from = "population")
    
    


## Age, Median (IQR) - Brazil and Regions - Total population
df_pop_ibge_2020_med_age <- 
    df_pop_ibge_2020 %>% 
    bind_rows(
        df_pop_ibge_2020 %>%
            mutate(REGIAO = "Brazil")
    ) %>%
    select(REGIAO, IDADE, POPULACAO) %>%
    # filter(IDADE >= 20) %>%
    uncount(POPULACAO) %>% 
    group_by(REGIAO) %>% 
    summarise(MEAN_AGE_ADULTS = paste0(round(mean(IDADE), 1), " (", round(sd(IDADE), 1),")")) %>% 
    pivot_wider(names_from = "REGIAO", values_from = "MEAN_AGE_ADULTS") %>% 
    mutate(FAIXA_IDADE = "Mean_Age_Total") %>% 
    rename(variable = FAIXA_IDADE)


## Age, Median (IQR) - Brazil and Regions - ADULT POPULATIONS
df_pop_ibge_2020_med_age_adults <- 
    df_pop_ibge_2020 %>% 
    bind_rows(
        df_pop_ibge_2020 %>%
            mutate(REGIAO = "Brazil")
    ) %>%
    select(REGIAO, IDADE, POPULACAO) %>%
    filter(IDADE >= 20) %>%
    uncount(POPULACAO) %>% 
    group_by(REGIAO) %>% 
    summarise(MEAN_AGE_ADULTS = paste0(round(mean(IDADE), 1), " (", round(sd(IDADE), 1),")")) %>% 
    pivot_wider(names_from = "REGIAO", values_from = "MEAN_AGE_ADULTS") %>% 
    mutate(FAIXA_IDADE = "Mean_Age_Adults") %>% 
    rename(variable = FAIXA_IDADE)


## Age groups - Brazil and Regions 
df_pop_ibge_2020_age_group <- 
    df_pop_ibge_2020 %>% 
    bind_rows(
        df_pop_ibge_2020 %>% 
            mutate(REGIAO = "Brazil")
    ) %>% 
    group_by(REGIAO, FAIXA_IDADE) %>% 
    summarise(POP_TOTAL = sum(POPULACAO)) %>% 
    ungroup() %>% 
    mutate(REGIAO = factor(REGIAO, 
                           levels = c("Brazil", "North", "Northeast", "Central-West", "Southeast", "South")
                           )
           ) %>% 
    pivot_wider(names_from = "REGIAO", values_from = "POP_TOTAL") %>%
    mutate(FAIXA_IDADE = paste0("Age_", FAIXA_IDADE)) %>% 
    rename(variable = FAIXA_IDADE) 

    

## Proportion of females - Brazil and Regions
df_pop_ibge_2020_females <-
    df_pop_ibge_2020 %>% 
    bind_rows(
        df_pop_ibge_2020 %>% 
            mutate(REGIAO = "Brazil")
    ) %>% 
    group_by(REGIAO) %>% 
    summarise(PROP_TOTAL_F = sum(POPULACAO_F) / sum(POPULACAO)) %>% 
    ungroup() %>% 
    mutate(REGIAO = factor(REGIAO, 
                           levels = c("Brazil", "North", "Northeast", "Central-West", "Southeast", "South"))
    ) %>% 
    pivot_wider(names_from = "REGIAO", values_from = "PROP_TOTAL_F") %>% 
    mutate(FAIXA_IDADE = "Proprotion_Females") %>% 
    rename(variable = FAIXA_IDADE)



df_pop_ibge_stat_geral <- 
    tibble(
        rbind(
            df_pop_ibge_2020_population,
            df_pop_ibge_2020_med_age,
            df_pop_ibge_2020_med_age_adults,
            df_pop_ibge_2020_age_group,
            df_pop_ibge_2020_females
            )
    ) %>% 
    select(variable, 
           Brazil,
           North,
           Northeast,
           `Central-West`,
           Southeast,
           South)







write_csv(df_pop_ibge_stat_geral, "Outputs/Tables/Data_Brazil_Population_Char.csv")




