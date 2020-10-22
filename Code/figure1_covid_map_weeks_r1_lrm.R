#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 200,000 hospitalizations for COVID-19 in Brazil: a nationwide study ##
### Coding Otavio Ranzani, Leonardo Bastos, João Gabriel Gelli                                           ##
### October 2020                                                                                         ##
###                                                                                                      ##
## Figure 1. Epidemic evolution showed during three-time frames in Brazil                                ##    
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidylog)
library(geobr)
library(sf)


# Input data --------------------------------------------------------------
### Map shapefiles
map_year <- 2018
shp_brasil_mun <- read_municipality(year = map_year)

shp_brasil_uf <- read_state(year = map_year)

shp_brasil_reg <- read_region(year = map_year) %>% 
    mutate(
        name_region = case_when(
            name_region == "Norte" ~ "North",
            name_region == "Nordeste" ~ "Northeast",
            name_region == "Centro Oeste" ~ "Central-West",
            name_region == "Sudeste" ~ "Southeast",
            name_region == "Sul" ~ "South"
        )
    ) 
    

## Brazilian City data
file_br_cities <- "https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/main/csv/municipios.csv"

## Brazil COVID-19 confirmed cases data - Brasil.IO
file_br_covid19 <- "https://data.brasil.io/dataset/covid19/caso_full.csv.gz"

## Brazil SARI COVID data - hospitalizations
file_br_srag_covid <- "Data/srag_adults_pcr_12_10.csv"


### City Shapefile + Lat/Long Information (github.com/kelvins)
df_br_cities <-
    read_csv(file_br_cities) %>%
    mutate(codigo_ibge_6dig = str_sub(codigo_ibge, 1, 6)) %>% 
    mutate(
        REGIAO = case_when(
            codigo_uf %in% c(11, 12, 13, 14, 15, 16, 17) ~ "North",
            codigo_uf %in% c(21, 22, 23, 24, 25, 26, 27, 28, 29) ~ "Northeast",
            codigo_uf %in% c(50, 51, 52, 53) ~ "Central-West",
            codigo_uf %in% c(31, 32, 33, 35) ~ "Southeast",
            codigo_uf %in% c(41, 42, 43) ~ "South"
            )
        )
## Shapefile object with city information
shp_brasil_mun_centr <- 
    shp_brasil_mun %>% 
    left_join(
        df_br_cities %>% 
            select(codigo_ibge, codigo_ibge_6dig, latitude, longitude, capital, REGIAO),
        by = c("code_muni" = "codigo_ibge")
        )



### COVID-19 Data
### Brasil IO - Total Cases - Cities
df_covid_cases_total <- 
    read_csv(file_br_covid19) %>% 
    filter(place_type == "city") %>% 
    mutate(REGIAO = case_when(
        state  %in% c("SP", "RJ", "ES", "MG") ~ "Southeast",
        state  %in% c("SC", "RS", "PR") ~ "South",
        state  %in% c("MT", "MS", "GO", "DF") ~ "Central-West",
        state  %in% c("AM", "AP", "TO", "PA", "RO", "RR", "AC") ~ "North",
        state  %in% c("BA", "AL", "SE", "PE", "MA", "RN", "PB", "CE", "PI") ~ "Northeast"
        )
    )




### SRAG Data - Hospitalizations
srag_adults_pcr <- 
    vroom::vroom(file_br_srag_covid) 






# Map - COVID Total -------------------------------------------------------
## Set of epidemiological weeks for analysis
ls_weeks <- 
    list(
        c(8, 12),
        c(19, 22),
        c(27, 30)
        )


## Lists for saving plot objects
ls_covid_plots <- list() # Plots with COVID-19 confirmed cases
ls_srag_adults_pcr_plots <- list() # Plots with COVID-19 hospitalizations
ls_srag_adults_pcr_deaths_plots <- list() # Plots with COVID-19 in-hospital deaths





# Loops through epidemiological weeks and generate maps
for (w in seq_along(ls_weeks)) {

## COVID-19 confirmed cases (Brasil.IO - Brazilian Health secretaries)

# Sets current week in the loop
weeks <- ls_weeks[[w]]

df_covid_cases_week <- 
    df_covid_cases_total %>% 
    filter(between(epidemiological_week, weeks[1], weeks[2])) %>%
    group_by(city_ibge_code) %>%
    filter(new_confirmed >= 0) %>% 
    summarise(cases = sum(new_confirmed)) %>%
    ungroup() %>% 
    mutate(had_covid = 1) 

df_map_covid_cases <- 
    shp_brasil_mun_centr %>% 
    left_join(
        df_covid_cases_week,
        by = c("code_muni" = "city_ibge_code")
        ) %>% 
    mutate(
        had_covid = ifelse(is.na(had_covid), 0, 1),
        cases = ifelse(is.na(cases) | cases == 0, NA, cases)
        ) %>% 
    arrange(cases)


plot_map_covid_cases <-
    df_map_covid_cases %>%
    ggplot() +
    geom_sf(color = NA, fill = NA) +
    geom_point(
        aes(
            y = latitude, x = longitude,
            size = cases, fill = cases
            ),
        stroke = 0.2, shape = 21, alpha = 0.9) +
    scale_size_continuous(
        breaks = c(200, 1000, 5000, 25000, 55000),
        limit = c(1, 60000),
        name = "",
        labels = scales::comma_format()
        ) +
    scale_fill_gradient(
        breaks = c(200, 1000, 5000, 25000, 55000),
        limit = c(1, 60000),
        low = "paleturquoise1",
        high = "navyblue",
        name = "",
        labels = scales::comma_format(),
        guide = "legend"
        ) +
    geom_sf(data = shp_brasil_reg, fill = NA, size = 0.4) +
    geom_sf(data = shp_brasil_uf, fill = NA, size = 0.1) +
    labs(
        title = paste0("Epidemiological weeks ", weeks[1], "-", weeks[2])
         ) +
    coord_sf() +
    theme_void() +
    theme(
        title = element_text(size = 6),
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(-0.1, -0.15, -0.5, -0.15), "cm"),
        legend.text = element_text(size = 7)
        )


    ls_covid_plots[[w]] <- plot_map_covid_cases

    
    
    
    
    
    
    
    
## Map: COVID-19 Hospitalizations (SARI database)
    
df_srag_adults_pcr_week <- 
    srag_adults_pcr %>%
    mutate(epi_week = lubridate::epiweek(date_int)) %>% 
    filter(between(epi_week, weeks[1], weeks[2])) %>% 
    group_by(CO_MU_INTE) %>%
    summarise(notif = n()) %>%
    ungroup() %>% 
    mutate(had_covid = 1)


df_map_srag_adults_pcr <- 
    shp_brasil_mun_centr %>%
    mutate(code_muni_6dig = str_sub(code_muni, 1, 6)) %>%
    left_join(df_srag_adults_pcr_week %>%
                  mutate_at("CO_MU_INTE", as.character),
              by = c("code_muni_6dig" = "CO_MU_INTE")
              ) %>% 
    mutate(had_covid = ifelse(is.na(had_covid), 0, 1),
           notif = ifelse(is.na(notif) | notif == 0, NA, notif)
    ) %>% 
    arrange(notif)


plot_map_srag_adults_pcr <- 
    df_map_srag_adults_pcr %>% 
    ggplot() +
    geom_sf(color = NA, fill = NA) +
    geom_point(aes(y = latitude, x = longitude,
                   size = notif, fill = notif), 
               stroke = 0.2, shape = 21, alpha = 0.9) +
    scale_size_continuous(
        breaks = c(50, 200, 1000, 5000, 12000),
        limit = c(1, 13000),
        name = "",
        labels = scales::comma_format(),
        # labels = c("\u2264 40", "41-200", "201-1,000", "\u2265 1,000"),
        ) +
    scale_fill_gradient(
        breaks = c(50, 200, 1000, 5000, 12000),
        limit = c(1, 12900),
        low = "lightyellow1", 
        high = "darkgreen",
        name = "",
        labels = scales::comma_format(),
        # labels = c("\u2264 400", "401-2,000", "2,001-10,000", "\u2265 50,000"),
        guide = "legend"
        ) +
    geom_sf(data = shp_brasil_reg, fill = NA, size = 0.4) +
    geom_sf(data = shp_brasil_uf, fill = NA, size = 0.1) +
    coord_sf() +
    theme_void() +
    theme(title = element_text(size = 8),
          plot.title = element_text(hjust = 0.5),
          plot.margin = unit(c(-0.15, 0, -0.5, -0.2), "cm"),
          legend.text = element_text(size = 7)
          
    )

    ls_srag_adults_pcr_plots[[w]] <- plot_map_srag_adults_pcr

    
    
    
    
    
    
    
    
## Map: COVID-19 in-hospital deaths (SARI database)
df_srag_adults_pcr_deaths_week <- 
    srag_adults_pcr %>%
    filter(EVOLUCAO == "Death") %>% 
    mutate(epi_week = lubridate::epiweek(date_int)) %>% 
    filter(between(epi_week, weeks[1], weeks[2])) %>% 
    group_by(CO_MU_INTE) %>%
    summarise(deaths = n()) %>%
    ungroup() %>% 
    mutate(had_covid = 1)


df_map_srag_adults_pcr_deaths <- 
    shp_brasil_mun_centr %>%
    mutate(code_muni_6dig = str_sub(code_muni, 1, 6)) %>%
    left_join(df_srag_adults_pcr_deaths_week %>%
                  mutate_at("CO_MU_INTE", as.character),
              by = c("code_muni_6dig" = "CO_MU_INTE")
    ) %>% 
    mutate(had_covid = ifelse(is.na(had_covid), 0, 1),
           deaths = ifelse(is.na(deaths) | deaths == 0, NA, deaths)
    ) %>% 
    arrange(deaths)



plot_map_srag_adults_pcr_deaths <- 
    df_map_srag_adults_pcr_deaths %>% 
    ggplot() +
    geom_sf(color = NA, fill = NA) +
    geom_point(aes(y = latitude, x = longitude,
                   size = deaths, fill = deaths), 
               stroke = 0.2, shape = 21, alpha = 0.9) +
    scale_size_continuous(
        breaks = c(10, 50, 300, 1500, 3500),
        limit = c(1, 3500),
        name = "",
        labels = scales::comma_format()) +
    scale_fill_gradient(
        breaks = c(10, 50, 300, 1500, 3500),
        limit = c(1, 3500),
        low = "mistyrose", 
        high = "red4",
        name = "",
        labels = scales::comma_format(),
        guide = "legend") +
    geom_sf(data = shp_brasil_reg, fill = NA, size = 0.4) +
    geom_sf(data = shp_brasil_uf, fill = NA, size = 0.1) +
    coord_sf() +
    theme_void() +
    theme(title = element_text(size = 8),
          plot.title = element_text(hjust = 0.5),
          plot.margin = unit(c(-0.15, 0, -0.5, -0.2), "cm"),
          legend.text = element_text(size = 7)
          
    )


    ls_srag_adults_pcr_deaths_plots[[w]] <- plot_map_srag_adults_pcr_deaths


    
} # end of loop



## Combining plots per type of variable (Weekly maps for each variable)
comb_plot_covid <- 
    ggpubr::ggarrange(
        ls_covid_plots[[1]], 
        ls_covid_plots[[2]], 
        ls_covid_plots[[3]],
        align = "h",
        nrow = 1, ncol = 3, common.legend = TRUE, legend = "right"
    )


comb_plot_srag_adults_pcr <- 
    ggpubr::ggarrange(
        ls_srag_adults_pcr_plots[[1]], 
        ls_srag_adults_pcr_plots[[2]], 
        ls_srag_adults_pcr_plots[[3]],
        align = "h",
        nrow = 1, ncol = 3, common.legend = TRUE, legend = "right"
    )
    
comb_plot_srag_adults_pcr_deaths <- 
    ggpubr::ggarrange(
        ls_srag_adults_pcr_deaths_plots[[1]], 
        ls_srag_adults_pcr_deaths_plots[[2]], 
        ls_srag_adults_pcr_deaths_plots[[3]],
        align = "h",
        nrow = 1, ncol = 3, common.legend = TRUE, legend = "right"
        )


## Combining all maps in a 3x3 panel
comb_plot_maps <- 
    ggpubr::ggarrange(
        comb_plot_covid,
        comb_plot_srag_adults_pcr,
        comb_plot_srag_adults_pcr_deaths,
        align = "v",
        nrow = 3, ncol = 1
        )



ggsave("Outputs/Figures/Maps/plot_covid_maps.png",
       comb_plot_maps,
       width = 7, height = 6, units = "in", dpi = 600)





## Mini-map of Brazil with coloured regions (legend)
v_color_regions <- c("#42B540FF","#FDAF91FF", "#AD002AFF", 
                     "#00468BFF", "#925E9FFF", "#ADB6B6FF")


plot_mini_map_brazil_regions <-
    shp_brasil_reg %>%
    mutate(REGIAO = factor(name_region,
                           levels = c("North", "Northeast", "Central-West",
                                      "Southeast", "South"))
           ) %>%
    ggplot() +
    geom_sf(aes(fill = REGIAO)) +
    scale_fill_manual(values = v_color_regions,
                      guide = FALSE) +
    geom_sf(data = shp_brasil_uf, fill = NA, size = 0.2) +
    # geom_hline(aes(yintercept = 0), linetype = "dashed") +
    # annotate("text", x = -30, y = 1,
    #          label = "Equator", size = 3) +
    # geom_hline(aes(yintercept = -23.4366), linetype = "dashed") +
    # annotate("text", x = -33, y = -23.4366 + 1,
    #          label = "Tropic of Capricorn", size = 3) +
    # labs(y = "Latitude", x = "Longitude") +
    theme_void() +
    theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        )


ggsave("Outputs/Figures/Maps/plot_mini_map_brazil_aux.png",
       plot_mini_map_brazil_regions,  
       bg = "transparent",
       width = 2, height = 1, units = "in", dpi = 800)





















# Rates per 100k inhabitants ----------------------------------------------
df_pop_ibge_2020_reg_total <- 
    read_csv2("Data/pop_ibge_proj_2020_gender.csv") %>%
    bind_rows(
        read_csv2("Data/pop_ibge_proj_2020_gender.csv") %>% 
            mutate(REGIAO = "Brazil")
        ) %>%
    select(-PERC_F, -POPULACAO) %>% 
    rename(Male = POPULACAO_M,
           Female = POPULACAO_F) %>% 
    pivot_longer(c(-REGIAO, -IDADE), 
                 values_to = "POPULACAO", names_to = "CS_SEXO") %>% 
    mutate(FAIXA_IDADE = 
               case_when(
                   IDADE <= 19 ~ "<20",
                   IDADE <= 39 ~ "20-39",
                   IDADE <= 49 ~ "40-49",
                   IDADE <= 59 ~ "50-59",
                   IDADE <= 69 ~ "60-69",
                   IDADE <= 79 ~ "70-79",
                   TRUE ~ "80+")
    ) %>% 
    group_by(REGIAO, FAIXA_IDADE, CS_SEXO) %>% 
    summarise(POP = sum(POPULACAO)) %>% 
    ungroup() 


df_pop_ibge_2020_reg_adults <- 
    df_pop_ibge_2020_reg_total %>% 
    filter(FAIXA_IDADE != "<20")



df_br_pop_rate_adults_gender_age <- 
    df_pop_ibge_2020_reg_adults %>% 
    filter(FAIXA_IDADE != "<20", REGIAO == "Brazil") %>%  
    # mutate(PROP_STD_POP = POP/sum(POP)) %>% 
    select(-REGIAO) %>% 
    rename(POP_BRAZIL = POP)




ls_week_stats <- list()
# ls_week_stats_age_sex <- list()


for (w in seq_along(ls_weeks)) {
    
    weeks <- ls_weeks[[w]]
    
    df_stats_100k <- 
        bind_rows(
            df_covid_cases_total %>%
                bind_rows(
                    df_covid_cases_total %>% 
                        mutate(REGIAO = "Brazil")
                ) %>% 
                filter(between(epidemiological_week, weeks[1], weeks[2])) %>%
                filter(new_confirmed > 0 ) %>% 
                group_by(REGIAO) %>%
                summarise(cases = sum(new_confirmed)) %>%
                rename(total = cases) %>% 
                mutate(type = "covid_cases")
                ,
            srag_adults_pcr %>%
                bind_rows(
                    srag_adults_pcr %>% 
                        mutate(REGIAO = "Brazil")
                ) %>% 
                filter(between(SEM_NOT, weeks[1], weeks[2])) %>% 
                count(REGIAO) %>%
                rename(total = n) %>% 
                mutate(type = "hosp"),
            srag_adults_pcr %>%
                bind_rows(
                    srag_adults_pcr %>% 
                        mutate(REGIAO = "Brazil")
                ) %>% 
                filter(EVOLUCAO == "Death") %>% 
                filter(between(SEM_NOT, weeks[1], weeks[2])) %>% 
                count(REGIAO) %>%
                rename(total = n) %>% 
                mutate(type = "ih_deaths")
        ) %>% 
        mutate(
            weeks = paste0(weeks[1], "-",weeks[2])
        ) %>%
        left_join(
            df_pop_ibge_2020_reg_total %>% 
                group_by(REGIAO) %>% 
                summarise(POP_TOTAL = sum(POP))
                
        ) %>% 
        left_join(
            df_pop_ibge_2020_reg_adults %>% 
                group_by(REGIAO) %>% 
                summarise(POP_TOTAL_ADULT = sum(POP))
        ) %>% 
        mutate(
            total_100k = case_when(
                type == "covid_cases" ~ (total / POP_TOTAL) * 100000,
                type != "covid_cases" ~ (total / POP_TOTAL_ADULT) * 100000
            )
        ) 
    
    
    
    
    
    df_stats_100k_age_sex <- 
        bind_rows(
            srag_adults_pcr %>%
                bind_rows(
                    srag_adults_pcr %>% 
                        mutate(REGIAO = "Brazil")
                ) %>% 
                filter(between(SEM_NOT, weeks[1], weeks[2])) %>% 
                count(REGIAO, FAIXA_IDADE, CS_SEXO) %>%
                rename(total = n) %>% 
                mutate(type = "hosp"),
            srag_adults_pcr %>%
                bind_rows(
                    srag_adults_pcr %>% 
                        mutate(REGIAO = "Brazil")
                ) %>% 
                filter(EVOLUCAO == "Death") %>% 
                filter(between(SEM_NOT, weeks[1], weeks[2])) %>% 
                count(REGIAO, FAIXA_IDADE, CS_SEXO) %>%
                rename(total = n) %>% 
                mutate(type = "ih_deaths")
        ) %>% 
        mutate(
            weeks = paste0(weeks[1], "-",weeks[2])
        ) %>%
        filter(!is.na(CS_SEXO)) %>% 
        left_join(
            df_pop_ibge_2020_reg_adults
        ) %>% 
        left_join(
            df_br_pop_rate_adults_gender_age
        ) %>% 
        mutate(
            RATE_POP = (total / POP)
        ) %>% 
        mutate(
            EXP_POP = RATE_POP * POP_BRAZIL
        ) %>% 
        group_by(REGIAO, type, weeks) %>% 
        summarise(
            # total_100k =  100000 * (sum(total) / sum(POP)),
            total_100k_age_sex = 100000 * (sum(EXP_POP) / sum(POP_BRAZIL))
        ) %>% 
        ungroup() 
    
    ls_week_stats[[w]] <- 
        left_join(
            df_stats_100k,
            df_stats_100k_age_sex,
            by = c("REGIAO" = "REGIAO", "type" = "type", "weeks" = "weeks")
        )
    # ls_week_stats_age_sex[[w]] <- df_stats_100k_age_sex
}



# Calculating rates for the overall sample

ls_week_stats[[4]] <- 
    left_join(
        bind_rows(
            df_covid_cases_total %>%
                bind_rows(
                    df_covid_cases_total %>% 
                        mutate(REGIAO = "Brazil")
                ) %>% 
                filter(epidemiological_week <= 33) %>%
                filter(new_confirmed > 0 ) %>% 
                group_by(REGIAO) %>%
                summarise(total = sum(new_confirmed)) %>%
                mutate(type = "covid_cases")
            ,
            srag_adults_pcr %>%
                bind_rows(
                    srag_adults_pcr %>% 
                        mutate(REGIAO = "Brazil")
                ) %>% 
                count(REGIAO) %>%
                rename(total = n) %>% 
                mutate(type = "hosp"),
            srag_adults_pcr %>%
                bind_rows(
                    srag_adults_pcr %>% 
                        mutate(REGIAO = "Brazil")
                ) %>% 
                filter(EVOLUCAO == "Death") %>% 
                count(REGIAO) %>%
                rename(total = n) %>% 
                mutate(type = "ih_deaths")
        ) %>% 
            mutate(
                weeks = "Overall"
            ) %>%
            left_join(
                df_pop_ibge_2020_reg_total %>% 
                    group_by(REGIAO) %>% 
                    summarise(POP_TOTAL = sum(POP))
                
            ) %>% 
            left_join(
                df_pop_ibge_2020_reg_adults %>% 
                    group_by(REGIAO) %>% 
                    summarise(POP_TOTAL_ADULT = sum(POP))
            ) %>% 
            mutate(
                total_100k = case_when(
                    type == "covid_cases" ~ (total / POP_TOTAL) * 100000,
                    type != "covid_cases" ~ (total / POP_TOTAL_ADULT) * 100000
                )
            ),
        bind_rows(
            srag_adults_pcr %>%
                bind_rows(
                    srag_adults_pcr %>% 
                        mutate(REGIAO = "Brazil")
                ) %>% 
                count(REGIAO, FAIXA_IDADE, CS_SEXO) %>%
                rename(total = n) %>% 
                mutate(type = "hosp"),
            srag_adults_pcr %>%
                bind_rows(
                    srag_adults_pcr %>% 
                        mutate(REGIAO = "Brazil")
                ) %>% 
                filter(EVOLUCAO == "Death") %>% 
                count(REGIAO, FAIXA_IDADE, CS_SEXO) %>%
                rename(total = n) %>% 
                mutate(type = "ih_deaths")
        ) %>% 
            mutate(
                weeks = "Overall"
            ) %>%
            filter(!is.na(CS_SEXO)) %>% 
            left_join(
                df_pop_ibge_2020_reg_adults
            ) %>% 
            left_join(
                df_br_pop_rate_adults_gender_age
            ) %>% 
            mutate(
                RATE_POP = (total / POP)
            ) %>% 
            mutate(
                EXP_POP = RATE_POP * POP_BRAZIL
            ) %>% 
            group_by(REGIAO, type, weeks) %>% 
            summarise(
                total_100k_age_sex = 100000 * (sum(EXP_POP) / sum(POP_BRAZIL))
            ) %>% 
            ungroup() ,
        by = c("REGIAO" = "REGIAO", "type" = "type", "weeks" = "weeks")
    )


df_week_stats <- 
    bind_rows(ls_week_stats) %>% 
    pivot_longer(c(-REGIAO, -type, -weeks), names_to = "stats", values_to = "values") %>% 
    pivot_wider(names_from = weeks, values_from = values) %>% 
    filter(stats != "POP_TOTAL") 
    



write_excel_csv(df_week_stats, "Outputs/Tables/df_week_stats.csv")
