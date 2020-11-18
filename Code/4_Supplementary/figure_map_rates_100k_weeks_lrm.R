#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
### Epidemic evolution showed during three-time frames in Brazil - Rates per 100,000 population          ##    
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
    left_join(
        df_br_cities %>% 
            select(codigo_ibge, codigo_ibge_6dig, capital, REGIAO),
        by = c("city_ibge_code" = "codigo_ibge")
    ) %>% 
    filter(city != "Importados/Indefinidos")



### SRAG Data - Hospitalizations
srag_adults_pcr <- 
    vroom::vroom(file_br_srag_covid) 

## Brazilian TOTAL population per regions
df_pop_ibge_2020_reg_total <- 
    read_csv2("Data/pop_ibge_proj_2020.csv") %>% 
    # filter(IDADE >= 20) %>%
    group_by(REGIAO) %>% 
    summarise(POP_TOTAL = sum(POPULACAO)) %>% 
    ungroup()


## Brazilian ADULT population per regions
df_pop_ibge_2020_reg_adults <- 
    read_csv2("Data/pop_ibge_proj_2020.csv") %>% 
    filter(IDADE >= 20) %>%
    group_by(REGIAO) %>% 
    summarise(POP_TOTAL_ADULT = sum(POPULACAO)) %>% 
    ungroup()






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
        group_by(REGIAO, city_ibge_code) %>%
        summarise(total = sum(new_confirmed)) %>%
        mutate(had_covid = 1) %>% 
        filter(total > 0) %>% 
        group_by(REGIAO) %>%
        summarise(cases = sum(total)) %>%
        ungroup() %>% 
        left_join(
            df_pop_ibge_2020_reg_total
        ) %>% 
        mutate(
            total_100k = (cases / POP_TOTAL) * 100000
        ) 
    
    df_map_covid_cases <- 
        shp_brasil_reg %>% 
        left_join(
            df_covid_cases_week,
            by = c("name_region" = "REGIAO")
        )
    
    
    plot_map_covid_cases <-
        df_map_covid_cases %>% 
        ggplot() +
        geom_sf(aes(fill = total_100k), size = 0.4) +
        scale_size_continuous(
            breaks = c(10, 50, 150, 300, 750),
            limit = c(0, 750),
            name = "per 100,000\npopulation",
            labels = scales::comma_format(accuracy = 0.1)
        ) +
        scale_fill_gradient(
            breaks = c(10, 50, 150, 300, 750),
            limit = c(0, 750),
            # low = "white",
            low = "paleturquoise1",
            high = "navyblue",
            name = "per 100,000\npopulation",
            labels = scales::comma_format(accuracy = 0.1),
            guide = "legend"
        ) +
        geom_sf(data = shp_brasil_uf, fill = NA, size = 0.15) +
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
        count(REGIAO) %>% 
        rename(hosp = n) %>% 
        left_join(
            df_pop_ibge_2020_reg_adults
        ) %>% 
        mutate(
            hosp_100k = (hosp / POP_TOTAL_ADULT) * 100000
        ) 
    
    
    df_map_srag_adults_pcr <- 
        shp_brasil_reg %>% 
        left_join(
            df_srag_adults_pcr_week,
            by = c("name_region" = "REGIAO")
        )
    
    
    plot_map_srag_adults_pcr <- 
        df_map_srag_adults_pcr %>% 
        ggplot() +
        geom_sf(aes(fill = hosp_100k), size = 0.4) +
        scale_size_continuous(
            breaks = c(2L, 5L, 10L, 25L, 60L),
            limit = c(0, 70),
            name = "per 100,000\npopulation",
            labels = scales::comma_format()
        ) +
        scale_fill_gradient(
            breaks = c(2L, 5L, 10L, 25L, 60L),
            limit = c(0, 70),
            low = "lightyellow1",
            # low = "white",
            high = "#006d2c",
            name = "per 100,000\npopulation",
            labels = scales::comma_format(),
            guide = "legend"
        ) +
        geom_sf(data = shp_brasil_uf, fill = NA, size = 0.15) +
        labs(
            # title = paste0("Epidemiological weeks ", weeks[1], "-", weeks[2])
        ) +
        coord_sf() +
        theme_void() +
        theme(
            title = element_text(size = 8),
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
        group_by(REGIAO) %>%
        summarise(deaths = n()) %>%
        ungroup() %>%
        left_join(
            df_pop_ibge_2020_reg_adults
        ) %>% 
        mutate(
            deaths_100k = (deaths / POP_TOTAL_ADULT) * 100000
        ) 
    
    
    df_map_srag_adults_pcr_deaths <- 
        shp_brasil_reg %>% 
        left_join(
            df_srag_adults_pcr_deaths_week,
            by = c("name_region" = "REGIAO")
        )
    
    
    
    plot_map_srag_adults_pcr_deaths <- 
        df_map_srag_adults_pcr_deaths %>%
        ggplot() +
        geom_sf(aes(fill = deaths_100k), size = 0.4) +
        scale_size_continuous(
            breaks = c(0.5, 1, 5, 10, 20),
            limit = c(0, 30),
            name = "per 100,000\npopulation",
            labels = scales::comma_format()
        ) +
        scale_fill_gradient(
            breaks = c(0.5, 1,  5, 10, 20),
            limit = c(0, 30),
            # low = "white",
            low = "mistyrose",
            high = "red4",
            name = "per 100,000\npopulation",
            labels = scales::comma_format(),
            guide = "legend"
        ) +
        geom_sf(data = shp_brasil_uf, fill = NA, size = 0.15) +
        labs(
            # title = paste0("Epidemiological weeks ", weeks[1], "-", weeks[2])
        ) +
        coord_sf() +
        theme_void() +
        theme(
            title = element_text(size = 8),
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



ggsave("Outputs/Figures/Maps/plot_maps_rates_100k.png",
       comb_plot_maps,
       width = 7, height = 6, units = "in", dpi = 600)


