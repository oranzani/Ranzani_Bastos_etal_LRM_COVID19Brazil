#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
###                                                                                                      ##
### Table 3. Demographic, administrative and health system regional characteristics                      ##
###  CNES data - National Registry of Health Establishments                                              ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################


# Library -----------------------------------------------------------------
library(tidyverse)
library(tidylog)

## list to save data of hospital beds
ls_bed_stats <- list()

## Dados IBGE
df_dados_cidades <- 
    read_csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/master/csv/municipios.csv") %>%
    mutate(codigo_ibge_6dig = str_sub(codigo_ibge, 1, 6)) %>% 
    select(codigo_ibge, codigo_ibge_6dig, latitude, longitude, capital)


df_dados_capitais <- 
    df_dados_cidades %>% 
    filter(capital == 1)



## Data from the health Establishments
file_estab <- "Data/tbEstabelecimento202002.csv"
file_leitos <- "Data/rlEstabComplementar202002.csv"
file_estado <- "Data/tbEstado202002.csv"
    

## Decoding data from states and regions
df_estados <- 
    vroom::vroom(file_estado) %>% 
    mutate(
        REGIAO = 
            case_when(CO_SIGLA %in% c("SP", "RJ", "ES", "MG") ~ "Southeast",
                      CO_SIGLA %in% c("SC", "RS", "PR") ~ "South",
                      CO_SIGLA %in% c("MT", "MS", "GO", "DF") ~ "Central-West",
                      CO_SIGLA %in% c("AM", "AP", "TO", "PA", "RO", "RR", "AC") ~ "North",
                      CO_SIGLA %in% c("BA", "AL", "SE", "PE", "MA", "RN", "PB", "CE", "PI") ~ "Northeast")
    )
    
    
df_pop_ibge_2020 <- read_csv2("Data/pop_ibge_proj_2020.csv") 
    
    
    


# Calculating adult beds --------------------------------------------------

# Input CNES health facility data
df_cnes_estab <- 
    vroom::vroom(file_estab,
                 col_types = cols(
                     CO_CNES = col_integer(),
                     CO_UNIDADE = col_character()
                     )
                 ) 

# Input CNES beds data
df_cnes_leitos <- 
    vroom::vroom(file_leitos,
                 col_types = cols(
                     CO_UNIDADE = col_character()
                 )
    ) 


### Filtering and calculating adult beds
## We considered adult ICU and hospital beds and removed beds for psychiatric and obstetric patients
df_cnes_leitos_tipo <-
    df_cnes_leitos %>%
    filter(!(CO_LEITO %in% c("06", "10", "41", "43", "45", "47", "65", "68",
                             "73", "77", "78", "79", "80", "81", "82", "83",
                             "87", "88", "89", "90", "91", "92", "93", "94",
                             "52"))
    ) %>%
    mutate(
        tipo_leito = case_when(
            CO_LEITO %in% c("64", "74", "75", "76", "85", "86", "95", "51") ~ "ICU_adult",
            TRUE ~ "No_ICU"
        )
    ) %>%
    select(CO_UNIDADE, tipo_leito, QT_EXIST) %>%
    group_by(CO_UNIDADE, tipo_leito) %>%
    summarise(
        leitos = sum(QT_EXIST)
    ) %>% 
    ungroup() %>% 
    pivot_wider(names_from = "tipo_leito", values_from = "leitos") %>% 
    replace_na(
        list(ICU_adult = 0,
             No_ICU = 0)
        ) %>% 
    mutate(total_beds = ICU_adult + No_ICU)


# CNES Table  with facility info + beds
df_cnes_estab_leitos_adultos <- 
    df_cnes_estab %>% 
    select(CO_UNIDADE, CO_CNES, CO_ESTADO_GESTOR, NO_BAIRRO, CO_REGIAO_SAUDE, CO_MICRO_REGIAO, 
           TP_UNIDADE, CO_NATUREZA_JUR, 
           TP_GESTAO, CO_TIPO_ESTABELECIMENTO, TP_UNIDADE, CO_MUNICIPIO_GESTOR) %>% 
    left_join(df_cnes_leitos_tipo,
              by = c("CO_UNIDADE" = "CO_UNIDADE")) %>% 
    left_join(df_estados, 
              by = c("CO_ESTADO_GESTOR" = "CO_UF")) %>% 
    left_join(df_dados_capitais %>% 
                  mutate(codigo_ibge_6dig = as.numeric(codigo_ibge_6dig)), 
              by = c("CO_MUNICIPIO_GESTOR" = "codigo_ibge_6dig")) %>% 
    # select(-c(CO_MUNICIPIO_GESTOR, codigo_ibge_6dig)) %>%
    select(CO_UNIDADE, CO_CNES, CO_SIGLA, capital, everything())


# Exports CNES beds data
df_leitos_reg_adultos <- 
    df_cnes_estab_leitos_adultos %>% 
    bind_rows(df_cnes_estab_leitos_adultos %>% 
                  mutate(REGIAO = "Brazil")) %>% 
    group_by(REGIAO) %>% 
    summarise(adult_beds = sum(total_beds, na.rm = TRUE),
              adult_beds_ICU = sum(ICU_adult, na.rm = TRUE)) %>% 
    inner_join(df_pop_ibge_2020 %>%
                   bind_rows(
                       df_pop_ibge_2020 %>% 
                           mutate(REGIAO = "Brazil")
                   ) %>% 
                   filter(IDADE >= 20) %>% 
                   group_by(REGIAO) %>% 
                   summarise(adult_population = sum(POPULACAO)) %>% 
                   ungroup(), by = c("REGIAO" = "REGIAO")) %>% 
    mutate(beds_100k = (adult_beds / adult_population) * 100000,
           beds_ICU_100k = (adult_beds_ICU / adult_population) * 100000) %>% 
    pivot_longer(-REGIAO, values_to = "value", names_to = "var_adulto") %>% 
    pivot_wider(names_from = "REGIAO", values_from = "value") %>% 
    select(var_adulto, Brazil, North, Northeast, `Central-West`, Southeast, South) %>% 
    mutate(month = as.numeric(month_num)) %>% 
    select(month, var_adulto, everything())



# Exports CNES beds data - Capital/Non-capital
df_leitos_reg_adultos_capital <- 
    df_cnes_estab_leitos_adultos %>% 
    bind_rows(df_cnes_estab_leitos_adultos %>% 
                  mutate(REGIAO = "Brazil")) %>% 
    group_by(REGIAO, capital) %>% 
    summarise(adult_beds = sum(total_beds, na.rm = TRUE),
              adult_beds_ICU = sum(ICU_adult, na.rm = TRUE)) %>% 
    mutate(proportion = round(100*(adult_beds_ICU/sum(adult_beds_ICU)))) %>% 
    ungroup() %>% 
    select(-capital)

    

write_csv(df_leitos_reg_adultos_capital, "Outputs/Tables/df_ICU_beds_CNES_capital.csv")




