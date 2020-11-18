#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
### Analysis of Brazilian municipalities covered by the SIVEP-GRIPE                                      ##
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(tidylog)
library(gtsummary)
library(writexl)



# Input Data --------------------------------------------------------------

## Brazilian municipalities information
municipios <- read_csv("https://github.com/kelvins/Municipios-Brasileiros/raw/main/csv/municipios.csv") %>% 
  mutate(codigo_ibge_6dig = as.numeric(str_sub(codigo_ibge, 1, 6)))

## Brazilian municipalities population
muni_pop <- read_csv("Data/pop_br_municipal.csv")

municipios <- municipios %>% left_join(muni_pop, by = c("codigo_ibge_6dig" = "code_city"))

## States 
codigos_estados <- read.csv("https://raw.githubusercontent.com/leogermani/estados-e-municipios-ibge/master/estados.csv", encoding = "utf-8") %>% 
  select(-NOME)

## Brazilian municipalities - COVID-19
brazil <- vroom::vroom("~/Downloads/caso_full.csv") %>% 
  filter(!is.na(city)) %>%
  filter(!is.na(city_ibge_code)) %>% 
  select(city_ibge_code, date, last_available_confirmed) %>% 
  mutate(city_ibge_code_6dig = as.numeric(str_sub(city_ibge_code, 1, 6))) %>% 
  filter(date == "2020-08-15") %>% 
  select(-date, - city_ibge_code)
  

## SIVEP-Gripe with COVID-19 cases, municipalities of residence of hospitalization
srag_municip <- vroom::vroom("Data/srag_adults_pcr_12_10.csv") %>% 
  select(CO_MUN_RES) %>%
  group_by(CO_MUN_RES) %>% 
  summarise(hospitalizacoes = n())


## National Registry of Health Establishments
cnes_estab <- read_csv2("Data/tbEstabelecimento202002.csv", col_types = cols(CO_UNIDADE = col_character())) %>%
  select(CO_UNIDADE, CO_CNES, CO_MUNICIPIO_GESTOR)


## Hospital Beds
cnes_leitos <- read_csv2("Data/rlEstabComplementar202002.csv", col_types = cols(CO_UNIDADE = col_character())) %>% 
  filter(!(CO_LEITO %in% c("06", "10", "41", "43", "45", "47", "65", "68",
                           "73", "77", "78", "79", "80", "81", "82", "83",
                           "87", "88", "89", "90", "91", "92", "93", "94",
                           "52"))
  ) %>%
  mutate(
    tipo_leito = case_when(
      CO_LEITO %in% c("64", "74", "75", "76", "85", "86", "95", "51") ~ "leitos_UTI_adulto",
      TRUE ~ "leitos_enfermaria"
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
    list(leitos_UTI_adulto = 0,
         leitos_enfermaria = 0)
  ) %>% 
  mutate(leitos_total = leitos_UTI_adulto + leitos_enfermaria)


cnes_agregado <- cnes_estab %>% 
  left_join(cnes_leitos, by = c("CO_UNIDADE" = "CO_UNIDADE")) %>% 
  mutate(CO_MUNICIPIO = as.numeric(str_sub(CO_MUNICIPIO_GESTOR, 1, 6))) %>% 
  group_by(CO_MUNICIPIO) %>% 
  summarise(leitos_totais_municip     = sum(leitos_total, na.rm = TRUE),
            leitos_UTI_adulto_municip = sum(leitos_UTI_adulto, na.rm = TRUE)) %>% 
  filter(!is.na(CO_MUNICIPIO))


## Municipalities

agrega_dados <- 
  municipios %>% 
  select(-c(latitude, longitude, capital)) %>% 
  left_join(brazil, by = c("codigo_ibge_6dig" = "city_ibge_code_6dig")) %>%
  left_join(cnes_agregado, by = c("codigo_ibge_6dig" = "CO_MUNICIPIO")) %>%
  left_join(srag_municip, by = c("codigo_ibge_6dig" = "CO_MUN_RES")) %>%
  left_join(codigos_estados, by = c("codigo_uf" = "COD")) %>%
  mutate(regiao = case_when(codigo_uf %in% c("35", "33", "32", "31") ~ "Southeast",
                            codigo_uf %in% c("42", "43", "41") ~ "South",
                            codigo_uf %in% c("51", "50", "52", "53") ~ "Central-West",
                            codigo_uf %in% c("13", "16", "17", "15", "11", "14", "12") ~ "North",
                            codigo_uf %in% c("29", "27", "28", "26", "21", "24", "25", "23", "22") ~ "Northeast")) %>% 
  mutate(tem_confirmados = case_when(is.na(last_available_confirmed) ~ 0,
                                     last_available_confirmed == 0 ~ 0,
                                     TRUE ~ 1)) %>% 
  mutate(tem_leitos = case_when(leitos_totais_municip > 0 ~ 1,
                                TRUE ~ 0)) %>%
  mutate(tem_leitos_UTI_adulto = case_when(leitos_UTI_adulto_municip > 0 ~ 1,
                                           TRUE ~ 0)) %>% 
  mutate(tem_hospit_srag = case_when(hospitalizacoes > 0 ~ 1,
                                     TRUE ~ 0))

### Generating table, municipal level
db_region <- agrega_dados %>%
  group_by(regiao) %>% 
    summarise(mun_confirmados = sum(tem_confirmados),
              mun_leitos = sum(tem_leitos),
              mun_leitos_UTI_adulto = sum(tem_leitos_UTI_adulto),
              mun_hospitalizacoes_srag = sum(tem_hospit_srag),
              mun_totais = n())

db_brazil <- agrega_dados %>% 
    summarise(mun_confirmados = sum(tem_confirmados),
              mun_leitos = sum(tem_leitos),
              mun_leitos_UTI_adulto = sum(tem_leitos_UTI_adulto),
              mun_hospitalizacoes_srag = sum(tem_hospit_srag),
              mun_totais = n()) %>% 
    mutate(regiao = "Brazil")

db_municipal <- 
  bind_rows(db_brazil, db_region) %>% 
    mutate(regiao = as_factor(regiao),
           regiao = fct_relevel(regiao, "Brazil","North", "Northeast","Central-West", "Southeast")) %>% 
    arrange(regiao)

## population
db_region_pop <- agrega_dados %>% 
    filter(tem_hospit_srag == 1) %>% 
    group_by(regiao) %>% 
    summarise(pop = sum(pop_city, na.rm = TRUE))

db_brazil_pop <- agrega_dados %>% 
    filter(tem_hospit_srag == 1) %>% 
    summarise(pop = sum(pop_city, na.rm = TRUE)) %>% 
    mutate(regiao = "Brazil")

db_municipal_pop <- bind_rows(db_brazil_pop, db_region_pop) %>% 
    mutate(regiao = as_factor(regiao),
           regiao = fct_relevel(regiao, "Brazil","North", "Northeast","Central-West", "Southeast")) %>% 
    arrange(regiao)

