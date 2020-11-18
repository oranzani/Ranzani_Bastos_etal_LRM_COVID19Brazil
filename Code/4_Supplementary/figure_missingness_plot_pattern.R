#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################
### Article Characterizing the first 250,000 adult hospitalisations for COVID-19 in Brazil:              ##
###         an analysis of nationwide data                                                               ##
### Coding Otavio Ranzani, Leonardo Bastos, Joao Gabriel Gelli                                           ##
### October 2020                                                                                         ##
### Missing pattern plots                                                                                ##    
#################################### #################################### #################################
#################################### #################################### #################################
#################################### #################################### #################################

## Missing pattern

#### Imputed

srag_adults_pcr_outcomes_m <- srag_adults_pcr_outcomes %>% 
    select(REGIAO, NU_IDADE_N, FAIXA_IDADE, CS_SEXO, CS_RACA, UTI, SUPORT_VEN,
           EVOLUCAO, SEM_PRI, SRAG_original, capital, TIME_SYMPTOMS_HOSP,
           CARDIOPATI_m, HEMATOLOGI_m, HEPATICA_m, DIABETES_m, NEUROLOGIC_m,
           PNEUMOPATI_m, IMUNODEPRE_m, RENAL_m, OBESIDADE_m,
           SATURACAO_m, DESC_RESP_m, DISPNEIA_m)

library(VIM)


missing_support <- srag_adults_pcr_outcomes_m %>% 
    select(UTI, SUPORT_VEN, SATURACAO_m, DESC_RESP_m, DISPNEIA_m ) %>% 
    rename(ICU = UTI, Resp.Supp = SUPORT_VEN, O2.Sat = SATURACAO_m, Resp.Dist = DESC_RESP_m, Dyspn = DISPNEIA_m)


aggr_plot_resp <- aggr(missing_support, col=c('navyblue','red'), 
                  numbers=FALSE, sortVars=TRUE, labels=names(data), cex.axis=.8, gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))


missing_comordities <- srag_adults_pcr_outcomes_m %>% 
    select(CARDIOPATI_m, HEMATOLOGI_m, HEPATICA_m, DIABETES_m, NEUROLOGIC_m,
           PNEUMOPATI_m, IMUNODEPRE_m, RENAL_m, OBESIDADE_m) %>% 
    rename(CVD = CARDIOPATI_m, Haem = HEMATOLOGI_m, Hepa = HEPATICA_m, DM = DIABETES_m,
           Neuro = NEUROLOGIC_m, COPD = PNEUMOPATI_m, Immuno = IMUNODEPRE_m, Renal = RENAL_m, Obes =  OBESIDADE_m)

aggr_plot_como <- aggr(missing_comordities, col=c('navyblue','red'), 
                  numbers=FALSE, sortVars=TRUE, labels=names(data), cex.axis=.8, gap=2, 
                  ylab=c("Histogram of missing data","Pattern"))
