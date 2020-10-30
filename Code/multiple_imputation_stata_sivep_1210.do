version 13.1


************************************ ************************************ *********************************
************************************ ************************************ *********************************
************************************ ************************************ *********************************
*** Article Characterizing the first 200,000 hospitalizations for COVID-19 in Brazil: a nationwide study **
*** Coding Otavio Ranzani, Leonardo Bastos, Jo‹o Gabriel Gelli                                           **
*** October 2020                                                                                         **
***                                                                                                      **
*** Multiple imputation                                                                                  **    
************************************ ************************************ *********************************
************************************ ************************************ *********************************
************************************ ************************************ *********************************


set more off

** importing previous cleanned and prepared data. See R code
import delimited "/Users/Otavio/Dropbox/SRAG/Manuscript/Lancet_RM/R1/Analysis_R1_LRM/Data/srag_adults_pcr_12_10.csv", clear

**** keeping useful variables
keep index regiao nu_idade_n faixa_idade cs_sexo cs_raca uti suport_ven ///
           evolucao sem_pri *_m sg_uf_inte srag_original capital time_symptoms_hosp
**** only those with a defined outcome		   
drop if evolucao=="Ongoing"

*** data preparing before MI
gen sex    = (cs_sexo        == "Male")   if cs_sexo       != "NA"
gen icu    = (uti            == "Yes")    if uti           != "NA"
gen sari   = (srag_original  == "Yes")    if srag_original != "NA"
gen h_out  = (evolucao       == "Death")  if evolucao      != "NA"

gen    race = .
recode race .=0 if cs_raca == "White"
recode race .=1 if cs_raca == "Black/Mixed"
recode race .=2 if cs_raca == "Asian"
recode race .=3 if cs_raca == "Indigenous"

gen resp_sup = .
recode resp_sup .=0 if suport_ven == "No"
recode resp_sup .=1 if suport_ven == "Yes, non-invasive"
recode resp_sup .=2 if suport_ven == "Yes, invasive"

gen region = .
recode region .=0 if regiao == "Southeast"
recode region .=1 if regiao == "Northeast"
recode region .=2 if regiao == "South"
recode region .=3 if regiao == "Central-West"
recode region .=4 if regiao == "North"

gen sem_3 = .
recode sem_3 .=0 if sem_pri <=12
recode sem_3 .=1 if sem_pri <=15
recode sem_3 .=2 if sem_pri <=18
recode sem_3 .=3 if sem_pri <=21
recode sem_3 .=4 if sem_pri <=24
recode sem_3 .=5 if sem_pri <=27
recode sem_3 .=6 if sem_pri <=30
recode sem_3 .=7 if sem_pri <=33

gen age_cat = .
recode age_cat .=0 if faixa_idade == "20-39"
recode age_cat .=1 if faixa_idade == "40-49"
recode age_cat .=2 if faixa_idade == "50-59"
recode age_cat .=3 if faixa_idade == "60-69"
recode age_cat .=4 if faixa_idade == "70-79"
recode age_cat .=5 if faixa_idade == "80+"

foreach var of varlist *_m{
gen `var'_m2  = (`var' == "Yes") if `var' != "NA"
}

** time from symptoms onset to hospitalization
replace time_symptoms_hosp="" if time_symptoms_hosp=="NA"
destring time_symptoms_hosp, replace


gen    time_sh = .
recode time_sh .=0 if time_symptoms_hosp <=3
recode time_sh .=1 if time_symptoms_hosp <=6
recode time_sh .=2 if time_symptoms_hosp <=9
recode time_sh .=3 if time_symptoms_hosp <=12
recode time_sh .=4 if time_symptoms_hosp <=15
recode time_sh .=5 if time_symptoms_hosp >15 & time_symptoms_hosp != .


*****
***** imputation
mi set mlong

mi register imputed sex race icu resp_sup ///
cardiopati_m_m2 hematologi_m_m2 hepatica_m_m2 diabetes_m_m2 ///
neurologic_m_m2 pneumopati_m_m2 imunodepre_m_m2 renal_m_m2 obesidade_m_m2 ///
sari saturacao_m_m2 desc_resp_m_m2 dispneia_m_m2 time_sh

mi register regular index evolucao sem_pri age_cat nu_idade_n sem_3  capital region

set matsize 800
mi impute chained (logit)  sex icu sari cardiopati_m_m2 hematologi_m_m2 hepatica_m_m2 diabetes_m_m2  ///
                           neurologic_m_m2 pneumopati_m_m2 imunodepre_m_m2 renal_m_m2 obesidade_m_m2 ///
						   saturacao_m_m2 desc_resp_m_m2 dispneia_m_m2 ///
                  (mlogit) resp_sup race time_sh = i.h_out i.age_cat i.sem_3 i.capital i.region , ///
				  add(30) rseed (2020) noisily augment savetrace(trace, replace)

*save "/Users/Otavio/Downloads/srag_adults_pcr_12_10_imputed_2.dta"


/*
Multivariate imputation                     Imputations =       30
Chained equations                                 added =       30
Imputed: m=1 through m=30                       updated =        0

Initialization: monotone                     Iterations =      300
                                                burn-in =       10

               sex: logistic regression
               icu: logistic regression
              sari: logistic regression
    cardiopati_m~2: logistic regression
    hematologi_m~2: logistic regression
     hepatica_m_m2: logistic regression
     diabetes_m_m2: logistic regression
    neurologic_m~2: logistic regression
    pneumopati_m~2: logistic regression
    imunodepre_m~2: logistic regression
        renal_m_m2: logistic regression
    obesidade_m_m2: logistic regression
    saturacao_m_m2: logistic regression
    desc_resp_m_m2: logistic regression
     dispneia_m_m2: logistic regression
          resp_sup: multinomial logistic regression
              race: multinomial logistic regression
           time_sh: multinomial logistic regression

------------------------------------------------------------------
                   |               Observations per m             
                   |----------------------------------------------
          Variable |   Complete   Incomplete   Imputed |     Total
-------------------+-----------------------------------+----------
               sex |     231995           41        41 |    232036
               icu |     205493        26543     26543 |    232036
              sari |     193494        38542     38542 |    232036
    cardiopati_m~2 |     123187       108849    108849 |    232036
    hematologi_m~2 |      91161       140875    140875 |    232036
     hepatica_m_m2 |      90845       141191    141191 |    232036
     diabetes_m_m2 |     114921       117115    117115 |    232036
    neurologic_m~2 |      93969       138067    138067 |    232036
    pneumopati_m~2 |      93565       138471    138471 |    232036
    imunodepre_m~2 |      92142       139894    139894 |    232036
        renal_m_m2 |      93806       138230    138230 |    232036
    obesidade_m_m2 |      91744       140292    140292 |    232036
    saturacao_m_m2 |     194351        37685     37685 |    232036
    desc_resp_m_m2 |     191943        40093     40093 |    232036
     dispneia_m_m2 |     207780        24256     24256 |    232036
          resp_sup |     196248        35788     35788 |    232036
              race |     166882        65154     65154 |    232036
           time_sh |     228447         3589      3589 |    232036
------------------------------------------------------------------
(complete + incomplete = total; imputed is the minimum across m
 of the number of filled-in observations.)
*/



*** *** *** ***
* Proportions MI 
*** *** *** ***
set more off
label define icul 0"NoICU" 1"ICU"
label value icu icul

label define respl 0"None" 1"No-invasive" 2"Invasive"
label value resp_sup respl

label define saril 0"NoSari" 1"Sari"
label value sari saril

label define h_outl 0"Discharge" 1"Death"
label value h_out h_outl

label define yesnol 0 "No" 1 "Yes"
foreach var of varlist *_m2{
label value `var' yesnol
}

label define agecatl 0"20-39" 1"40-49" 2"50-59" 3"60-69" 4"70-79" 5"80+"
label value age_cat agecatl

label define regionl 0"Southeast" 1"Northeast" 2"South" 3"Central-West" 4"North"
label value region regionl

** generating 
mi passive: gen imv = resp_sup
mi passive: replace imv = 0 if resp_sup==1
mi passive: replace imv = . if resp_sup == .


mi passive: gen n_comorbidities = cardiopati_m_m2 + hematologi_m_m2 + hepatica_m_m2 + diabetes_m_m2 + ///
                           neurologic_m_m2 + pneumopati_m_m2 + imunodepre_m_m2 + renal_m_m2 + obesidade_m_m2
set more off						   
mi passive: gen     n_comorb_cat3 = 0 if n_comorbidities==0
mi passive: replace n_comorb_cat3 = 1 if n_comorbidities==1   | n_comorbidities==2
mi passive: replace n_comorb_cat3 = 2 if n_comorbidities>2    & n_comorbidities!=.


*save "/Users/Otavio/Downloads/srag_adults_pcr_12_10_imputed_2.dta", replace


** Figure 2
*ICU
mi estimate: proportion icu
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(ICU)

mi estimate: proportion icu, over(region)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(ICU_region) modify

mi estimate: proportion h_out, over(icu)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(outcome_ICU) modify

mi estimate: proportion h_out, over(icu age_cat)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(outcome_ICU_age) modify


mi estimate: proportion h_out, over(icu age_cat region)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(outcome_ICU_age_region) modify

set more off

** IMV
mi estimate: proportion resp_sup
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Resp_sup) modify

mi estimate: proportion imv
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(IMV) modify

mi estimate: proportion imv, over(region)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(IMV_region) modify



mi estimate: proportion h_out, over(imv)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(outcome_IMV) modify

mi estimate: proportion h_out, over(imv age_cat)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(outcome_IMV_age) modify


mi estimate: proportion h_out, over(imv age_cat region)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(outcome_IMV_age_region) modify

*** Comorbidities

mi estimate: proportion cardiopati_m_m2
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Comorb_cardio) modify
mi estimate: proportion diabetes_m_m2
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Comorb_diabetes) modify
mi estimate: proportion renal_m_m2
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Comorb_renal) modify
mi estimate: proportion obesidade_m_m2
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Comorb_obesidade) modify
mi estimate: proportion neurologic_m_m2
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Comorb_neuro) modify
mi estimate: proportion pneumopati_m_m2
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Comorb_resp) modify
mi estimate: proportion imunodepre_m_m2
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Comorb_imuno) modify
mi estimate: proportion hematologi_m_m2
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Comorb_hemato) modify
mi estimate: proportion hepatica_m_m2
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Comorb_heapto) modify

mi estimate: proportion n_comorb_cat3
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Comorb_cat3) modify


mi estimate: proportion n_comorb_cat2

mi estimate: proportion n_comorb_cat3, over(age_cat)

putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Comorb_cat2) modify


mi estimate: proportion h_out, over(n_comorb_cat3)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Outcome_Comorb_cat3) modify

mi estimate: proportion h_out, over(n_comorb_cat2)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Outcome_Comorb_cat2) modify


mi estimate: proportion h_out, over(age_cat n_comorb_cat3)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Outcome_age_Comorb_cat3) modify

mi estimate: proportion h_out, over(age_cat n_comorb_cat2)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Outcome_age_Comorb_cat2) modify


** SARI / severity

mi estimate: proportion sari
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(SARI) modify


mi estimate: proportion sari, over(region)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(SARI_region) modify


mi estimate: proportion h_out, over(sari)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Outcome_sari) modify


mi estimate: proportion h_out, over(sari region)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Outcome_sari_region) modify

* satu
set more off
mi estimate: proportion saturacao_m_m2
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Saturation) modify


mi estimate: proportion saturacao_m_m2, over(region)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Satu_region) modify


mi estimate: proportion h_out, over(saturacao_m_m2)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Outcome_satu) modify


mi estimate: proportion h_out, over(saturacao_m_m2 region)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Outcome_satu_region) modify




* satu
  
mi estimate: proportion desc_resp_m_m2
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Desconfort) modify


mi estimate: proportion desc_resp_m_m2, over(region)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Desconfort_region) modify


mi estimate: proportion h_out, over(desc_resp_m_m2)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Desconfort_satu) modify


mi estimate: proportion h_out, over(desc_resp_m_m2 region)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Desconfort_satu_region) modify



* satu
mi estimate: proportion dispneia_m_m2
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Dispnea) modify


mi estimate: proportion dispneia_m_m2, over(region)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Dispnea_region) modify


mi estimate: proportion h_out, over(dispneia_m_m2)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Dispnea_satu) modify


mi estimate: proportion h_out, over(dispneia_m_m2 region)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm.xls", sheet(Dispnea_satu_region) modify




**** general data

** Figure 2
*death region age



mi estimate: proportion sex
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm_1.xls", sheet(Sex) modify



mi estimate: proportion race
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm_1.xls", sheet(Race) modify


mi estimate: proportion time_sh
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm_1.xls", sheet(Time) modify


mi estimate: proportion h_out, over(age_cat n_comorb_cat2 region)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm_1.xls", sheet(Outcome_age_Comorb_cat2_region) replace


mi estimate: proportion h_out, over(age_cat region)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm_1.xls", sheet(Outcome_age_region) modify


mi estimate: proportion h_out, over(resp_sup)
putexcel A1=matrix(r(table), names) using "/Users/Otavio/Downloads/Export_stata/proportions_imputed_r1_lrm_1.xls", sheet(Outcome_resp) modify
