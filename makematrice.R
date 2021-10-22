library(tidyverse)
library(dplyr)
library(haven)
library(labelled)
library(openxlsx)
library(spatstat)

#just assume this is for country with adm1 and adm2 analysis and they collected using pcodes and standard variable names


#1 - import spss data and call it dataset
dataset <- read_sav("3_ProcessedData/exampledataFrancais_processed_plus.sav")
#1 - import spss data and call it dataset
dataset2 <- read_sav("3_ProcessedData/exampledataFrancais_processed_plus.sav")

#2 - create admin codes variable which will store p-codes and then set  everything else to label except for the admin codes
make_labelsandgeocodes <- function(datsetname, adm1_namevar, adm2_namevar) {
  dataset <<- {{datsetname}} %>% mutate(adm1_Pcod = {{adm1_namevar}}, adm2_Pcod = {{adm2_namevar}},  ADMIN1Name = {{adm1_namevar}}, ADMIN2Name = {{adm2_namevar}}) %>% set_value_labels(adm1_Pcod = NULL, adm2_Pcod = NULL) %>% to_factor()
 # adm1n2table <<- {{datsetname}} mutate(adm1_Pcod = ADMIN1Name, adm2_Pcod = ADMIN2Name) %>% set_value_labels(adm1_Pcod = NULL, adm2_Pcod = NULL) %>% to_factor() %>% select(ADMIN1Name, ADMIN2Name, adm1_Pcod,	adm2_Pcod)
  }
make_labelsandgeocodes(dataset, ADMIN1Name, ADMIN2Name)

# 3 calculate CH tables for 5 indicators
#Food Consumption Groups
make_CH_fcs_table_fr <- function(datasetname, fcscatvar, weightvar) {
#make table of % in food consumption Groups - make sure to use correct threshold FCSCat21 or FCSCat28
  dataset <- dataset %>% rename(FCSCat = {{fcscatvar}})
CH_FCSCat_table_wide <<- dataset %>%
  drop_na(FCSCat) %>%
  group_by(ADMIN1Name, adm1_Pcod, ADMIN2Name, adm2_Pcod) %>%
  count(FCSCat,  wt = {{weightvar}}) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>%
  pivot_wider(names_from = FCSCat,
              values_from = perc,
              values_fill = list(perc = 0)) %>%
  replace(., is.na(.), 0)  %>% mutate_if(is.numeric, round, 1) %>%
  #Apply the Cadre Harmonise rules for phasing the Food Consumption Groups
  mutate(PoorBorderline = Pauvre + Limite, FCG_finalphase = case_when(
    Pauvre < 5 ~ 1,  #if less than 5% are in the poor food group then phase 1
    Pauvre >= 20 ~ 4, #if 20% or more are in the poor food group then phase 4
    between(Pauvre,5,10) ~ 2, #if % of people are between 5 and 10%  then phase2
    between(Pauvre,10,20) & PoorBorderline < 30 ~ 2, #if % of people in poor food group are between 20 and 20% and the % of people who are in poor and borderline is less than 30 % then phase2
    between(Pauvre,10,20) & PoorBorderline >= 30 ~ 3)) %>% #if % of people in poor food group are between 20 and 20% and the % of people who are in poor and borderline is less than 30 % then phase2
  select(ADMIN1Name, adm1_Pcod, ADMIN2Name, adm2_Pcod, FCG_Poor = Pauvre, FCG_Borderline = Limite, FCG_Acceptable = Acceptable, FCG_finalphase) #select only relevant variables and order in proper sequence
}  
#test function
make_CH_fcs_table_fr(datasetname = dataset, fcscatvar = FCSCat21, weightvar = WeightHH)


#Household Dietary Diversity Score
make_CH_hdds_table <- function(datasetname, hddsvar, weightvar) {
#convert to CH phases
dataset <<- {{datasetname}} %>% mutate(CH_HDDS = case_when(
  {{hddsvar}} >= 5 ~ "Phase1",
  {{hddsvar}} == 4 ~ "Phase2",
  {{hddsvar}} == 3 ~ "Phase3",
  {{hddsvar}} == 2 ~ "Phase4",
  {{hddsvar}} < 2 ~ "Phase5"))
#calculate % of households in phases and then final phase
CH_HDDS_table_wide  <<- dataset %>%
  drop_na(CH_HDDS) %>%
  group_by(ADMIN1Name, adm1_Pcod, ADMIN2Name, adm2_Pcod) %>%
  count(CH_HDDS, wt = {{weightvar}}) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>%
  pivot_wider(names_from = CH_HDDS,
              values_from = perc,
              values_fill = list(perc = 0)) %>%
  mutate_if(is.numeric, round, 1) %>%
  #Apply the 20% rule (if it is 20% in that phase or the sum of higher phases equals 20%)
  mutate(
    phase2345 = `Phase2` + `Phase3` + `Phase4` + `Phase5`, #this variable will be used to see if phase 2 and higher phases equals 20                                 phase345 = `Phase3` + `Phase4` + `Phase5`, #this variable will be used to see if phase 3 and higher phases equal 20% or more
    phase345 = `Phase3` + `Phase4` + `Phase5`,
    phase45 = `Phase4` + `Phase5`, #this variable will be used to see if phase 3 and higher phases equal 20% or more
    HDDS_finalphase = case_when(
      `Phase5` >= 20 ~ 5, #if 20% or more is in phase 5 then assign phase 5
      `Phase4` >= 20 | phase45 >= 20 ~ 4, #if 20% or more is in phase 4 or the sum of phase4 and 5 is more than 20% then assign phase 4
      `Phase3` >= 20 | phase345 >= 20 ~ 3, #if 20% or more is in phase 3 or the sum of phase3, 4 and 5 is more than 20% then assign phase 3
      `Phase2` >= 20 | phase2345 >= 20 ~ 2, #if 20% or more is in phase 2 or the sum of phase 2, 3, 4 and 5 is more than 20% then assign phase 2
      TRUE ~ 1)) %>% #otherwise assign phase 1
  select(ADMIN1Name, adm1_Pcod, ADMIN2Name, adm2_Pcod, HDDS_Phase1 = Phase1, HDDS_Phase2 = Phase2, HDDS_Phase3 = Phase3, HDDS_Phase4 = Phase4, HDDS_Phase5 = Phase5, HDDS_finalphase) #select only relevant variables, rename them with indicator name and order in proper sequence
} 
#test function
make_CH_hdds_table(dataset, hddsvar = HDDS, weightvar = WeightHH)

#Create CH indicators
#3 - calculate CH proportions/phases - 
make_CH_HHS_table <- function(datasetname, hhsvar, weightvar) {
  dataset <- {{datasetname}} %>% mutate(CH_HHS = case_when(
    {{hhsvar}} == 0 ~ "Phase1",
    {{hhsvar}} == 1 ~ "Phase2",
    {{hhsvar}} %in% c(2,3) ~ "Phase3",
    {{hhsvar}} == 4 ~ "Phase4",
    {{hhsvar}} >= 5 ~ "Phase5"))
  #calculate % of households in phases and then final phase
  CH_HHS_tabe_wide <<- dataset %>% group_by(ADMIN1Name, adm1_Pcod, ADMIN2Name, adm2_Pcod ) %>%
    drop_na(CH_HHS) %>%
    count(CH_HHS, wt = {{weightvar}}) %>%
    mutate(perc = 100 * n / sum(n)) %>%
    ungroup() %>% select(-n) %>%
    pivot_wider(names_from = CH_HHS,
                values_from = perc,
                values_fill = list(perc = 0)) %>%
    mutate_if(is.numeric, round, 1) %>%
    mutate(phase2345 = `Phase2` + `Phase3` + `Phase4` + `Phase5`,
           phase345 = `Phase3` + `Phase4` + `Phase5`,
           phase45 = `Phase4` + `Phase5`,
           HHS_finalphase = case_when(
             Phase5 >= 20 ~ 5,
             Phase4 >= 20 | phase45 >= 20 ~ 4,
             Phase3 >= 20 | phase345 >= 20 ~ 3,
             Phase2 >= 20 | phase2345 >= 20 ~ 2,
             TRUE ~ 1)) %>%
    select(ADMIN1Name, adm1_Pcod, ADMIN2Name, adm2_Pcod, HHS_Phase1 = Phase1, HHS_Phase2 = Phase2, HHS_Phase3 = Phase3, HHS_Phase4 = Phase4, HHS_Phase5 = Phase5, HHS_finalphase)
}
#test function
make_CH_HHS_table(dataset, hhsvar = HHS, weightvar = WeightHH)


#Livelihood Coping Strategies
make_CH_lhcs_table_fr <- function(datasetname, lhcsvar, weightvar = NULL) {
CH_LhCSICat_table_wide <<- dataset %>%
  drop_na({{lhcsvar}}) %>%
  group_by(ADMIN1Name, adm1_Pcod, ADMIN2Name, adm2_Pcod) %>%
  count({{lhcsvar}}, wt = {{weightvar}}) %>%
  mutate(perc = 100 * n / sum(n)) %>%
  ungroup() %>% select(-n) %>%
  pivot_wider(names_from = {{lhcsvar}},
              values_from = perc,
              values_fill = list(perc = 0)) %>%
  mutate_if(is.numeric, round, 1) %>%
  #Apply the Cadre Harmonise rules for phasing the Livelihood Coping Strategies
  mutate(stresscrisisemergency = StrategiesdeStress + StrategiesdeCrise + StrategiesdUrgence,
         crisisemergency = StrategiesdeCrise + StrategiesdUrgence,
         LhHCSCat_finalphase = case_when(
           StrategiesdUrgence >= 20 ~ 4,
           StrategiesdeCrise >= 20 & StrategiesdUrgence < 20 ~ 3,
           Pasdestrategies < 80 & StrategiesdeCrise < 20 ~ 2,
           Pasdestrategies >= 80 ~ 1)) %>%
  select(ADMIN1Name, adm1_Pcod, ADMIN2Name, adm2_Pcod, LhHCSCat_NoStrategies = Pasdestrategies, LhHCSCat_StressStrategies = StrategiesdeStress, LhHCSCat_CrisisStategies = StrategiesdeCrise, LhHCSCat_EmergencyStrategies = StrategiesdUrgence, LhHCSCat_finalphase)
} 
#test function
make_CH_lhcs_table_fr(datasetname = dataset, lhcsvar = LhCSICat, weightvar = WeightHH)

#rCSI
make_CH_rcsi_table <- function(datasetname, rcsivar, weightvar) {
  dataset <- dataset %>% mutate(CH_rCSI = case_when(
    {{rcsivar}} <= 3 ~ "Phase1",
    between({{rcsivar}},4,18) ~ "Phase2",
    {{rcsivar}} >= 19 ~ "Phase3"))
  #calculate % of households in phases and then final phase
  CH_rCSI_table_wide <<- dataset %>%
    group_by(ADMIN1Name, adm1_Pcod, ADMIN2Name, adm2_Pcod ) %>%
    drop_na(CH_rCSI) %>%
    count(CH_rCSI, wt = {{weightvar}}) %>%
    mutate(perc = 100 * n / sum(n)) %>%
    ungroup() %>% select(-n) %>%
    pivot_wider(names_from = CH_rCSI,
                values_from = perc,
                values_fill = list(perc = 0)) %>%
    replace(., is.na(.), 0)  %>% mutate_if(is.numeric, round, 1) %>%
    mutate(rcsi23 = Phase2 + Phase3,
           rCSI_finalphase =
             case_when(
               Phase3 >= 20 ~ 3,
               Phase2 >= 20 | rcsi23 >= 20 ~ 2,
               TRUE ~ 1)) %>%
    select(ADMIN1Name, adm1_Pcod, ADMIN2Name, adm2_Pcod, rCSI_Phase1 = Phase1, rCSI_Phase2 = Phase2, rCSI_Phase3 = Phase3, rCSI_finalphase)
}  
#test function
make_CH_rcsi_table(datasetname = dataset, rcsivar = rCSI, weightvar = WeightHH)

# 4 put together all direct evidence
# join together the different tables - how to make this a function - which enforces order and if certain table isnt availible - insert column names

matrice_intermediaire_direct <- list(CH_FCSCat_table_wide,CH_HDDS_table_wide,CH_HHS_tabe_wide,CH_LhCSICat_table_wide,CH_rCSI_table_wide) %>%  reduce(full_join, by = c("ADMIN1Name", "adm1_Pcod", "ADMIN2Name", "adm2_Pcod"))

matrice_intermediaire_direct_complete <- matrice_intermediaire_direct %>% mutate(ADMIN0Name = "Nigeria", adm0_pcod = "NG", Population = NA) %>%
  select(ADMIN0Name, adm0_pcod, ADMIN1Name, ADMIN2Name, adm1_Pcod,	adm2_Pcod, Population, everything())

# show_countries <- function(){
#   table_adm0 <- data.frame(ADMIN0Name = c("Benin","Burkina Faso","Cabo Verde","Cameroon","Central African Republic","Chad","Cote d'Ivoire","Gambia","Ghana","Guinea","Guinea-Bissau","Liberia","Mali","Mauritania","Niger","Nigeria",
#                                           "Senegal","Sierra Leone","Togo"),
#                            adm0_pcod = c("BJ","BF","CV","CM","CF","TD","CI","GM","GH","GN","GW","LR","ML","MR","NE","NG","SN","SL","TG"))
#   print(table_adm0)
# }
# input_country <- function(ADMIN0Name,adm0_pcod){
#   adm0_table <<- data.frame(ADMIN0Name = {{ADMIN0Name}},
#                             adm0_pcod = {{adm0_pcod}})
# }
# input_country("Nigeria","NG")



# 5 Generate contributing factors
#more work needed on this to make it smooth enough

#create blank list of variables that cf tables will go into
var_list <- list()

#function that creates table of proportions and renames according to input of the prefix 
cf_tablemaker_prop <- function(dataset, prefix, cfvar, weightvar){
  temp_var = 4
  result <- dataset %>%
    drop_na({{cfvar}}) %>%
    group_by(ADMIN1Name, adm1_Pcod, ADMIN2Name, adm2_Pcod) %>%
    count({{cfvar}}, wt = WeightHH) %>%
    mutate(perc = 100 * n / sum(n)) %>%
    ungroup() %>% select(-n) %>%
    pivot_wider(names_from = {{cfvar}},
                values_from = perc,
                values_fill = list(perc = 0)) %>%
    mutate(across(where(is.numeric), round, 1)) %>% 
    purrr::set_names(c(names(.)[1:temp_var],
                       paste0(prefix,names(.)[(1+temp_var):ncol(.)] )))
    return(result)
}

#make first table 
prefix <-  '01_shock_'
var_list[[prefix]] <- cf_tablemaker_prop(dataset, prefix = prefix,
                                   cfvar = q101a_chocs_subis_derniers6_mois,
                                   weightvar = WeightHH)
#make second table 
prefix <-  '02_firstshock_'
var_list[[prefix]] <- cf_tablemaker_prop(dataset, prefix = prefix,
                                         cfvar = q101b_princip_choc_premer,
                                         weightvar = WeightHH)
#make third table 
prefix <-  '03_toilet_'
var_list[[prefix]] <- cf_tablemaker_prop(dataset, prefix = prefix,
                                   cfvar = q46_type_toilettes_mnage_utilisent,
                                   weightvar = WeightHH)

#join all together third table
join_var <- var_list %>%  reduce(full_join)


#function that creates table of weighted mean and renames according to input of the prefix 
cf_tablemaker_mean <- function(dataset, prefix, cfvar, weightvar){
  result <- dataset %>%
    drop_na({{cfvar}}) %>%
    group_by(ADMIN1Name, adm1_Pcod, ADMIN2Name, adm2_Pcod) %>%
    summarise(mean = weighted.mean({{cfvar}}, wt = {{weightvar}})) %>%
    purrr::set_names(c(names(.)[1:(ncol(.)-1)],
                       paste0(prefix,names(.)[ncol(.)] )))
  return(result)
}
prefix <-  '04_savings_'
var_list[[prefix]] <- cf_tablemaker_mean(dataset, prefix = prefix, 
                   cfvar = q96a2_montant_epargne, weightvar = WeightHH)

# function for weighted mean that creates table taking inputs
cf_tablemaker_median <- function(dataset, prefix, cfvar, weightvar){
  result <- dataset %>%
    drop_na({{cfvar}}) %>%
    group_by(ADMIN1Name, adm1_Pcod, ADMIN2Name, adm2_Pcod) %>%
    summarise(median = weighted.median({{cfvar}}, w = {{weightvar}})) %>% 
    purrr::set_names(c(names(.)[1:(ncol(.)-1)],
                       paste0(prefix,names(.)[ncol(.)] )))
    return(result)
}
prefix <-  '05_savings_'
var_list[[prefix]] <- cf_tablemaker_median(dataset, prefix = prefix, 
                                          cfvar = q96a2_montant_epargne, weightvar = WeightHH)

# join all in list - to make contributing factors
contributing_factors <- var_list %>%   reduce(full_join)

# 6 join together direct evidence and contributing factors and other variables
#add direct evidence and contributing factors 
direct_and_cf <- left_join(matrice_intermediaire_direct_complete, contributing_factors, by = c('ADMIN1Name','adm1_Pcod','ADMIN2Name','adm2_Pcod'))
#add in other variables
matrice_intermediaire_complete <- direct_and_cf %>% mutate(
  # if some contributing factors are already generated - not sure that need to include the blank ones - lets see
  #`01_xxx`= NA,`02_xxx`=NA,`06_xxx`=NA,`07_xxx`=NA,`11_xxx`=NA,`12_xxx`=NA,`26_xxx`=NA,`27_xxx`=NA,`41_xxx`=NA,`42_xxx`=NA,`56_xxx`=NA,`57_xxx`=NA,
    Z1_DPME_C=NA,Z1_DPME_pop_C=NA,Z1_DS_C=NA,Z1_Pop_DS_C=NA,Z1_DPME_Pr=NA,Z1_Pop_DPME_Pr=NA,Z1_DS_Pr=NA,Z1_pop_DS_Pr=NA,
    Z2_DPME_C=NA,Z2_DPME_pop_C=NA,Z2_DS_C=NA,Z2_Pop_DS_C=NA,Z2_DPME_Pr=NA,Z2_Pop_DPME_Pr=NA,Z2_DS_Pr=NA,Z2_pop_DS_Pr=NA,
    Z3_DPME_C=NA,Z3_DPME_pop_C=NA,Z3_DS_C=NA,Z3_Pop_DS_C=NA,Z3_DPME_Pr=NA,Z3_Pop_DPME_Pr=NA,Z3_DS_Pr=NA,Z3_pop_DS_Pr=NA,
    Z4_DPME_C=NA,Z4_DPME_pop_C=NA,Z4_DS_C=NA,Z4_Pop_DS_C=NA,Z4_DPME_Pr=NA,Z4_Pop_DPME_Pr=NA,Z4_DS_Pr=NA,Z4_pop_DS_Pr=NA,
    Proxy_cal=NA,
    MAG_pt=NA,
    IPC_AMN_curt=NA,
    MAG_Pharv=NA,
    MAG_Soud=NA,
    IPC_AMN_prjt=NA,
    IMC=NA,
    MUAC=NA,
    TBM=NA,
    TMM5=NA)

#7 format colors and save as excel sheet 
#formats color of columns and begin end of columns

format_excel_matrice <- function(firstcf, lastcf) {
direct <- createStyle(fgFill = "#4F81BD", halign = "left", textDecoration = "Bold",border = "Bottom", fontColour = "white")
contributifs <- createStyle(fgFill = "#FFC7CE", halign = "left", textDecoration = "Bold",border = "Bottom", fontColour = "black")
hea <- createStyle(fgFill = "#C6EFCE", halign = "left", textDecoration = "Bold",border = "Bottom", fontColour = "black")
nutrition <- createStyle(fgFill = "yellow", halign = "left", textDecoration = "Bold",border = "Bottom", fontColour = "black")
mortalite <- createStyle(fgFill = "orange", halign = "left", textDecoration = "Bold",border = "Bottom", fontColour = "black")
proxyvar <- createStyle(fgFill = "lightgreen", halign = "left", textDecoration = "Bold",border = "Bottom", fontColour = "black")

col1stDirectVariable <- which(colnames(matrice_intermediaire_complete)=="FCG_Poor")
colLastDirectVariable <- which(colnames(matrice_intermediaire_complete)=="rCSI_finalphase")
# modify these two based on which contributing factors are chosen
col1stFactCont <- which(colnames(matrice_intermediaire_complete)=={{firstcf}})
colLastFactCont <- which(colnames(matrice_intermediaire_complete)=={{lastcf}})
col1stHEAVariable <- which(colnames(matrice_intermediaire_complete)=="Z1_DPME_C")
colLastHEAVarible <- which(colnames(matrice_intermediaire_complete)=="Z4_pop_DS_Pr")
colLastproxycal <- which(colnames(matrice_intermediaire_complete)=="Proxy_cal")
col1stNutritionVariable <- which(colnames(matrice_intermediaire_complete)=="MAG_pt")
colLastNutritionVariable <- which(colnames(matrice_intermediaire_complete)=="MUAC")
col1stMortalityVariable <- which(colnames(matrice_intermediaire_complete)=="TBM")
colLastMortalityVariable <- which(colnames(matrice_intermediaire_complete)=="TMM5")

Matrice_intermediaire <- createWorkbook()
addWorksheet(Matrice_intermediaire, "Matrice intermediaire")

writeData(Matrice_intermediaire,sheet = 1,matrice_intermediaire_complete,startRow = 1,startCol = 1,)
addStyle(Matrice_intermediaire,1,rows = 1,cols = col1stDirectVariable:colLastDirectVariable
         ,style = direct,gridExpand = TRUE,)
addStyle(Matrice_intermediaire,1,rows = 1,cols =(colLastDirectVariable+1) :(col1stHEAVariable-1),
         style = contributifs,gridExpand = TRUE,)
addStyle(Matrice_intermediaire,1,rows = 1,cols =col1stHEAVariable :colLastHEAVarible,
         style = hea,gridExpand = TRUE,)
addStyle(Matrice_intermediaire,1,rows = 1,cols =col1stNutritionVariable :colLastNutritionVariable,
         style = nutrition,gridExpand = TRUE,)
addStyle(Matrice_intermediaire,1,rows = 1,cols =col1stNutritionVariable-1 ,
         style = proxyvar,gridExpand = TRUE,)
addStyle(Matrice_intermediaire,1,rows = 1,cols = col1stMortalityVariable:colLastMortalityVariable,
         style = mortalite,gridExpand = TRUE,)
saveWorkbook(Matrice_intermediaire,file ="Matrice_intermediaire.xlsx",overwrite = TRUE)
}

format_excel_matrice("01_shock_Non", "05_savings_median")
   








