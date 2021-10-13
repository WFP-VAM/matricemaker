library(haven)
library(tidyverse)
library(sf)
library(openxlsx)
library(knitr)
library(kableExtra)
library(readxl)
library(labelled)


dataset <- read_sav("3_ProcessedData/MLI_201909_ENSAN_external.sav")

dataset <- dataset %>% sample_n(size = 3000) 

dataset <- dataset %>% select(q101a_chocs_subis_derniers6_mois, q101b_princip_choc_premer, q61b_pratiq_actuel_agricul,
                              q41_sourc_appro_o_boisson, q46_type_toilettes_mnage_utilisent, 
                              q96a2_montant_epargne)
                              
exampledataFrancais_processed <- read_sav("3_ProcessedData/exampledataFrancais_processed.sav")

exampledataFrancais_processed_plus <- bind_cols(exampledataFrancais_processed, dataset)

exampledataFrancais_processed_plus %>% write_sav("3_ProcessedData\\exampledataFrancais_processed_plus.sav")

