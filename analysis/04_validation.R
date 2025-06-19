################################################################################ 
# This script conducts an internal validation of the Sick-Sicker               # 
# state-transition model (STM) by comparing the model-predicted outputs        #
# evaluated at the calibrated parameters vs the calibration targets. This      #
# script could be modified by adding an external validation exercise.          #
#                                                                              # 
# Authors:                                                                     #
#     - Fernando Alarid-Escudero, PhD, <fernando.alarid@cide.edu>              # 
#     - Eline Krijkamp, MS                                                     #
#     - Petros Pechlivanoglou, PhD                                             #
#     - Hawre Jalal, MD, PhD                                                   #
#     - Eva A. Enns, PhD                                                       # 
################################################################################ 
# The structure of this code is according to the DARTH framework               #
# https://github.com/DARTH-git/Decision-Modeling-Framework                     #
################################################################################ 

# rm(list = ls()) # to clean the workspace

#### 04.1 Load packages and functions ####
#### 04.1.1 Load packages ####
# Dependencies have been loaded with 'darthpack'

#### 04.1.2 Load inputs ####
### Save summary statistics of posterior distribution
## As .RData
## code to prepare `01_init_params` dataset goes here

l_all_params <- load_all_params(file.init_gen = "data-raw/01_init_params_general.csv",
                                file.init_pop = "data-raw/01_init_params_pop.csv",
                                file.init_uptake = "data-raw/01_init_params_uptake_screen.csv",
                                file.init_tris = "data-raw/01_init_params_prev_common_tris.csv",
                                file.init_fct = "data-raw/01_init_params_fct.csv",
                                file.init_nipt = "data-raw/01_init_params_nipt.csv",
                                file.init_scan = "data-raw/01_init_params_scan.csv")




# val_input <- generate_val_input(l_all_params) 

# l_out_val <- val_decision_model(val_input = val_input, file.val_params = "data-raw/04_val_params_decision_tree.csv")

# 1 --- Validation of:
#     1. prevalence t21, t18, t13, sa, rat
#     2. live birth prevalence t21, t18, t13
#     3. screen outcomes seo
#     4. uptake and screen outcomes fct
#     5. screen outcomes wg nipt
#     6. invasive test uptake per indication

validation_outcomes <- compare_with_val_targets(l_all_params = l_all_params,
                                                file.val_params = "data-raw/04_val_params_decision_tree.csv",
                                                file.val_targets_prev = "data-raw/04_validation_targets_prev.csv",
                                                file.val_targets_tris_birth_prev = "data-raw/04_validation_targets_tris_birth_prev.csv",
                                                file.val_targets_screen_wg = "data-raw/04_validation_targets_screen_wg.csv",
                                                file.val_targets_seo = "data-raw/04_validation_targets_seo.csv",
                                                file.val_targets_fct = "data-raw/04_validation_targets_fct.csv",
                                                file.val_targets_it = "data-raw/04_validation_targets_it.csv",
                                                file.dm_out_seo = "tables manuscript/detailed outcomes/cea_base_case_outc_strat1_R_1409.xlsx",
                                                file.dm_out_fct = "tables manuscript/detailed outcomes/cea_base_case_outc_strat2_R_1409.xlsx",
                                                file.dm_out_wg_nipt = "tables manuscript/detailed outcomes/cea_base_case_outc_strat4_R_1409.xlsx")



# 2 --- Save validation outcomes in excel files (manuscript supplemental tables S5 & figure S2)
#     1. prevalence t21, t18, t13, sa, rat 
openxlsx::write.xlsx(validation_outcomes$df_val_outc_prev, file = 'figs/fig_S2A_R.xlsx') 

#     2. live birth prevalence t21, t18, t13
openxlsx::write.xlsx(validation_outcomes$df_val_outc_birth_prev, file = 'figs/fig_S2B_R.xlsx') 

#     3. screen outcomes seo
openxlsx::write.xlsx(validation_outcomes$df_val_outc_seo, file = 'tables manuscript/table_S5A_R.xlsx') 

#     4. uptake and screen outcomes fct
openxlsx::write.xlsx(validation_outcomes$df_val_outc_fct, file = 'tables manuscript/table_S5B_R.xlsx') 

#     5. screen outcomes wg nipt
openxlsx::write.xlsx(validation_outcomes$df_val_outc_screen_wg, file = 'tables manuscript/table_S5C_R.xlsx') 

#     6. invasive test uptake per indication
openxlsx::write.xlsx(validation_outcomes$df_val_outc_it, file = 'tables manuscript/table_S5D_R.xlsx') 
