################################################################################ 
# This script generates all the required input parameters for the cohort       #
# implementation of the Sick-Sicker state-transition model (STM)               #
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

#### 01.1 Load packages and functions ####
#### 01.1.1 Load packages and functions ####
# no required packages

#### 01.1.2 Load functions ####
# no required functions

#### 01.2 Load all parameters ####
l_all_params <- load_all_params(file.init_gen = "data-raw/01_init_params_general.csv",
                                file.init_pop = "data-raw/01_init_params_pop.csv",
                                file.init_uptake = "data-raw/01_init_params_uptake_screen.csv",
                                file.init_tris = "data-raw/01_init_params_prev_common_tris.csv",
                                file.init_fct = "data-raw/01_init_params_fct.csv",
                                file.init_nipt = "data-raw/01_init_params_nipt.csv",
                                file.init_scan = "data-raw/01_init_params_scan.csv")


cea_input <- generate_decision_model_input(l_all_params) 



