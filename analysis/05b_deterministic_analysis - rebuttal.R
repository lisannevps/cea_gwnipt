################################################################################ 
# This script runs the cost-effectiveness analysis of a hypothetical treatment #
# for the simulated cohort of the Sick-Sicker state-transition model (STM)     #
#                                                                              #                                                                          # 
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

#### 05b.6 Deterministic sensitivity analysis (DSA) ####
#### 05b.6.1 One-way sensitivity analysis (OWSA) ####

# TEST
owsa_uptake_fct_prob <- owsa_det(parms = c('v_p_fct_part'), # parameter names
                        ranges = list("v_p_fct_part" = c(-0.2,0.2,'prob','prob_all_param')),
                        nsamps = 5, # number of values  
                        params_basecase = cea_input # List with base-case parameters
)


# Save as .Rdata file
save(owsa_uptake_fct_prob,file = "data/05b_owsa_uptake_fct_prob_020924.RData")
# load("data/05b_owsa_uptake_fct_prob_020924.RData")



owsa_uptake_nipt_prob <- owsa_det(parms = c('v_p_nipt_part'), # parameter names
                             ranges = list("v_p_nipt_part" = c(-0.2,0.2,'prob','prob_all_param')),
                             nsamps = 5, # number of values  
                             params_basecase = cea_input # List with base-case parameters
)


# Save as .Rdata file
save(owsa_uptake_nipt_prob,file = "data/05b_owsa_uptake_nipt_prob_020924.RData")
# load("data/05b_owsa_uptake_nipt_prob_020924.RData")



# --- 1 OWSA: uptake rates
owsa_uptake <- owsa_det(parms = c('v_p_fct_part',
                                      'v_p_nipt_part',
                                      'v_p_targ_nipt',
                                      'p_part_scan',
                                      'p_part_it_direct',
                                      'p_part_it_fct',
                                      'p_part_it_screen',
                                      'p_part_it_rat_fet',
                                      'p_part_it_rat_other',
                                      'p_part_it_sa_fet',
                                      'p_part_it_sa_other',
                                      'p_part_it_scan'), # parameter names
                            ranges = list("v_p_fct_part" = c(0.1,1.0,'lim','prob_all_param'),
                                          "v_p_nipt_part" = c(0.1,1.0,'lim','prob_all_param'),
                                          "v_p_targ_nipt" = c(0.1,1.0,'lim','prob_all_param'),
                                          "p_part_scan" = c(-0.2,0.2,'prob','prob_all_param'),
                                          "p_part_it_direct" = c(-0.2,0.2,'prob','prob_all_param'),
                                          "p_part_it_fct" = c(-0.2,0.2,'prob','prob_all_param'),
                                          "p_part_it_screen" = c(-0.2,0.2,'prob','prob_all_param'),
                                          "p_part_it_rat_fet" = c(-0.2,0.2,'prob','prob_all_param'),
                                          "p_part_it_rat_other" = c(-0.2,0.2,'prob','prob_all_param'),
                                          "p_part_it_sa_fet" = c(-0.2,0.2,'prob','prob_all_param'),
                                          "p_part_it_sa_other" = c(-0.2,0.2,'prob','prob_all_param'),
                                          "p_part_it_scan" = c(-0.2,0.2,'prob','prob_all_param')),
                            nsamps = 5, # number of values  
                            params_basecase = cea_input # List with base-case parameters
)

# Save as .Rdata file
save(owsa_uptake,file = "data/05b_owsa_uptake_new_prices_270824.RData")
# load("data/05b_owsa_uptake_new_prices_270824.RData")

#openxlsx::write.xlsx(owsa_uptake, file = '../paper cea nipt/tables manuscript/detailed outcomes/owsa_uptake_new_prices_2.xlsx') 

# --- 2 OWSA: prev RAT & SA
owsa_prev <- owsa_det(parms = c('p_rat',
                                      'p_sa'), # parameter names
                            ranges = list("p_rat" = c(-0.1,0.1,'prob','prob_all_param'),
                                          "p_sa" = c(-0.1,0.1,'prob','prob_all_param')),
                            nsamps = 5, # number of values  
                            params_basecase = cea_input # List with base-case parameters
)



# Save as .Rdata file
owsa_prev_rebuttal = owsa_prev
save(owsa_prev_rebuttal,file = "data/05b_owsa_prev_rebuttal.RData")
# load("data/05b_owsa_prev_rebuttal.RData")

#openxlsx::write.xlsx(owsa_prev, file = '../paper cea nipt/tables manuscript/detailed outcomes/owsa_prev_new_prices_2.xlsx') 


# --- 3 OWSA: test screen characteristics
owsa_screen_char <- owsa_det(parms = c('p_fct_sens_t21',
                                       'p_fct_sens_t18',
                                       'p_fct_sens_t13',
                                       'p_fct_fpr',
                                       'p_nipt_sens_t21',
                                       'p_nipt_sens_t18',
                                       'p_nipt_sens_t13',
                                       'p_nipt_fpr',
                                       'p_nipt_fail',
                                       'p_nipt_fail_2'), # parameter names
                             ranges = list("p_fct_sens_t21" = c(0.76,0.95,'lim','prob_all_param'),
                                           "p_fct_sens_t18" = c(0.85,0.95,'lim','prob_all_param'),
                                           "p_fct_sens_t13" = c(0.75,0.90,'lim','prob_all_param'),
                                           "p_fct_fpr" = c(0.002,0.084,'lim','prob_all_param'),
                                           "p_nipt_sens_t21" = c(0.9781,0.9934,'lim','prob_all_param'),
                                           "p_nipt_sens_t18" = c(0.9545,0.9997,'lim','prob_all_param'),
                                           "p_nipt_sens_t13" = c(0.999,1,'lim','prob_all_param'),
                                           "p_nipt_fpr" = c(0.0002,0.0008,'lim','prob_all_param'),
                                           "p_nipt_fail" = c(-0.2,0.2,'prob','prob_all_param'),
                                           "p_nipt_fail_2" = c(-0.2,0.2,'prob','prob_all_param')
                             ),
                             nsamps = 5, # number of values  
                             params_basecase = cea_input # List with base-case parameters
)

# Save as .Rdata file
save(owsa_screen_char,file = "data/05b_owsa_screen_char_new_prices_270824.RData")
# load("data/05b_owsa_screen_char_new_prices_270824.RData")


owsa_screen_char <- owsa_det(parms = c('p_nipt_fail',
                                       'p_nipt_fail_2'), # parameter names
                             ranges = list("p_nipt_fail" = c(-0.1,0.1,'prob','prob_all_param'),
                                           "p_nipt_fail_2" = c(-0.1,0.1,'prob','prob_all_param')
                             ),
                             nsamps = 5, # number of values  
                             params_basecase = cea_input # List with base-case parameters
)

# Save as .Rdata file
owsa_screen_char_rebuttal = owsa_screen_char
# save(owsa_screen_char_rebuttal,file = "data/05b_owsa_nipt_fail_rebuttal.RData")
# load("data/05b_owsa_nipt_fail_rebuttal.RData")


# openxlsx::write.xlsx(owsa_screen_char, file = '../paper cea nipt/tables manuscript/detailed outcomes/owsa_screen_char_new_prices_2.xlsx') 



# 4. --- OWSA: scan characteristics
owsa_scan_char <- owsa_det(parms = c('p_scan_sens_t21',
                                     'p_scan_sens_t18',
                                     'p_scan_sens_t13',
                                     'p_scan_sens_rat',
                                     'p_scan_sens_sa',
                                     'p_scan_sens_no_chrom',
                                     'p_adv_scan_conf_chrom',
                                     'p_adv_scan_conf_no_chrom'), # parameter names
                           ranges = list("p_scan_sens_t21" = c(0.4,0.69,'lim','prob_all_param'),
                                         "p_scan_sens_t18" = c(-0.1,0.1,'prob','prob_all_param'),
                                         "p_scan_sens_t13" = c(-0.1,0.1,'prob','prob_all_param'),
                                         "p_scan_sens_rat" = c(-0.1,0.1,'prob','prob_all_param'),
                                         "p_scan_sens_sa" = c(-0.1,0.1,'prob','prob_all_param'),
                                         "p_scan_sens_no_chrom" = c(-0.1,0.1,'prob','prob_all_param'),
                                         "p_adv_scan_conf_chrom" = c(-0.1,0.1,'prob','prob_all_param'),
                                         "p_adv_scan_conf_no_chrom" = c(-0.1,0.1,'prob','prob_all_param')),
                           nsamps = 5, # number of values  
                           params_basecase = cea_input # List with base-case parameters
)

# Save as .Rdata file
owsa_scan_char_rebuttal = owsa_scan_char
save(owsa_scan_char_rebuttal,file = "data/05b_owsa_scan_char_rebuttal.RData")
# load("data/05b_owsa_scan_char_rebuttal.RData")

# openxlsx::write.xlsx(owsa_scan_char, file = '../paper cea nipt/tables manuscript/detailed outcomes/owsa_scan_char_new_prices_2.xlsx') 




# 5 --- OWSA: costs
owsa_costs <- owsa_det(parms = c('c_fct',
                                 'c_nipt',
                                 'c_it',
                                 'c_gen_fu',
                                 'c_anom_scan',
                                 'c_biom_scan',
                                 'c_adv_scan',
                                 'c_pre_couns',
                                 'c_post_couns'), # parameter names
                       ranges = list("c_fct" = c(-0.5,0.5,'prob','cost'),
                                     "c_nipt" = c(50,1000,'lim','cost'),
                                     "c_it" = c(-0.5,0.5,'prob','cost'),
                                     "c_gen_fu" = c(-0.5,0.5,'prob','cost'),
                                     "c_anom_scan" = c(-0.5,0.5,'prob','cost'),
                                     "c_biom_scan" = c(-0.5,0.5,'prob','cost'),
                                     "c_adv_scan" = c(-0.5,0.5,'prob','cost'),
                                     "c_pre_couns" = c(-0.5,0.5,'prob','cost'),
                                     "c_post_couns" = c(-0.5,0.5,'prob','cost'),
                                     "c_clin_gen_simple" = c(-0.5,0.5,'prob','cost'),
                                     "c_clin_gen_complex" = c(-0.5,0.5,'prob','cost')),
                       nsamps = 5, # number of values  
                       params_basecase = cea_input # List with base-case parameters
)



# Save as .Rdata file
save(owsa_costs,file = "data/05b_owsa_costs_new_prices_27082024.RData")
# load("data/05b_owsa_costs_new_prices_27082024.RData")

# openxlsx::write.xlsx(owsa_costs, file = '../paper cea nipt/tables manuscript/detailed outcomes/owsa_costs_new_prices_2.xlsx') 


