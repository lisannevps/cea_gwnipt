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

#### 05c Scenario analysis ####
l_all_params_scen <- load_all_params(file.init_gen = "data-raw/01_init_params_general - scenario3.csv",
                                file.init_pop = "data-raw/01_init_params_pop.csv",
                                file.init_uptake = "data-raw/05c_init_params_uptake_screen - scenario4.csv", # only difference with base case analysis is other inputs for screening test uptake
                                file.init_tris = "data-raw/01_init_params_prev_common_tris.csv",
                                file.init_fct = "data-raw/01_init_params_fct.csv",
                                file.init_nipt = "data-raw/01_init_params_nipt.csv",
                                file.init_scan = "data-raw/01_init_params_scan.csv")

cea_input_scen <- generate_decision_model_input(l_all_params_scen) 
l_out_dm_scen <- decision_model(cea_input = cea_input_scen, file.init_tree = "data-raw/01_init_params_decision_tree.csv")


# Create output for Table S7 in manuscript
v_rows_table_s7 <- c('cost total population (M€)','cost per individual (€)','screened population','invasive tests','euploid fetal losses','total diagnosed fetal abnormalities',
                   't21 diagnosed','t18 diagnosed','t13 diagnosed','other fetal aberrations diagnosed',
                   'invasive tests per case diagnosed','cost per diagnosed case (k€)','incr. cost per diagnosed case (ref: scan) (k€)','incr. cost per diagnosed case (ref: fct) (k€)',
                   'incr. cost per diagnosed case (ref: targeted nipt) (k€)')
v_cols_table_s7 <- c('scan','scan & fct', "scan & targeted nipt","scan & wg nipt") 

table_s7 <- as.data.frame(matrix(NA,
                               nrow=length(v_rows_table_s7),
                               ncol=length(v_cols_table_s7)
))

row.names(table_s7) <- v_rows_table_s7
colnames(table_s7) <- v_cols_table_s7

# fill table
table_s7[rownames(table_s7)=='cost total population (M€)',] <- c(round(l_out_dm_scen$costs_strat1[1,1],0),
                                                             round(l_out_dm_scen$costs_strat2[1,1],0),
                                                             round(l_out_dm_scen$costs_strat3[1,1],0),
                                                             round(l_out_dm_scen$costs_strat4[1,1],0)) / 1000000


table_s7[rownames(table_s7)=='screened population',] <- c(round(sum(l_out_dm_scen$df_test_route_outc_tt_strat1[,c('screen','screen_it','screen_scan','screen_scan_it')]),0),
                                                      round(sum(l_out_dm_scen$df_test_route_outc_tt_strat2[,c('screen','screen_it','screen_scan','screen_scan_it')]),0),
                                                      round(sum(l_out_dm_scen$df_test_route_outc_tt_strat3[,c('screen','screen_it','screen_scan','screen_scan_it')]),0),
                                                      round(sum(l_out_dm_scen$df_test_route_outc_tt_strat4[,c('screen','screen_it','screen_scan','screen_scan_it')]),0))

table_s7[rownames(table_s7)=='cost per individual (€)',] <- table_s7[rownames(table_s7)=='cost total population (M€)',] / table_s7[rownames(table_s7)=='screened population',] * 1000000 
table_s7[rownames(table_s7)=='cost per individual (€)','scan'] = NA


table_s7[rownames(table_s7)=='invasive tests',] <- c(round(sum(l_out_dm_scen$df_it_outc_tt_strat1[,2]),0),
                                                 round(sum(l_out_dm_scen$df_it_outc_tt_strat2[,2]),0),
                                                 round(sum(l_out_dm_scen$df_it_outc_tt_strat3[,2]),0),
                                                 round(sum(l_out_dm_scen$df_it_outc_tt_strat4[,2]),0))

table_s7[rownames(table_s7)=='euploid fetal losses',] <- c(round(sum(l_out_dm_scen$df_birth_outc_tt_strat1$im[c(1,2,4,5,7)]),0), # 1 = 'no_aber', 2 = 'rat_cpm', 4 = 'rat_mat', 5 = 'sa_cpm', 7 = 'sa_mat'
                                                       round(sum(l_out_dm_scen$df_birth_outc_tt_strat2$im[c(1,2,4,5,7)]),0),
                                                       round(sum(l_out_dm_scen$df_birth_outc_tt_strat3$im[c(1,2,4,5,7)]),0),
                                                       round(sum(l_out_dm_scen$df_birth_outc_tt_strat4$im[c(1,2,4,5,7)]),0))


table_s7[rownames(table_s7)=='t21 diagnosed',] <- c(round(sum(l_out_dm_scen$df_screen_outc_tt_strat1[10,c('conf_itdirect','conf_scan','conf_screen')]),0), # row 10 = t21
                                                round(sum(l_out_dm_scen$df_screen_outc_tt_strat2[10,c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                round(sum(l_out_dm_scen$df_screen_outc_tt_strat3[10,c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                round(sum(l_out_dm_scen$df_screen_outc_tt_strat4[10,c('conf_itdirect','conf_scan','conf_screen')]),0))

table_s7[rownames(table_s7)=='t18 diagnosed',] <- c(round(sum(l_out_dm_scen$df_screen_outc_tt_strat1[9,c('conf_itdirect','conf_scan','conf_screen')]),0), # row 9 = t18
                                                round(sum(l_out_dm_scen$df_screen_outc_tt_strat2[9,c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                round(sum(l_out_dm_scen$df_screen_outc_tt_strat3[9,c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                round(sum(l_out_dm_scen$df_screen_outc_tt_strat4[9,c('conf_itdirect','conf_scan','conf_screen')]),0))

table_s7[rownames(table_s7)=='t13 diagnosed',] <- c(round(sum(l_out_dm_scen$df_screen_outc_tt_strat1[8,c('conf_itdirect','conf_scan','conf_screen')]),0), # row 8 = t13
                                                round(sum(l_out_dm_scen$df_screen_outc_tt_strat2[8,c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                round(sum(l_out_dm_scen$df_screen_outc_tt_strat3[8,c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                round(sum(l_out_dm_scen$df_screen_outc_tt_strat4[8,c('conf_itdirect','conf_scan','conf_screen')]),0))

table_s7[rownames(table_s7)=='other fetal aberrations diagnosed',] <- c(round(sum(l_out_dm_scen$df_screen_outc_tt_strat1[c(3,6),c('conf_itdirect','conf_scan','conf_screen')]),0), # row 3 = rat_fet, row 6 = sa_fet
                                                                    round(sum(l_out_dm_scen$df_screen_outc_tt_strat2[c(3,6),c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                                    round(sum(l_out_dm_scen$df_screen_outc_tt_strat3[c(3,6),c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                                    round(sum(l_out_dm_scen$df_screen_outc_tt_strat4[c(3,6),c('conf_itdirect','conf_scan','conf_screen')]),0))

table_s7[rownames(table_s7)=='total diagnosed fetal abnormalities',] <- colSums(table_s7[c('t21 diagnosed','t18 diagnosed','t13 diagnosed','other fetal aberrations diagnosed'),])



table_s7[rownames(table_s7)=='invasive tests per case diagnosed',] <- (c(round(sum(l_out_dm_scen$df_it_outc_tt_strat1[,2]),0),
                                                                     round(sum(l_out_dm_scen$df_it_outc_tt_strat2[,2]),0),
                                                                     round(sum(l_out_dm_scen$df_it_outc_tt_strat3[,2]),0),
                                                                     round(sum(l_out_dm_scen$df_it_outc_tt_strat4[,2]),0))) / (round(l_out_dm_scen$df_cea_icer$Effect,0))

custom_order <- c("scan","scan & fct","scan & targeted nipt","scan & wg nipt")
order_indices <- order(match(l_out_dm_scen$df_cea_icer$Strategy, custom_order))
sorted_df <- l_out_dm_scen$df_cea_icer[order_indices , ]

table_s7[rownames(table_s7)=='cost per diagnosed case (k€)',] <- (c(round(l_out_dm_scen$costs_strat1[1,1],0),
                                                                round(l_out_dm_scen$costs_strat2[1,1],0),
                                                                round(l_out_dm_scen$costs_strat3[1,1],0),
                                                                round(l_out_dm_scen$costs_strat4[1,1],0))) / (round(sorted_df$Effect,0)) / 1000

table_s7[rownames(table_s7)=='incr. cost per diagnosed case (ref: scan) (k€)',]          <- (table_s7[rownames(table_s7)=='cost total population (M€)',] - table_s7[rownames(table_s7)=='cost total population (M€)','scan']) * 1000/ 
  (table_s7[rownames(table_s7)=='total diagnosed fetal abnormalities',] - table_s7[rownames(table_s7)=='total diagnosed fetal abnormalities','scan'])
table_s7[rownames(table_s7)=='incr. cost per diagnosed case (ref: scan) (k€)','scan'] = NA

table_s7[rownames(table_s7)=='incr. cost per diagnosed case (ref: fct) (k€)',]           <- (table_s7[rownames(table_s7)=='cost total population (M€)',] - table_s7[rownames(table_s7)=='cost total population (M€)','scan & fct']) * 1000/ 
  (table_s7[rownames(table_s7)=='total diagnosed fetal abnormalities',] - table_s7[rownames(table_s7)=='total diagnosed fetal abnormalities','scan & fct'])
table_s7[rownames(table_s7)=='incr. cost per diagnosed case (ref: fct) (k€)',c('scan','scan & fct')] = NA


table_s7[rownames(table_s7)=='incr. cost per diagnosed case (ref: targeted nipt) (k€)',] <- (table_s7[rownames(table_s7)=='cost total population (M€)',] - table_s7[rownames(table_s7)=='cost total population (M€)','scan & targeted nipt']) * 1000/ 
  (table_s7[rownames(table_s7)=='total diagnosed fetal abnormalities',] - table_s7[rownames(table_s7)=='total diagnosed fetal abnormalities','scan & targeted nipt'])

table_s7[rownames(table_s7)=='incr. cost per diagnosed case (ref: targeted nipt) (k€)',c('scan','scan & fct','scan & targeted nipt')] = NA

#table_s8 =  table_s7
#openxlsx::write.xlsx(table_s7, file = '../paper cea nipt/tables manuscript/table_s7_rebuttal.xlsx', rowNames = TRUE) 

#table_s9 = table_s7
#openxlsx::write.xlsx(table_s9, file = '../paper cea nipt/tables manuscript/table_s9_rebuttal.xlsx', rowNames = TRUE) 

table_s10 = table_s7
openxlsx::write.xlsx(table_s10, file = '../paper cea nipt/tables manuscript/table_s10_rebuttal.xlsx', rowNames = TRUE) 

# Save table 3 in Excel
#openxlsx::write.xlsx(table_s7, file = '../paper cea nipt/tables manuscript/table_s7_R_new_prices.xlsx', rowNames = TRUE) 



# --- Plot cea output
plot(l_out_dm_scen$df_cea_icer,currency = "€",effect_units="Fetal chromosomal aberrations detected",
     label="all") + 
  theme_classic() +
  ggtitle("Cost-effectiveness of prenatal screening strategies") +
  theme(plot.title = element_text(hjust = 0.5))
#ggsave("figs/05_scenario_plot.png", width = 8, height = 6)




