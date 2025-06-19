################################################################################ 
# This script runs the cohort implementation of the Sick-Sicker                #
# state-transition model (STM)                                                 #
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

#rm(list = ls()) # to clean the workspace

#### 02.1 Load packages and functions ####
#### 02.1.1 Load packages and functions ####
#library(dplyr)    # For data manipulation
#library(survival) # For plotting state-transition diagram

#### 02.1.3 Load functions ####
# no functions required

#### 02.2 Run STM ####
### Evaluate decision tree and store output
l_out_dm <- decision_model(cea_input = cea_input, file.init_tree = "data-raw/01_init_params_decision_tree.csv")

# Create output for Table 3 in manuscript
v_rows_table3 <- c('cost total population (M€)','cost per individual (€)','screened population','invasive tests','euploid fetal losses','total diagnosed fetal abnormalities',
                 't21 diagnosed','t18 diagnosed','t13 diagnosed','other fetal aberrations diagnosed',
                 'invasive tests per case diagnosed','cost per diagnosed case (k€)','incr. cost per diagnosed case (ref: scan) (k€)','incr. cost per diagnosed case (ref: fct) (k€)',
                 'incr. cost per diagnosed case (ref: targeted nipt) (k€)')
v_cols_table3 <- c('scan','scan & fct', "scan & targeted nipt","scan & wg nipt") 

table3 <- as.data.frame(matrix(NA,
                               nrow=length(v_rows_table3),
                               ncol=length(v_cols_table3)
                               ))

row.names(table3) <- v_rows_table3
colnames(table3) <- v_cols_table3

# fill table
table3[rownames(table3)=='cost total population (M€)',] <- c(round(l_out_dm$costs_strat1[1,1],0),
                                                             round(l_out_dm$costs_strat2[1,1],0),
                                                             round(l_out_dm$costs_strat3[1,1],0),
                                                             round(l_out_dm$costs_strat4[1,1],0)) / 1000000


table3[rownames(table3)=='screened population',] <- c(round(sum(l_out_dm$df_test_route_outc_tt_strat1[,c('screen','screen_it','screen_scan','screen_scan_it')]),0),
                                                      round(sum(l_out_dm$df_test_route_outc_tt_strat2[,c('screen','screen_it','screen_scan','screen_scan_it')]),0),
                                                      round(sum(l_out_dm$df_test_route_outc_tt_strat3[,c('screen','screen_it','screen_scan','screen_scan_it')]),0),
                                                      round(sum(l_out_dm$df_test_route_outc_tt_strat4[,c('screen','screen_it','screen_scan','screen_scan_it')]),0))

table3[rownames(table3)=='cost per individual (€)',] <- table3[rownames(table3)=='cost total population (M€)',] / table3[rownames(table3)=='screened population',] * 1000000 
table3[rownames(table3)=='cost per individual (€)','scan'] = NA


table3[rownames(table3)=='invasive tests',] <- c(round(sum(l_out_dm$df_it_outc_tt_strat1[,2]),0),
                                                 round(sum(l_out_dm$df_it_outc_tt_strat2[,2]),0),
                                                 round(sum(l_out_dm$df_it_outc_tt_strat3[,2]),0),
                                                 round(sum(l_out_dm$df_it_outc_tt_strat4[,2]),0))

table3[rownames(table3)=='euploid fetal losses',] <- c(round(sum(l_out_dm$df_birth_outc_tt_strat1$im[c(1,2,4,5,7)]),0), # 1 = 'no_aber', 2 = 'rat_cpm', 4 = 'rat_mat', 5 = 'sa_cpm', 7 = 'sa_mat'
                                                       round(sum(l_out_dm$df_birth_outc_tt_strat2$im[c(1,2,4,5,7)]),0),
                                                       round(sum(l_out_dm$df_birth_outc_tt_strat3$im[c(1,2,4,5,7)]),0),
                                                       round(sum(l_out_dm$df_birth_outc_tt_strat4$im[c(1,2,4,5,7)]),0))


table3[rownames(table3)=='t21 diagnosed',] <- c(round(sum(l_out_dm$df_screen_outc_tt_strat1[10,c('conf_itdirect','conf_scan','conf_screen')]),0), # row 10 = t21
                                                round(sum(l_out_dm$df_screen_outc_tt_strat2[10,c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                round(sum(l_out_dm$df_screen_outc_tt_strat3[10,c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                round(sum(l_out_dm$df_screen_outc_tt_strat4[10,c('conf_itdirect','conf_scan','conf_screen')]),0))

table3[rownames(table3)=='t18 diagnosed',] <- c(round(sum(l_out_dm$df_screen_outc_tt_strat1[9,c('conf_itdirect','conf_scan','conf_screen')]),0), # row 9 = t18
                                                round(sum(l_out_dm$df_screen_outc_tt_strat2[9,c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                round(sum(l_out_dm$df_screen_outc_tt_strat3[9,c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                round(sum(l_out_dm$df_screen_outc_tt_strat4[9,c('conf_itdirect','conf_scan','conf_screen')]),0))

table3[rownames(table3)=='t13 diagnosed',] <- c(round(sum(l_out_dm$df_screen_outc_tt_strat1[8,c('conf_itdirect','conf_scan','conf_screen')]),0), # row 8 = t13
                                                round(sum(l_out_dm$df_screen_outc_tt_strat2[8,c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                round(sum(l_out_dm$df_screen_outc_tt_strat3[8,c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                round(sum(l_out_dm$df_screen_outc_tt_strat4[8,c('conf_itdirect','conf_scan','conf_screen')]),0))

table3[rownames(table3)=='other fetal aberrations diagnosed',] <- c(round(sum(l_out_dm$df_screen_outc_tt_strat1[c(3,6),c('conf_itdirect','conf_scan','conf_screen')]),0), # row 3 = rat_fet, row 6 = sa_fet
                                                                    round(sum(l_out_dm$df_screen_outc_tt_strat2[c(3,6),c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                                    round(sum(l_out_dm$df_screen_outc_tt_strat3[c(3,6),c('conf_itdirect','conf_scan','conf_screen')]),0),
                                                                    round(sum(l_out_dm$df_screen_outc_tt_strat4[c(3,6),c('conf_itdirect','conf_scan','conf_screen')]),0))

table3[rownames(table3)=='total diagnosed fetal abnormalities',] <- colSums(table3[c('t21 diagnosed','t18 diagnosed','t13 diagnosed','other fetal aberrations diagnosed'),])


custom_order <- c("scan","scan & fct","scan & targeted nipt","scan & wg nipt")
order_indices <- order(match(l_out_dm$df_cea_icer$Strategy, custom_order))
sorted_df <- l_out_dm$df_cea_icer[order_indices , ]

table3[rownames(table3)=='invasive tests per case diagnosed',] <- (c(round(sum(l_out_dm$df_it_outc_tt_strat1[,2]),0),
                                                                     round(sum(l_out_dm$df_it_outc_tt_strat2[,2]),0),
                                                                     round(sum(l_out_dm$df_it_outc_tt_strat3[,2]),0),
                                                                     round(sum(l_out_dm$df_it_outc_tt_strat4[,2]),0))) / (round(sorted_df$Effect,0))
  
table3[rownames(table3)=='cost per diagnosed case (k€)',] <- (c(round(l_out_dm$costs_strat1[1,1],0),
                                                            round(l_out_dm$costs_strat2[1,1],0),
                                                            round(l_out_dm$costs_strat3[1,1],0),
                                                            round(l_out_dm$costs_strat4[1,1],0))) / (round(sorted_df$Effect,0)) / 1000

table3[rownames(table3)=='incr. cost per diagnosed case (ref: scan) (k€)',]          <- (table3[rownames(table3)=='cost total population (M€)',] - table3[rownames(table3)=='cost total population (M€)','scan']) * 1000/ 
                                                                                       (table3[rownames(table3)=='total diagnosed fetal abnormalities',] - table3[rownames(table3)=='total diagnosed fetal abnormalities','scan'])
table3[rownames(table3)=='incr. cost per diagnosed case (ref: scan) (k€)','scan'] = NA
  
table3[rownames(table3)=='incr. cost per diagnosed case (ref: fct) (k€)',]           <- (table3[rownames(table3)=='cost total population (M€)',] - table3[rownames(table3)=='cost total population (M€)','scan & fct']) * 1000/ 
                                                                                       (table3[rownames(table3)=='total diagnosed fetal abnormalities',] - table3[rownames(table3)=='total diagnosed fetal abnormalities','scan & fct'])
table3[rownames(table3)=='incr. cost per diagnosed case (ref: fct) (k€)',c('scan','scan & fct')] = NA


table3[rownames(table3)=='incr. cost per diagnosed case (ref: targeted nipt) (k€)',] <- (table3[rownames(table3)=='cost total population (M€)',] - table3[rownames(table3)=='cost total population (M€)','scan & targeted nipt']) * 1000/ 
                                                                                       (table3[rownames(table3)=='total diagnosed fetal abnormalities',] - table3[rownames(table3)=='total diagnosed fetal abnormalities','scan & targeted nipt'])

table3[rownames(table3)=='incr. cost per diagnosed case (ref: targeted nipt) (k€)',c('scan','scan & fct','scan & targeted nipt')] = NA

# Save table 3 in Excel
#openxlsx::write.xlsx(table3, file = '../paper cea nipt/tables manuscript/table_3_R_new_prices_final.xlsx', rowNames = TRUE) 



# --- Plot cea output

l_out_dm$df_cea_icer$Strategy <- c('Scan','Scan and FCT', 'Scan and targeted NIPT','Scan and targeted- or GW-NIPT')

plot(l_out_dm$df_cea_icer,currency = "€",effect_units=" Total number of fetal chromosomal abnormalities detected",
     label="all") + 
  theme_classic() +
  ggtitle("Cost-Effectiveness of Prenatal Screening Strategies") +
 theme(plot.title = element_text(hjust = 0.5))  +
  xlab("Effects (Total number of fetal chromosomal abnormalities detected") +  # Change "Your X-axis Label" to your desired X-axis label
  ylab("Total costs of screening programme (€)")  # Change Y-axis label

#ggsave("figs/02_base_case_plot_figure_S1_rebuttal2.png", width = 8, height = 6, dpi = 300)


# save screen outcomes in excel files
#library(openxlsx)

# Save outcomes per strategy in Excel
# Strategy 1
l_outc_strat1_excel <- list('base_case_screen_outc' = l_out_dm$df_screen_outc_tt_strat1,'base_case_birth_outc' = l_out_dm$df_birth_outc_tt_strat1,
                            'base_case_n_it' = l_out_dm$df_it_outc_tt_strat1,'base_case_n_test_route' = l_out_dm$df_test_route_outc_tt_strat1,
                            'base_case_costs' = l_out_dm$costs_strat1)
#openxlsx::write.xlsx(l_outc_strat1_excel, file = '../paper cea nipt/tables manuscript/detailed outcomes/cea_base_case_outc_strat1_R_1409.xlsx') 


# Strategy 2
#define sheet names for each data frame
l_outc_strat2_excel <- list('base_case_screen_outc' = l_out_dm$df_screen_outc_tt_strat2,'base_case_birth_outc' = l_out_dm$df_birth_outc_tt_strat2,
                            'base_case_n_it' = l_out_dm$df_it_outc_tt_strat2,'base_case_n_test_route' = l_out_dm$df_test_route_outc_tt_strat2,
                            'base_case_costs' = l_out_dm$costs_strat2)
#openxlsx::write.xlsx(l_outc_strat2_excel, file = '../paper cea nipt/tables manuscript/detailed outcomes/cea_base_case_outc_strat2_R_1409.xlsx') 

# Strategy 3
#define sheet names for each data frame
l_outc_strat3_excel <- list('base_case_screen_outc' = l_out_dm$df_screen_outc_tt_strat3,'base_case_birth_outc' = l_out_dm$df_birth_outc_tt_strat3,
                            'base_case_n_it' = l_out_dm$df_it_outc_tt_strat3,'base_case_n_test_route' = l_out_dm$df_test_route_outc_tt_strat3,
                            'base_case_costs' = l_out_dm$costs_strat3)
#openxlsx::write.xlsx(l_outc_strat3_excel, file = '../paper cea nipt/tables manuscript/detailed outcomes/cea_base_case_outc_strat3_R_1409.xlsx') 


# Strategy 4
#define sheet names for each data frame
l_outc_strat4_excel <- list('base_case_screen_outc' = l_out_dm$df_screen_outc_tt_strat4,'base_case_birth_outc' = l_out_dm$df_birth_outc_tt_strat4,
                            'base_case_n_it' = l_out_dm$df_it_outc_tt_strat4,'base_case_n_test_route' = l_out_dm$df_test_route_outc_tt_strat4,
                            'base_case_costs' = l_out_dm$costs_strat4)
#openxlsx::write.xlsx(l_outc_strat4_excel, file = '../paper cea nipt/tables manuscript/detailed outcomes/cea_base_case_outc_strat4_R_1409.xlsx') 



