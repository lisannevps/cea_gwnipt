#' Decision Model
#'
#' \code{decision_model} implements the decision model used.
#'
#' @param l_params_all List with all parameters of decision model
#' @return 
#' The transition probability array and the cohort trace matrix.
#' @export
#' 
decision_model <- function(cea_input, file.init_tree = NULL){ # User defined
  ### Definition:
  ##   Decision model implementation function
  ### Arguments:  
  ##   cea_input: List with all input parameters to be able to implement the decision model
  ##   file.init_tree: file containing placeholder input parameters to be able to set up the decision tree
  ### Returns:
  ##  List with screening outcomes, birth outcomes, invasive tests performed, test routes, costs and ICER of the four strategies 
 
  #### Run decision model to get start information (number of paths per strategy, 
  # and which paths belong to which outcome measure [screen outcome & birth outcome]) using init tree .csv file 
  # containing a list with all parameters and placeholder values ####
 
   df_params_init_tree <- read.csv(file = file.init_tree, sep=";",stringsAsFactors = F)

   # Initialize data frame for screen outcomes for the different strategies
   # Outcomes are saved per analysis (age category x aberration category)
   # 1. Screen outcomes
   n_analyses <- cea_input$l_all_params$n_age * cea_input$l_all_params$n_chrom_aber_cat
  
   v_screen_outc           <- c('conf_itdirect','conf_scan','conf_screen',
                                'susp_scan','susp_screen','susp_screen_scan',
                                'miss_notest','miss_screen','miss_scan','miss_scan_adv','miss_screen_scan','miss_screen_scan_adv')
   df_screen_outc_strat1 <- as.data.frame(matrix(0,nrow = length(v_screen_outc),ncol = n_analyses))
   rownames(df_screen_outc_strat1) <- v_screen_outc
   colnames(df_screen_outc_strat1) <- rownames(cea_input[[c(3)]])
   df_screen_outc_strat2 <- df_screen_outc_strat3 <- df_screen_outc_strat4 <- df_screen_outc_strat1
  
   # 2. Birth outcomes
   v_birth_out <- c('lb','im','top','iufd')
   df_birth_outc_strat1 <- as.data.frame(matrix(0,nrow = length(v_birth_out),ncol = n_analyses))
   rownames(df_birth_outc_strat1) <- v_birth_out
   colnames(df_birth_outc_strat1) <- rownames(cea_input[[c(3)]])
   df_birth_outc_strat2 <- df_birth_outc_strat3 <- df_birth_outc_strat4 <- df_birth_outc_strat1
   
   # 3. Number of invasive tests performed
   df_it_outc_strat1 <- as.data.frame(matrix(0,nrow = n_analyses))
   colnames(df_it_outc_strat1) <- 'n_it'
   rownames(df_it_outc_strat1) <- rownames(cea_input[[c(3)]])
   df_it_outc_strat4 <- df_it_outc_strat3 <- df_it_outc_strat2 <- df_it_outc_strat1
   
   # 4. Test route
   v_test_route <- c('notest','itdirect','scan','scan_it','screen','screen_it','screen_scan','screen_scan_it','fail_itdirect')
   df_test_route_outc_strat1 <- as.data.frame(matrix(0,nrow = length(v_test_route),ncol = n_analyses))
   rownames(df_test_route_outc_strat1) <- v_test_route
   colnames(df_test_route_outc_strat1) <- rownames(cea_input[[c(3)]])
   df_test_route_outc_strat2 <- df_test_route_outc_strat3 <- df_test_route_outc_strat4 <- df_test_route_outc_strat1
   
   # 5. Born alive after confirmed or suspected fetal/cpm (to calculate costs of intensive monitoring of these cases)
   v_monitor_cases <- c('lb_conf_itdirect','lb_conf_scan','lb_conf_screen','lb_susp_scan','lb_susp_screen','lb_susp_screen_scan')
   df_monitor_outc_strat1 <- as.data.frame(matrix(0,nrow = length(v_monitor_cases),ncol = n_analyses))
   rownames(df_monitor_outc_strat1) <- v_monitor_cases
   colnames(df_monitor_outc_strat1) <- rownames(cea_input[[c(3)]])
   df_monitor_outc_strat4 <- df_monitor_outc_strat3 <- df_monitor_outc_strat2 <- df_monitor_outc_strat1
   
   # 6. Costs
   df_costs_strat1 <- as.data.frame(matrix(0,nrow=n_analyses))
   rownames(df_costs_strat1) <- rownames(cea_input[[c(3)]])
   colnames(df_costs_strat1) <- 'tt_costs'
   df_costs_strat2 <- df_costs_strat3 <- df_costs_strat4 <- df_costs_strat1
 
  
   # ---- Create outcome tables to save outcomes temporary and calculate summary outcomes to be saved 
   
   # -- Store placeholder parameters in global environment to be accessible for the evaluate_model function
   # This first evaluation of the decision tree with the placeholder parameters is used to set up all outcome tables
   # Hereafter the decision tree will be evaluated for every subcategory and outcomes are stored in the outcome tables
   v_1 <- colnames(df_params_init_tree)
   v_2 <- as.numeric(df_params_init_tree[1,])
   sapply(1:length(v_1),function(i) assign(v_1[i],v_2[i],envir = globalenv())) # store parameters in global environment to be accessible for evaluate_model function
   
   tree_outc <- evaluate_model(cea_input$name_tree,n_payoffs = 5)
   
   # Tree outcomes (per strategy):
   # c(,1) = path (1,2,3,...)
   # c(,2) = prob
   # c(,3) = payoff1 (screen outc, 10-21) [po_conf_itdirect,po_conf_scan,po_conf_screen,po_susp_scan,po_susp_screen,po_susp_screen_scan,po_miss_notest,po_miss_screen,po_miss_scan,po_miss_scan_adv,po_miss_screen_scan,po_miss_screen_scan_adv]
   # c(,4) = payoff2 (birth outc, 22-25) [po_lb, po_im, po_top, po_iufd]
   # c(,5) = payoff3 (invasive test yes/no, 1/0) 
   # c(,6) = payoff4 (test route, 1-9) [po_notest,po_itdirect,po_scan,po_scan_it,po_screen,po_screen_it,po_screen_scan,po_screen_scan_it,po_fail_itdirect]
   # c(,7) = payoff5 (costs) [c_nipt,c_fct,c_it,c_gen_fu,c_biom_scan,c_anom_scan,c_adv_scan,c_pre_couns,c_post_couns]
   
   # -- Create temporary tables
   # Column names are equal for all strategies (we calculate the same outputs for every strategy)
   v_tree_outc_names <- c('screen_outc','birth_outc','n_it','test_route','n_cases')
   n_col_outc <- length(v_tree_outc_names)
   
   # Get number of paths per strategy from decision tree
   n_paths_strat1 <- length(tree_outc[[c(1,1)]])
   n_paths_strat2 <- length(tree_outc[[c(2,1)]])
   n_paths_strat3 <- length(tree_outc[[c(3,1)]])
   n_paths_strat4 <- length(tree_outc[[c(4,1)]])
   
   # Create empty tables to save decision tree outcomes for each output
   df_outcomes_strat1 <- as.data.frame(matrix(0,nrow=n_paths_strat1,ncol=n_col_outc))
   df_outcomes_strat2 <- as.data.frame(matrix(0,nrow=n_paths_strat2,ncol=n_col_outc))
   df_outcomes_strat3 <- as.data.frame(matrix(0,nrow=n_paths_strat3,ncol=n_col_outc))
   df_outcomes_strat4 <- as.data.frame(matrix(0,nrow=n_paths_strat4,ncol=n_col_outc))
   
   # set column names
   colnames(df_outcomes_strat1) = colnames(df_outcomes_strat2) = colnames(df_outcomes_strat3) = colnames(df_outcomes_strat4) <- v_tree_outc_names
  
   # For every strategy, add the pay offs per path to be able to summarize outcomes over different paths with similar outcomes 
   df_outcomes_strat1[,1] <- tree_outc[[c(1,3)]]
   df_outcomes_strat1[,2] <- tree_outc[[c(1,4)]]
   df_outcomes_strat1[,3] <- tree_outc[[c(1,5)]]
   df_outcomes_strat1[,4] <- tree_outc[[c(1,6)]]
   
   df_outcomes_strat2[,1] <- tree_outc[[c(2,3)]]
   df_outcomes_strat2[,2] <- tree_outc[[c(2,4)]]
   df_outcomes_strat2[,3] <- tree_outc[[c(2,5)]]
   df_outcomes_strat2[,4] <- tree_outc[[c(2,6)]]
   
   df_outcomes_strat3[,1] <- tree_outc[[c(3,3)]]
   df_outcomes_strat3[,2] <- tree_outc[[c(3,4)]]
   df_outcomes_strat3[,3] <- tree_outc[[c(3,5)]]
   df_outcomes_strat3[,4] <- tree_outc[[c(3,6)]]
   
   df_outcomes_strat4[,1] <- tree_outc[[c(4,3)]]
   df_outcomes_strat4[,2] <- tree_outc[[c(4,4)]]
   df_outcomes_strat4[,3] <- tree_outc[[c(4,5)]]
   df_outcomes_strat4[,4] <- tree_outc[[c(4,6)]]



   #### ---- Calculate model outcomes for all subgroups and generate summary outcomes for each strategy ####
   df_probs <- cea_input[[c(3)]]

   # -- Add payoffs to global environment to be accessible within evaluate_model function
   # Costs (fixed for all analyses, therefore not in 'for' loop)
   v_cost_names <- c('c_nipt','c_fct','c_it','c_gen_fu','c_biom_scan','c_anom_scan','c_adv_scan','c_pre_couns','c_post_couns','c_clin_gen_simple','c_clin_gen_complex')
   v_cost_values <- c(cea_input$l_all_params$c_nipt,cea_input$l_all_params$c_fct,cea_input$l_all_params$c_it,
                      cea_input$l_all_params$c_gen_fu,
                      cea_input$l_all_params$c_biom_scan,cea_input$l_all_params$c_anom_scan,cea_input$l_all_params$c_adv_scan,
                      cea_input$l_all_params$c_pre_couns,cea_input$l_all_params$c_post_couns,cea_input$l_all_params$c_clin_gen_simple,cea_input$l_all_params$c_clin_gen_complex)
   
   sapply(1:length(v_cost_names),function(i) assign(v_cost_names[i],v_cost_values[i],envir = globalenv()))
   
   # Payoffs (1. screen path [1:9], 2. screen outcomes [10:19], 3. birth outcome categories [20:23]) (fixed for all analyses)
   v_payoffs_names <- c('po_notest','po_itdirect','po_scan','po_scan_it','po_screen','po_screen_it','po_screen_scan','po_screen_scan_it','po_fail_itdirect',
                        'po_conf_itdirect','po_conf_scan','po_conf_screen','po_susp_scan','po_susp_screen','po_susp_screen_scan',
                        'po_miss_notest','po_miss_screen','po_miss_scan','po_miss_scan_adv','po_miss_screen_scan','po_miss_screen_scan_adv',
                        'po_lb','po_im','po_top','po_iufd'
                        )
   v_payoffs_values <- 1:length(v_payoffs_names)
   
   sapply(1:length(v_payoffs_names),function(i) assign(v_payoffs_names[i],v_payoffs_values[i],envir = globalenv()))

   # -- Create vector with parameter names for the different strategies
   # These values are updated for every subgroup in the 'for' loop below
   v_strat1_param_names <- c('p_part_it_direct','p_part_scan','p_part_it_scan','p_perf_scan',
                             'p_anom_conf','p_im','p_top_it','p_top_scan','p_iufd')
   
   v_strat2_param_names <- c('p_part_it_direct','p_part_fct','p_part_scan_fct','p_part_it_fct','p_part_it_scan',
                             'p_perf_fct','p_perf_scan','p_anom_conf','p_perf_adv_scan',
                             'p_im','p_top_it','p_top_scan','p_iufd')
   
   v_strat3_param_names <- c('p_part_it_direct','p_part_nipt','p_part_scan_nipt','p_part_it_screen','p_part_it_scan',
                             'p_fail_nipt','p_2nd_fail_nipt',
                             'p_perf_targ_nipt','p_perf_scan','p_anom_conf','p_perf_adv_scan',
                             'p_im','p_top_it','p_top_scan','p_iufd')
   
   v_strat4_param_names <- c('p_part_it_direct','p_part_nipt','p_part_targ_nipt','p_part_scan_nipt','p_part_it_screen_wg','p_part_it_scan',
                             'p_fail_nipt','p_2nd_fail_nipt',
                             'p_perf_targ_nipt','p_perf_wg_nipt','p_perf_scan','p_anom_conf','p_perf_adv_scan',
                             'p_im','p_top_it','p_top_scan','p_iufd')
   
   v_pop_cea <- as.data.frame(as.vector(t(cea_input$decision_model_pop))) # for analysis with 'for loop'
   
   
   #-- Evaluate decision tree for every subgroup (age cat x chrom aber = 50)
   for(i in 1:n_analyses){
     
     #- Subtract probabilities from probs matrix
     # Strategy 1 (anomaly scan)
     v_strat1_param_values <- c(df_probs[i,"p_part_it_direct"],df_probs[i,"p_part_scan"],
                                df_probs[i,"p_part_it_scan"],df_probs[i,"p_perf_scan"],
                                df_probs[i,"p_anom_conf"], df_probs[i,"p_im"],
                                df_probs[i,"p_top_it"],df_probs[i,"p_top_scan"],
                                df_probs[i,"p_iufd"])
     
     sapply(1:length(v_strat1_param_names),function(i) assign(v_strat1_param_names[i],v_strat1_param_values[i],envir = globalenv()))
     
     # Strategy 2 (anomaly scan & FCT)
     v_strat2_param_values <- c(df_probs[i,"p_part_it_direct"],df_probs[i,"p_part_fct"],df_probs[i,"p_part_scan_fct"],
                                df_probs[i,"p_part_it_fct"],df_probs[i,"p_part_it_scan"],
                                df_probs[i,"p_perf_fct"],df_probs[i,"p_perf_scan"],df_probs[i,"p_anom_conf"],df_probs[i,"p_perf_adv_scan"],
                                df_probs[i,"p_im"],df_probs[i,"p_top_it"],df_probs[i,"p_top_scan"],df_probs[i,"p_iufd"])
     
     sapply(1:length(v_strat2_param_names),function(i) assign(v_strat2_param_names[i],v_strat2_param_values[i],envir = globalenv()))
     
     
     # Strategy 3 (anomaly scan & targeted NIPT)
     v_strat3_param_values <- c(df_probs[i,"p_part_it_direct"],df_probs[i,"p_part_nipt"],df_probs[i,"p_part_scan_nipt"],
                                df_probs[i,"p_part_it_screen"],df_probs[i,"p_part_it_scan"],
                                df_probs[i,"p_fail_nipt"],df_probs[i,"p_2nd_fail_nipt"],
                                df_probs[i,"p_perf_targ_nipt"],df_probs[i,"p_perf_scan"],df_probs[i,"p_anom_conf"],df_probs[i,"p_perf_adv_scan"],
                                df_probs[i,"p_im"],df_probs[i,"p_top_it"],df_probs[i,"p_top_scan"],df_probs[i,"p_iufd"])
     
     sapply(1:length(v_strat3_param_names),function(i) assign(v_strat3_param_names[i],v_strat3_param_values[i],envir = globalenv()))
     
  
     # Strategy 4 (anomaly scan & whole genome NIPT)
     v_strat4_param_values <- c(df_probs[i,"p_part_it_direct"],df_probs[i,"p_part_nipt"],df_probs[i,"p_part_targ_nipt"],df_probs[i,"p_part_scan_nipt"],
                                df_probs[i,"p_part_it_screen_wg"],df_probs[i,"p_part_it_scan"],
                                df_probs[i,"p_fail_nipt"],df_probs[i,"p_2nd_fail_nipt"],
                                df_probs[i,"p_perf_targ_nipt"],df_probs[i,"p_perf_wg_nipt"],df_probs[i,"p_perf_scan"],df_probs[i,"p_anom_conf"],df_probs[i,"p_perf_adv_scan"],
                                df_probs[i,"p_im"],df_probs[i,"p_top_it"],df_probs[i,"p_top_scan"],df_probs[i,"p_iufd"])
     
     sapply(1:length(v_strat4_param_names),function(i) assign(v_strat4_param_names[i],v_strat4_param_values[i],envir = globalenv()))
     

     
     #- Evaluate decision tree -> calculate path probabilities
     decision_model_outc <- evaluate_model(cea_input$name_tree, n_payoffs = 5)
     
     #- Calculate and save outcomes per strategy
     # Fill temporary table to be able to calculate screen and birth outcomes, number of inv tests performed and costs
     # Path prob x number of pregnant women in sub-category being analysed
     df_outcomes_strat1[,5] <- decision_model_outc[[c(1,2)]] * v_pop_cea[i,1] 
     df_outcomes_strat2[,5] <- decision_model_outc[[c(2,2)]] * v_pop_cea[i,1] 
     df_outcomes_strat3[,5] <- decision_model_outc[[c(3,2)]] * v_pop_cea[i,1] 
     df_outcomes_strat4[,5] <- decision_model_outc[[c(4,2)]] * v_pop_cea[i,1] 
     

     #- Update screen outcomes
     outc1_strat1 <- df_outcomes_strat1 %>% 
       group_by(screen_outc) %>%
       summarize(n = sum(n_cases)) 
     df_screen_outc_strat1[c('conf_itdirect','conf_scan','susp_scan','miss_notest','miss_scan','miss_scan_adv'),i] <- outc1_strat1[,2] # Sub selection of screening outcomes, because not all screening outcomes are applicable for this strategy
     
     outc1_strat2 <- df_outcomes_strat2 %>% 
       group_by(screen_outc) %>%
       summarize(n = sum(n_cases)) 
     df_screen_outc_strat2[,i] <- outc1_strat2[,2]
     
     outc1_strat3 <- df_outcomes_strat3 %>% 
       group_by(screen_outc) %>%
       summarize(n = sum(n_cases)) 
     df_screen_outc_strat3[,i] <- outc1_strat3[,2]
     
     outc1_strat4 <- df_outcomes_strat4 %>% 
       group_by(screen_outc) %>%
       summarize(n = sum(n_cases)) 
     df_screen_outc_strat4[,i] <- outc1_strat4[,2]
     
     # Update birth outcomes
     outc2_strat1 <- df_outcomes_strat1 %>% 
       group_by(birth_outc) %>%
       summarize(n = sum(n_cases))
     df_birth_outc_strat1[,i] <- outc2_strat1[,2]
     
     outc2_strat2 <- df_outcomes_strat2 %>% 
       group_by(birth_outc) %>%
       summarize(n = sum(n_cases))
     df_birth_outc_strat2[,i] <- outc2_strat2[,2]

     outc2_strat3 <- df_outcomes_strat3 %>% 
       group_by(birth_outc) %>%
       summarize(n = sum(n_cases))
     df_birth_outc_strat3[,i] <- outc2_strat3[,2]
     
     outc2_strat4 <- df_outcomes_strat4 %>% 
       group_by(birth_outc) %>%
       summarize(n = sum(n_cases))
     df_birth_outc_strat4[,i] <- outc2_strat4[,2]
     
     #- Update number of invasive tests performed
     df_it_outc_strat1[i,1] <- sum(df_outcomes_strat1[,5] * decision_model_outc[[c(1,5)]])
     df_it_outc_strat2[i,1] <- sum(df_outcomes_strat2[,5] * decision_model_outc[[c(2,5)]])
     df_it_outc_strat3[i,1] <- sum(df_outcomes_strat3[,5] * decision_model_outc[[c(3,5)]])
     df_it_outc_strat4[i,1] <- sum(df_outcomes_strat4[,5] * decision_model_outc[[c(4,5)]])
     
     #- Update test route
     outc4_strat1 <- df_outcomes_strat1 %>% 
       group_by(test_route) %>%
       summarize(n = sum(n_cases))
     df_test_route_outc_strat1[c('notest','itdirect','scan','scan_it'),i] <- outc4_strat1[,2] # sub selection as not all test routes are applicable for this strategy

     outc4_strat2 <- df_outcomes_strat2 %>% 
       group_by(test_route) %>%
       summarize(n = sum(n_cases))
     df_test_route_outc_strat2[c('notest','itdirect','scan','scan_it','screen','screen_it','screen_scan','screen_scan_it'),i] <- outc4_strat2[,2] # sub selection..
     
     outc4_strat3 <- df_outcomes_strat3 %>% 
       group_by(test_route) %>%
       summarize(n = sum(n_cases))
     df_test_route_outc_strat3[,i] <- outc4_strat3[,2]
     
     outc4_strat4 <- df_outcomes_strat4 %>% 
       group_by(test_route) %>%
       summarize(n = sum(n_cases))
     df_test_route_outc_strat4[,i] <- outc4_strat4[,2]
     
     #- Update number of cases to monitor (born alive after confirmed or suspected fetal/cpm, to calculate costs of FU of these cases with ultrasound))
     outc5_strat1 <- df_outcomes_strat1 %>% 
       filter(birth_outc==22 & screen_outc %in% c(10,11,12,13,14,15)) %>% # Only 10,11,13 applicable as no screening test is offered in this strategy 
       group_by(screen_outc) %>%
       summarize(n = sum(n_cases))
     df_monitor_outc_strat1[c('lb_conf_itdirect','lb_conf_scan','lb_susp_scan'),i] <- outc5_strat1[,2] # Sub selection as not all options applicable (no screening test in this strategy)
     
     outc5_strat2 <- df_outcomes_strat2 %>% 
       filter(birth_outc==22 & screen_outc %in% c(10,11,12,13,14,15)) %>%
       group_by(screen_outc) %>%
       summarize(n = sum(n_cases))
     df_monitor_outc_strat2[,i] <- outc5_strat2[,2] 
     
     outc5_strat3 <- df_outcomes_strat3 %>% 
       filter(birth_outc==22 & screen_outc %in% c(10,11,12,13,14,15)) %>%
       group_by(screen_outc) %>%
       summarize(n = sum(n_cases))
     df_monitor_outc_strat3[,i] <- outc5_strat3[,2] 
     
     outc5_strat4 <- df_outcomes_strat4 %>% 
       filter(birth_outc==22 & screen_outc %in% c(10,11,12,13,14,15)) %>%
       group_by(screen_outc) %>%
       summarize(n = sum(n_cases))
     df_monitor_outc_strat4[,i] <- outc5_strat4[,2] 
     
     #- Update costs
     df_costs_strat1[i,1] <- t(df_outcomes_strat1[,5]) %*% decision_model_outc[[c(1,7)]]
     df_costs_strat2[i,1] <- t(df_outcomes_strat2[,5]) %*% decision_model_outc[[c(2,7)]]
     df_costs_strat3[i,1] <- t(df_outcomes_strat3[,5]) %*% decision_model_outc[[c(3,7)]]
     df_costs_strat4[i,1] <- t(df_outcomes_strat4[,5]) %*% decision_model_outc[[c(4,7)]]
     
   }

   #- Transpose screening outcomes and save as data frame
   df_screen_outc_strat1 <- t(df_screen_outc_strat1)
   df_screen_outc_strat1 <- as.data.frame(df_screen_outc_strat1)
   
   df_screen_outc_strat2 <- t(df_screen_outc_strat2)
   df_screen_outc_strat2 <- as.data.frame(df_screen_outc_strat2)
   
   df_screen_outc_strat3 <- t(df_screen_outc_strat3)
   df_screen_outc_strat3 <- as.data.frame(df_screen_outc_strat3)
      
   df_screen_outc_strat4 <- t(df_screen_outc_strat4)
   df_screen_outc_strat4 <- as.data.frame(df_screen_outc_strat4)
   
   #- Transpose birth outcomes and save as data frame
   df_birth_outc_strat1 <- t(df_birth_outc_strat1)
   df_birth_outc_strat1 <- as.data.frame(df_birth_outc_strat1)
   
   df_birth_outc_strat2 <- t(df_birth_outc_strat2)
   df_birth_outc_strat2 <- as.data.frame(df_birth_outc_strat2)
   
   df_birth_outc_strat3 <- t(df_birth_outc_strat3)
   df_birth_outc_strat3 <- as.data.frame(df_birth_outc_strat3)
   
   df_birth_outc_strat4 <- t(df_birth_outc_strat4)
   df_birth_outc_strat4 <- as.data.frame(df_birth_outc_strat4)
   
   #- Save number of invasive tests as data frame
   df_it_outc_strat1 <- as.data.frame(df_it_outc_strat1)
   df_it_outc_strat2 <- as.data.frame(df_it_outc_strat2)
   df_it_outc_strat3 <- as.data.frame(df_it_outc_strat3)
   df_it_outc_strat4 <- as.data.frame(df_it_outc_strat4)
   
   #- Transpose outcomes of cases to be monitored and save as data frame
   df_monitor_outc_strat1 <- t(df_monitor_outc_strat1)
   df_monitor_outc_strat1 <- as.data.frame(df_monitor_outc_strat1)
   
   df_monitor_outc_strat2 <- t(df_monitor_outc_strat2)
   df_monitor_outc_strat2 <- as.data.frame(df_monitor_outc_strat2)
   
   df_monitor_outc_strat3 <- t(df_monitor_outc_strat3)
   df_monitor_outc_strat3 <- as.data.frame(df_monitor_outc_strat3)
   
   df_monitor_outc_strat4 <- t(df_monitor_outc_strat4)
   df_monitor_outc_strat4 <- as.data.frame(df_monitor_outc_strat4)
   
   #- Transpose test route outcomes and save as data frame
   df_test_route_outc_strat1 <- t(df_test_route_outc_strat1)
   df_test_route_outc_strat1 <- as.data.frame(df_test_route_outc_strat1)
   
   df_test_route_outc_strat2 <- t(df_test_route_outc_strat2)
   df_test_route_outc_strat2 <- as.data.frame(df_test_route_outc_strat2)
   
   df_test_route_outc_strat3 <- t(df_test_route_outc_strat3)
   df_test_route_outc_strat3 <- as.data.frame(df_test_route_outc_strat3)
   
   df_test_route_outc_strat4 <- t(df_test_route_outc_strat4)
   df_test_route_outc_strat4 <- as.data.frame(df_test_route_outc_strat4)
   
   #- Add column with aberration type to be able to summarize over chrom categories
   v_cat_names <-rep(cea_input$l_all_params$v_chrom_aber_cat,times=cea_input$l_all_params$n_age)
   
   df_screen_outc_strat1$cat = df_screen_outc_strat2$cat = df_screen_outc_strat3$cat = df_screen_outc_strat4$cat <- v_cat_names
   df_birth_outc_strat1$cat = df_birth_outc_strat2$cat = df_birth_outc_strat3$cat = df_birth_outc_strat4$cat <- v_cat_names
   df_it_outc_strat1$cat = df_it_outc_strat2$cat = df_it_outc_strat3$cat = df_it_outc_strat4$cat <- v_cat_names
   df_test_route_outc_strat1$cat = df_test_route_outc_strat2$cat = df_test_route_outc_strat3$cat = df_test_route_outc_strat4$cat <- v_cat_names
   df_monitor_outc_strat1$cat = df_monitor_outc_strat2$cat = df_monitor_outc_strat3$cat = df_monitor_outc_strat4$cat <- v_cat_names

   # Screening outcomes per category
   # For 'no aber'/ additional findings in case of FCT/targeted NIPT: confirmed, suspected = false positive, invasive testing will not show the aberration; missed = true negative
   df_screen_outc_tt_strat1 <- df_screen_outc_strat1 %>% 
     group_by(cat) %>%
     summarize_all(sum)
   
   df_screen_outc_tt_strat2 <- df_screen_outc_strat2 %>% 
     group_by(cat) %>%
     summarize_all(sum)
   
   df_screen_outc_tt_strat3 <- df_screen_outc_strat3 %>% 
     group_by(cat) %>%
     summarize_all(sum)
   
   df_screen_outc_tt_strat4 <- df_screen_outc_strat4 %>% 
     group_by(cat) %>%
     summarize_all(sum)
   
   # Birth outcomes per category
   df_birth_outc_tt_strat1 <- df_birth_outc_strat1 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   df_birth_outc_tt_strat2 <- df_birth_outc_strat2 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   df_birth_outc_tt_strat3 <- df_birth_outc_strat3 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   df_birth_outc_tt_strat4 <- df_birth_outc_strat4 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   # Number of invasive tests performed per category
   df_it_outc_tt_strat1 <- df_it_outc_strat1 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   df_it_outc_tt_strat2 <- df_it_outc_strat2 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   df_it_outc_tt_strat3 <- df_it_outc_strat3 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   df_it_outc_tt_strat4 <- df_it_outc_strat4 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   # Test routes per category
   df_test_route_outc_tt_strat1 <- df_test_route_outc_strat1 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   df_test_route_outc_tt_strat2 <- df_test_route_outc_strat2 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   df_test_route_outc_tt_strat3 <- df_test_route_outc_strat3 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   df_test_route_outc_tt_strat4 <- df_test_route_outc_strat4 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   # Confirmed/suspected cases born alive per category
   df_monitor_outc_tt_strat1 <- df_monitor_outc_strat1 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   df_monitor_outc_tt_strat2 <- df_monitor_outc_strat2 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   df_monitor_outc_tt_strat3 <- df_monitor_outc_strat3 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   df_monitor_outc_tt_strat4 <- df_monitor_outc_strat4 %>% 
     group_by(cat) %>%
     summarize_all(sum) 
   
   # Calculate total costs for every strategy
   v_costs <- matrix(NA,
                     nrow=1,
                     ncol=4)
   
   
   v_screen_outc           <- c('conf_itdirect','conf_scan','conf_screen',
                                'susp_scan','susp_screen','susp_screen_scan',
                                'miss_notest','miss_screen','miss_scan','miss_scan_adv','miss_screen_scan','miss_screen_scan_adv')
   
   v_test_route <- c('notest','itdirect','scan','scan_it','screen','screen_it','screen_scan','screen_scan_it','fail_itdirect')
   
   
   temp1  <- df_monitor_outc_tt_strat1 %>% filter(cat %in% c('t13','t18','t21','rat_fet','rat_cpm','sa_cpm','sa_fet'))
   temp12 <- df_screen_outc_tt_strat1 %>% filter(cat %in% c('t13','t18','t21','no_aber')) # common trisomy + false positive no_aber
   temp13 <- df_screen_outc_tt_strat1 %>% filter(cat %in% c('rat_cpm','rat_fet','rat_mat','sa_cpm','sa_fet','sa_mat')) # additional finding
   
   df_costs_tt_strat1 <- as.data.frame(round(sum(df_costs_strat1) +  # 1 (total costs of tests performed in screening strategy 1)
                                       cea_input$l_all_params$n_pop * cea_input$l_all_params$p_part_couns * c_pre_couns + # 2 (prenatal screening counseling coverage = 90.2% in 2020 according to screenings' monitor)
                                         sum(temp12[,c('susp_scan','susp_screen','susp_screen_scan',
                                                       'conf_itdirect','conf_scan','conf_screen')])*(c_clin_gen_simple) + # 3 (costs of counseling by clinical geneticist, common trisomy = simple)
                                         sum(temp13[,c('susp_scan','susp_screen','susp_screen_scan',
                                                       'conf_itdirect','conf_scan','conf_screen')])*(c_clin_gen_complex) +  # 4 (costs of counseling by clinical geneticist, additional finding = complex)
                                         sum(df_monitor_outc_tt_strat1[,c('lb_susp_scan','lb_susp_screen','lb_susp_screen_scan')])*(c_biom_scan * 3) + # 5 (monitoring/fu of cases with suspected fetal/cpm chrom aberrations with biometry ultrasound - these also contain false positives)
                                         sum(temp1[,c('lb_conf_itdirect','lb_conf_scan','lb_conf_screen')])*(c_biom_scan * 3),0)   # 6 (monitoring/fu of cases with confirmed fetal/cpm chrom aberrations -> therefor from 'temp')

   )
   colnames(df_costs_tt_strat1) <- "tt_costs_strat1"
   
   temp2 <- df_monitor_outc_tt_strat2 %>% filter(cat %in% c('t13','t18','t21','rat_fet','rat_cpm','sa_cpm','sa_fet'))
   temp22 <- df_screen_outc_tt_strat2 %>% filter(cat %in% c('t13','t18','t21','no_aber')) # common trisomy  + false positive no_aber
   temp23 <- df_screen_outc_tt_strat2 %>% filter(cat %in% c('rat_cpm','rat_fet','rat_mat','sa_cpm','sa_fet','sa_mat')) # additional finding
   df_costs_tt_strat2 <- as.data.frame(round(sum(df_costs_strat2) +  # 1
                                         cea_input$l_all_params$n_pop * cea_input$l_all_params$p_part_couns * c_pre_couns + # 2
                                           sum(temp22[,c('susp_scan','susp_screen','susp_screen_scan',
                                                         'conf_itdirect','conf_scan','conf_screen')])*(c_clin_gen_simple) + # 3 
                                           sum(temp23[,c('susp_scan','susp_screen','susp_screen_scan',
                                                         'conf_itdirect','conf_scan','conf_screen')])*(c_clin_gen_complex) + # 4  
                                         sum(df_monitor_outc_tt_strat2[,c('lb_susp_scan','lb_susp_screen','lb_susp_screen_scan')])*(c_biom_scan * 3) + # 5
                                         sum(temp2[,c('lb_conf_itdirect','lb_conf_scan','lb_conf_screen')])*(c_biom_scan * 3),0) # 6
                                    
                           
   )
   colnames(df_costs_tt_strat2) <- "tt_costs_strat2"
   
   temp3 <- df_monitor_outc_tt_strat3 %>% filter(cat %in% c('t13','t18','t21','rat_fet','rat_cpm','sa_cpm','sa_fet'))
   temp32 <- df_screen_outc_tt_strat3 %>% filter(cat %in% c('t13','t18','t21','no_aber')) # common trisomy + false positive no_aber
   temp33 <- df_screen_outc_tt_strat3 %>% filter(cat %in% c('rat_cpm','rat_fet','rat_mat','sa_cpm','sa_fet','sa_mat')) # additional finding
   df_costs_tt_strat3 <- as.data.frame(round(sum(df_costs_strat3) +  # 1
                                         cea_input$l_all_params$n_pop * cea_input$l_all_params$p_part_couns * c_pre_couns + # 2
                                           sum(temp32[,c('susp_scan','susp_screen','susp_screen_scan',
                                                         'conf_itdirect','conf_scan','conf_screen')])*(c_clin_gen_simple) + # 3 
                                           sum(temp33[,c('susp_scan','susp_screen','susp_screen_scan',
                                                         'conf_itdirect','conf_scan','conf_screen')])*(c_clin_gen_complex) + # 4 
                                         sum(df_monitor_outc_tt_strat3[,c('lb_susp_scan','lb_susp_screen','lb_susp_screen_scan')])*(c_biom_scan * 3) + # 5
                                         sum(temp3[,c('lb_conf_itdirect','lb_conf_scan','lb_conf_screen')])*(c_biom_scan * 3),0)  # 6

   )
   colnames(df_costs_tt_strat3) <- "tt_costs_strat3"
   
   temp41 <- df_monitor_outc_tt_strat4 %>% filter(cat %in% c('t13','t18','t21','rat_fet','rat_cpm','sa_fet','sa_cpm'))
   temp42 <- df_screen_outc_tt_strat4 %>% filter(cat %in% c('rat_fet','rat_cpm','rat_mat'))
   temp43 <- df_screen_outc_tt_strat4 %>% filter(cat %in% c('sa_fet','sa_cpm','sa_mat'))
   temp44 <- df_screen_outc_tt_strat4 %>% filter(cat %in% c('t13','t18','t21','no_aber')) # common trisomy + false positive no_aber
   temp45 <- df_screen_outc_tt_strat4 %>% filter(cat %in% c('rat_cpm','rat_fet','rat_mat','sa_cpm','sa_fet','sa_mat')) # additional finding
   
      df_costs_tt_strat4 <- as.data.frame(round(sum(df_costs_strat4) +  # 1
                                             cea_input$l_all_params$n_pop * cea_input$l_all_params$p_part_couns * c_pre_couns + # 2
                                         sum(df_monitor_outc_tt_strat4[,c('lb_susp_scan','lb_susp_screen','lb_susp_screen_scan')])*(c_biom_scan * 3) + # 3
                                         sum(temp41[,c('lb_conf_itdirect','lb_conf_scan','lb_conf_screen')])*(c_biom_scan * 3) + # 4
                                         sum(temp42[,c('susp_scan','susp_screen','susp_screen_scan',
                                                       'conf_scan','conf_screen')])*(c_gen_fu)*cea_input$l_all_params$p_part_gen_fu_rat + # 5
                                         sum(temp43[,c('susp_scan','susp_screen','susp_screen_scan',
                                                       'conf_scan','conf_screen')])*(c_gen_fu)*cea_input$l_all_params$p_part_gen_fu_sa,0) + # 6
                                           sum(temp44[,c('susp_scan','susp_screen','susp_screen_scan',
                                                         'conf_itdirect','conf_scan','conf_screen')])*(c_clin_gen_simple) + # 7 
                                           sum(temp45[,c('susp_scan','susp_screen','susp_screen_scan',
                                                         'conf_itdirect','conf_scan','conf_screen')])*(c_clin_gen_complex)  # 8 
                                         )                                             
    
   
   colnames(df_costs_tt_strat4) <- "tt_costs_strat4"
   
   # Input for calculation of incremental cost-effectiveness ratios (ICERs) > number of fetal chromosomal aberrations detected
   eff1  <- df_screen_outc_tt_strat1 %>% filter(cat %in% c('t13','t18','t21','rat_fet','sa_fet'))
   eff2  <- df_screen_outc_tt_strat2 %>% filter(cat %in% c('t13','t18','t21','rat_fet','sa_fet'))
   eff3  <- df_screen_outc_tt_strat3 %>% filter(cat %in% c('t13','t18','t21','rat_fet','sa_fet'))
   eff4  <- df_screen_outc_tt_strat4 %>% filter(cat %in% c('t13','t18','t21','rat_fet','sa_fet'))
   
   eff_strat1 <- round(sum(eff1[,c('conf_itdirect','conf_scan','conf_screen')]),0)
   eff_strat2 <- round(sum(eff2[,c('conf_itdirect','conf_scan','conf_screen')]),0)
   eff_strat3 <- round(sum(eff3[,c('conf_itdirect','conf_scan','conf_screen')]),0)
   eff_strat4 <- round(sum(eff4[,c('conf_itdirect','conf_scan','conf_screen')]),0)
   
   cost = c(df_costs_tt_strat1[1,1],df_costs_tt_strat2[1,1],df_costs_tt_strat3[1,1],df_costs_tt_strat4[1,1])
   effect = c(eff_strat1,eff_strat2,eff_strat3,eff_strat4)
   strategies = cea_input$l_all_params$v_names_str      # https://cran.r-project.org/web/packages/dampack/vignettes/basic_cea.html
   df_cea_icer <- calculate_icers(cost       = cost,    # The default view is ordered by dominance status (ND = non-dominated, ED = extended/weak dominance, or D= strong dominance), and then ascending by cost.
                                  effect     = effect,
                                  strategies = strategies)
   
   # costs 
   # 1. Costs in decision tree (screen & scan costs)
   # 2. c_pre_couns: +! aankaarten maar echt een los counselingsgesprek = alle deelnemers + 20% -> alles - no test + 20%
   # 4. 3x c_biom_scan alle suspected + confirmed fetal / cpm; 
   # 5. c_dop_scan 1/2 confirmed / suspected fetal / cpm;
   # 6. c_gen_fu alleen bij nipt_wg and rat / sa (uptake modelled as 100%; in tree 75% RAT also invasive testing & 70% SA also invasive testing); 
    
 
   
    return(list(df_screen_outc_tt_strat1 = df_screen_outc_tt_strat1, 
                df_screen_outc_tt_strat2 = df_screen_outc_tt_strat2, 
                df_screen_outc_tt_strat3 = df_screen_outc_tt_strat3, 
                df_screen_outc_tt_strat4 = df_screen_outc_tt_strat4, 
                df_birth_outc_tt_strat1 = df_birth_outc_tt_strat1,
                df_birth_outc_tt_strat2 = df_birth_outc_tt_strat2,
                df_birth_outc_tt_strat3 = df_birth_outc_tt_strat3,
                df_birth_outc_tt_strat4 = df_birth_outc_tt_strat4,
                df_it_outc_tt_strat1 = df_it_outc_tt_strat1,
                df_it_outc_tt_strat2 = df_it_outc_tt_strat2,
                df_it_outc_tt_strat3 = df_it_outc_tt_strat3,
                df_it_outc_tt_strat4 = df_it_outc_tt_strat4,
                df_test_route_outc_tt_strat1 = df_test_route_outc_tt_strat1,
                df_test_route_outc_tt_strat2 = df_test_route_outc_tt_strat2,
                df_test_route_outc_tt_strat3 = df_test_route_outc_tt_strat3,
                df_test_route_outc_tt_strat4 = df_test_route_outc_tt_strat4,
                costs_strat1 = df_costs_tt_strat1,
                costs_strat2 = df_costs_tt_strat2,
                costs_strat3 = df_costs_tt_strat3,
                costs_strat4 = df_costs_tt_strat4,
                df_cea_icer = df_cea_icer))
  
  
}


