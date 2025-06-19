#' Generate one list with all inputs that are needed to do the analysis (general input parameters, 
#' details of the screened population, probability matrix and the json file showing the different screening pathways)
#'
#' \code{generate_decision_model_input} is used to call the other input functions to generate the probability matrix, population and set the name of the decision tree (json file) and summarize all inputs
#'
#' @param all_parameters List of all parameters used to analyse the decision model. 
#' @return A list with all inputs to perform the decision tree analysis for all four screening strategies
generate_decision_model_input_det <- function(l_all_params,param){ # User defined
  
  name_param = param[[1]]
 # value_param  = as.numeric(param[[2]])
 # length_param = length(l_all_params[[names(l_all_params) == name_param]])
  
  value_param <- as.numeric(param[[2]])

  if(param[[3]] %in% c('cost','pop','prob_all_param')){
    # Access l_all_params using 'name_param'
    # Replace its value by the value_param list
    l_all_params[[name_param]] <- value_param
    
    # This does not work!!!! 
    # l_all_params[names(l_all_params) == name_param] <- value_param
    
    decision_model_pop   <- generate_population(l_all_params)
    decision_model_probs <- generate_probs_df(l_all_params)
    name_tree            <- 'cea_tree_prenatal_screening'    
    
  }
  
  else{
    
  decision_model_pop   <- generate_population(l_all_params)
  decision_model_probs <- generate_probs_df(l_all_params)
  decision_model_probs[names(decision_model_probs) == name_param] <- value_param
  name_tree            <- 'cea_tree_prenatal_screening'
  
  }
  
  return(list(l_all_params = l_all_params,
              decision_model_pop = decision_model_pop,
              decision_model_probs = decision_model_probs,
              name_tree = name_tree))
  
}



#' Generate one probability matrix containing all input parameters for the four screening strategies
#' During the analysis, the required probabilities per strategy will be selected from this one probability matrix 
#' Therefore, we do not need separate prob matrices per strategy  
#'
#' \code{generate_probs_df} is used to create one probability matrix for all four prenatal screening strategies 
#' that are being analysed in the decision tree.
#' The different screening strategies require different probabilities in the model and will be selected from the matrix during the analysis
#'
#' @param all_parameters List of all parameters used to analyse the decision model. 
#' @return One probability matrix used to analyse all for screening strategies (data frame)
generate_probs_df_det <- function(all_parameters,param){
  
  #### 1. Generate df with probs for all strategies ####
  df_probs <- with(as.list(all_parameters), {
    
    #### General setup ####
    v_age_names                <- paste0("_",v_age_names)
    list1                      <- as.list(v_chrom_aber_cat)
    list2                      <- as.list(v_age_names)
    v_row_names                <- do.call(paste0, expand.grid(list1, list2))
    
    n_df_rows                  <- length(v_row_names)  # subgroups
    
    v_col_names                <- c('p_part_it_direct','p_part_scan','p_part_it_scan', # vector with probabilities in decision tree for all four strategies, matching the names in the decision tree (json file)
                                    'p_perf_scan','p_anom_conf',
                                    'p_part_fct','p_perf_fct','p_part_scan_fct','p_part_it_fct','p_perf_adv_scan',
                                    'p_part_nipt','p_part_targ_nipt','p_fail_nipt','p_2nd_fail_nipt', 
                                    'p_perf_targ_nipt','p_perf_wg_nipt','p_part_it_screen','p_part_it_screen_wg','p_part_scan_nipt',
                                    'p_im','p_top_it','p_top_scan','p_iufd')
    
    n_df_cols                  <- length(v_col_names) # number of unique probabilities in the decision tree
    
    
    df_probs            <- as.data.frame(matrix(0,nrow = n_df_rows,
                                                ncol = n_df_cols))
    colnames(df_probs)  <- v_col_names
    row.names(df_probs) <- v_row_names
    
    # ---- Add the constant parameters to the probability matrix:
    
    # (1) participation in 2nd trimester scan 
    # (2) participation in invasive testing following anomalies detected on scan 
    # (3) probability of iatrogenic miscarriage)
    # (4) opting for invasive testing without a prior screening test
    # (5) probability of parents opting for a termination of pregnancy based on the result of the adv scan is assumed to be constant
    
    
    # (6) participation in invasive testing following anomalies detected with fct
    # (7) participation in 2nd trimester scan after screening with fct
    
    # (8) probability that nipt will fail the first time, 
    # (9) probability that nipt will also fail the second time
    
    # (10) participation in invasive testing following high-risk screen (targ nipt/fct) result 
    # (11) participation in invasive testing following high-risk wg nipt result
    # (12) participation in 2nd trimester scan after screening with nipt 
    
    df_probs$p_part_scan      <- p_part_scan      # (1)
    df_probs$p_part_it_scan   <- p_part_it_scan   # (2)
    df_probs$p_im             <- p_im_it          # (3)
    df_probs$p_part_it_direct <- p_part_it_direct # (4)
    df_probs$p_top_scan       <- p_top_scan       # (5)
    
    df_probs$p_part_it_fct    <- p_part_it_fct    # (6)
    df_probs$p_part_scan_fct  <- p_part_scan_fct  # (7)
    
    df_probs$p_fail_nipt      <- p_nipt_fail      # (8)
    df_probs$p_2nd_fail_nipt  <- p_nipt_fail_2    # (9)
    
    df_probs$p_part_it_screen <- p_part_it_screen # (10)
    df_probs$p_part_it_screen_wg <- rep(c(p_part_it_screen,p_part_it_screen,p_part_it_screen,    # (11)
                                          p_part_it_rat_fet,p_part_it_rat_other,p_part_it_rat_other,
                                          p_part_it_sa_fet,p_part_it_sa_other,p_part_it_sa_other,
                                          p_part_it_screen),times=n_age)
    df_probs$p_part_scan_nipt <- p_part_scan_nipt # (12)    
    
    # ---- Add the parameters depending on age to the probability matrix
    
    
    # (13) participation in fct increases with age
    # (14)(15) participation in nipt increases with age
    
    df_probs$p_part_fct       <- rep(v_p_fct_part,each=n_chrom_aber_cat)       # (13)
    df_probs$p_part_nipt      <- rep(v_p_nipt_part,each=n_chrom_aber_cat)      # (14) [total nipt part]
    df_probs$p_part_targ_nipt <- rep(v_p_targ_nipt,each=n_chrom_aber_cat)      # (15) [percentage that opts for targeted]
    
    
    # ---- Add the parameters depending on the type of chromosomal aberration to the probability matrix
    
    # (16) probability of an affected fetus being detected with the 2nd trim scan depends on the chromosomal aberration (sensitivity of scan)
    # in addition, the scan also detects structural abnormalities that are not related to a chromosome abnormality (p_scan_sens_no_chrom)
    # (17) probability of the aberration being confirmed by the advanced scan, depends on the chromosomal aberration involved (sensitivity of advanced scan)
    # (18) probability of the aberration being detected/confirmed by the advanced scan (without a prior 2nd trimester scan), depends on the chromosomal aberration involved
    # (19) probability of the aberration being detected by the fct, depends on the chromosomal aberration involved (sensitivity of fct)  
    # (20) probability of the aberration being detected by the targeted nipt, depends on the chromosomal aberration involved (sensitivity of targeted nipt)  
    # (21) probability of the aberration being detected by the wg nipt, depends on the chromosomal aberration involved (sensitivity of wg nipt)  
    
    # (22) probability of parents opting for a termination of pregnancy is dependent on the confirmed chromosomal aberration
    # (23) probability of an iufd is dependent on the chromosomal aberration involved. also fetuses without chrom aber have a small risk of iufd (p_iufd_noaber)
    
    df_probs$p_perf_scan <- rep(c(p_scan_sens_t21,p_scan_sens_t18,p_scan_sens_t13,                       # (16)
                                  p_scan_sens_rat,p_scan_sens_no_chrom,p_scan_sens_no_chrom,
                                  p_scan_sens_sa,p_scan_sens_no_chrom,p_scan_sens_no_chrom,
                                  p_scan_sens_no_chrom),times=n_age)
    
    df_probs$p_anom_conf <- rep(c(p_adv_scan_conf_chrom,p_adv_scan_conf_chrom,p_adv_scan_conf_chrom,     # (17)
                                  p_adv_scan_conf_chrom,p_adv_scan_conf_no_chrom,p_adv_scan_conf_no_chrom,
                                  p_adv_scan_conf_chrom,p_adv_scan_conf_no_chrom,p_adv_scan_conf_no_chrom,
                                  p_adv_scan_conf_no_chrom),times=n_age)
    
    df_probs$p_perf_adv_scan <- rep(c(p_adv_scan_sens_t21,p_adv_scan_sens_t18,p_adv_scan_sens_t13,       # (18)
                                      p_adv_scan_sens_rat,p_adv_scan_no_chrom,p_adv_scan_no_chrom,p_adv_scan_sens_sa,p_adv_scan_no_chrom,p_adv_scan_no_chrom,
                                      p_adv_scan_no_chrom),times=n_age)
    
    df_probs$p_perf_fct      <- rep(c(p_fct_sens_t21,p_fct_sens_t18,p_fct_sens_t13,                      # (19)
                                      p_fct_fpr,p_fct_fpr,p_fct_fpr,
                                      p_fct_fpr,p_fct_fpr,p_fct_fpr,
                                      p_fct_fpr),times=n_age)
    
    df_probs$p_perf_targ_nipt      <- rep(c(p_nipt_sens_t21,p_nipt_sens_t18,p_nipt_sens_t13,             # (20)
                                            p_nipt_fpr,p_nipt_fpr,p_nipt_fpr,
                                            p_nipt_fpr,p_nipt_fpr,p_nipt_fpr,
                                            p_nipt_fpr),times=n_age)
    
    df_probs$p_perf_wg_nipt      <- rep(c(p_nipt_sens_t21,p_nipt_sens_t18,p_nipt_sens_t13,               # (21)
                                          p_nipt_sens_rat,p_nipt_sens_rat,p_nipt_sens_rat,
                                          p_nipt_sens_sa,p_nipt_sens_sa,p_nipt_sens_sa,
                                          p_nipt_fpr),times=n_age)
    
    df_probs$p_top_it        <- rep(c(p_top_t21,p_top_t18,p_top_t13,p_top_rat,0,0,p_top_sa,0,0,0),times=n_age)  # (22)
    
    #df_probs_strat1$p_top_scan       <- rep(c(p_top_scan,p_top_scan,p_top_scan,
    #                                          p_top_scan,0,0,
    #                                          p_top_scan,0,0,
    #                                          0),times=n_age)
    
    df_probs$p_iufd           <- rep(c(p_iufd_t21,p_iufd_t18,p_iufd_t13,                                 # (23)
                                       p_iufd_rat,p_iufd_noaber,p_iufd_noaber,
                                       p_iufd_sa,p_iufd_noaber,p_iufd_noaber,
                                       p_iufd_noaber))
    
    
    
    
    return(df_probs)
  }
  )
  
}

