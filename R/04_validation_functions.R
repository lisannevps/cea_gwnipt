#' Summarize posterior output
#'
#' \code{data_summary} is used to to calculate the mean, standard deviation and 
#' 95% credible interval.
#' @param data Data frame.
#' @param varname Name of a column containing the variable.
#' @param groupnames Vector of column names to be used as grouping variables.
#' @return 
#' A data frame containing the posterior output.
#' @export
# data_summary <- function(data, varname, groupnames){
#   summary_func <- function(x, col){
#     c(mean = mean(x[[col]], na.rm = TRUE),
#       median = quantile(x[[col]], probs = 0.5, names = FALSE),
#       sd = sd(x[[col]], na.rm=TRUE),
#       lb = quantile(x[[col]], probs = 0.025, names = FALSE),
#       ub = quantile(x[[col]], probs = 0.975, names = FALSE))
#   }
#   data_sum <- plyr::ddply(data, groupnames, .fun = summary_func, 
#                     varname)
#   data_sum <- plyr::rename(data_sum, c("mean" = varname))
#   return(data_sum)
# }

compare_with_val_targets <- function(l_all_params,
                                     file.val_params = NULL, # file with parameters for decision tree
                                     file.val_targets_prev = NULL, # prevalence targets for trisomy 21,18,13 (literature)
                                     file.val_targets_tris_birth_prev = NULL, # birth prev for trisomy 21,18,13 (literature)
                                     file.val_targets_screen_wg = NULL,
                                     file.val_targets_seo = NULL,
                                     file.val_targets_fct = NULL,
                                     file.val_targets_it = NULL,
                                     file.dm_out_seo = NULL,
                                     file.dm_out_fct = NULL,
                                     file.dm_out_wg_nipt = NULL){ # trident 2 outcomes
  
  
    df_val_params <- read.csv(file = file.val_params, sep=";",stringsAsFactors = F)
    df_val_targets_prev <- read.csv(file = file.val_targets_prev, sep=";",stringsAsFactors = F)
    df_val_targets_tris_birth_prev <- read.csv(file = file.val_targets_tris_birth_prev, sep=";",stringsAsFactors = F)
    df_val_targets_screen_wg <- read.csv(file = file.val_targets_screen_wg, sep=";",stringsAsFactors = F)
    df_val_targets_seo <- read.csv(file = file.val_targets_seo, sep=";",stringsAsFactors = F)
    df_dm_out_seo <- read_excel(file.dm_out_seo,sheet=1,col_names = TRUE)
    df_val_targets_fct <- read.csv(file = file.val_targets_fct, sep=";",stringsAsFactors = F)
    df_dm_out_fct <- read_excel(file.dm_out_fct,sheet=1,col_names = TRUE)
    df_dm_out_fct_route <- read_excel(file.dm_out_fct,sheet=4,col_names = TRUE)
    df_val_targets_it <- read.csv(file = file.val_targets_it, sep=";",stringsAsFactors = F)
    df_dm_out_wg_nipt_birth <- read_excel(file.dm_out_wg_nipt,sheet=2,col_names = TRUE)
    df_dm_out_wg_nipt_route <- read_excel(file.dm_out_wg_nipt,sheet=4,col_names = TRUE)
    
    
    
    
  # --- 0. preps: generate input 
    decision_model_pop   <- generate_population(l_all_params)
  
    
  # --- 1. check prevalence of chromosomal aberrations in simulated population
  df_val_outc_prev <- df_val_targets_prev
  df_val_outc_prev$prev_cea <- c(round(sum(decision_model_pop$t21)/l_all_params$n_pop,4),
                                  round(sum(decision_model_pop$t18)/l_all_params$n_pop,4),
                                  round(sum(decision_model_pop$t13)/l_all_params$n_pop,4),
                                  round(sum(decision_model_pop[,c('rat_fet','rat_cpm','rat_mat')])/l_all_params$n_pop,4),
                                  round(sum(decision_model_pop[,c('sa_fet','sa_cpm','sa_mat')])/l_all_params$n_pop,4))
  
  # --- 2. check birth prevalence of common trisomies in screening strategy wg nipt 
  df_val_birth <- df_dm_out_wg_nipt_birth

  tris_birth_prev_val <- df_val_birth[df_val_birth$cat %in% c('t21','t18','t13'),'lb']/l_all_params$n_pop
  df_tris_birth_prev_val <- as.data.frame(tris_birth_prev_val)
  df_tris_birth_prev_val$cat <- c('t13','t18','t21')
  target <- c('t21','t18','t13')
  df_tris_birth_prev_val <- df_tris_birth_prev_val[match(target, df_tris_birth_prev_val$cat),] # rearrange row order to t21 t18 t13
  colnames(df_tris_birth_prev_val) <- c('birth_prev_cea','cat')
  
  df_val_outc_birth_prev <- df_val_targets_tris_birth_prev
  df_val_outc_birth_prev$prev_cea <- df_tris_birth_prev_val[,'birth_prev_cea'] 
  # birth prev higher than numbers in literature, but this model neglects ultrasound screening & most t18 t13 cases will be seen on ultrasound and terminated
  
  # --- 3. check outcomes of seo & guo (strategy 1) # assumption: uptake of guo 100%
  n_val_uptake_seo <- sum(df_dm_out_seo[,c('conf_scan','susp_scan','miss_scan','miss_scan_adv')])
  n_val_abn_seo <- sum(df_dm_out_seo[,c('conf_scan','susp_scan','miss_scan_adv')])
  
  n_val_abn_guo <- n_val_abn_seo-sum(df_dm_out_seo[,c('miss_scan_adv')])
  
  p_val_abn_seo <- round(n_val_abn_seo/n_val_uptake_seo,3)
  p_val_abn_guo <- round(n_val_abn_guo/n_val_abn_seo,3)
  
  df_val_outc_seo <- df_val_targets_seo
  df_val_outc_seo$val_outc <- c(p_val_abn_seo,p_val_abn_guo)
  

  # --- 4. check screening outcomes of screening strategy fct > uptake + tt abnormal fct tests
  n_val_uptake_fct <- sum(df_dm_out_fct[,c('conf_screen','susp_screen','susp_screen_scan','miss_screen_scan')])
  p_val_uptake_fct <- round(n_val_uptake_fct/l_all_params$n_pop,3)
  n_val_abn_fct <- sum(df_dm_out_fct[,c('conf_screen','susp_screen','susp_screen_scan')])
  p_val_abn_fct <- round(n_val_abn_fct/n_val_uptake_fct,3)
  
  temp_fct_fet  <- df_dm_out_fct_route %>% filter(cat %in% c('t21','t18','t13','rat_fet','sa_fet'))
  n_val_fct_conf <- sum(temp_fct_fet[,c('screen_it')])
  p_val_fct_conf <- n_val_fct_conf/175000
  
  df_val_outc_fct <- df_val_targets_fct
  df_val_outc_fct$val_outc <- round(c(p_val_uptake_fct,p_val_abn_fct,p_val_fct_conf),4)
    
  # --- 5. check screening outcomes of screening strategy wg nipt
  df_val_screen_wg <- df_dm_out_wg_nipt_route
  
  n_val_pop_screened <- sum(df_dm_out_wg_nipt_route[,c('screen','screen_it','screen_scan','screen_scan_it')]) 
  
  df_val_comp <- as.data.frame(df_val_screen_wg[,c("screen_it")]) # to compare with trident numbers that do not take into account anomalies detected by scan
  df_val_comp <- df_val_comp / n_val_pop_screened * 10000 # calculate numbers per 10.000 screened women
  df_val_comp$cat <- df_val_screen_wg$cat
  
  temp_tris  <- df_val_comp %>% filter(cat %in% c('t21','t18','t13'))
  temp_fet   <- df_val_comp %>% filter(cat %in% c('rat_fet','sa_fet'))
  
  v_val_conf <- c(temp_tris[temp_tris$cat == 't21','screen_it' ],
                  temp_tris[temp_tris$cat == 't18','screen_it' ],
                  temp_tris[temp_tris$cat == 't13','screen_it' ],
                  sum(temp_fet$screen_it)
  )
  
  df_val_outc_screen_wg <- df_val_targets_screen_wg[,1:2]
  df_val_outc_screen_wg$conf.val <- round(v_val_conf,1)
  
  # --- 6. check proportion of total population with invasive test | indication
  n_val_fct_it <- sum(df_dm_out_fct_route[,c('screen_it')])
  n_val_fct_scan_it <- sum(df_dm_out_fct_route[,c('scan_it','screen_scan_it')])
  n_val_fct_direct_it <- sum(df_dm_out_fct_route[,c('itdirect')])
  v_p_fct_it <- round(c(n_val_fct_scan_it,n_val_fct_it,NA,n_val_fct_direct_it)/175000*100,2)
  
  n_val_nipt_it <- sum(df_dm_out_wg_nipt_route[,c('screen_it')])
  n_val_nipt_scan_it <- sum(df_dm_out_wg_nipt_route[,c('scan_it','screen_scan_it')])
  n_val_nipt_direct_it <- sum(df_dm_out_wg_nipt_route[,c('itdirect')])
  v_p_nipt_it <- round(c(n_val_nipt_scan_it,NA,n_val_nipt_it,n_val_nipt_direct_it)/175000*100,2)
  
  df_val_outc_it <- df_val_targets_it
  df_val_outc_it$val_outc_fct <- v_p_fct_it
  df_val_outc_it$val_outc_nipt <- v_p_nipt_it
  
  
  return(list(df_val_outc_prev=df_val_outc_prev,
              df_val_outc_birth_prev=df_val_outc_birth_prev,
              df_val_outc_seo=df_val_outc_seo,
              df_val_outc_fct=df_val_outc_fct,
              df_val_outc_screen_wg=df_val_outc_screen_wg,
              df_val_outc_it=df_val_outc_it))
}

