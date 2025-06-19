#' Generate PSA dataset of CEA parameters
#'
#' \code{generate_psa_params} generates PSA input dataset by sampling decision 
#' model parameters from their distributions. The sample of the calibrated
#' parameters is a draw from their posterior distribution obtained with the
#' IMIS algorithm.
#' @param n_sim Number of PSA samples.
#' @param seed Seed for reproducibility of Monte Carlo sampling.
#' @return 
#' A data frame with \code{n_sim} rows and 15 columns of parameters for PSA. 
#' Each row is a parameter set sampled from distributions that characterize 
#' their uncertainty
#' @examples 
#' generate_psa_params()
#' @export
#' 


generate_psa_params <- function(file.dm_psa,n_sim, seed){ # User defined
  ## Load calibrated parameters
  set_seed <- seed
  
  df_psa_inputs <- read_excel(file.dm_psa,sheet=1,col_names = TRUE)
  df_psa_inputs$param1 <- NA
  df_psa_inputs$param2 <- NA
  
  df_psa_inputs[, paste0("draw_", 1:n_sim)] <- NA
  
  for(i in 1:nrow(df_psa_inputs)){
    
    func <- paste0(df_psa_inputs$distribution[i],"_params")
    
    if (func=="beta_params"){
      
    df_psa_inputs$param1[i] <- do.call(func,list(df_psa_inputs$`base case value`[i],df_psa_inputs$sd[i]))$alpha
    df_psa_inputs$param2[i] <- do.call(func,list(df_psa_inputs$`base case value`[i],df_psa_inputs$sd[i]))$beta
   
    temp = rbeta(n_sim,shape1 = df_psa_inputs$param1[i], shape2 = df_psa_inputs$param2[i])
    
    if(is.na(temp[1])==TRUE){
      df_psa_inputs[i,(length(df_psa_inputs)-n_sim+1):length(df_psa_inputs)] = df_psa_inputs$`base case value`[i]
      
    }
     else{
    df_psa_inputs[i,(length(df_psa_inputs)-n_sim+1):length(df_psa_inputs)] <- t(temp)
     }
    
     }
    else if (func=="gamma_params"){
      
      df_psa_inputs$param1[i] <- do.call(func,list(df_psa_inputs$`base case value`[i],df_psa_inputs$sd[i]))$shape
      df_psa_inputs$param2[i] <- do.call(func,list(df_psa_inputs$`base case value`[i],df_psa_inputs$sd[i]))$scale  
      
      temp = rgamma(n_sim,shape = df_psa_inputs$param1[i], scale = df_psa_inputs$param2[i])
      
      if(is.na(temp[1])==TRUE){
        df_psa_inputs[i,(length(df_psa_inputs)-n_sim+1):length(df_psa_inputs)] = df_psa_inputs$`base case value`[i]
        
      }
      else{
        df_psa_inputs[i,(length(df_psa_inputs)-n_sim+1):length(df_psa_inputs)] <- t(temp)
      }
      
    }
    
    else {
      
      df_psa_inputs$param1[i] <- df_psa_inputs$param2[i] < NA
      df_psa_inputs[i,(length(df_psa_inputs)-n_sim+1):length(df_psa_inputs)] <- df_psa_inputs$`base case value`[i]
      
      
    }
  }
   
  return(df_psa_inputs)
}



#' Load all parameters
#'
#' \code{load_all_params} loads all input parameters necessary to analyse the decision model from multiple .cvs files and creates a list with all input parameters.
#'
#' @param df_psa_params string with the location and name of the file with general initial parameters
#' 
#' @return A list of all parameters used for the decision model.
#' 
#' 
load_all_params_psa <- function(df_psa_params,
                              file.init_pop = NULL,
                              file.init_tris = NULL
){ # User defined
  
  #### Load initial set of initial parameters from .csv file ####
  df_psa_params_m = as.matrix(df_psa_params)
  
  temp = as.list(setNames(as.numeric(df_psa_params_m[,2]),df_psa_params_m[,1]))
  
  
  df_params_init_pop    <- read.csv(file = file.init_pop, sep=";",row.names = 1,stringsAsFactors = F)
  df_params_init_tris   <- read.csv(file = file.init_tris, sep=";",row.names = 1,stringsAsFactors = F)
  
    #### General setup ####
    v_names_str         <- c('scan','scan & fct', "scan & targeted nipt","scan & wg nipt")  # screening strategies
    n_str               <- length(v_names_str) # number of strategies
    v_age_names         <- c('younger than/equal to 25','26-30','31-35','36-40','41 and older') # age categories
    n_age               <- length(v_age_names)
    v_chrom_aber_cat    <- c("t21","t18","t13","rat_fet","rat_cpm","rat_mat","sa_fet","sa_cpm","sa_mat","no_aber") # types of chromosomal aberrations
    n_chrom_aber_cat    <- length(v_chrom_aber_cat)
    
    v_p_fct_part = unlist(c(df_psa_params[df_psa_params$Variable=="p_fct_part_25",2],
                            df_psa_params[df_psa_params$Variable=="p_fct_part_30",2],
                            df_psa_params[df_psa_params$Variable=="p_fct_part_35",2],
                            df_psa_params[df_psa_params$Variable=="p_fct_part_40",2],
                            df_psa_params[df_psa_params$Variable=="p_fct_part_45",2]))
    
    v_p_nipt_part = unlist(c(df_psa_params[df_psa_params$Variable=="p_nipt_part_25",2],
                             df_psa_params[df_psa_params$Variable=="p_nipt_part_30",2],
                             df_psa_params[df_psa_params$Variable=="p_nipt_part_35",2],
                             df_psa_params[df_psa_params$Variable=="p_nipt_part_40",2],
                             df_psa_params[df_psa_params$Variable=="p_nipt_part_45",2]))
    
    v_p_targ_nipt = unlist(c(df_psa_params[df_psa_params$Variable=="p_targ_nipt_25",2],
                             df_psa_params[df_psa_params$Variable=="p_targ_nipt_30",2],
                             df_psa_params[df_psa_params$Variable=="p_targ_nipt_35",2],
                             df_psa_params[df_psa_params$Variable=="p_targ_nipt_40",2],
                             df_psa_params[df_psa_params$Variable=="p_targ_nipt_45",2]))
    
    #### Create list with all parameters ####
    l_params_all <- list(
      v_names_str      = v_names_str,
      n_str            = n_str      ,
      v_age_names      = v_age_names,
      n_age            = n_age      ,
      v_chrom_aber_cat = v_chrom_aber_cat,
      n_chrom_aber_cat = n_chrom_aber_cat,
      v_p_fct_part = v_p_fct_part,
      v_p_nipt_part = v_p_nipt_part,
      v_p_targ_nipt = v_p_targ_nipt)
  
  
  l_params_all <- c(l_params_all, 
                    temp,
                    df_params_init_pop,
                    df_params_init_tris) 
}






