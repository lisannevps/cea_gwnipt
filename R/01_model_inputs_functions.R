#' Generate decision tree containing all screening strategies
#'
#' \code{create_tree()} is a standard function within the OpenTree Darth package. This function
#' creates a blank tree. The function \code{open_tree()} opens an existing decision tree.
#' *IMPORTANT*: since `create_tree()` always creates blank new tree, do not use it to access 
#' or modify an existing tree, or else the tree will get erased. Always use `open_tree()` to 
#' open and modify existing trees.  
#' @return 
#' Any changes made to the tree in OpenTree are automatically saved as a `.json` file to the 
#' working directory. 
#' @export
#' create_tree(file_name = "...", dir_name = paste0(getwd(),"./trees"))
#' The tree that reflects the four prenatal screening strategies that are being compared in this study is detailed in het following json file: 'cea_tree_prenatal_screening.json'
# open_tree(file_name = "cea_tree_prenatal_screening", dir_name = getwd())

#' Load all parameters
#'
#' \code{load_all_params} loads all input parameters necessary to analyse the decision model from multiple .cvs files and creates a list with all input parameters.
#'
#' @param file.init_gen string with the location and name of the file with general initial parameters
#' @param file.init_pop string with the location and name of the file with maternal age distribution
#' @param file.init_tris string with the location and name of the file with prevelance of the common trisomies depending on maternal age
#' @param file.init_uptake string with the location and name of the file with screening test uptake
#' @param file.init_fct string with the location and name of the file with test characteristics of fct depending on maternal age
#' @param file.init_nipt string with the location and name of the file with test characteristics of nipt
#' @param file.init_scan string with the location and name of the file with test characteristics of second trimester ultrasound & advanced scan
#' 
#' @return A list of all parameters used in the decision model.

load_all_params <- function(file.init_gen    = NULL,
                            file.init_pop    = NULL,
                            file.init_tris   = NULL,
                            file.init_uptake = NULL,
                            file.init_fct    = NULL,
                            file.init_nipt   = NULL,
                            file.init_scan   = NULL
                            ){ # User defined

    #### Load initial set of initial parameters from .csv file ####
    df_params_init_gen    <- read.csv(file = file.init_gen, sep=";",stringsAsFactors = F)
    df_params_init_pop    <- read.csv(file = file.init_pop, sep=";",row.names = 1,stringsAsFactors = F)
    df_params_init_tris   <- read.csv(file = file.init_tris, sep=";",row.names = 1,stringsAsFactors = F)
    df_params_init_uptake <- read.csv(file = file.init_uptake, sep=";",row.names = 1,stringsAsFactors = F)
    df_params_init_fct    <- read.csv(file = file.init_fct, sep=";",stringsAsFactors = F)
    df_params_init_nipt   <- read.csv(file = file.init_nipt, sep=";",stringsAsFactors = F)
    df_params_init_scan   <- read.csv(file = file.init_scan, sep=";",stringsAsFactors = F)

  
    l_params_all <- with(as.list(df_params_init_gen), {
    
    #### General setup of the analyses: define screening strategies, age categories, types of chrom aberrations ####
    v_names_str         <- c('scan','scan & fct', "scan & targeted nipt","scan & wg nipt")  # screening strategies
    n_str               <- length(v_names_str) # number of strategies
    v_age_names         <- c('younger than/equal to 25','26-30','31-35','36-40','41 and older') # age categories
    n_age               <- length(v_age_names)
    v_chrom_aber_cat    <- c("t21","t18","t13","rat_fet","rat_cpm","rat_mat","sa_fet","sa_cpm","sa_mat","no_aber") # types of chromosomal aberrations
    n_chrom_aber_cat    <- length(v_chrom_aber_cat)
    
    #### Create list with all parameters ####
    l_params_all <- list(
      v_names_str      = v_names_str,
      n_str            = n_str      ,
      v_age_names      = v_age_names,
      n_age            = n_age      ,
      v_chrom_aber_cat = v_chrom_aber_cat,
      n_chrom_aber_cat = n_chrom_aber_cat
    )
    return(l_params_all)
  }
  )


  l_params_all <- c(l_params_all, 
                    df_params_init_gen,
                    df_params_init_pop,
                    df_params_init_tris,
                    df_params_init_uptake,
                    df_params_init_fct,
                    df_params_init_nipt,
                    df_params_init_scan) 
}


#' Generate population for cea
#'
#' \code{generate_population} is used to divide the total number of pregnant women
#' into subgroups according to:
#' (1) to the maternal age distribution in five categories (younger than/equal to 25,26-30,31-35,36-40,41 and older), and
#' (2) the incidence of the different chromosomal aberrations being screened for (t21,t18,t13,fetal rat/sa, cpm rat/sa, mat rat/sa).
#' Outcomes of the decision model are calculated per group as parameters in the decision tree can differ per age & chrom category) 
#' and summarized to get overall outcomes.
#'
#' @param all_parameters List of all parameters used to analyse the decision model. 
#' @return A data frame with the number of pregnancies per maternal age and per subgroup. 
#' data frame rows: younger than/equal to 25,26-30,31-35,36-40,41 and older
#' data frame columns: t21, t18, t13, fetal rat, cpm rat, maternal rat, fetal sa, cpm sa, mat sa, no aberrations

generate_population <- function(all_parameters){
  
  df_pop_analysis <- with(as.list(all_parameters), {
   
    #### Create data frame with number of pregnant women per age category ####
    df_pop_cea <- as.data.frame(v_pop_distr * n_pop)  # total N * % per age category
    colnames(df_pop_cea) <- c("v_n_per_age") # define column name
    rownames(df_pop_cea) <- 15:49 # define row names
    n_age_long <- length(15:49) # number of ages between 15-49
    
    #### Create data frame with prob of diff chrom aber per age cat ####
    p_rat_fet <- p_rat*p_rat_fet   # calculate percentage of rats being fetal
    p_rat_cpm <- p_rat*p_rat_cpm   # calculate percentage of rats being cpm
    p_rat_mat <- p_rat*p_rat_mat   # calculate percentage of rats being maternal
    
    p_sa_fet <- p_sa*p_sa_fet      # calculate percentage of sas being fetal
    p_sa_cpm <- p_sa*p_sa_cpm      # calculate percentage of sas being cpm
    p_sa_mat <- p_sa*p_sa_mat      # calculate percentage of sas being maternal
    
    #### Generate probability matrix with rows 15:49 (ages) and,
    # columns: age-related probability of the different chromosome aberrations ####  
    df_p_chrom <- as.data.frame(cbind(v_p_t21,v_p_t18,v_p_t13,  
                                      rep(p_rat_fet,times=n_age_long),rep(p_rat_cpm,times=n_age_long),rep(p_rat_mat,times=n_age_long),
                                      rep(p_sa_fet,times=n_age_long),rep(p_sa_cpm,times=n_age_long),rep(p_sa_mat,times=n_age_long)))
    
    # Add column for the age related probability of it being a pregnancy without a chromosome abnormality 
    df_p_chrom$no_aber <- 1- rowSums(df_p_chrom[,c(1:9)])
    rownames(df_p_chrom) <- 15:49  # define row names
    colnames(df_p_chrom) <- v_chrom_aber_cat # define column  names
    
    #### Create data frame with number of pregnancies per age category, that match the probability matrix,
    # rows: ages (15-49), columns: chromosome aberrations categories ####
    df_pop_cea <- rep(1,times=n_chrom_aber_cat) %*% t(df_pop_cea)
    df_pop_cea <- t(df_pop_cea)
    
    #### Create data frame with the number of pregnancies in each subgroup (age/aber cat), by multiplying the probability matrix with the number of pregnancies
    df_pop_cea <- df_pop_cea * df_p_chrom
    rownames(df_pop_cea)  <- 15:49 # define row names
    colnames(df_pop_cea)  <- v_chrom_aber_cat # define column names
    df_pop_cea <- as.data.frame(df_pop_cea)
    
    #### Transform the data frame with number of pregnancies per age to a data frame with number of pregnancies per age category 
    v_n_per_age_cat <- c(0,11,16,21,26,35) # these are the boundaries to split the rows to match the predefined age groups. E.g. younger than/equal to 25 = first 11 rows (15-49)
    temp <- as.data.frame(matrix(NA,nrow = n_age,ncol = n_chrom_aber_cat)) 
    rownames(temp)  <- v_age_names
    colnames(temp)  <- v_chrom_aber_cat
    
    for(i in 1:n_age){
      
    temp[i,] <- colSums(df_pop_cea[(v_n_per_age_cat[i]+1):v_n_per_age_cat[i+1],]) # Sum over the rows
    
    }

    df_pop_cea <- temp
    
    
    return(df_pop_cea)
  }
  )
  
}


#' \code{generate_probs_df} Generate one probability matrix containing all input parameters for the four screening strategies
#' In the analysis the required probabilities per strategy will be selected from this one probability matrix 
#' We do not need separate prob matrices per strategy 
#' @param all_parameters List of all parameters used to analyse the decision model. 
#' @return One probability matrix for the four screening strategies (data frame)
generate_probs_df <- function(all_parameters){
  
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
    

    df_probs$p_iufd           <- rep(c(p_iufd_t21,p_iufd_t18,p_iufd_t13,                                 # (23)
                                       p_iufd_rat,p_iufd_noaber,p_iufd_noaber,
                                       p_iufd_sa,p_iufd_noaber,p_iufd_noaber,
                                       p_iufd_noaber))
    
    


    return(df_probs)
    }
    )
    
}







#' Generate one list with all inputs that are needed to do the analysis: probabilities, costs, effects,  
#' details of the screened population, probability matrix and the json file showing the different screening pathways)
#'
#' \code{generate_decision_model_input} is used to call the other input functions to generate the probability matrix, population and set the name of the decision tree (json file) and summarize all inputs
#'
#' @param all_parameters List of all parameters of the decision model. 
#' @return A list with all inputs to perform analysis for all four screening strategies
generate_decision_model_input <- function(l_all_params){ # User defined

  decision_model_pop   <- generate_population(l_all_params)
  decision_model_probs <- generate_probs_df(l_all_params)
  name_tree            <- 'cea_tree_prenatal_screening'

  return(list(l_all_params = l_all_params,
              decision_model_pop = decision_model_pop,
              decision_model_probs = decision_model_probs,
              name_tree = name_tree))
  
}



