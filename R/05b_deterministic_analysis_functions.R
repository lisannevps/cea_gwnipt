#' One-way sensitivity analysis (OWSA)
#'
#' This function runs a deterministic one-way sensitivity analysis (OWSA) on a
#' given function that produces outcomes.
#' @param parms Vector with strings with the name of the parameters of interest
#' @param ranges A named list of the form c("parm" = c(0, 1, 'lim'/'prob', 'cost'/'pop'/'prob_all_param'/'prob_probs_matrix'), ...) that gives 
#' the ranges for the parameters of interest. The number of samples from this 
#' range is determined by \code{nsamp}
#' @param nsamps number of parameter values. If NULL, 100 parameter values are 
#' used
#' @param params_basecase List with parameters for the base case
#' @param FUN Function that takes \code{params_basecase} and \code{...} and 
#' produces \code{outcome} of interest
#' @param outcome String with the outcome of interest produced by \code{nsamp}
#' @param strategies vector of strategy names. The default (NULL) will use 
#' strategy names in FUN
#' @param ... Further arguments to FUN (not used)
#' @keywords owsa
#' @return A dataframe with the results of the sensitivity analysis. Can be 
#' visualized with \code{plot.owsa}, \code{owsa_opt_strat} and 
#' \code{owsa_tornado} from \code{dampack}
#' @section Details:
#' FUN must return a dataframe where the first column are the strategy names
#' and the rest of the columns must be outcomes.
#' @export
#' 
#' 
#' 
#' 



owsa_det <- function(parms, ranges, nsamps, params_basecase){
  
  l_owsa_input <- params_basecase
  n_str = l_owsa_input$l_all_params$n_str
  strategies = l_owsa_input$l_all_params$v_names_str
  
  df_owsa_all <- NULL
  
  
  for (i in 1:length(parms)) { # i <- 2
    ### Generate matrix of inputs
    
    if((ranges[[i]][4]=='cost' | ranges[[i]][4]=='pop') &  ranges[[i]][3]=='lim'){
      
      v_owsa_input <- seq(as.numeric(ranges[[i]][1]), 
                          as.numeric(ranges[[i]][2]), 
                          length.out = nsamps)
      
      v_owsa_input <- pmax(v_owsa_input,0)
      
    } else if((ranges[[i]][4]=='cost' | ranges[[i]][4]=='pop') & ranges[[i]][3]=='prob'){
      
      
      value_base_case <- as.numeric(l_owsa_input$l_all_params[names(l_owsa_input$l_all_params) == parms[i]])
      seq_temp <- seq(as.numeric(ranges[[i]][1]), 
                      as.numeric(ranges[[i]][2]), 
                      length.out = nsamps)
      
      
      v_owsa_input <- value_base_case * seq_temp + value_base_case
      v_owsa_input <- pmax(v_owsa_input, 0)
      
    } 
    
    else if((ranges[[i]][4]=='prob_all_param') & ranges[[i]][3]=='lim'){
      
      
      name_param = parms[i]
      v_base_case <- l_owsa_input$l_all_params[names(l_owsa_input$l_all_params) == parms[i]]
      v_base_case_unlist <- unlist(v_base_case)
      n_base_case_vector <- length(v_base_case_unlist)
      
      # Create an empty list to store the vectors
      v_owsa_input <- list()
      
      temp <- seq(as.numeric(ranges[[i]][1]), 
                  as.numeric(ranges[[i]][2]), 
                  length.out = nsamps)
      
      for (j in 1:nsamps) {
        vector_name <- paste0("vector_", j)
        vector_values <- rep(temp[j],times=n_base_case_vector)
        v_owsa_input[[vector_name]] <- vector_values
      }
      
    } 
    
    else if((ranges[[i]][4]=='prob_all_param') & ranges[[i]][3]=='prob'){ # vector & probability
      
      name_param = parms[i]
      v_base_case <- l_owsa_input$l_all_params[names(l_owsa_input$l_all_params) == parms[i]]
      v_base_case_unlist <- unlist(v_base_case)
      n_base_case_vector <- length(v_base_case_unlist)
      
      # Create an empty list to store the vectors
      v_owsa_input <- list()
      
      temp <- seq(as.numeric(ranges[[i]][1]), 
                  as.numeric(ranges[[i]][2]), 
                  length.out = nsamps)
      
      # Create a sequence of vectors
      for (j in 1:nsamps) {
        vector_name <- paste0("vector_", j)
        vector_values <- temp[j]*v_base_case_unlist+v_base_case_unlist
        vector_values <- pmin(pmax(vector_values, 0), 1)
        
        v_owsa_input[[vector_name]] <- vector_values
      }
      
      
    } 
    
    else if((ranges[[i]][4]=='prob_probs_matrix') & ranges[[i]][3]=='lim'){ # vector & probability
      
      name_param = parms[i]
      v_base_case <- l_owsa_input$decision_model_probs[names(l_owsa_input$decision_model_probs) == parms[i]]
      v_base_case_unlist <- unlist(v_base_case)
      n_base_case_vector <- length(v_base_case_unlist)
      
      # Create an empty list to store the vectors
      v_owsa_input <- list()
      
      temp <- seq(as.numeric(ranges[[i]][1]), 
                  as.numeric(ranges[[i]][2]), 
                  length.out = nsamps)
      
      for (j in 1:nsamps) {
        vector_name <- paste0("vector_", j)
        vector_values <- rep(temp[j],times=n_base_case_vector)
        v_owsa_input[[vector_name]] <- vector_values
      }
      
      
    }
    
    else if((ranges[[i]][4]=='prob_probs_matrix') & ranges[[i]][3]=='prob'){ # vector & probability
      
      name_param = parms[i]
      v_base_case <- l_owsa_input$decision_model_probs[names(l_owsa_input$decision_model_probs) == parms[i]]
      v_base_case_unlist <- unlist(v_base_case)
      n_base_case_vector <- length(v_base_case_unlist)
      
      # Create an empty list to store the vectors
      v_owsa_input <- list()
      
      temp <- seq(as.numeric(ranges[[i]][1]), 
                  as.numeric(ranges[[i]][2]), 
                  length.out = nsamps)
      
      # Create a sequence of vectors
      for (j in 1:nsamps) {
        vector_name <- paste0("vector_", j)
        vector_values <- temp[j]*v_base_case_unlist+v_base_case_unlist
        vector_values <- pmin(pmax(vector_values, 0), 1)
        
        v_owsa_input[[vector_name]] <- vector_values
      }
      
      
    } 
    
    else{
      
      v_owsa_input = NA
      
    }
    
    # m_owsa_input[,i] <- v_owsa_input
    
    ### Initialize matrix to store outcomes from a OWSA of the CEA
    # outcomes
    m_out_owsa <- matrix(0, 
                         nrow = nsamps, 
                         ncol = n_str)

    m_e <- matrix(0, 
                  nrow = nsamps, 
                  ncol = n_str)
    
    ### Run model and capture outcome
    
    for (j in 1:nsamps){ # 
      
      param <- as.list(c(parms[i],v_owsa_input[j],ranges[[i]][4]))
      cea_input_det = generate_decision_model_input_det(l_owsa_input$l_all_params,param) # output = cea_input
      l_out_dm <- decision_model(cea_input = cea_input_det, file.init_tree = "data-raw/01_init_params_decision_tree.csv")
      
      custom_order <- c("scan","scan & fct","scan & targeted nipt","scan & wg nipt")
      order_indices <- order(match(l_out_dm$df_cea_icer$Strategy, custom_order))
      sorted_df <- l_out_dm$df_cea_icer[order_indices , ]
      
      m_out_owsa[j, ] <- sorted_df$Cost/sorted_df$Effect
      m_e[j,] <- sorted_df$Effect
    }
    
    v_owsa_input <- unlist(lapply(v_owsa_input, function(x) x[1]))
    df_owsa <- data.frame(parameter = parms[i],
                          v_owsa_input,
                          m_out_owsa)
    names(df_owsa)[-1] <- c("param_val", strategies)
    
    df_owsa_all <- rbind(df_owsa_all, df_owsa)

    
  }
  
  
  df_owsa_lng <- reshape2::melt(df_owsa_all, 
                                id.vars = c("parameter", "param_val"), 
                                variable.name = "strategy", 
                                value.name = "outcome_val")

  
  class(df_owsa_lng) <- c("owsa", "data.frame")

    
  return(list(df_owsa_lng=df_owsa_lng,
              effects = m_e))
}


