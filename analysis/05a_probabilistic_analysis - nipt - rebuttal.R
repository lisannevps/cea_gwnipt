################################################################################ 
# This script conducts the probabilistic sensitivity analysis (PSA) of the     #
# cost-effectiveness analysis of a hypothetical treatment for the simulated    #
# cohort of the Sick-Sicker state-transition model (STM) to create PSA dataset #
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

#### 05a.1 Load packages and functions ####
#### 05a.1.1 Load packages ####
# PSA functionality
library(dampack,ggplot)   # decision-analytic modeling visualization tool

#### 05a.1.2 Load inputs ####

#### 05a.1.3 Load functions ####
# no required functions

#### 05a.2 Cost-effectiveness analysis parameters ####
### Strategy names
v_names_str = c('scan','scan & fct', "scan & targeted nipt","scan & wg nipt") 
### Number of strategies
n_str <- length(v_names_str)

#### 05a.3 Setup probabilistic analysis ####
### Number of simulations
n_sim <- 1

### Generate PSA input dataset
df_psa_inputs <- generate_psa_params(file.dm_psa = "data-raw/05a_psa input_rebuttal_fixed_uptake.xlsx", #"data-raw/05a_psa input_2.xlsx for psa with constant test uptake rates"
                                     n_sim,
                                     seed = 19940701)
df_psa_input <- df_psa_inputs[, grep('draw_', names(df_psa_inputs))]
df_psa_input <- t(df_psa_input)


### Initialize matrices for PSA output 
## Matrix of costs
df_c <- as.data.frame(matrix(0, 
                             nrow = n_sim,
                             ncol = n_str))
colnames(df_c) <- v_names_str

df_c_tt <- as.data.frame(matrix(0, 
                             nrow = n_sim,
                             ncol = n_str))
colnames(df_c_tt) <- v_names_str


## Matrix of effectiveness
df_e <- as.data.frame(matrix(0, 
                             nrow = n_sim,
                             ncol = n_str))
colnames(df_e) <- v_names_str


#### 05a.4 Conduct probabilistic sensitivity analysis ####
### Run decision model on each parameter set of PSA input dataset to produce
### PSA outputs for cost and effects
for(i in 1:n_sim){
  
  temp_name <- paste0("draw_", i)
  
  psa_input <- df_psa_inputs %>%
    select(Variable, !!temp_name)
  
  all_param_psa <- load_all_params_psa(df_psa_params = psa_input,
                                       file.init_pop = "data-raw/01_init_params_pop.csv",
                                       file.init_tris = "data-raw/01_init_params_prev_common_tris.csv")
  
  cea_input <- generate_decision_model_input(all_param_psa)
  
  l_out_dm <- decision_model(cea_input = cea_input, file.init_tree = "data-raw/01_init_params_decision_tree.csv")
  
  custom_order <- c("scan","scan & fct","scan & targeted nipt","scan & wg nipt")
  order_indices <- order(match(l_out_dm$df_cea_icer$Strategy, custom_order))
  sorted_df <- l_out_dm$df_cea_icer[order_indices , ]
  
  df_e[i, ] <- sorted_df$Effect
  df_c[i, ] <- sorted_df$Cost/sorted_df$Effect
  df_c_tt[i,] <- sorted_df$Cost
  
}

#save(df_c_tt,file = "data/05b_psa_costs_tt_rebut1.RData") 
#save(df_c,file = "data/05b_psa_costs_rebut1.RData") 
#save(df_e,file = "data/05b_psa_effects_rebut1.RData")
#save(df_psa_input,file = "data/05b_psa_inputs_rebut1.RData") 

# save(df_c_tt,file = "data/05b_psa_costs_tt_rebut2.RData") 
# save(df_c,file = "data/05b_psa_costs_rebut2.RData")
# save(df_e,file = "data/05b_psa_effects_rebut2.RData") 
# save(df_psa_input,file = "data/05b_psa_inputs_rebut2.RData") 

# save(df_c_tt,file = "data/05b_psa_costs_tt_rebut3.RData") 
# save(df_c,file = "data/05b_psa_costs_rebut3.RData") 
# save(df_e,file = "data/05b_psa_effects_rebut3.RData")
# save(df_psa_input,file = "data/05b_psa_inputs_rebut3.RData")


load("data/05b_psa_costs_rebut1.RData")
df_c_1 <- df_c
load("data/05b_psa_costs_tt_rebut1.RData")
df_c_tt_1 <- df_c_tt
load("data/05b_psa_effects_rebut1.RData")
df_e_1 <- df_e
load("data/05b_psa_inputs_rebut1.RData")
df_psa_input_1 <- as.data.frame(df_psa_input)

load("data/05b_psa_costs_rebut2.RData")
df_c_2 <- df_c
load("data/05b_psa_costs_tt_rebut2.RData")
df_c_tt_2 <- df_c_tt
load("data/05b_psa_effects_rebut2.RData")
df_e_2 <- df_e
load("data/05b_psa_inputs_rebut2.RData")
df_psa_input_2 <- as.data.frame(df_psa_input)

load("data/05b_psa_costs_rebut3.RData")
df_c_3 <- df_c
load("data/05b_psa_costs_tt_rebut3.RData")
df_c_tt_3 <- df_c_tt
load("data/05b_psa_effects_rebut3.RData")
df_e_3 <- df_e
load("data/05b_psa_inputs_rebut3.RData")
df_psa_input_3 <- as.data.frame(df_psa_input)

df_e <- rbind(df_e_1,df_e_2,df_e_3)
df_c <- rbind(df_c_1,df_c_2,df_c_3)
df_c_tt <- rbind(df_c_tt_1,df_c_tt_2,df_c_tt_3)
df_psa_input <- rbind(df_psa_input_1,df_psa_input_2,df_psa_input_3)



# -------------------
ci_95 <- function(x) {
  quantile(x, probs = c(0.025, 0.975))
}

# Calculate the 95% CI for costs
costs_ci <- apply(df_c_tt, 2, ci_95)
costs_per_case_ci <- apply(df_c, 2, ci_95)


# Calculate the 95% CI for effects
effects_ci <- apply(df_e, 2, ci_95)

# Print the results
costs_ci
costs_per_case_ci
effects_ci

# CI ICERS
# SCAN FCT 
mean_fct_scan_icer <- mean((df_c_tt$`scan & fct`-df_c_tt$scan)/(df_e$`scan & fct`-df_e$scan))
mean_fct_scan_icer
df_fct_scan_icer <- as.data.frame((df_c_tt$`scan & fct`-df_c_tt$scan)/(df_e$`scan & fct`-df_e$scan))
fct_scan_icer_ic <- apply(df_fct_scan_icer, 2, ci_95)
fct_scan_icer_ic

# SCAN targeted NIPT 
mean_tnipt_scan_icer <- mean((df_c_tt$`scan & targeted nipt`-df_c_tt$scan)/(df_e$`scan & targeted nipt`-df_e$scan))
mean_tnipt_scan_icer
df_tnipt_scan_icer <- as.data.frame((df_c_tt$`scan & targeted nipt`-df_c_tt$scan)/(df_e$`scan & targeted nipt`-df_e$scan))
tnipt_scan_icer_ic <- apply(df_tnipt_scan_icer, 2, ci_95)
tnipt_scan_icer_ic

# SCAN gw-NIPT 
mean_gwnipt_scan_icer <- mean((df_c_tt$`scan & wg nipt`-df_c_tt$scan)/(df_e$`scan & wg nipt`-df_e$scan))
mean_gwnipt_scan_icer
df_gwnipt_scan_icer <- as.data.frame((df_c_tt$`scan & wg nipt`-df_c_tt$scan)/(df_e$`scan & wg nipt`-df_e$scan))
gwnipt_scan_icer_ic <- apply(df_gwnipt_scan_icer, 2, ci_95)
gwnipt_scan_icer_ic

# FCT targeted NIPT
test1 = df_c_tt$`scan & targeted nipt`-df_c_tt$`scan & fct`
test2 = df_e$`scan & targeted nipt`-df_e$`scan & fct`
test3 = test1/test2
mean_tnipt_fct_icer = mean(test3[is.finite(test3)])
mean_tnipt_fct_icer
#mean_tnipt_fct_icer <- mean((df_c_tt$`scan & targeted nipt`-df_c_tt$`scan & fct`)/(df_e$`scan & targeted nipt`-df_e$`scan & fct`))
#mean_tnipt_fct_icer
df_tnipt_fct_icer <- as.data.frame((df_c_tt$`scan & targeted nipt`-df_c_tt$`scan & fct`)/(df_e$`scan & targeted nipt`-df_e$`scan & fct`))
tnipt_fct_icer_ic <- apply(df_tnipt_fct_icer, 2, function(x) ci_95(x[is.finite(x)]))
tnipt_fct_icer_ic

# FCT gw NIPT
test4 = df_c_tt$`scan & wg nipt`-df_c_tt$`scan & fct`
test5 = df_e$`scan & wg nipt`-df_e$`scan & fct`
test6 = test4/test5
mean_gwnipt_fct_icer = mean(test6[is.finite(test6)])
mean_gwnipt_fct_icer
#mean_gwnipt_fct_icer <- mean((df_c_tt$`scan & wg nipt`-df_c_tt$`scan & fct`)/(df_e$`scan & wg nipt`-df_e$`scan & fct`))
#mean_gwnipt_fct_icer
df_gwnipt_fct_icer <- as.data.frame((df_c_tt$`scan & wg nipt`-df_c_tt$`scan & fct`)/(df_e$`scan & wg nipt`-df_e$`scan & fct`))
gwnipt_fct_icer_ic <- apply(df_gwnipt_fct_icer, 2, function(x) ci_95(x[is.finite(x)]))
gwnipt_fct_icer_ic

# targeted NIPT / gw nipt
test1 = df_c_tt$`scan & wg nipt`-df_c_tt$`scan & targeted nipt`
test2 = df_e$`scan & wg nipt`-df_e$`scan & targeted nipt`
test3 = test1/test2
mean_gwnipt_tnipt_icer = mean(test3[is.finite(test3)])
mean_gwnipt_tnipt_icer

df_gwnipt_tnipt_icer <- as.data.frame((df_c_tt$`scan & wg nipt`-df_c_tt$`scan & targeted nipt`)/(df_e$`scan & wg nipt`-df_e$`scan & targeted nipt`))
gwnipt_tnipt_icer_ic <- apply(df_gwnipt_tnipt_icer, 2, function(x) ci_95(x[is.finite(x)]))
gwnipt_tnipt_icer_ic

# PLOT RESULTS
# Create a combined data frame for costs and effects
df_combined <- data.frame(
  Costs = as.vector(as.matrix(df_c_tt)),
  Effects = as.vector(as.matrix(df_e)),
  Strategy = rep(c("Scan", "Scan and FCT", "Scan and targeted NIPT", "Scan and targeted- or GW-NIPT"), each = nrow(df_c))
)

# Create a data frame for the mean values and CIs
mean_values <- data.frame(
  Strategy = c("Scan", "Scan and FCT", "Scan and targeted NIPT", "Scan and targeted- or GW-NIPT"),
  Mean_Costs = colMeans(df_c_tt),
  Mean_Effects = colMeans(df_e),
  Cost_Lower = costs_ci[1,],
  Cost_Upper = costs_ci[2,],
  Effect_Lower = effects_ci[1,],
  Effect_Upper = effects_ci[2,]
)

# Create the plot

ggplot(df_combined, aes(x = Effects, y = Costs, color = Strategy)) +
  geom_point(alpha = 0.5) +  # Scatter plot with transparency
  stat_ellipse(level = 0.95) +  # Add 95% confidence ellipses
  
    geom_point(data = mean_values, aes(x = Mean_Effects, y = Mean_Costs, fill = Strategy),
             size = 4, shape = 21, stroke = 1.5, color = "black") +  # Mean points with black outline and fill based on Strategy
  
  # Add horizontal and vertical lines for 95% CIs
  geom_segment(data = mean_values, 
               aes(x = Effect_Lower, xend = Effect_Upper, y = Mean_Costs, yend = Mean_Costs, color = Strategy), 
               linetype = "dashed", size = 1) +  # Horizontal lines for costs CI
  geom_segment(data = mean_values, 
               aes(x = Mean_Effects, xend = Mean_Effects, y = Cost_Lower, yend = Cost_Upper, color = Strategy), 
               linetype = "dashed", size = 1) +  # Vertical lines for effects CI
  theme_minimal() +  # Use a minimal theme
  labs(title = "Cost-Effectiveness Analysis",
       x = "Effects (Total number of fetal chromosomal abnormalities detected)",
       y = "Total costs of screening programme (€)",
       color = "Strategy", fill = "Strategy") +  # Add fill legend
  theme(
    legend.position = "right",  # Place legend on the right
    plot.title = element_text(hjust = 0.5)  # Center the title
  )+
  
  # Set x and y axis limits to start at 0
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +  # Ensure x-axis starts at 0
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0, NA))  # Ensure y-axis starts at 0

#ggsave("figs/05a_figure_S5_a_cea_psa_scatter_13092024.png", width = 10, height = 6)


# test other colors
# Define a color‑blind–friendly palette (Okabe–Ito)
cb_palette <- c(
  "Scan" = "#E69F00",  # orange
  "Scan and FCT" = "#56B4E9",  # sky blue
  "Scan and targeted NIPT" = "#F0E442",  # yellow
  "Scan and targeted- or GW-NIPT" =  "#CC79A7"  # purple
)



ggplot(df_combined, aes(x = Effects, y = Costs, color = Strategy)) +
  geom_point(alpha = 0.5) +
  stat_ellipse(level = 0.95) +
  
  # Mean points with black outline, fill from palette
  geom_point(data = mean_values,
             aes(x = Mean_Effects, y = Mean_Costs, fill = Strategy),
             size = 4, shape = 21, stroke = 1.5, color = "black") +
  
  # 95% CI segments
  geom_segment(data = mean_values,
               aes(x = Effect_Lower, xend = Effect_Upper, 
                   y = Mean_Costs, yend = Mean_Costs, color = Strategy),
               linetype = "dashed", size = 1) +
  geom_segment(data = mean_values,
               aes(x = Mean_Effects, xend = Mean_Effects, 
                   y = Cost_Lower, yend = Cost_Upper, color = Strategy),
               linetype = "dashed", size = 1) +
  
  # Apply the palette to both color and fill scales
  scale_color_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +
  
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = scales::comma, expand = c(0, 0), limits = c(0, NA)) +
  
  labs(
    title = "Cost-Effectiveness Analysis",
    x = "Effects (Total number of fetal chromosomal abnormalities detected)",
    y = "Total costs of screening programme (€)",
    color = "Strategy",
    fill  = "Strategy"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title     = element_text(hjust = 0.5)
  )

ggsave("figs/05a_figure_S4_a_cea_psa_scatter_rebuttal_col.png", width = 10, height = 6)


# Count the percentage of simulations in which the order is equal to base case based on effectiveness (wg-nipt, targeted nipt, fct, scan)
# Count the percentage of simulations in which the order is equal to base case based on costs (wg-nipt, targeted nipt, fct, scan)
# Creating base case outcomes (random integers for demonstration)
v_effects_base_case <- c(295,452,513,544)

df_base <- as.data.frame(do.call(rbind, replicate(999, v_effects_base_case, simplify = FALSE)))

df_base_screen <- df_base[,-1]
df_e_screen <- df_e[,-1]

# Function to compare order of values in each row
compare_order <- function(row_base, row_e) {
  identical(order(row_base), order(row_e))
}

# Compare order for each row
comparison_results <- mapply(compare_order, as.data.frame(t(df_base)), as.data.frame(t(df_e)))
comparison_results_screen <- mapply(compare_order, as.data.frame(t(df_base_screen)), as.data.frame(t(df_e_screen)))

# Calculate percentage of rows with similar order
percentage <- mean(comparison_results)
percentage_screen <- mean(comparison_results_screen)
  
# Print the percentage
cat("Percentage of rows with similar order (scan, fct, targeted nipt, wg nipt):", percentage * 100, "%\n")
cat("Percentage of rows with similar order (fct, targeted nipt, wg nipt):", percentage_screen * 100, "%\n")




#psa_outcomes <- list(df_c,df_e)
#names(psa_outcomes) <- c("psa_costs", "psa_effects")
#openxlsx::write.xlsx(psa_outcomes, file = '../paper cea nipt/tables manuscript/detailed outcomes/table_psa_detailed_outc_24022024.xlsx', rowNames = TRUE) 

#### 05a.6 Create probabilistic analysis graphs ####


#### 05a.6.1 Cost-effectiveness scatter plot ####
#plot(l_psa) + 
#  ylab("Total cost of screening programme in Euros (€)")
# +ylab("Cost per detected fetal chromosomal anomaly in Euros (€)")

#ggsave("figs/05a_figure_S4_a_cea_psa_scatter_new_prices.png", width = 8, height = 6)

#### 05a.6.2 Conduct CEA with probabilistic output ####
### Compute expected costs and effects for each strategy from the PSA
#df_out_ce_psa <- summary(l_psa)
#df_out_ce_psa <- summary(l_psa)
### Calculate incremental cost-effectiveness ratios (ICERs)
#df_cea_psa <- calculate_icers(cost = df_out_ce_psa$meanCost, 
#                              effect = df_out_ce_psa$meanEffect,
#                              strategies = df_out_ce_psa$Strategy)



#### 05a.6.3 Plot cost-effectiveness frontier ####

# Create output for Table S6 in manuscript
v_rows_table_s6 <- c('total costs screening programme (M€)','total diagnosed fetal abnormalities',
                   'cost per diagnosed case (k€)','incr. cost per diagnosed case (ref: scan) (k€)',
                   'incr. cost per diagnosed case (ref: fct) (k€)','incr. cost per diagnosed case (ref: targeted nipt) (k€)')
v_cols_table_s6 <- c('scan','scan & fct', "scan & targeted nipt","scan & wg nipt") 

table_s6 <- as.data.frame(matrix(NA,
                               nrow=length(v_rows_table_s6),
                               ncol=length(v_cols_table_s6)
))

row.names(table_s6) <- v_rows_table_s6
colnames(table_s6) <- v_cols_table_s6

# fill table
table_s6[rownames(table_s6)=='total costs screening programme (M€)',] <- c(df_out_ce_psa$meanCost) / 1000000


table_s6[rownames(table_s6)=='total diagnosed fetal abnormalities',] <- c(df_out_ce_psa$meanEffect)


table_s6[rownames(table_s6)=='cost per diagnosed case (k€)',] <- (c(df_out_ce_psa$meanCost/df_out_ce_psa$meanEffect)) / 1000

custom_order <- c("scan","scan & fct","scan & targeted nipt","scan & wg nipt")
order_indices <- order(match(df_out_ce_psa$Strategy, custom_order))
sorted_df <- df_out_ce_psa[order_indices , ]

table_s6[rownames(table_s6)=='incr. cost per diagnosed case (ref: scan) (k€)',]  <- (table_s6[rownames(table_s6)=='total costs screening programme (M€)',] - table_s6[rownames(table_s6)=='total costs screening programme (M€)','scan']) * 1000/ 
  (table_s6[rownames(table_s6)=='total diagnosed fetal abnormalities',] - table_s6[rownames(table_s6)=='total diagnosed fetal abnormalities','scan'])
table_s6[rownames(table_s6)=='incr. cost per diagnosed case (ref: scan) (k€)','scan'] = NA

table_s6[rownames(table_s6)=='incr. cost per diagnosed case (ref: fct) (k€)',] <- (table_s6[rownames(table_s6)=='total costs screening programme (M€)',] - table_s6[rownames(table_s6)=='total costs screening programme (M€)','scan & fct']) * 1000/ 
  (table_s6[rownames(table_s6)=='total diagnosed fetal abnormalities',] - table_s6[rownames(table_s6)=='total diagnosed fetal abnormalities','scan & fct'])
table_s6[rownames(table_s6)=='incr. cost per diagnosed case (ref: fct) (k€)',c('scan','scan & fct')] = NA

table_s6[rownames(table_s6)=='incr. cost per diagnosed case (ref: targeted nipt) (k€)',] <- (table_s6[rownames(table_s6)=='total costs screening programme (M€)',] - table_s6[rownames(table_s6)=='total costs screening programme (M€)','scan & targeted nipt']) * 1000/ 
  (table_s6[rownames(table_s6)=='total diagnosed fetal abnormalities',] - table_s6[rownames(table_s6)=='total diagnosed fetal abnormalities','scan & targeted nipt'])

table_s6[rownames(table_s6)=='incr. cost per diagnosed case (ref: targeted nipt) (k€)',c('scan','scan & fct','scan & targeted nipt')] = NA

# Save table s6 in Excel
# openxlsx::write.xlsx(table_s6, file = '../paper cea nipt/tables manuscript/table_S6_R.xlsx', rowNames = TRUE) 

