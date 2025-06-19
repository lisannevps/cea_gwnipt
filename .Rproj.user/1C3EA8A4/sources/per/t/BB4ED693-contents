
# -----------
library(cowplot)

load("data/05b_owsa_uptake_fct_prob_020924.RData")
load("data/05b_owsa_uptake_nipt_prob_020924.RData")
load("data/05b_owsa_uptake_new_prices_270824.RData")
load("data/05b_owsa_prev_rebuttal.RData")
load("data/05b_owsa_screen_char_new_prices_270824.RData")
load("data/05b_owsa_nipt_fail_rebuttal.RData")
load("data/05b_owsa_scan_char_rebuttal.RData")
load("data/05b_owsa_costs_new_prices_27082024.RData")

# Create a data frame with 9 parameter names and their base values
df_base_values_fig_2 <- data.frame(
  ParameterName = c("c_fct", "c_nipt", "p_fct_fpr",
                    "p_fct_sens_t21","p_scan_sens_t21","v_p_fct_part",
                    "p_part_it_screen","p_part_it_fct","v_p_nipt_part"),
  BaseValue = c(191.23, 350, 0.055,
                0.84,0.43,0.35,
                0.924,0.88,0.45),
  ParamPlotName = c("h. Cost of FCT","i. Cost of NIPT","f. False-positive FCT",
                    "e. Sensitivity FCT Trisomy 21","g. Sensitivity of Scan for T21","d. Uptake FCT",
                    "b. Uptake IT: Abn NIPT for T21, T18, T13","a. Uptake IT: Abn FCT","c. Uptake NIPT")
)


# Variables with most impact
load("data/05b_owsa_prev_rebuttal.RData")
load("data/05b_owsa_nipt_fail_rebuttal.RData")
load("data/05b_owsa_scan_char_rebuttal.RData")


owsa_uptake_main <- owsa_uptake$df_owsa_lng[owsa_uptake$df_owsa_lng$parameter %in% c('p_part_it_screen','p_part_it_fct'), ]
owsa_uptake_main_2 <- owsa_uptake_nipt_prob$df_owsa_lng[owsa_uptake_nipt_prob$df_owsa_lng$parameter %in% c('v_p_nipt_part'), ]
owsa_uptake_main_2$param_val = c(0.362,0.407,0.452,0.497,0.543,0.362,0.407,0.452,0.497,0.543,0.362,0.407,0.452,0.497,0.543,0.362,0.407,0.452,0.497,0.543)
owsa_uptake_main_3  <- owsa_uptake_fct_prob$df_owsa_lng[owsa_uptake_fct_prob$df_owsa_lng$parameter %in% c('v_p_fct_part'), ]
owsa_uptake_main_3$param_val = c(0.283,0.319,0.354,0.390,0.425,0.283,0.319,0.354,0.390,0.425,0.283,0.319,0.354,0.390,0.425,0.283,0.319,0.354,0.390,0.425)
owsa_screen_char_main <- owsa_screen_char$df_owsa_lng[owsa_screen_char$df_owsa_lng$parameter %in% c('p_fct_fpr', 'p_fct_sens_t21'), ]
#owsa_scan_char_main <- owsa_scan_char$df_owsa_lng[owsa_scan_char$df_owsa_lng$parameter %in% c('p_scan_sens_t21'), ]
owsa_scan_char_main <- owsa_scan_char_rebuttal$df_owsa_lng[owsa_scan_char_rebuttal$df_owsa_lng$parameter %in% c('p_scan_sens_t21'), ]

owsa_costs_main <- owsa_costs$df_owsa_lng[owsa_costs$df_owsa_lng$parameter %in% c('c_fct','c_nipt'), ]

combined_df <- rbind(owsa_uptake_main,owsa_uptake_main_2,owsa_uptake_main_3,owsa_screen_char_main,owsa_scan_char_main,owsa_costs_main)
levels(combined_df$strategy)[levels(combined_df$strategy) == "scan"] <- "Scan"
levels(combined_df$strategy)[levels(combined_df$strategy) == "scan & wg nipt"] <- "Scan and Targeted or GW-NIPT"
levels(combined_df$strategy)[levels(combined_df$strategy) == "scan & targeted nipt"] <- "Scan and Targeted NIPT"
levels(combined_df$strategy)[levels(combined_df$strategy) == "scan & fct"] <- "Scan and FCT"
combined_df <- combined_df %>%
  rename("Strategy" = strategy)


combined_list <- list(df_owsa_lng = combined_df)

# Color palette
color_palette <- c(
  "#E69F00", # orange
  "#56B4E9", # light blue
  "#F0E442", # yellow
  "#CC79A7"  # purple
)

# List to store individual plots
plots <- list()

# Example loop to generate plots for each parameter
for (param in unique(combined_list$df_owsa_lng$parameter)) {
  
  # Subset the data for the specific parameter
  data_subset <- subset(combined_list$df_owsa_lng, parameter == param)
  
  # Look up the base case value from df_base_values
  base_value <- df_base_values_fig_2$BaseValue[df_base_values_fig_2$ParameterName == param]
  plot_name <- df_base_values_fig_2$ParamPlotName[df_base_values_fig_2$ParameterName == param]
  
  # Create the plot
  p <- ggplot(data_subset, aes(x = param_val, y = outcome_val, color = Strategy)) +
    geom_line() +
    geom_vline(xintercept = base_value, linetype = "dashed", color = "black") +
    scale_color_manual(values = color_palette) +  # Use custom colors
    labs(title = paste(plot_name), x = "Parameter Value", y = "Cost per Case Detected (€)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 7, hjust = 0.5,face = "bold"),
      axis.title.x = element_text(size = 7),  # X-axis title size
      axis.title.y = element_text(size = 7),  # Y-axis title size
                                  # Center the title
      legend.position = "none"                 # Remove individual legends
    )
  
  # Add the plot to the list
  plots[[param]] <- p
}


legend <- get_legend(
  ggplot(subset(combined_list$df_owsa_lng, parameter == unique(combined_list$df_owsa_lng$parameter)[1]), 
         aes(x = param_val, y = outcome_val, color = Strategy)) +
    geom_line() +
    scale_color_manual(values = color_palette) +  # Use custom colors
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 6),   # Adjust legend text size
      legend.title = element_text(size = 6,face="bold"), # Adjust legend title size
      legend.key.size = unit(0.5, "lines")    # Adjust legend key size
    )
)



# Combine all plots into one figure
combined_plot <- plot_grid(plotlist = plots, align = "v")

# Wrap the legend in a smaller area to control its size
wrapped_legend <- ggdraw() + draw_grob(grid::grobTree(legend), x = 0, y = 0, width = 1, height = 1)

# Arrange the combined plot with the legend on the right
final_plot <- plot_grid(combined_plot, wrapped_legend, ncol = 2, rel_widths = c(4, 1))

# Save Figure 2
print(final_plot)
# ggsave("figs/05b_figure_2_owsa_rebuttal_col.png", width = 8, height = 6)

# --------------------------------

# -- CREATE FIGURE S3
# (S3.1) [working]

# Variables selection
owsa_uptake_s3_1 <- owsa_uptake$df_owsa_lng[owsa_uptake$df_owsa_lng$parameter %in% c('v_p_targ_nipt','p_part_it_direct','p_part_it_scan','p_part_scan','v_p_fct_part','v_p_nipt_part'), ]
combined_df_s3_1 <-  rbind(owsa_uptake_s3_1)
  
levels(combined_df_s3_1$strategy)[levels(combined_df_s3_1$strategy) == "scan"] <- "Scan"
levels(combined_df_s3_1$strategy)[levels(combined_df_s3_1$strategy) == "scan & wg nipt"] <- "Scan and Targeted- or GW-NIPT"
levels(combined_df_s3_1$strategy)[levels(combined_df_s3_1$strategy) == "scan & targeted nipt"] <- "Scan and Targeted NIPT"
levels(combined_df_s3_1$strategy)[levels(combined_df_s3_1$strategy) == "scan & fct"] <- "Scan and FCT"
combined_df_s3_1 <- combined_df_s3_1 %>%
  rename("Strategy" = strategy)


combined_list_s3_1 <- list(df_owsa_lng = combined_df_s3_1)


df_base_values_fig_s3_1 <- data.frame(
  ParameterName = c('v_p_targ_nipt','p_part_it_direct','p_part_it_scan','p_part_scan','v_p_fct_part','v_p_nipt_part'),
  BaseValue = c(0.200,0.0043,0.66,0.847,0.35,0.45),
  ParamPlotName = c("Proportion opting for targeted NIPT","Uptake IT without prior screening",'Uptake IT: abnormal scan','Uptake second trimester anomaly scan','Uptake FCT','Uptake NIPT')
)

# HERE
plots <- list()

# Example loop to generate plots for each parameter
for (param in unique(combined_list_s3_1$df_owsa_lng$parameter)) {
  
  # Subset the data for the specific parameter
  data_subset <- subset(combined_list_s3_1$df_owsa_lng, parameter == param)
  
    # Look up the base case value from df_base_values
  base_value <- df_base_values_fig_s3_1$BaseValue[df_base_values_fig_s3_1$ParameterName == param]
  plot_name <- df_base_values_fig_s3_1$ParamPlotName[df_base_values_fig_s3_1$ParameterName == param]
  
  # Create the plot
  p <- ggplot(data_subset, aes(x = param_val, y = outcome_val, color = Strategy)) +
    geom_line() +
    geom_vline(xintercept = base_value, linetype = "dashed", color = "black") +
    scale_color_manual(values = color_palette) +  # Use custom colors
    labs(title = paste(plot_name), x = "Parameter Value", y = "Cost per Case Detected (€)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8, hjust = 0.5,face = "bold"),
      axis.title.x = element_text(size = 7),  # X-axis title size
      axis.title.y = element_text(size = 7),  # Y-axis title size
      # Center the title
      legend.position = "none"                 # Remove individual legends
    )
  
  # Conditionally set x-axis limits for v_p_fct_part
  #if (param == "v_p_fct_part") {
  #  p <- p + xlim(-0.2, 0.2)
  #}
  
  # Add the plot to the list
  plots[[param]] <- p
}

legend <- get_legend(
  ggplot(subset(combined_list_s3_1$df_owsa_lng, parameter == unique(combined_list_s3_1$df_owsa_lng$parameter)[1]), 
         aes(x = param_val, y = outcome_val, color = Strategy)) +
    geom_line() +
    scale_color_manual(values = color_palette) +  # Use custom colors
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 8),   # Adjust legend text size
      legend.title = element_text(size = 8,face="bold"), # Adjust legend title size
      legend.key.size = unit(0.5, "lines")    # Adjust legend key size
    )
)


# Combine all plots into one figure
combined_plot <- plot_grid(plotlist = plots, ncol=2,nrow=3,align = "v")

# Wrap the legend in a smaller area to control its size
wrapped_legend <- ggdraw() + draw_grob(grid::grobTree(legend), x = 0, y = 0, width = 1, height = 1)

# Arrange the combined plot with the legend on the right
final_plot <- plot_grid(combined_plot, wrapped_legend, ncol = 2, rel_widths = c(4, 1))

# Save Figure S3.1
print(final_plot)
# ggsave("figs/05b_figure_s3.1_owsa_rebuttal_col.png", width = 12, height = 6)

#-------
# (S3.2) [working]

# Variables selection
owsa_uptake_s3_2_a <- owsa_screen_char_rebuttal$df_owsa_lng[owsa_screen_char_rebuttal$df_owsa_lng$parameter %in% c('p_nipt_fail','p_nipt_fail_2'), ]
owsa_uptake_s3_2_b <- owsa_screen_char$df_owsa_lng[owsa_screen_char$df_owsa_lng$parameter %in% c('p_nipt_fpr'), ]
owsa_uptake_s3_2_c <-owsa_scan_char_rebuttal$df_owsa_lng[owsa_scan_char_rebuttal$df_owsa_lng$parameter %in% c('p_scan_sens_t13','p_scan_sens_t18','p_scan_sens_t21'), ]
  
combined_df_s3_2 <- rbind(owsa_uptake_s3_2_a,owsa_uptake_s3_2_b,owsa_uptake_s3_2_c)

levels(combined_df_s3_2$strategy)[levels(combined_df_s3_2$strategy) == "scan"] <- "Scan"
levels(combined_df_s3_2$strategy)[levels(combined_df_s3_2$strategy) == "scan & wg nipt"] <- "Scan and Targeted- or GW-NIPT"
levels(combined_df_s3_2$strategy)[levels(combined_df_s3_2$strategy) == "scan & targeted nipt"] <- "Scan and Targeted NIPT"
levels(combined_df_s3_2$strategy)[levels(combined_df_s3_2$strategy) == "scan & fct"] <- "Scan and FCT"
combined_df_s3_2 <- combined_df_s3_2 %>%
  rename("Strategy" = strategy)


combined_list_s3_2 <- list(df_owsa_lng = combined_df_s3_2)


df_base_values_fig_s3_2 <- data.frame(
  ParameterName = c('p_nipt_fail','p_nipt_fail_2','p_nipt_fpr','p_scan_sens_t13','p_scan_sens_t18','p_scan_sens_t21'),
  BaseValue = c(0.015,0.14,0.0004,0.95,0.93,0.43),
  ParamPlotName = c("Failure rate first NIPT draw","Failure rate second NIPT draw",'False-positive NIPT',
                    'Sensitivity of scan for T13','Sensitivity of scan for T18','Sensitivity of scan for T21')
)

# HERE
plots <- list()

# Example loop to generate plots for each parameter
for (param in unique(combined_list_s3_2$df_owsa_lng$parameter)) {
  
  # Subset the data for the specific parameter
  data_subset <- subset(combined_list_s3_2$df_owsa_lng, parameter == param)
  
  # Look up the base case value from df_base_values
  base_value <- df_base_values_fig_s3_2$BaseValue[df_base_values_fig_s3_2$ParameterName == param]
  plot_name <- df_base_values_fig_s3_2$ParamPlotName[df_base_values_fig_s3_2$ParameterName == param]
  
  # Create the plot
  p <- ggplot(data_subset, aes(x = param_val, y = outcome_val, color = Strategy)) +
    geom_line() +
    geom_vline(xintercept = base_value, linetype = "dashed", color = "black") +
    scale_color_manual(values = color_palette) +  # Use custom colors
    labs(title = paste(plot_name), x = "Parameter Value", y = "Cost per Case Detected (€)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8, hjust = 0.5,face = "bold"),
      axis.title.x = element_text(size = 7),  # X-axis title size
      axis.title.y = element_text(size = 7),  # Y-axis title size# Center the title
      legend.position = "none"                 # Remove individual legends
    )
  
  # Add the plot to the list
  plots[[param]] <- p
}


legend <- get_legend(
  ggplot(subset(combined_list_s3_2$df_owsa_lng, parameter == unique(combined_list_s3_2$df_owsa_lng$parameter)[1]), 
         aes(x = param_val, y = outcome_val, color = Strategy)) +
    geom_line() +
    scale_color_manual(values = color_palette) +  # Use custom colors
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 8),   # Adjust legend text size
      legend.title = element_text(size = 8,face="bold"), # Adjust legend title size
      legend.key.size = unit(0.5, "lines")    # Adjust legend key size
    )
)



# Combine all plots into one figure
combined_plot <- plot_grid(plotlist = plots, ncol=2,nrow=3,align = "v")

# Wrap the legend in a smaller area to control its size
wrapped_legend <- ggdraw() + draw_grob(grid::grobTree(legend), x = 0, y = 0, width = 1, height = 1)

# Arrange the combined plot with the legend on the right
final_plot <- plot_grid(combined_plot, wrapped_legend, ncol = 2, rel_widths = c(4, 1))

# Save Figure S3.2
print(final_plot)
# ggsave("figs/05b_figure_s3.2_owsa_rebuttal_col.png", width = 12, height = 6)


# -------
# (S3.3)

# Variables selection
owsa_uptake_s3_3_a <- owsa_scan_char_rebuttal$df_owsa_lng[owsa_scan_char_rebuttal$df_owsa_lng$parameter %in% c('p_scan_sens_rat','p_scan_sens_sa'), ]
owsa_uptake_s3_3_b <-owsa_costs$df_owsa_lng[owsa_costs$df_owsa_lng$parameter %in% c('c_it','c_gen_fu','c_pre_couns','c_anom_scan'), ]

combined_df_s3_3 <- rbind(owsa_uptake_s3_3_a,owsa_uptake_s3_3_b)

levels(combined_df_s3_3$strategy)[levels(combined_df_s3_3$strategy) == "scan"] <- "Scan"
levels(combined_df_s3_3$strategy)[levels(combined_df_s3_3$strategy) == "scan & wg nipt"] <- "Scan and Targeted- or GW-NIPT"
levels(combined_df_s3_3$strategy)[levels(combined_df_s3_3$strategy) == "scan & targeted nipt"] <- "Scan and Targeted NIPT"
levels(combined_df_s3_3$strategy)[levels(combined_df_s3_3$strategy) == "scan & fct"] <- "Scan and FCT"
combined_df_s3_3 <- combined_df_s3_3 %>%
  rename("Strategy" = strategy)


combined_list_s3_3 <- list(df_owsa_lng = combined_df_s3_3)


df_base_values_fig_s3_3 <- data.frame(
  ParameterName = c('p_scan_sens_rat','p_scan_sens_sa','c_it','c_gen_fu','c_pre_couns','c_anom_scan'),
  BaseValue = c(0.15,0.50,2654.61,2761.16,76.64,166.13),
  ParamPlotName = c('Sensitivity of scan for fetal RAT','Sensitivity of scan for fetal SA','Cost of invasive testing',
                    'Cost of genetic FU testing in mother','Cost pre-genetic screening counseling','Cost of second trimester anomaly scan')
)

# HERE
plots <- list()

# Example loop to generate plots for each parameter
for (param in unique(combined_list_s3_3$df_owsa_lng$parameter)) {
  
  # Subset the data for the specific parameter
  data_subset <- subset(combined_list_s3_3$df_owsa_lng, parameter == param)
  
  # Look up the base case value from df_base_values
  base_value <- df_base_values_fig_s3_3$BaseValue[df_base_values_fig_s3_3$ParameterName == param]
  plot_name <- df_base_values_fig_s3_3$ParamPlotName[df_base_values_fig_s3_3$ParameterName == param]
  
  # Create the plot
  p <- ggplot(data_subset, aes(x = param_val, y = outcome_val, color = Strategy)) +
    geom_line() +
    scale_color_manual(values = color_palette) +  # Use custom colors
    geom_vline(xintercept = base_value, linetype = "dashed", color = "black") +
    labs(title = paste(plot_name), x = "Parameter Value", y = "Cost per Case Detected (€)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 8, hjust = 0.5,face = "bold"),  # Center the title
      axis.title.x = element_text(size = 7),  # X-axis title size
      axis.title.y = element_text(size = 7),  # Y-axis title size
      legend.position = "none"                 # Remove individual legends
    )
  
  # Add the plot to the list
  plots[[param]] <- p
}


legend <- get_legend(
  ggplot(subset(combined_list_s3_3$df_owsa_lng, parameter == unique(combined_list_s3_3$df_owsa_lng$parameter)[1]), 
         aes(x = param_val, y = outcome_val, color = Strategy)) +
    geom_line() +
    scale_color_manual(values = color_palette) +  # Use custom colors
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 8),   # Adjust legend text size
      legend.title = element_text(size = 8,face="bold"), # Adjust legend title size
      legend.key.size = unit(0.5, "lines")    # Adjust legend key size
    )
)



# Combine all plots into one figure
combined_plot <- plot_grid(plotlist = plots, ncol=2,nrow=3,align = "v")

# Wrap the legend in a smaller area to control its size
wrapped_legend <- ggdraw() + draw_grob(grid::grobTree(legend), x = 0, y = 0, width = 1, height = 1)

# Arrange the combined plot with the legend on the right
final_plot <- plot_grid(combined_plot, wrapped_legend, ncol = 2, rel_widths = c(4, 1))

# Save Figure S3.3
print(final_plot)
# ggsave("figs/05b_figure_s3.3_owsa_rebuttal_col.png", width = 12, height = 6)


