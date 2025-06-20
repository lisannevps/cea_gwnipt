### Run all code in this script to download the desired R packages
### from either CRAN or GitHub
# The entire process will take several minutes


#############################################################
# Run the code LINE BY LINE , in the specified order below
############################################################
# first download and use this package to conveniently install other packages
#install.packages('pacman')
library(pacman) 


#load (install if required) packages from CRAN
p_load("abind",  "dampack", "data.table", "DES", "devtools", "diagram", "dplyr", 
       "ellipse", "flexsurv", "flexsurvcure", "gdata", "gems", "grid", "gridExtra", 
       "igraph", "jsonlite", "knitr", "lazyeval", "lhs", 
       "markdown", "matrixStats", "mgcv", "msm", "mstate",
       "plotrix", "purrr", "psych", "reshape2", "rstudioapi",   
       "scales", "scatterplot3d", "stringr", "survHE", "survminer", "shiny",
       "tidyverse", "tidyr", "triangle", "truncnorm","readxl") 


# Enter an empty line to skip updates when prompted
#install_version("ggplot2", version = "3.3.3", repos = "http://cran.us.r-project.org") 
p_load("ggplot2")

# Install additional packages
p_load("ggrepel")

# load (install if required) packages from GitHub

#install_github("DARTH-git/darthtools", force = TRUE) # (Un)comment if there is a newer version#
p_load_gh("DARTH-git/darthtools")

#install_github("DARTH-git/OpenTree", force = TRUE) # (Un)comment if there is a newer version
p_load_gh("DARTH-git/OpenTree")


# Install additional packages
p_load("ggraph") 
