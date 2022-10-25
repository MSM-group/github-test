# Install packages
pacman::p_load("janitor", "readxl", "ggpubr", "tidyverse")

# Read in the raw dataset
raw <- read_csv("data/4_k-rates_table_single_replicates_38BT_EI2_1.csv") %>%
  janitor::clean_names()
# Column descriptions k: biotransformation rate constants
# r2 at the beginning is the strength of the fit
# r1-r4 at the end are replicates 