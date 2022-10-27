# clear R environment
rm(list = ls())
# Install packages
library(janitor)
library(readxl)
library(ggpubr)
library(tidyverse)
# Read in the raw dataset
raw <- read_csv("data/4_k-rates_table_single_replicates_38BT_EI2_1.csv") %>%
  janitor::clean_names() %>%
  rename(compound = x1)
# Column descriptions k: biotransformation rate constants
# r2 at the beginning is the strength of the fit
# r1-r4 at the end are replicates
# pivot to long format in order to parse above information
long <- raw %>%
  pivot_longer(-compound, names_to = "specs") %>%
  mutate(specs = str_remove(specs, "_1_day"), .keep = "unused") %>% #remove unnecessary info in names
  separate(specs, sep = "_", into = c("stat", "treatment", "replicate")) #parse names for sample metadata
#pivot to wide format to have one column each for rate and r-squared per sample
wide <- long %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  rename(rate = k, r_squared = r2) %>%
  mutate(compound = as_factor(compound), treatment = as_factor(treatment))
#Create faceted boxplot of transformation rate for each sample
rate_boxplot <- ggplot(wide, aes(x = treatment, y = rate)) +
  geom_boxplot() +
  facet_wrap(vars(compound), scales = "free_y") +
  theme_pubr() +
  labs(x = "Treatment", y = "Biotransformation rate") +
  scale_x_discrete(labels = c("0% WW", "10% WW", "30% WW", "80% WW"))
# save plot
ggsave("output/biotransformation_rate_faceted_boxplot.png",
       rate_boxplot,
       device = "png",
       dpi = 300,
       width = 8000,
       height = 5000,
       units = "px")
#do kruskal-wallis test (other methods, such as ANOVA or t-test, are also possible) to find significantly different compounds
rate_kruskal_wallis <- compare_means(rate ~ treatment,
                                     wide,
                                     group.by = "compound",
                                     method = "kruskal")
#add statistical test result to plot
wide_DEET <- wide %>%
  filter(str_detect(compound, "DEET"))
rate_DEET_boxplot <- ggplot(wide, aes(x = treatment, y = rate)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal") +
  theme_pubr() +
  labs(x = "Treatment", y = "Biotransformation rate") +
  scale_x_discrete(labels = c("0% WW", "10% WW", "30% WW", "80% WW"))
# save plot
ggsave("output/DEET_biotransformation_rate_boxplot.png",
       rate_DEET_boxplot,
       device = "png",
       dpi = 300,
       width = 1600,
       height = 1000,
       units = "px")
#calculate mean and sd for rates and r2
means <- wide %>%
  group_by(compound, treatment)%>%
  summarise(rate_mean = mean(rate), rate_sd = sd(rate), r_squared_mean = mean(r_squared), r_squared_sd = sd(r_squared))
#plot treatment-wise means and SD for DEET
means_DEET <- means %>%
  filter(str_detect(compound, "DEET"))
mean_rate_DEET_plot <- ggplot(means_DEET, aes(x = treatment, y = rate_mean)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(x = treatment, ymin = rate_mean - rate_sd, ymax = rate_mean + rate_sd), width = 0.4) +
  theme_pubr() +
  labs(x = "Treatment", y = "Biotransformation rate") +
  scale_x_discrete(labels = c("0% WW", "10% WW", "30% WW", "80% WW"))
# save plot
ggsave("output/DEET_mean_biotransformation_rate_barplot.png",
       mean_rate_DEET_plot,
       device = "png",
       dpi = 300,
       width = 1600,
       height = 1000,
       units = "px")
means$compound <- fct_rev(means$compound)
#create heatmap of mean transformation rates per compound and condition
rate_heatmap <- ggplot(means, aes(x = treatment, y = compound, fill = rate_mean)) +
  geom_tile() +
  theme_pubr() +
  labs(x = "Treatment", y= "Compound", fill = "Biotransformation rate") +
  scale_x_discrete(labels = c("0% WW", "10% WW", "30% WW", "80% WW")) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = max(means$rate_mean)/2)
# save plot
ggsave("output/mean_biotransformation_rate_heatmap.png",
       rate_heatmap,
       device = "png",
       dpi = 300,
       width = 2000,
       height = 3000,
       units = "px")
