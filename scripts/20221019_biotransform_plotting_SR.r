# Install packages
pacman::p_load("janitor", "readxl", "ggpubr", "tidyverse")
# note that ggplot2 used here is part of the 'tidyverse' core packages https://www.tidyverse.org/packages/

# Read in the biotransformation data sheet 1
biotransform1 <- read_excel("data/Fenner_biotransform_experimental_data.xlsx", sheet = 1) %>% 
  janitor::clean_names()

biotransform2 <- read_excel("data/Fenner_biotransform_experimental_data.xlsx", sheet = 2) %>%
  janitor::clean_names()

deet_wide <- biotransform1 %>%
  bind_rows(., biotransform2) %>% # combine two sheets
  dplyr::filter(compound_name == "DEET") # select compound of interest

deet_long <- deet_wide %>%
  pivot_longer(., cols = -c(compound_name, experiment), names_to = "treatment", values_to = "rate")

deet_filt <- deet_long %>%
  dplyr::filter(str_detect(treatment, "_uf_",negate = TRUE)) %>% # remove ultrafiltered (uf) treatment
  tidyr::drop_na() %>% # remove all NAs
  dplyr::mutate(mean_ww_perc = word(treatment, sep = "_", 2)) %>%
  dplyr::mutate(ww_perc = as.numeric(str_remove(pattern =  "mean", string = mean_ww_perc)), .keep = "unused") 

# Plot the biotransformation rate vs. WW % as a scatterplot
pdf("output/deet_biotransformation_rates_by_experiment_scatterplot.pdf", height = 3, width = 3)
ggplot(deet_filt, aes(x = ww_perc, y = rate)) +
  geom_point(aes(color = factor(experiment))) + 
  scale_color_manual(values = c("maroon", "steelblue")) +
  theme_pubr() +
  labs(x = "% Treated WW", y = "Biotransformation rate", color = "Experiment") +
  scale_x_continuous(breaks=c(0, 10, 30, 80))
dev.off()

# Plot the biotransformation rate vs. WW % as a barplot
pdf("output/deet_biotransformation_rates_by_experiment_barplot.pdf", height = 3, width = 3)
ggplot(deet_filt, aes(x = ww_perc, y = rate)) +
  geom_bar(aes(color = factor(experiment), fill = factor(experiment)), stat = "identity", position = "dodge") + 
  scale_color_manual(values = c("maroon", "steelblue")) +
  scale_fill_manual(values = c("maroon", "steelblue")) +
  theme_pubr() +
  labs(x = "% Treated WW", y = "Biotransformation rate", color = "Experiment", fill = "Experiment") +
  scale_x_continuous(breaks=c(0, 10, 30, 80))
dev.off()


