# Install packages
pacman::p_load("janitor", "tidyverse", "readxl","ggplot2", "ggpubr")

# Read in the biotransformation data
biotransform1 <- read_excel("data/Fenner_biotransform_experimental_data.xlsx", sheet = 1) %>%
  janitor::clean_names()
colnames(biotransform1)

deet1 <- biotransform1 %>%
  dplyr::filter(compound_name == "DEET") %>%
  t() %>%
  data.frame() %>%
  rownames_to_column(.) %>%
  dplyr::slice(3:6) %>%
  dplyr::mutate(expt = "Expt1")
colnames(deet1) <- c("treatment", "kbio", "expt")

biotransform2 <- read_excel("data/Fenner_biotransform_experimental_data.xlsx", sheet = 2) %>%
  janitor::clean_names()

deet2 <- biotransform2 %>%
  dplyr::filter(compound_name == "DEET") %>%
  dplyr::select(-contains("_uf_")) %>%
  t() %>%
  data.frame() %>%
  rownames_to_column(.) %>%
  dplyr::slice(3:5) %>%
  dplyr::mutate(expt = "Expt2") 
colnames(deet2) <- c("treatment", "kbio", "expt") 

deet_comb <- bind_rows(deet1, deet2) %>%
  dplyr::mutate(ww_perc = as.numeric(gsub("mean", "", word(treatment, sep = "_", 2)))) %>%
  dplyr::mutate(expt_ww = paste0(expt, "_", ww_perc)) %>%
  dplyr::mutate(kbio = as.numeric(kbio))
deet_comb

# Plot biotransformation rate
pdf("data/deet_biotransformation_rates_by_experiment.pdf", height = 3, width = 3)
ggplot(deet_comb, aes(ww_perc, kbio, color = factor(expt), fill = factor(expt))) +
  geom_point(aes(color = factor(expt))) + 
  scale_color_manual(values = c("maroon", "steelblue")) +
  geom_smooth(method="lm", se = "none") +
  theme_pubr() +
  xlab("% Treated WW") +
  ylab("kbio") +
  scale_x_continuous(breaks=c(0, 10, 30, 80))
dev.off()
