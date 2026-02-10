
source("scripts/03_models/3_1_Models_input_technocratic.R")
source("scripts/03_models/3_2_Models_input_democratic.R")
source("scripts/03_models/4_1_Models_output_technocratic.R")
source("scripts/03_models/4_2_Models_output_democratic.R")

library(tidyverse)

coefplot <- coef_plot_input_dem %>%
  bind_rows(coef_plot_output_dem) %>%
  bind_rows(coef_plot_output_tech) %>%
  bind_rows(coef_plot_input_tech) %>%
  mutate(standard = str_to_sentence(standard)) %>%
  mutate(Var = str_c(dv, ": ", standard))

# Define the colors
colors <- c(
  "Region Rae Index: Societal" = "#F8766D",  # ggplot default red
  "Region Shannon H: Societal" = "#F8766D",
  "Sector Rae Index: Societal" = "#F8766D",
  "Sector Shannon H: Societal" = "#F8766D",
  
  "Region Rae Index: Physical" = "#00BFC4",  # ggplot default blue
  "Region Shannon H: Physical" = "#00BFC4",
  "Sector Rae Index: Physical" = "#00BFC4",
  "Sector Shannon H: Physical" = "#00BFC4",
  
  "CSR: Community: Societal" = "#F8766D",
  "CSR: Environment: Societal" = "#F8766D",
  "CSR: Labor: Societal" = "#F8766D",
  "CSR: Human Rights: Societal" = "#F8766D",
  "UNGA: Societal" = "#F8766D",
  "Legitimation statements: Societal" = "#F8766D",
  
  "CSR: Community: Physical" = "#00BFC4",
  "CSR: Environment: Physical" = "#00BFC4",
  "CSR: Labor: Physical" = "#00BFC4",
  "CSR: Human Rights: Physical" = "#00BFC4",
  "UNGA: Physical" = "#00BFC4",
  "Legitimation statements: Physical" = "#00BFC4",
  
  "Research papers: Physical" = "#00BFC4",
  "Patents: Physical" = "#00BFC4",
  "Legitimation  statements: Physical" = "#00BFC4",
  
  "Research papers: Societal" = "#F8766D",
  "Patents: Societal" = "#F8766D",
  "Legitimation  statements: Societal" = "#F8766D",
  
  "R&D: Physical" = "#00BFC4",
  "Researchers: Physical" = "#00BFC4",
  "High-tech export: Physical" = "#00BFC4",
  "Uni-Industry collab.: Physical" = "#00BFC4",
  "Articles: Physical" = "#00BFC4",
  "Business GERD: Physical" = "#00BFC4",
  "Knowledge emp.: Physical" = "#00BFC4",
  
  "R&D: Societal" = "#F8766D",
  "Researchers: Societal" = "#F8766D",
  "High-tech export: Societal" = "#F8766D",
  "Uni-Industry collab.: Societal" = "#F8766D",
  "Articles: Societal" = "#F8766D",
  "Business GERD: Societal" = "#F8766D",
  "Knowledge emp.: Societal" = "#F8766D"
)


coefplot %>%
  tidyr::drop_na(term) %>%
  mutate(model = factor(model, levels = c("Technocratic input legitimation", "Technocratic output legitimation",
                                          "Democratic input legitimation", "Democratic output legitimation"))) %>%
  mutate(group = standard) %>%
  ggplot(aes(estimate, group, group = Var, color = Var,
             xmin = conf.low, xmax = conf.high)) +
  geom_point(position = position_dodge(width = 1), size = 2.5) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), 
                 position = position_dodge(width = 1), size = 0.5) +
  geom_text(aes(label = term), position = position_dodge(width = 1),
            vjust = -0.7, size = 12) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.4) +
  geom_hline(yintercept = 1.5, lty = 2, alpha = 0.4) +
  xlim(-0.42, 0.42) +
  facet_wrap(~ model, scales = "free_x") +
  scale_colour_manual(values = colors) +
  xlab("Standardized coefficient") +
  ylab("") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 80))

