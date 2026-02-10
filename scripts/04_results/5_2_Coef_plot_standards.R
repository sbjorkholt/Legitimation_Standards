
# NOTE: Set working directory to the repository root before running this script.

# source("scripts/02_data_preparation/2_Make_data.R")
library(modelsummary)
options(modelsummary_factory_default = 'kableExtra')

source("scripts/03_models/3_1_Models_input_technocratic.R")
source("scripts/03_models/3_2_Models_input_democratic.R")
source("scripts/03_models/4_1_Models_output_technocratic.R")
source("scripts/03_models/4_2_Models_output_democratic.R")

library(tidyverse)
library(forcats)

coefplot <- coef_plot_input_dem %>%
  bind_rows(coef_plot_output_dem) %>%
  bind_rows(coef_plot_output_tech) %>%
  bind_rows(coef_plot_input_tech) %>%
  mutate(standard = str_to_sentence(standard)) %>%
  mutate(Var = str_c(dv, ": ", standard))

saveRDS(coefplot, file = "./Data/coefplot.rds")

# # Define the colors
# colors <- c("Region Rae Index: Societal" = "#1b7837",  
#             "Region Shannon H: Societal" = "#1b7837",  
#             "Sector Rae Index: Societal" = "#1b7837",      # 5aae61  
#             "Sector Shannon H: Societal" = "#1b7837", 
#             "Region Rae Index: Physical" = "#1b3a78",  
#             "Region Shannon H: Physical" = "#1b3a78",  
#             "Sector Rae Index: Physical" = "#1b3a78",     # 5a7eae   
#             "Sector Shannon H: Physical" = "#1b3a78", 
#             
#             "CSR: Community: Societal" = "#d7a527",            
#             "CSR: Environment: Societal" = "#d7a527",          
#             "CSR: Labor: Societal" = "#d7a527",                
#             "CSR: Human Rights: Societal" = "#d7a527",  # 1b3a78      
#             "UNGA: Societal" = "#d7a527",
#             "Legitimation statements: Societal" = "#d7a527",
#             "CSR: Community: Physical" = "#762a83",            
#             "CSR: Environment: Physical" = "#762a83",          
#             "CSR: Labor: Physical" = "#762a83",                
#             "CSR: Human Rights: Physical" = "#762a83",        
#             "UNGA: Physical" = "#762a83", # d73027
#             "Legitimation statements: Physical" = "#762a83", # 4C8FB5
#             
#             "Research papers: Physical" = "#4C8FB5", # 40E0D0
#             "Patents: Physical" = "#4C8FB5",
#             "Legitimation  statements: Physical" = "#4C8FB5",
#             "Research papers: Societal" = "#6b8e23", # FFD700 
#             "Patents: Societal" = "#6b8e23",
#             "Legitimation  statements: Societal" = "#6b8e23",
#             
#             "R&D: Physical" = "#007BFF",
#             "Researchers: Physical" = "#007BFF",
#             "High-tech export: Physical" = "#007BFF",
#             "Uni-Industry collab.: Physical" = "#007BFF",
#             "Articles: Physical" = "#007BFF",
#             "Business GERD: Physical" = "#007BFF",
#             "Knowledge emp.: Physical" = "#007BFF",
#             "R&D: Societal" = "#FF7B00",
#             "Researchers: Societal" = "#FF7B00",
#             "High-tech export: Societal" = "#FF7B00",
#             "Uni-Industry collab.: Societal" = "#FF7B00",
#             "Articles: Societal" = "#FF7B00",
#             "Business GERD: Societal" = "#FF7B00",
#             "Knowledge emp.: Societal" = "#FF7B00")

# Define the colors
colors <- c(
  "Region Rae Index: Societal" = "#FDAA9D",  # lighter
  "Region Shannon H: Societal" = "#F8766D",
  "Sector Rae Index: Societal" = "#FDAA9D",  # lighter
  "Sector Shannon H: Societal" = "#FDAA9D",
  
  "Region Rae Index: Physical" = "#66D9DC",  # lighter
  "Region Shannon H: Physical" = "#00BFC4",
  "Sector Rae Index: Physical" = "#66D9DC",  # lighter
  "Sector Shannon H: Physical" = "#66D9DC",
  
  "CSR: Community: Societal" = "#FDAA9D",
  "CSR: Environment: Societal" = "#FDAA9D",
  "CSR: Labor: Societal" = "#FDAA9D",
  "CSR: Human Rights: Societal" = "#FDAA9D",
  "UNGA: Societal" = "#FDAA9D",
  "Legitimation statements: Societal" = "#F8766D",
  
  "CSR: Community: Physical" = "#66D9DC",
  "CSR: Environment: Physical" = "#66D9DC",
  "CSR: Labor: Physical" = "#66D9DC",
  "CSR: Human Rights: Physical" = "#66D9DC",
  "UNGA: Physical" = "#66D9DC",
  "Legitimation statements: Physical" = "#00BFC4",

  "Research papers: Physical" = "#66D9DC",
  "Patents: Physical" = "#66D9DC",
  "Legitimation  statements: Physical" = "#00BFC4",
  
  "Research papers: Societal" = "#FDAA9D",
  "Patents: Societal" = "#FDAA9D",
  "Legitimation  statements: Societal" = "#F8766D",
  
  "Researchers: Physical" = "#00BFC4",
  "Business GERD: Physical" = "#66D9DC",
  "R&D: Physical" = "#66D9DC",
  "High-tech export: Physical" = "#66D9DC",
  "Uni-Industry collab.: Physical" = "#66D9DC",
  "Articles: Physical" = "#66D9DC",
  "Knowledge emp.: Physical" = "#66D9DC",
  
  "Researchers: Societal" = "#F8766D",
  "Business GERD: Societal" = "#FDAA9D",
  "R&D: Societal" = "#FDAA9D",
  "High-tech export: Societal" = "#FDAA9D",
  "Uni-Industry collab.: Societal" = "#FDAA9D",
  "Articles: Societal" = "#FDAA9D",
  "Knowledge emp.: Societal" = "#FDAA9D"
)

darkness_rank <- c(
  "Region Rae Index: Societal" = 1,   # lighter (#FDAA9D)
  "Region Shannon H: Societal" = 2,   # darker (#F8766D)
  "Sector Rae Index: Societal" = 1,   # lighter
  "Sector Shannon H: Societal" = 1,   # darker
  
  "Region Rae Index: Physical" = 1,   # lighter (#66D9DC)
  "Region Shannon H: Physical" = 2,   # darker (#00BFC4)
  "Sector Rae Index: Physical" = 1,   # lighter
  "Sector Shannon H: Physical" = 1,   # darker
  
  "CSR: Community: Societal" = 1,     # lighter (#FDAA9D)
  "CSR: Environment: Societal" = 1,
  "CSR: Labor: Societal" = 1,
  "CSR: Human Rights: Societal" = 1,
  "UNGA: Societal" = 1,
  "Legitimation statements: Societal" = 2,  # darker (#F8766D)
  
  "CSR: Community: Physical" = 1,     # lighter (#66D9DC)
  "CSR: Environment: Physical" = 1,
  "CSR: Labor: Physical" = 1,
  "CSR: Human Rights: Physical" = 1,
  "UNGA: Physical" = 1,
  "Legitimation statements: Physical" = 2,  # darker (#00BFC4)
  
  "Research papers: Physical" = 1,    # lighter (#66D9DC)
  "Patents: Physical" = 1,
  "Legitimation  statements: Physical" = 2, # darker (#00BFC4)
  
  "Research papers: Societal" = 1,    # lighter (#FDAA9D)
  "Patents: Societal" = 1,
  "Legitimation  statements: Societal" = 2, # darker (#F8766D)
  
  "Researchers: Physical" = 2,         # darker (#00BFC4)
  "Business GERD: Physical" = 1,
  "R&D: Physical" = 1,                 # lighter (#66D9DC)
  "High-tech export: Physical" = 1,
  "Uni-Industry collab.: Physical" = 1,
  "Articles: Physical" = 1,
  "Knowledge emp.: Physical" = 1,
  
  "Researchers: Societal" = 2,         # darker (#F8766D)
  "Business GERD: Societal" = 1,
  "R&D: Societal" = 1,                 # lighter (#FDAA9D)
  "High-tech export: Societal" = 1,
  "Uni-Industry collab.: Societal" = 1,
  "Articles: Societal" = 1,
  "Knowledge emp.: Societal" = 1
)

coefplot %>%
  tidyr::drop_na(term) %>%
  mutate(model = factor(model, levels = c("Technocratic input legitimation", "Technocratic output legitimation",
                                          "Democratic input legitimation", "Democratic output legitimation"))) %>%
  mutate(group = standard) %>%
  mutate(Var = fct_reorder(Var, darkness_rank[as.character(Var)])) %>%
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

ggsave("./Figures/coef_plot_new_fourtable.pdf", height = 33, width = 35)

