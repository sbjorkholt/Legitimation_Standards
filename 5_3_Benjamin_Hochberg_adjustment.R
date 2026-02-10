
setwd("C:/Users/solvebjo/UiO Dropbox/Solveig Bjørkholt/Apps/Overleaf/Legitimacy_standards")

# source("./Scripts/ISO/2_Make_data.R")
library(modelsummary)
options(modelsummary_factory_default = 'kableExtra')

source("./Scripts/ISO/3_1_Models_input_technocratic.R")
source("./Scripts/ISO/3_2_Models_input_democratic.R")
source("./Scripts/ISO/4_1_Models_output_technocratic.R")
source("./Scripts/ISO/4_2_Models_output_democratic.R")

# Combine all p-values into one vector
all_pvals <- c(unlist(pvalues_techin), 
               unlist(pvalues_demin),
               unlist(pvalues_techout),
               unlist(pvalues_demout)) 

#all_pvals <- all_pvals[!grepl("month", names(all_pvals))]

# Apply Benjamini-Hochberg (FDR) correction across all
p_adj_all <- p.adjust(all_pvals, method = "BH")

p_adj_all
sig_flags <- p_adj_all < 0.05
sum(sig_flags)  


# Define the term names (your 40 coefficient labels)
terms <- c(
  "R&D: Physical",
  "Researchers: Physical",
  "High-tech export: Physical",
  "Uni-Industry collab.: Physical",
  "Articles: Physical",
  "Business GERD: Physical",
  "Knowledge emp.: Physical",
  
  "Researchers: Societal",
  "R&D: Societal",
  "High-tech export: Societal",
  "Uni-Industry collab.: Societal",
  "Articles: Societal",
  "Business GERD: Societal",
  "Knowledge emp.: Societal",
  
  "Region Rae Index: Societal",
  "Region Shannon H: Societal",
  "Sector Rae Index: Societal",
  "Sector Shannon H: Societal",
  
  "Region Rae Index: Physical",
  "Region Shannon H: Physical",
  "Sector Rae Index: Physical",
  "Sector Shannon H: Physical",
  
  "Research papers: Physical",
  "Patents: Physical",
  "Legitimation statements: Physical",
  
  "Research papers: Societal",
  "Patents: Societal",
  "Legitimation statements: Societal",
  
  "CSR: Community: Societal",
  "CSR: Environment: Societal",
  "CSR: Labor: Societal",
  "CSR: Human Rights: Societal",
  "UNGA: Societal",
  "Legitimation statements: Societal",
  
  "CSR: Community: Physical",
  "CSR: Environment: Physical",
  "CSR: Labor: Physical",
  "CSR: Human Rights: Physical",
  "UNGA: Physical",
  "Legitimation statements: Physical"
)

# Combine into data frame
results_df <- data.frame(
  Coefficient = terms,
  `Original p-value` = all_pvals,
  `Adjusted p-value` = p_adj_all,
  `Significance` = ifelse(p_adj_all < 0.05, TRUE, FALSE)
)

# Check result
print(results_df)

results_df %>%
  mutate(`Significance` = ifelse(`Significance`, "\\textbf{TRUE}", "FALSE")) %>%  # optional: bold significant
  kable(format = "latex", 
        escape = FALSE, 
        booktabs = TRUE, 
        digits = 4) %>%
        #caption = "Regression p-values for Physical and Societal coefficients") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

