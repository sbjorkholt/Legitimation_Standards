
# NOTE: Set working directory to the repository root before running this script.

# source("scripts/02_data_preparation/2_Make_data.R")
library(modelsummary)
options(modelsummary_factory_default = 'kableExtra')

#### INPUT ####

source("scripts/03_models/3_1_Models_input_technocratic.R")
source("scripts/03_models/3_2_Models_input_democratic.R")

cm <- c("physical" = "Physical",
        "societal" = "Societal")

mods <- list(mod2_techin_p, 
             mod2_techin_s, 
             mod2_demin_p_country,
             mod2_demin_s_country)

names(mods) <- c("Researchers", 
                 "Researchers", 
                 "Shannon H", 
                 "Shannon H") 

mods_input <- modelsummary(mods,
                       stars = TRUE,
                       gof_omit = 'AIC|BIC|Within|Std.Errors|RMSE|FE',
                       fmt = function(x) format(x, scientific = FALSE, digits = 5),
                       coef_map = cm,
                       format = "latex",
                       statistic = c("{std.error}"),
                       notes = c("Fixed effects: Year and committee",
                                 "Clustered standard errors by committee and year",
                                 "Coverage: 2004 - 2022")) 

mods_input %>%
  add_header_above(c(" " = 1, "Technocratic Input" = 2, "Democratic Input" = 2)) %>%
  kable_styling(font_size = 10, full_width = FALSE)

#rm(list = ls())

#### OUTPUT ####

source("scripts/03_models/4_1_Models_output_technocratic.R")
source("scripts/03_models/4_2_Models_output_democratic.R")

cm <- c("physical" = "Physical standards (per news piece)",
        "societal" = "Societal standards (per news piece)")

mods <- list(mod3_techout_p, mod3_techout_s, 
             mod6_demout_p, mod6_demout_s)

names(mods) <- c("Legitimation statements",
                 "Legitimation statements", 
                 "Legitimation statements", 
                 "Legitimation statements")

mods_input <- modelsummary(mods,
                           stars = TRUE,
                           gof_omit = 'AIC|BIC|Within|Std.Errors|RMSE|FE',
                           fmt = function(x) format(x, scientific = FALSE, digits = 2),
                           coef_map = cm,
                           format = "latex",
                           statistic = c("{std.error}"),
                           notes = c("Fixed effects: Year and committee",
                                     "Control variables: Month",
                                     "Clustered standard errors by committee and year",
                                     "Coverage: 2004 - 2022")) 

mods_input %>%
  add_header_above(c(" " = 1, "Technocratic Output" = 2, "Democratic Output" = 2)) %>%
  kable_styling(font_size = 10, full_width = FALSE)

#rm(list = ls())
