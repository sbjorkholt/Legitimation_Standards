
library(tidyverse)
library(kableExtra)

source("./Scripts/ISO/2_Make_data.R")

source("./Scripts/ISO/3_1_Models_input_technocratic.R")
source("./Scripts/ISO/3_2_Models_input_democratic.R")
source("./Scripts/ISO/4_1_Models_output_technocratic.R")
source("./Scripts/ISO/4_2_Models_output_democratic.R")

#### DEMOCRATIC INPUT LEGITIMATION ####

frac_na <- participation_dat_merged_frac %>% summarise(Mean = mean(frac),
                                             Median = median(frac),
                                             Max = max(frac),
                                             Min = min(frac)) %>%
  mutate(across(1:4, round, 3))

sh_na <- participation_dat_merged_sh %>% summarise(Mean = mean(shannon_index), 
                                           Median = median(shannon_index),
                                           Max = max(shannon_index),
                                           Min = min(shannon_index)) %>%
  mutate(across(1:4, round, 3))

frac_li <- liaison_dat_merged_frac %>% summarise(Mean = mean(frac),
                                             Median = median(frac),
                                             Max = max(frac),
                                             Min = min(frac)) %>%
  mutate(across(1:4, round, 3))

sh_li <- liaison_dat_merged_sh %>% summarise(Mean = mean(shannon_index), 
                                           Median = median(shannon_index),
                                           Max = max(shannon_index),
                                           Min = min(shannon_index)) %>%
  mutate(across(1:4, round, 3))

descriptives1 <- bind_cols(frac_na %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "Rae Index "),
                           sh_na %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "Shannon H "),
                           frac_li %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "Rae Index"),
                           sh_li %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "Shannon H")) %>%
  select(-` ...3`, -` ...5`, -` ...7`) %>%
  rename(" " = ` ...1`) %>%
  kable() %>%
  kable_minimal("striped", font_size = 12) %>% 
  add_header_above(c(" " = 1, "Regional committee diversity" = 2, "Sector committee diversity" = 2), italic = TRUE) %>% 
  add_header_above(c(" " = 1, "Input legitimation" = 4), bold = TRUE) 

#save_kable(descriptives1, file = "./Tables/iso_descriptives1.tex")


#### TECHNOCRATIC INPUT LEGITIMATION ####

rndgdp <- tech_leg %>% summarise(Mean = mean(GDEXPRD, na.rm = TRUE),
                              Median = median(GDEXPRD, na.rm = TRUE),
                              Max = max(GDEXPRD, na.rm = TRUE),
                              Min = min(GDEXPRD, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))

researchers <- tech_leg %>% summarise(Mean = mean(RESEARCHER, na.rm = TRUE),
                                   Median = median(RESEARCHER, na.rm = TRUE),
                                   Max = max(RESEARCHER, na.rm = TRUE),
                                   Min = min(RESEARCHER, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))

hightechexp <- tech_leg %>% summarise(Mean = mean(hightechexp, na.rm = TRUE),
                                   Median = median(hightechexp, na.rm = TRUE),
                                   Max = max(hightechexp, na.rm = TRUE),
                                   Min = min(hightechexp, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))


unicollab <- tech_leg %>% summarise(Mean = mean(unicollab, na.rm = TRUE),
                                   Median = median(unicollab, na.rm = TRUE),
                                   Max = max(unicollab, na.rm = TRUE),
                                   Min = min(unicollab, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))


scienceart <- tech_leg %>% summarise(Mean = mean(scienceart, na.rm = TRUE),
                                   Median = median(scienceart, na.rm = TRUE),
                                   Max = max(scienceart, na.rm = TRUE),
                                   Min = min(scienceart, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))


businessgerd <- tech_leg %>% summarise(Mean = mean(businessgerd, na.rm = TRUE),
                                   Median = median(businessgerd, na.rm = TRUE),
                                   Max = max(businessgerd, na.rm = TRUE),
                                   Min = min(businessgerd, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))

knowledgeemp <- tech_leg %>% summarise(Mean = mean(knowledgeemp, na.rm = TRUE),
                                    Median = median(knowledgeemp, na.rm = TRUE),
                                    Max = max(knowledgeemp, na.rm = TRUE),
                                    Min = min(knowledgeemp, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))

descriptives3 <-  bind_cols(rndgdp %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "R&D % of GDP"),
                            researchers %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "Researchers per million people"),
                            hightechexp %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "High-tech exports, % total trade"),
                            unicollab %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "University-industry R&D collaboration"),
                            scienceart %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "Scientific and technical articles/bn PPP$ GDP"),
                            businessgerd %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "GERD performed by business, % GDP"),
                            knowledgeemp %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "Knowledge-intensive employment, %")) %>%
  select(-` ...3`, -` ...5`, -` ...7`, -` ...9`, -` ...11`, -` ...13`) %>%
  rename(" " = ` ...1`) %>%
  kable("latex") %>%
  kable_minimal("striped", font_size = 12) %>%
  add_header_above(c(" " = 1,  "Coverage: 2004-2021" = 2, "Coverage: 2013-2021" = 5), italic = TRUE) %>%
  add_header_above(c(" " = 1, "Technocratic input legitimation" = 7), bold = TRUE) #

#save_kable(descriptives3, file = "./Tables/iso_descriptives3.tex")


#### DEMOCRATIC AND TECHNOCRATIC OUTPUT LEGITIMATION ####

un <- output_dem_leg %>% summarise(Mean = mean(mean_share_unga, na.rm = TRUE),
                                 Median = median(mean_share_unga, na.rm = TRUE),
                                 Max = max(mean_share_unga, na.rm = TRUE),
                                 Min = min(mean_share_unga, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))

hum <- output_dem_leg %>% summarise(Mean = mean(mean_share_humanrights, na.rm = TRUE),
                                  Median = median(mean_share_humanrights, na.rm = TRUE),
                                  Max = max(mean_share_humanrights, na.rm = TRUE),
                                  Min = min(mean_share_humanrights, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))

emp <- output_dem_leg %>% summarise(Mean = mean(mean_share_employee, na.rm = TRUE),
                                  Median = median(mean_share_employee, na.rm = TRUE),
                                  Max = max(mean_share_employee, na.rm = TRUE),
                                  Min = min(mean_share_employee, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))

env <- output_dem_leg %>% summarise(Mean = mean(mean_share_environment, na.rm = TRUE),
                                  Median = median(mean_share_environment, na.rm = TRUE),
                                  Max = max(mean_share_environment, na.rm = TRUE),
                                  Min = min(mean_share_environment, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))

com <- output_dem_leg %>% summarise(Mean = mean(mean_share_community, na.rm = TRUE),
                                  Median = median(mean_share_community, na.rm = TRUE),
                                  Max = max(mean_share_community, na.rm = TRUE),
                                  Min = min(mean_share_community, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))

statement_dem <- legitimacy_coding %>% summarise(Mean = mean(share_democratic, na.rm = TRUE),
                                    Median = median(share_democratic, na.rm = TRUE),
                                    Max = max(share_democratic, na.rm = TRUE),
                                    Min = min(share_democratic, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))

research <- output_tech_leg %>% summarise(Mean = mean(mean_share_paper, na.rm = TRUE),
                                       Median = median(mean_share_paper, na.rm = TRUE),
                                       Max = max(mean_share_paper, na.rm = TRUE),
                                       Min = min(mean_share_paper, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))

patents <- output_tech_leg %>% summarise(Mean = mean(mean_share_patent, na.rm = TRUE),
                                      Median = median(mean_share_patent, na.rm = TRUE),
                                      Max = max(mean_share_patent, na.rm = TRUE),
                                      Min = min(mean_share_patent, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))

statement_tech <- legitimacy_coding %>% summarise(Mean = mean(share_technocratic, na.rm = TRUE),
                                           Median = median(share_technocratic, na.rm = TRUE),
                                           Max = max(share_technocratic, na.rm = TRUE),
                                           Min = min(share_technocratic, na.rm = TRUE)) %>%
  mutate(across(1:4, round, 3))

descriptives2 <- bind_cols(un %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "UNGA"),
                           hum %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "Human Rights"),
                           emp %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "Labor"),
                           env %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "Environment"),
                           com %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "Community and Social"),
                           statement_dem %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "Democratic"),
                           research %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "Research papers"),
                           patents %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "Patents"),
                           statement_tech %>% select(Mean, Median, Max, Min) %>% gather(key = " ", value = "Technocratic")) %>%
  select(-` ...3`, -` ...5`, -` ...7`, -` ...9`, -` ...11`, -` ...13`, -` ...15`, -` ...17`) %>%
  rename(" " = ` ...1`) %>%
  kable() %>% # "latex"
  kable_minimal("striped", font_size = 12) %>% 
  add_header_above(c(" " = 1,  "UNGA Dictionary" = 1, "CSR Dictionary" = 4, "Legitimation statements" = 1,
                     "Reseach papers" = 1, "Patents" = 1, "Legitimation statements" = 1), italic = TRUE) %>% 
  add_header_above(c(" " = 1, "Democratic output legitimation" = 6, "Technocratic output legitimation" = 3), bold = TRUE) #%>%

# save_kable(descriptives2, file = "./Tables/iso_descriptives2.tex")




  