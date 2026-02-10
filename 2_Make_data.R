
library(DBI)
library(RSQLite)
library(tidyverse)

con <- dbConnect(RSQLite::SQLite(), "../../../Legitimacy/Data/iso_standards.sqlite")

standards <- dbReadTable(con, "standards_status")

tc_creation <- dbReadTable(con, "historical_tc_creation") %>%
  mutate(year = as.numeric(year)) %>%
  na.omit()

tc_creation_merge <- tc_creation %>%
  mutate(committee = str_remove(committee, "ISO/IEC"),
         committee = str_remove(committee, "ISO/"),
         committee = str_squish(committee)) %>%
  mutate(year = as.numeric(year))

dbDisconnect(con)

abstracts <- standards %>%
  rename(text = abstract) %>%
  mutate(year = str_extract(publication_date, "[0-9]{4}")) %>%
  select(stdno, text, year, ics_number) %>%
  filter(text != "") %>%
  na.omit() %>%
  distinct(stdno, .keep_all = TRUE)

completions1 <- lapply(list.files("../../../Legitimacy/Data/standard_type/", full.names = TRUE, pattern = ".txt"), read.table) %>% # ./../../Legitimacy
  bind_rows()
completions2 <- lapply(list.files(paste0("./Data/standards2/"), full.names = TRUE, pattern = ".txt"), read.table) %>%
  bind_rows()

ids1 <- as_tibble(completions1, .name_repair = "universal") %>%
  mutate(stdno = str_extract(list.files("../../../Legitimacy/Data/standard_type/", ".txt"), "[0-9]+")) %>% # ./../../Legitimacy
  pull(stdno)
ids2 <- str_remove(str_extract(list.files(paste0("./Data/standards2/"), full.names = TRUE, pattern = ".txt"), "[0-9]+\\.txt"), ".txt")

std_df1 <- as_tibble(completions1, .name_repair = "universal") %>%
  mutate(stdno = as.numeric(ids1)) %>%
  mutate(type_code = str_extract(completion, "(\\|)?(\\s*)?\\([0-9]+\\)"),
         type_text = str_extract(completion, "(\\|)(\\s*)?[a-z]+(\\s*)?(\\|)")) %>%
  mutate(type_code = str_extract(type_code, "[0-9]{1}"),
         type_text = str_extract(type_text, "[a-z]+")) %>%
  mutate(type_text = ifelse(type_code == 1, "physical",
                            ifelse(type_code == 2, "societal", type_code))) %>%
  mutate(type_code = ifelse(str_detect(completion, "physical"), 1, type_code),
         type_code = ifelse(str_detect(completion, "societal"), 2, type_code)) %>%
  mutate(type_text = ifelse(type_code == 1, "physical",
                            ifelse(type_code == 2, "societal", type_code))) %>%
  mutate(stdno = as.character(stdno)) %>%
  select(stdno, type_code, type_text)

std_df2 <- as_tibble(completions2, .name_repair = "universal") %>%
  mutate(stdno = ids2) %>%
  select(stdno, 
         type_text = V3,
         type_code = V1) %>%
  mutate(type_code = str_extract(type_code, "[0-9]+")) %>%
  distinct(.keep_all = TRUE) 

std_df1 %>%
  left_join(std_df2 %>% rename(type_code2 = type_code, type_text2 = type_text)) %>%
  mutate(like = ifelse(type_text == type_text2, 1, 0)) %>%
  summarise(mean(like))

standards2 <- std_df2 %>%
  left_join(standards %>% 
              distinct(stdno, name, year, title, committee, .keep_all = TRUE) %>% 
              mutate(year = as.numeric(year))) %>%
  mutate(committee = str_replace(committee, "(\\d+) /", "\\1/")) %>%
  mutate(committee = str_remove(committee, "ISO/IEC")) %>%
  mutate(committee = str_remove(committee, "ISO/")) %>%
  mutate(committee = str_squish(committee)) %>%
  left_join(tc_creation_merge %>% select(-year, -title) %>% distinct(committee, .keep_all = TRUE), by = join_by(committee)) %>%
  filter(committee != "") %>%
  # mutate(social = ifelse(sector %in% c("Sustainability and environment",
  #                                      "Business management and innovation",
  #                                      "Services",
  #                                      "Security, safety and risk"), "Social", "Technical")) %>%
  #group_by(type_text, year) %>%
  #add_count() %>%
  #group_by(year) %>%
  #add_count() %>%
  #ungroup() %>%
  #unique() %>%
  #mutate(share = (n/nn)*100) %>%
  #select(year, committee, type_text, n, nn) %>%
  group_by(year, committee, type_text) %>% 
  count() %>%
  ungroup() %>%
  group_by(committee, type_text) %>% 
  mutate(n = cumsum(n)) %>%
  ungroup() %>%
  spread(type_text, n) %>%
  tidyr::complete(committee = committee, year = 1960:2022) %>%
  group_by(committee) %>%
  fill(physical, .direction = "down") %>%
  fill(societal, .direction = "down") %>%
  replace_na(list(physical = 0)) %>%
  replace_na(list(societal = 0)) %>%
  ungroup() %>%
  filter(year > 1970) %>%
  filter(year <= 2022) %>%
  unique()

saveRDS(standards2, file = "../../../Legitimacy/Data/standards2.rds")

standards3 <- std_df2 %>%
  left_join(standards %>%
              distinct(stdno, name, year, title, committee, .keep_all = TRUE) %>% 
              mutate(year = as.numeric(year))) %>%
  #inner_join(standards %>% select(-title)) %>%
  mutate(committee = str_replace(committee, "(\\d+) /", "\\1/")) %>%
  mutate(committee = str_remove(committee, "ISO/IEC")) %>%
  mutate(committee = str_remove(committee, "ISO/")) %>%
  mutate(committee = str_squish(committee)) %>%
  left_join(tc_creation_merge %>% select(-year, -title) %>% distinct(committee, .keep_all = TRUE), by = join_by(committee)) %>%
  filter(committee != "") %>%
  # inner_join(tc_creation_merge %>% select(-year), by = join_by(committee), relationship = "many-to-many") %>%
  # mutate(social = ifelse(sector %in% c("Sustainability and environment",
  #                                      "Business management and innovation",
  #                                      "Services",
  #                                      "Security, safety and risk"), "Social", "Technical"))  %>%
  distinct(.keep_all = TRUE) %>%
  spread(type_text, type_code) %>%
  mutate(physical = as.numeric(physical),
         societal = as.numeric(societal)) %>%
  replace_na(list(physical = 0)) %>%
  replace_na(list(societal = 0)) %>%
  mutate(societal = ifelse(societal == 2, 1, societal)) %>%
  ungroup() %>%
  filter(year > 1970) %>%
  filter(year <= 2022) %>%
  unique() %>%
  mutate(year = as.numeric(year))

# set.seed(1930)
# 
# val_std <- std_df2 %>%
#   left_join(abstracts) %>% 
#   group_by(type_text) %>%
#   slice_sample(n = 50, replace = FALSE) %>% 
#   ungroup() %>%
#   select(-type_text, -type_code) %>%
#   slice_sample(prop = 1)
# 
# write_csv(val_std, file = "./Data/standards_validation.csv")
# 
read_csv("./Data/standards_validation.csv") %>%
  mutate(stdno = as.character(stdno),
         year = as.character(year)) %>%
  left_join(std_df2) %>%
  mutate(like =ifelse(coding == type_text, 1, 0)) %>%
  #group_by(type_text) %>%
  summarise(mean(like))
 
saveRDS(standards3, file = "../../../Legitimacy/Data/standards3.rds")

rm(abstracts, std_df, tc_creation, tc_creation_merge, completions)

dbDisconnect(con)
