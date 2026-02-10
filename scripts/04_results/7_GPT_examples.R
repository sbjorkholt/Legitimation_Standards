
library(tidyverse)
library(DBI)
library(RSQLite)
library(kableExtra)


con <- dbConnect(RSQLite::SQLite(), "../../../ISO-standards/iso_standards.sqlite")

standards <- dbReadTable(con, "standards_status")

completions <- lapply(list.files("../../../Legitimacy/Data/standard_type/", full.names = TRUE, pattern = ".txt"), read.table) %>%
  bind_rows()

ids <- as_tibble(completions, .name_repair = "universal") %>% 
  mutate(stdno = str_extract(list.files("../../../Legitimacy/Data/standard_type/", ".txt"), "[0-9]+")) %>%
  pull(stdno)

dbDisconnect(con)

abstracts <- standards %>%
  rename(text = abstract) %>%
  mutate(year = str_extract(publication_date, "[0-9]{4}")) %>%
  select(stdno, name, text, year, ics_text) %>%
  filter(text != "") %>%
  na.omit() %>%
  distinct(stdno, .keep_all = TRUE)

std_examples <- as_tibble(completions, .name_repair = "universal") %>%
  mutate(stdno = as.numeric(ids)) %>%
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
  select(stdno, type_code, type_text) %>%
  inner_join(abstracts) %>%
  group_by(stdno) %>%
  select(-type_code) %>%
  mutate(Type_category = paste0(type_text, collapse = " ——————— ")) %>%
  unique() %>%
  rename(Abstract = text,
         ICS = ics_text,
         Year = year,
         Name = name,
         "Type category" = Type_category) %>%
  ungroup() %>%
  select(Name, Year, ICS, Abstract, `Type category`)

set.seed(103)

std_examples <- std_examples %>%
  group_by(`Type category`) %>%
  slice_sample(n = 10) %>%
  ungroup()

table <- kable(std_examples, "latex", booktabs = T, longtable = TRUE) %>%
  kable_styling(font_size = 8, latex_options = c("striped", "scale_down", "repeat_header")) %>%
  column_spec(1, width = "3em" ) %>%
  column_spec(2, width = "2em" ) %>%
  column_spec(3, width = "5em") %>%
  column_spec(4, width = "30em") %>%
  column_spec(5, width = "8em")

save_kable(table, file = "./Tables/gpt_examples.tex")

