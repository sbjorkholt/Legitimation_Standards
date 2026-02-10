
library(DBI)
library(RSQLite)
library(tidyverse)

con <- dbConnect(RSQLite::SQLite(), "../../../ISO-standards/iso_standards.sqlite")

pallette <- c("skyblue", "salmon")

standards <- dbReadTable(con, "standards_status")

abstracts <- standards %>%
  rename(text = abstract) %>%
  mutate(year = str_extract(publication_date, "[0-9]{4}")) %>%
  select(stdno, text, year, ics_number) %>%
  filter(text != "") %>%
  na.omit() %>%
  distinct(stdno, .keep_all = TRUE)

# completions <- lapply(list.files("../../../Legitimacy/Data/standard_type/", full.names = TRUE, pattern = ".txt"), read.table) %>% 
#   bind_rows()

completions2 <- lapply(list.files(paste0("./Data/standards2/"), full.names = TRUE, pattern = ".txt"), read.table) %>%
  bind_rows()

# ids <- as_tibble(completions, .name_repair = "universal") %>% 
#   mutate(stdno = str_extract(list.files("../../../Legitimacy/Data/standard_type/", ".txt"), "[0-9]+")) %>%
#   pull(stdno)

ids2 <- str_remove(str_extract(list.files(paste0("./Data/standards2/"), full.names = TRUE, pattern = ".txt"), "[0-9]+\\.txt"), ".txt")

# std_df <- as_tibble(completions, .name_repair = "universal") %>%
#   mutate(stdno = as.numeric(ids2)) %>%
#   mutate(type_code = str_extract(completion, "(\\|)?(\\s*)?\\([0-9]+\\)"),
#          type_text = str_extract(completion, "(\\|)(\\s*)?[a-z]+(\\s*)?(\\|)")) %>%
#   mutate(type_code = str_extract(type_code, "[0-9]{1}"),
#          type_text = str_extract(type_text, "[a-z]+")) %>%
#   mutate(type_text = ifelse(type_code == 1, "physical",
#                             ifelse(type_code == 2, "societal", type_code))) %>%
#   mutate(type_code = ifelse(str_detect(completion, "physical"), 1, type_code),
#          type_code = ifelse(str_detect(completion, "societal"), 2, type_code)) %>%
#   mutate(type_text = ifelse(type_code == 1, "physical",
#                             ifelse(type_code == 2, "societal", type_code))) %>%
#   mutate(stdno = as.character(stdno)) %>%
#   select(stdno, type_code, type_text) 

std_df <- as_tibble(completions2, .name_repair = "universal") %>%
  mutate(stdno = ids2) %>%
  select(stdno, 
         type_text = V3,
         type_code = V1) %>%
  mutate(type_code = str_extract(type_code, "[0-9]+")) %>%
  distinct(.keep_all = TRUE) 

p1 <- std_df %>%
  left_join(abstracts) %>%
  filter(year <= 2022) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(type_text, year) %>%
  add_count() %>%
  group_by(year) %>%
  add_count() %>%
  unique() %>%
  mutate(share = (n/nn)*100) %>%
  filter(share != 100) %>%
  filter(type_text == "physical") %>%
  ungroup() %>%
  tidyr::complete(type_text, year = 1960:1975, fill = list(share = 100)) %>%
  mutate(type_text = str_to_sentence(type_text)) %>%
  rename(` ` = type_text) %>%
  ggplot(aes(year, share, color = ` `)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(se = TRUE, alpha = .2) + 
  scale_color_manual(values = pallette[1]) +
  labs(y = "", x = "") +
  xlim(1960, 2025) +
  ylim(0,100) +
  ggtitle("Physical") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 20)) 

p2 <- std_df %>%
  left_join(abstracts) %>%
  filter(year <= 2022) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(type_text, year) %>%
  add_count() %>%
  group_by(year) %>%
  add_count() %>%
  unique() %>%
  mutate(share = (n/nn)*100) %>%
  filter(type_text == "societal") %>%
  ungroup() %>%
  tidyr::complete(type_text, year = 1960:1975, fill = list(share = 0)) %>%
  group_by(year) %>%
  mutate(type_text = str_to_sentence(type_text)) %>%
  rename(` ` = type_text) %>%
  ggplot(aes(year, share, color = ` `)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(se = TRUE, alpha = .2) + 
  scale_color_manual(values = pallette[2]) +
  labs(y = "", x = "") +
  xlim(1960, 2025) +
  ylim(0,100) +
  ggtitle("Societal") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size = 20)) 

cowplot::plot_grid(p1, p2, ncol = 2)

ggsave("./Figures/figure3.pdf", width = 12, height = 8)

dbDisconnect(con)
