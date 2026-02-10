
#### DATA GENERATION FOR OUTPUT LEGITIMATION MODELS

## Packages and data 

pacman::p_load(dplyr, readr, stringr, tidytext, quanteda, SnowballC, tibble, ggplot2, purrr, fixest, tidyr, modelsummary, kableExtra)
options(modelsummary_factory_default = 'kableExtra')

standards3 <- read_rds("../../../Legitimacy/Data/standards3.rds") 
newsdata <- read_rds("../../../Legitimacy/Data/news.rds")

stopwords <- tibble(word = c(stop_words$word, 
                             c("iso.org", "iso", "standards", "copyright", "international",
                               "iso's", "standard", "including", "iso’s", "isos")))

symbols <- str_c("\\©|\\!|\\@|\\#|\\$|\\%|\\^|\\&|\\*|\\(|\\)|\\{|\\}|\\-|\\=|\\_|\\+|\\:|\\|\\<|\\>|\\?|\\,|\\.|\\/|\\;|\\'|\\[|\\]|\\-=")

## Data generation

news_text <- newsdata %>%
  mutate(text = str_squish(text)) %>%
  mutate(year = as.numeric(substr(date, 1, 4))) %>%
  filter(!name %in% c("Improving energy efficiency with ISO 50001 (Youtube)",
                      "IBM leverages ISO 14001 for sustainable business"))

standard_text <- standards3 %>%
  select(stdno, name, year, physical, societal, sector, status, edition, pages, committee, title) %>%
  mutate(edition = as.numeric(edition),
         pages = as.numeric(pages)) %>%
  rename(year_standard = year)

standard_news_text <- news_text %>%
  left_join(standard_text, by = join_by("name"), relationship = "many-to-many") %>% # One standard can match many news articles
  distinct(date, name, text, year, .keep_all = TRUE) %>%
  drop_na(physical, societal) 

# Cleaning out non-relevant text and textual duplicates

standard_news_sentences <- standard_news_text %>%
  unnest_sentences(input = text, output = text) %>% 
  group_by(rowid, name, stdno, year, date, committee, societal, physical) %>%
  distinct(text, .keep_all = TRUE) %>%
  filter(!str_detect(text, "inside scoop on standards"),
         !str_detect(text, "media kit"),
         !str_detect(text, "any use, including reproduction"),
         !str_detect(text, "all copyright requests"),
         !str_detect(text, "developed by iso technical committee"),
         !str_detect(text, "all rights reserved"),
         !str_detect(text, "conditions of copyright"),
         !str_detect(text, "national iso member or the iso store"),
         !str_detect(text, "almost done!"),
         !str_detect(text, "please see our privacy note"),
         !str_detect(text, "confirm your subscription"),
         !str_detect(text, "check your spam folder"),
         !str_detect(text, "please contact us"),
         !str_detect(text, "to learn how your data will be used"),
         !str_detect(text, "joining the iso subscriber"),
         !str_detect(text, "ensuring that our website is accessible"),
         !str_detect(text, "本文已由我"),
         !str_detect(text, "se puede descargar")) %>%
  ungroup() %>% 
  rowid_to_column(var = "sentence_id")

# source("./Scripts/1_3_GPT_coding_legitimacy.R")

legitimacy_coding_sen <- read_rds("../../../Legitimacy/Data/legitimation_output6.rds") %>%
  mutate(type_text = ifelse(type_code == 0 & is.na(type_text), "no", type_text))# %>%
  #filter(rowid != "1603")

## Validation

# set.seed(60)
#
# valid_df <- legitimacy_coding_sen %>%
#   sample_frac(0.05) %>%
#   select(sentence_id, rowid, text)
#
# # write_excel_csv(valid_df, file = "../../../Legitimacy/Data/validation_df.csv")
#
# valid_check <- read.csv("../../../Legitimacy/Data/validation_df2.csv") %>%
#   na.omit()
#
# valid_check %>%
#   left_join(legitimacy_coding_sen %>%
#               select(rowid, sentence_id, type_code, type_text),
#             by = join_by(sentence_id, rowid)) %>%
#   mutate(type_text_manual = ifelse(Technocratic.legitimation == 1, "technocratic",
#                                    ifelse(Democratic.legitimation == 1, "democratic",
#                                           "no"))) %>%
#   select(sentence_id, type_text_manual, type_text) %>%
#   #filter(type_text == "democratic") %>%
#   #filter(type_text == "technocratic") %>%
#   mutate(different = ifelse(type_text != type_text_manual, 1, 0)) -> test
#   #summarise(mean_overlap = mean(overlap, na.rm = TRUE))
#
# valid_check %>%
#   inner_join(legitimacy_coding_sen %>%
#               select(sentence_id, type_code, type_text),
#             by = join_by(sentence_id)) %>%
#   mutate(type_text_manual = ifelse(Technocratic.legitimation == 1, "TECHNOCRATIC",
#                                    ifelse(Democratic.legitimation == 1, "DEMOCRATIC",
#                                           ifelse(Legitimation.statement == 0, "NO",
#                                                  NA)))) %>%
#   drop_na(type_text_manual) %>%
#   group_by(type_text_manual, type_text) %>%
#   count() %>%
#   #gather(type_text, type_text_manual, key = "codetype", value = "value") %>%
#   mutate(type = paste0(type_text_manual, type_text)) %>%
#   mutate(coding = ifelse(type_text == "democratic" & type_text_manual == "democratic", "democratic",
#                              ifelse(type_text == "technocratic" & type_text_manual == "technocratic", "technocratic",
#                                     ifelse(type_text == "no" & type_text_manual == "no", "no", "unequal")))) %>%
#   group_by(coding) %>%
#  # summarise(sum = sum(n))
#  ggplot(aes(type, n, fill = type)) +
#  coord_flip() +
#  geom_bar(stat = "identity", position = "dodge")
#
# check <- valid_check %>%
#   inner_join(legitimacy_coding_sen %>%
#                 select(sentence_id, type_code, type_text),
#              by = join_by(sentence_id)) %>%
#   mutate(type_text_manual = ifelse(Technocratic.legitimation == 1, "technocratic",
#                                    ifelse(Democratic.legitimation == 1, "democratic",
#                                           ifelse(Legitimation.statement == 0, "no",
#                                                  NA)))) %>%
#   select(type_text, type_text_manual) %>%
#   mutate(type_text = as.factor(type_text),
#          type_text_manual = as.factor(type_text_manual))
#
# psych::cohen.kappa(check)
# caret::confusionMatrix(check$type_text, check$type_text_manual)

#check %>%
#  spread(type_text_manual, n) %>%
#  mutate(share_dem = democratic/(democratic+no+technocratic),
#         share_tech = technocratic/(democratic+no+technocratic),
#         share_no = no/(democratic+no+technocratic))

legitimacy_coding_sen %>%
  group_by(type_text) %>%
  count() %>%
  spread(type_text, n) %>%
  mutate(share_dem = democratic/(democratic+no+technocratic),
         share_tech = technocratic/(democratic+no+technocratic),
         share_no = no/(democratic+no+technocratic))

fig1 <- legitimacy_coding_sen %>%
  mutate(type_text = ifelse(type_text == "no", "None",
                            ifelse(type_text == "democratic", "Democratic",
                                   ifelse(type_text == "technocratic", "Technocratic", type_text)))) %>%
  group_by(type_text) %>%
  count() %>%
  ggplot(aes(type_text, n)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Number of sentences") +
  theme_bw()  +
  theme(legend.position = "none",
        text = element_text(size = 30))

#fig1
#ggsave("./Figures/legitimation_sentences1_2.pdf", height = 10, width = 10)

# ISO 14034:2016
# ISO/IEC 38500:2008
# ISO/IEC 20546:2019

legitimacy_coding <- legitimacy_coding_sen %>%
  mutate(month = as.factor(substr(date, 6, 7))) %>% 
  group_by(committee, year, rowid, name, type_text, month, physical, societal) %>%
  count() %>%
  ungroup() %>%
  spread(type_text, n) %>%
  mutate(democratic = replace_na(democratic, 0),
         technocratic = replace_na(technocratic, 0),
         no = replace_na(no, 0)) %>%
  mutate(share_democratic = democratic/(democratic+technocratic+no),
         share_technocratic = technocratic/(democratic+technocratic+no),
         share_no = no/(democratic+technocratic+no))

legitimacy_coding %>%
  summarise(mean_tech = mean(share_technocratic),
            mean_dem = mean(share_democratic),
            mean_no = mean(share_no))

fig2 <- legitimacy_coding %>%
  gather(share_democratic, share_technocratic,
         key = "Legitimation", value = "Share") %>%
  mutate(`Legitimation strategy` = ifelse(Legitimation == "share_democratic", "Democratic", 
                                          ifelse(Legitimation == "share_technocratic", "Technocratic",
                                                 Legitimation))) %>%
  filter(year <= 2022) %>%
  ggplot(aes(year, Share, color = `Legitimation strategy`, group = `Legitimation strategy`)) + 
  geom_point() + 
  labs(x = "") +
  geom_smooth() + 
  theme_bw() + 
  theme(legend.position = "bottom",
        text = element_text(size = 30))

#fig2
#ggsave("./Figures/legitimation_sentences2_2.pdf", height = 10, width = 10)

standard_news_text <- standard_news_sentences %>%
  group_by(rowid, name, stdno, year, date, committee, societal, physical) %>%
  summarise(text = paste(text, collapse = " "), .groups = 'drop') %>%
  mutate(text = str_remove_all(text, "[0-9]+")) %>%
  mutate(text = str_remove_all(text, "[[:punct:]]")) %>%
  mutate(text = str_remove_all(text, symbols)) %>%
  distinct(name, stdno, year, date, .keep_all = TRUE) %>%
  mutate(text = str_squish(text)) 

standard_news_text%>%
  count(societal)

rm(standards3, standard_text, newsdata, fig1, fig2)

