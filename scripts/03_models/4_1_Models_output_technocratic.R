
#### 4.1 MODEL OUTPUT LEGITIMATION TECHNOCRATIC ####

pacman::p_load(dplyr, readr, stringr, tidytext, quanteda, SnowballC, wordcloud2, purrr, fixest, tidyr, modelsummary, kableExtra)
options(modelsummary_factory_default = 'kableExtra')

# Set data directory (adjust this path to where your replication data is stored)
if (!exists("data_dir")) data_dir <- "../../../Legitimacy/Data"


source("scripts/03_models/4_0_Models_output_data_generation.R")

## Scopus

# library(rscopus)
#
# apikey <- Sys.getenv("SCOPUS_API_KEY")
#
# fetch_top_cited_articles <- function(subjarea) {
#   query <- paste0("SRCTYPE(j) AND SUBJAREA(", subjarea, ") AND LANGUAGE(ENGLISH) AND PUBYEAR AFT 2003 AND PUBYEAR BEF 2023")
#   search <- scopus_search(api_key = apikey, query = query, max_count = 1000, view = "COMPLETE", sort = "-citedby-count")
#   return(search)
# }
# 
# # List of subject areas
# fields <- c("AGRI", "ARTS", "BIOC", "BUSI", "CENG", "CHEM", "DECI", "DENT", "EART", "ECON",
#             "ENER", "ENGI", "ENVI", "HEAL", "IMMU", "MATE", "MATH", "MEDI", "NEUR", "NURS",
#             "PHAR", "PHYS", "SOCI", "VETE", "MULT")
# 
# # Function to fetch, parse, and add field information to the data
# fetch_and_process <- function(field) {
#   fetch_top_cited_articles(field) %>%
#     pluck("entries") %>%
#     parse_scopus() %>%
#     mutate(field = as.character(field))
# }
# 
# results <- map_dfr(fields, fetch_and_process, .id = "field")
# 
# field_mapping <- setNames(fields, 1:length(fields))
# 
# results <- results %>%
#   mutate(field = recode(as.character(field), !!!field_mapping))
#
# saveRDS(results, file = "../../../Legitimacy/Data/scopus_results.rds")

papers <- readRDS("../../../Legitimacy/Data/scopus_results.rds") %>%
  select(dc_identifier, abstract, field) %>%
  unnest_sentences(input = abstract, output = abstract) %>%
  mutate(filterword = str_detect(abstract, "best practice|rational|scientific|technical|professional|neutral|knowledge|quality|innovation|solution|solve|efficient|efficiency|compatible|compatibility|effective|coordinate|coordination|streamline|strategic|systematic|optimize|optimal|optimization|cost-effective|cost-efficient|quality|performance|expert|experts|expertise|functional")) %>%
  filter(filterword == TRUE) %>%
  select(-filterword)

table(papers$field)

paper_topwords <- papers %>%
  #filter(field %in% c("BIOC", "IMMU", "MEDI", "NEUR", "NURS", "PHAR", "VETE", "DENT"))%>%
  filter(!field %in% c("ARTS", "BUSI", "COMP", "DECI", "ECON", "ENGI", "HEAL", "PSYC", "SOCI", "MULT")) %>%
  #filter(field %in% c("AGRI", "CENG", "CHEM", "EART", "ENER", "ENVI", "MATE", "PHYS", "MATH", "ENGI")) %>%
  select(text = abstract) %>%
  mutate(text = str_remove_all(text, "[0-9]+")) %>%
  mutate(text = str_remove_all(text, "[[:punct:]]")) %>%
  mutate(text = str_remove_all(text, symbols)) %>%
  unnest_tokens(token, text, token = "words") %>%
  anti_join(tidytext::stop_words %>% rename(token = word), by = "token") %>%
  mutate(token = wordStem(token, language = "english")) %>%
  count(token, sort = TRUE) %>%
  ungroup() %>%
  arrange(desc(n))

paper_words <- paper_topwords %>%
  # left_join(parts_of_speech %>%
  #             rename(token = word) %>%
  #             mutate(token = wordStem(token, language = "english")) %>%
  #             distinct(token, .keep_all = TRUE)) %>%
  # filter(pos %in% c("Adjective", "Adverb")) %>%
  select(token, n) %>%
  distinct(token, .keep_all = TRUE) %>%
  filter(n > 100) 

wordcloud2(data = paper_words %>% select(token, n), size = 1, shape = 'pentagon')

tech_ling1 <- tibble(token = c(paper_words %>% pull(token)),
                      dimension = "papers")

# saveRDS(tech_ling1, file = "../../../Legitimacy/Data/technocratic_dict_papers.rds")

## PatentView

class_patents <- readRDS(file.path(data_dir, "class_patents.rds"))

patent_sentence <- class_patents %>%
  filter(patent_year > 2003 & patent_year < 2023) %>%
  distinct(patent_number, .keep_all = TRUE) %>%
  select(patent_number, patent_abstract) %>% # tech
  unnest_sentences(input = patent_abstract, output = patent_abstract)

patents <- patent_sentence %>%
  mutate(filter_word = str_detect(patent_abstract, "scientific|technical|professional|neutral|knowledge|quality|innovation|solution|solve|efficient|efficiency|compatible|compatibility|effective|coordinate|coordination|streamline|strategic|systematic|optimize|optimal|optimization|cost-effective|cost-efficient|quality|performance|expert|experts|expertise|functional")) %>%
  filter(filter_word == TRUE) %>%
  select(-filter_word) %>%
  select(text = patent_abstract) %>%
  mutate(text = str_remove_all(text, "[0-9]+")) %>%
  mutate(text = str_remove_all(text, "[[:punct:]]")) %>%
  mutate(text = str_remove_all(text, symbols)) %>%
  unnest_tokens(token, text, token = "words") %>%
  anti_join(tidytext::stop_words %>% rename(token = word), by = "token") %>%
  mutate(token = wordStem(token, language = "english")) %>%
  count(token, sort = TRUE) %>%
  ungroup() %>%
  arrange(desc(n))

patents_words <- patents %>%
  # left_join(parts_of_speech %>%
  #             rename(token = word) %>%
  #             mutate(token = wordStem(token, language = "english")) %>%
  #             distinct(token, .keep_all = TRUE)) %>%
  # filter(pos %in% c("Adjective", "Adverb")) %>%
  distinct(token, .keep_all = TRUE) %>%
  filter(n > 4000) # 15000

wordcloud2(data = patents_words, size = 1, shape = 'pentagon')

tech_ling2 <- tibble(token = c(patents_words %>% pull(token)),
                       dimension = "patent")

# saveRDS(tech_ling2, file = "../../../Legitimacy/Data/technocratic_dict_patents.rds")

## Merge with news text and generate shares

standard_news_text <- standard_news_text %>%
  mutate(month = as.factor(substr(date, 6, 7))) 

standard_news_token <- standard_news_text %>%
  unnest_tokens(token, text, token = "words") %>%
  anti_join(stopwords %>% rename(token = word), by = "token") 

paper_share <- standard_news_token %>%
  mutate(token = wordStem(token, language = "english")) %>%
  mutate(tech_ling1 = ifelse(token %in% c(tech_ling1$token), 1, 0)) %>%
  group_by(tech_ling1, rowid, year) %>%
  count() %>%
  spread(tech_ling1, n) %>%
  mutate(share_paper = `1`/(`0`+`1`)) %>%
  ungroup() %>%
  select(-`1`, -`0`) %>%
  left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))

patent_share <- standard_news_token %>%
  mutate(token = wordStem(token, language = "english")) %>%
  mutate(tech_ling2 = ifelse(token %in% c(tech_ling2$token), 1, 0)) %>%
  group_by(tech_ling2, rowid, year) %>%
  count() %>%
  spread(tech_ling2, n) %>%
  mutate(share_patent = `1`/(`0`+`1`)) %>%
  ungroup() %>%
  select(-`1`, -`0`) %>%
  left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))

standard_news_text_shares <- standard_news_text %>%
  left_join(paper_share, by = join_by(year, name, rowid)) %>%
  left_join(patent_share, by = join_by(year, name, rowid)) %>%
  unique()

mean_share_techleg <- standard_news_text_shares %>%
  group_by(name) %>%
  summarise(mean_share_paper = mean(share_paper, na.rm = TRUE),
            mean_share_patent = mean(share_patent, na.rm = TRUE))

output_tech_leg <- standard_news_text_shares %>%
  inner_join(mean_share_techleg, by = join_by(name)) %>%
  ungroup() %>%
  distinct(name, stdno, year, date, .keep_all = TRUE)

output_tech_leg %>%
  group_by(physical, societal) %>%
  count()

## Models

mod1 <- feols(mean_share_paper ~ physical + month | year + committee, 
              cluster = "year+committee",
              data = output_tech_leg) 

mod2 <- feols(mean_share_patent ~ physical + month | year + committee, 
              cluster = "year+committee",
              data = output_tech_leg)

mod3_techout_p <- feols(share_technocratic ~ physical + month | year + committee, 
              cluster = "year+committee", 
               data = legitimacy_coding)

# Extract p-values from each model
p1_techout_p <- summary(mod1)$coeftable[, "Pr(>|t|)"]
p2_techout_p <- summary(mod2)$coeftable[, "Pr(>|t|)"]
p3_techout_p <- summary(mod3_techout_p)$coeftable[, "Pr(>|t|)"]
p1_techout_p <- p1_techout_p[grepl("physical|societal", names(p1_techout_p))]
p2_techout_p <- p2_techout_p[grepl("physical|societal", names(p2_techout_p))]
p3_techout_p <- p3_techout_p[grepl("physical|societal", names(p3_techout_p))]

# output_tech_leg %>%
#   select(name) %>%
#   pull() -> isonewsids
# 
# output_tech_leg %>% 
#   ggplot(aes(factor(societal), mean_share_patent)) + geom_boxplot()

## Models with societal standards

mod1_s <- feols(mean_share_paper ~ societal + month | year + committee, 
              cluster = "year+committee",
              data = output_tech_leg)

mod2_s <- feols(mean_share_patent ~ societal + month | year + committee, 
              cluster = "year+committee",
              data = output_tech_leg)

mod3_techout_s <- feols(share_technocratic ~ societal + month | year + committee,
                cluster = "year+committee",
                data = legitimacy_coding)

# Extract p-values from each model
p1_techout_s <- summary(mod1_s)$coeftable[, "Pr(>|t|)"]
p2_techout_s <- summary(mod2_s)$coeftable[, "Pr(>|t|)"]
p3_techout_s <- summary(mod3_techout_s)$coeftable[, "Pr(>|t|)"]
p1_techout_s <- p1_techout_s[grepl("physical|societal", names(p1_techout_s))]
p2_techout_s <- p2_techout_s[grepl("physical|societal", names(p2_techout_s))]
p3_techout_s <- p3_techout_s[grepl("physical|societal", names(p3_techout_s))]
cm <- c("physical" = "Physical",
        "societal" = "Societal")

models <- list(mod1, mod2, mod3_techout_p,
               mod1_s, mod2_s, mod3_techout_s)

names(models) <- c("Research papers", "Patents", "Legitimation Statements",
                   "Research papers", "Patents", "Legitimation Statements")

mod <- modelsummary(models,
                    stars = TRUE,
                    gof_omit = 'AIC|BIC|Within|Std.Errors|RMSE|FE',
                    fmt = 3,
                    coef_map = cm,
                    format = "latex",
                    statistic = c("{std.error}"),
                    notes = c("Fixed effects: Year and committee",
                              "Control variable: Month",
                              "Clustered standard errors by committee and year",
                              "Coverage: 1999 - 2022")) %>%
  kable_styling(font_size = 11, full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Percent technocratic legitimation words/statements" = 6))

mod
#save_kable(mod, file = "./Tables/iso_mod2_tech.tex")

pvalues_techout <- list(p1_techout_p, p2_techout_p, p3_techout_p,
                       p1_techout_s, p2_techout_s, p3_techout_s)

## Coefficient plot

tech_leg1_physical <- broom::tidy(feols(mean_share_paper ~ physical + month | year + committee, 
                               cluster = "year+committee",
                             data = output_tech_leg %>%
                               mutate(mean_share_paper = scale(mean_share_paper, center = TRUE, scale = TRUE),
                                      physical = scale(physical, center = TRUE, scale = TRUE))),
                       conf.int = TRUE) %>% filter(term == "physical") %>% mutate(term = ifelse(term == "physical", "Research papers", NA))

tech_leg2_physical <- broom::tidy(feols(mean_share_patent ~ physical + month | year+committee, 
                               cluster = "year+committee",
                              data = output_tech_leg %>%
                                mutate(mean_share_patent = scale(mean_share_patent, center = TRUE, scale = TRUE),
                                       physical = scale(physical, center = TRUE, scale = TRUE))),
                        conf.int = TRUE) %>% filter(term == "physical") %>% mutate(term = ifelse(term == "physical", "Patents", NA))

tech_leg3_physical <- broom::tidy(feols(share_technocratic ~ physical + month | year+committee, 
                                        cluster = "year+committee",
                                        data = legitimacy_coding %>%
                                          mutate(share_technocratic = scale(share_technocratic, center = TRUE, scale = TRUE),
                                                 physical = scale(physical, center = TRUE, scale = TRUE))),
                                  conf.int = TRUE) %>% filter(term == "physical") %>% mutate(term = ifelse(term == "physical", "Legitimation  statements", NA))

tech_leg1_societal <- broom::tidy(feols(mean_share_paper ~ societal + month | year + committee, 
                               cluster = "year+committee",
                               data = output_tech_leg %>%
                                 mutate(mean_share_paper = scale(mean_share_paper, center = TRUE, scale = TRUE),
                                        societal = scale(societal, center = TRUE, scale = TRUE))),
                         conf.int = TRUE) %>% filter(term == "societal") %>% mutate(term = ifelse(term == "societal", "Research papers", NA))

tech_leg2_societal <- broom::tidy(feols(mean_share_patent ~ societal + month | year+committee, 
                               cluster = "year+committee",
                               data = output_tech_leg %>%
                                 mutate(mean_share_patent = scale(mean_share_patent, center = TRUE, scale = TRUE),
                                        societal = scale(societal, center = TRUE, scale = TRUE))),
                         conf.int = TRUE) %>% filter(term == "societal") %>% mutate(term = ifelse(term == "societal", "Patents", NA))

tech_leg3_societal <- broom::tidy(feols(share_technocratic ~ societal + month | year+committee, 
                                        cluster = "year+committee",
                                        data = legitimacy_coding %>%
                                          mutate(share_technocratic = scale(share_technocratic, center = TRUE, scale = TRUE),
                                                 societal = scale(societal, center = TRUE, scale = TRUE))),
                                  conf.int = TRUE) %>% filter(term == "societal") %>% mutate(term = ifelse(term == "societal", "Legitimation  statements", NA))

coef_plot_output_tech <- bind_rows(tech_leg1_physical %>% mutate(dv = "Research papers", model = "Technocratic output legitimation", standard = "physical"),
                                   tech_leg2_physical %>% mutate(dv = "Patents", model = "Technocratic output legitimation", standard = "physical"),
                                   tech_leg3_physical %>% mutate(dv = "Legitimation  statements", model = "Technocratic output legitimation", standard = "physical")) %>%
  bind_rows(tech_leg1_societal %>% mutate(dv = "Research papers", model = "Technocratic output legitimation", standard = "societal"),
            tech_leg2_societal %>% mutate(dv = "Patents", model = "Technocratic output legitimation", standard = "societal"),
            tech_leg3_societal %>% mutate(dv = "Legitimation  statements", model = "Technocratic output legitimation", standard = "societal"))


#### ROBUSTNESS ####

# 
# ### 50 / 2000 ###
# 
# paper_words <- paper_topwords %>%
#   distinct(token, .keep_all = TRUE) %>%
#   filter(n > 50)
# 
# patents_words <- patents %>%
#   distinct(token, .keep_all = TRUE) %>%
#   filter(n > 2000)
# 
# tech_ling1 <- tibble(token = c(paper_words %>% pull(token)),
#                      dimension = "papers")
# 
# tech_ling2 <- tibble(token = c(patents_words %>% pull(token)),
#                      dimension = "patent")
# 
# standard_news_token <- standard_news_text %>%
#   unnest_tokens(token, text, token = "words") %>%
#   anti_join(stopwords %>% rename(token = word), by = "token")
# 
# paper_share <- standard_news_token %>%
#   mutate(token = wordStem(token, language = "english")) %>%
#   mutate(tech_ling1 = ifelse(token %in% c(tech_ling1$token), 1, 0)) %>%
#   group_by(tech_ling1, rowid, year) %>%
#   count() %>%
#   spread(tech_ling1, n) %>%
#   mutate(share_paper = `1`/(`0`+`1`)) %>%
#   ungroup() %>%
#   select(-`1`, -`0`) %>%
#   left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))
# 
# patent_share <- standard_news_token %>%
#   mutate(token = wordStem(token, language = "english")) %>%
#   mutate(tech_ling2 = ifelse(token %in% c(tech_ling2$token), 1, 0)) %>%
#   group_by(tech_ling2, rowid, year) %>%
#   count() %>%
#   spread(tech_ling2, n) %>%
#   mutate(share_patent = `1`/(`0`+`1`)) %>%
#   ungroup() %>%
#   select(-`1`, -`0`) %>%
#   left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))
# 
# standard_news_text_shares <- standard_news_text %>%
#   left_join(paper_share, by = join_by(year, name, rowid)) %>%
#   left_join(patent_share, by = join_by(year, name, rowid)) %>%
#   unique()
# 
# mean_share_techleg <- standard_news_text_shares %>%
#   group_by(name) %>%
#   summarise(mean_share_paper = mean(share_paper, na.rm = TRUE),
#             mean_share_patent = mean(share_patent, na.rm = TRUE))
# 
# output_tech_leg <- standard_news_text_shares %>%
#   inner_join(mean_share_techleg, by = join_by(name)) %>%
#   ungroup() %>%
#   distinct(name, stdno, year, date, .keep_all = TRUE)
# 
# mod1 <- feols(mean_share_paper ~ physical | year + committee,
#               cluster = "year+committee",
#               data = output_tech_leg)
# 
# mod2 <- feols(mean_share_patent ~ physical | year + committee,
#               cluster = "year+committee",
#               data = output_tech_leg)
# 
# cm <- c("physical" = "Physical",
#         "societal" = "Societal")
# 
# models <- list(mod1, mod2)
# 
# names(models) <- c("Research papers", "Patents")
# 
# mod_rob1 <- modelsummary(models,
#                     stars = TRUE,
#                     gof_omit = 'AIC|BIC|Within|Std.Errors|RMSE|FE',
#                     fmt = 3,
#                     coef_map = cm,
#                     format = "latex",
#                     statistic = c("{std.error}"),
#                     notes = c("Fixed effects: Year and committee",
#                               "Clustered standard errors by committee and year",
#                               "Coverage: 1999 - 2022")) %>%
#   kable_styling(font_size = 11, full_width = FALSE) %>%
#   add_header_above(c(" " = 1, "Cutoff: 100" = 1, "Cutoff: 4000" = 1)) %>%
#   add_header_above(c(" " = 1, "Percent technocratic legitimation words" = 2))
# 
# ### 400 / 6000 ###
# 
# paper_words <- paper_topwords %>%
#   distinct(token, .keep_all = TRUE) %>%
#   filter(n > 400)
# 
# patents_words <- patents %>%
#   distinct(token, .keep_all = TRUE) %>%
#   filter(n > 6000)
# 
# tech_ling1 <- tibble(token = c(paper_words %>% pull(token)),
#                      dimension = "papers")
# 
# tech_ling2 <- tibble(token = c(patents_words %>% pull(token)),
#                      dimension = "patent")
# 
# standard_news_token <- standard_news_text %>%
#   unnest_tokens(token, text, token = "words") %>%
#   anti_join(stopwords %>% rename(token = word), by = "token")
# 
# paper_share <- standard_news_token %>%
#   mutate(token = wordStem(token, language = "english")) %>%
#   mutate(tech_ling1 = ifelse(token %in% c(tech_ling1$token), 1, 0)) %>%
#   group_by(tech_ling1, rowid, year) %>%
#   count() %>%
#   spread(tech_ling1, n) %>%
#   mutate(share_paper = `1`/(`0`+`1`)) %>%
#   ungroup() %>%
#   select(-`1`, -`0`) %>%
#   left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))
# 
# patent_share <- standard_news_token %>%
#   mutate(token = wordStem(token, language = "english")) %>%
#   mutate(tech_ling2 = ifelse(token %in% c(tech_ling2$token), 1, 0)) %>%
#   group_by(tech_ling2, rowid, year) %>%
#   count() %>%
#   spread(tech_ling2, n) %>%
#   mutate(share_patent = `1`/(`0`+`1`)) %>%
#   ungroup() %>%
#   select(-`1`, -`0`) %>%
#   left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))
# 
# standard_news_text_shares <- standard_news_text %>%
#   left_join(paper_share, by = join_by(year, name, rowid)) %>%
#   left_join(patent_share, by = join_by(year, name, rowid)) %>%
#   unique()
# 
# mean_share_techleg <- standard_news_text_shares %>%
#   group_by(name) %>%
#   summarise(mean_share_paper = mean(share_paper, na.rm = TRUE),
#             mean_share_patent = mean(share_patent, na.rm = TRUE))
# 
# output_tech_leg <- standard_news_text_shares %>%
#   inner_join(mean_share_techleg, by = join_by(name)) %>%
#   ungroup() %>%
#   distinct(name, stdno, year, date, .keep_all = TRUE)
# 
# mod1 <- feols(mean_share_paper ~ physical | year + committee,
#               cluster = "year+committee",
#               data = output_tech_leg)
# 
# mod2 <- feols(mean_share_patent ~ physical | year + committee,
#               cluster = "year+committee",
#               data = output_tech_leg)
# 
# cm <- c("physical" = "Physical",
#         "societal" = "Societal")
# 
# models <- list(mod1, mod2)
# 
# names(models) <- c("Research papers", "Patents")
# 
# mod_rob2 <- modelsummary(models,
#                          stars = TRUE,
#                          gof_omit = 'AIC|BIC|Within|Std.Errors|RMSE|FE',
#                          fmt = 3,
#                          coef_map = cm,
#                          format = "latex",
#                          statistic = c("{std.error}"),
#                          notes = c("Fixed effects: Year and committee",
#                                    "Clustered standard errors by committee and year",
#                                    "Coverage: 1999 - 2022")) %>%
#   kable_styling(font_size = 11, full_width = FALSE) %>%
#   add_header_above(c(" " = 1, "Cutoff: 400" = 1, "Cutoff: 6000" = 1)) %>%
#   add_header_above(c(" " = 1, "Percent technocratic legitimation words" = 2))
# 
# ### 600 / 8000 ###
# 
# paper_words <- paper_topwords %>%
#   distinct(token, .keep_all = TRUE) %>%
#   filter(n > 600)
# 
# patents_words <- patents %>%
#   distinct(token, .keep_all = TRUE) %>%
#   filter(n > 8000)
# 
# tech_ling1 <- tibble(token = c(paper_words %>% pull(token)),
#                      dimension = "papers")
# 
# tech_ling2 <- tibble(token = c(patents_words %>% pull(token)),
#                      dimension = "patent")
# 
# standard_news_token <- standard_news_text %>%
#   unnest_tokens(token, text, token = "words") %>%
#   anti_join(stopwords %>% rename(token = word), by = "token")
# 
# paper_share <- standard_news_token %>%
#   mutate(token = wordStem(token, language = "english")) %>%
#   mutate(tech_ling1 = ifelse(token %in% c(tech_ling1$token), 1, 0)) %>%
#   group_by(tech_ling1, rowid, year) %>%
#   count() %>%
#   spread(tech_ling1, n) %>%
#   mutate(share_paper = `1`/(`0`+`1`)) %>%
#   ungroup() %>%
#   select(-`1`, -`0`) %>%
#   left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))
# 
# patent_share <- standard_news_token %>%
#   mutate(token = wordStem(token, language = "english")) %>%
#   mutate(tech_ling2 = ifelse(token %in% c(tech_ling2$token), 1, 0)) %>%
#   group_by(tech_ling2, rowid, year) %>%
#   count() %>%
#   spread(tech_ling2, n) %>%
#   mutate(share_patent = `1`/(`0`+`1`)) %>%
#   ungroup() %>%
#   select(-`1`, -`0`) %>%
#   left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))
# 
# standard_news_text_shares <- standard_news_text %>%
#   left_join(paper_share, by = join_by(year, name, rowid)) %>%
#   left_join(patent_share, by = join_by(year, name, rowid)) %>%
#   unique()
# 
# mean_share_techleg <- standard_news_text_shares %>%
#   group_by(name) %>%
#   summarise(mean_share_paper = mean(share_paper, na.rm = TRUE),
#             mean_share_patent = mean(share_patent, na.rm = TRUE))
# 
# output_tech_leg <- standard_news_text_shares %>%
#   inner_join(mean_share_techleg, by = join_by(name)) %>%
#   ungroup() %>%
#   distinct(name, stdno, year, date, .keep_all = TRUE)
# 
# mod1 <- feols(mean_share_paper ~ physical | year + committee,
#               cluster = "year+committee",
#               data = output_tech_leg)
# 
# mod2 <- feols(mean_share_patent ~ physical | year + committee,
#               cluster = "year+committee",
#               data = output_tech_leg)
# 
# cm <- c("physical" = "Physical",
#         "societal" = "Societal")
# 
# models <- list(mod1, mod2)
# 
# names(models) <- c("Research papers", "Patents")
# 
# mod_rob3 <- modelsummary(models,
#                          stars = TRUE,
#                          gof_omit = 'AIC|BIC|Within|Std.Errors|RMSE|FE',
#                          fmt = 3,
#                          coef_map = cm,
#                          format = "latex",
#                          statistic = c("{std.error}"),
#                          notes = c("Fixed effects: Year and committee",
#                                    "Clustered standard errors by committee and year",
#                                    "Coverage: 1999 - 2022")) %>%
#   kable_styling(font_size = 11, full_width = FALSE) %>%
#   add_header_above(c(" " = 1, "Cutoff: 600" = 1, "Cutoff: 8000" = 1)) %>%
#   add_header_above(c(" " = 1, "Percent technocratic legitimation words" = 2))
# 
# ### 1000 / 10000 ###
# 
# paper_words <- paper_topwords %>%
#   distinct(token, .keep_all = TRUE) %>%
#   filter(n > 1000)
# 
# patents_words <- patents %>%
#   distinct(token, .keep_all = TRUE) %>%
#   filter(n > 10000)
# 
# tech_ling1 <- tibble(token = c(paper_words %>% pull(token)),
#                      dimension = "papers")
# 
# tech_ling2 <- tibble(token = c(patents_words %>% pull(token)),
#                      dimension = "patent")
# 
# standard_news_token <- standard_news_text %>%
#   unnest_tokens(token, text, token = "words") %>%
#   anti_join(stopwords %>% rename(token = word), by = "token")
# 
# paper_share <- standard_news_token %>%
#   mutate(token = wordStem(token, language = "english")) %>%
#   mutate(tech_ling1 = ifelse(token %in% c(tech_ling1$token), 1, 0)) %>%
#   group_by(tech_ling1, rowid, year) %>%
#   count() %>%
#   spread(tech_ling1, n) %>%
#   mutate(share_paper = `1`/(`0`+`1`)) %>%
#   ungroup() %>%
#   select(-`1`, -`0`) %>%
#   left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))
# 
# patent_share <- standard_news_token %>%
#   mutate(token = wordStem(token, language = "english")) %>%
#   mutate(tech_ling2 = ifelse(token %in% c(tech_ling2$token), 1, 0)) %>%
#   group_by(tech_ling2, rowid, year) %>%
#   count() %>%
#   spread(tech_ling2, n) %>%
#   mutate(share_patent = `1`/(`0`+`1`)) %>%
#   ungroup() %>%
#   select(-`1`, -`0`) %>%
#   left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))
# 
# standard_news_text_shares <- standard_news_text %>%
#   left_join(paper_share, by = join_by(year, name, rowid)) %>%
#   left_join(patent_share, by = join_by(year, name, rowid)) %>%
#   unique()
# 
# mean_share_techleg <- standard_news_text_shares %>%
#   group_by(name) %>%
#   summarise(mean_share_paper = mean(share_paper, na.rm = TRUE),
#             mean_share_patent = mean(share_patent, na.rm = TRUE))
# 
# output_tech_leg <- standard_news_text_shares %>%
#   inner_join(mean_share_techleg, by = join_by(name)) %>%
#   ungroup() %>%
#   distinct(name, stdno, year, date, .keep_all = TRUE)
# 
# mod1 <- feols(mean_share_paper ~ physical | year + committee,
#               cluster = "year+committee",
#               data = output_tech_leg)
# 
# mod2 <- feols(mean_share_patent ~ physical | year + committee,
#               cluster = "year+committee",
#               data = output_tech_leg)
# 
# cm <- c("physical" = "Physical",
#         "societal" = "Societal")
# 
# models <- list(mod1, mod2)
# 
# names(models) <- c("Research papers", "Patents")
# 
# mod_rob4 <- modelsummary(models,
#                          stars = TRUE,
#                          gof_omit = 'AIC|BIC|Within|Std.Errors|RMSE|FE',
#                          fmt = 3,
#                          coef_map = cm,
#                          format = "latex",
#                          statistic = c("{std.error}"),
#                          notes = c("Fixed effects: Year and committee",
#                                    "Clustered standard errors by committee and year",
#                                    "Coverage: 1999 - 2022")) %>%
#   kable_styling(font_size = 11, full_width = FALSE) %>%
#   add_header_above(c(" " = 1, "Cutoff: 1000" = 1, "Cutoff: 10000" = 1)) %>%
#   add_header_above(c(" " = 1, "Percent technocratic legitimation words" = 2))
# 
# 
# mod_rob1
# mod_rob2
# mod_rob3
# mod_rob4

## Clean

rm(list=ls()[! ls() %in% c("coef_plot_input_dem", "coef_plot_input_tech", "coef_plot_output_dem", "coef_plot_output_tech",
                           "liaison_dat_merged_sh", "liaison_dat_merged_frac", "participation_dat_merged_sh", "participation_dat_merged_frac",
                           "tech_leg",
                           "output_tech_leg", "output_dem_leg",
                           "legitimacy_coding",
                           "pvalues_techin", "pvalues_demin", "pvalues_techout", "pvalues_demout",
                           #"mod1_techin_s", "mod1_techin_p", "mod6_techin_p", "mod6_techin_p",
                           #"mod2_demin_s_country", "mod4_demin_s_liaison", "mod2_demin_p_country", "mod4_demin_p_liaison",
                           "mod3_techout_p", "mod3_techout_s", "mod6_demout_p", "mod6_demout_s")])
gc()


