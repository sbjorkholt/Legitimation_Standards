
#### 4.2 MODEL OUTPUT LEGITIMATION DEMOCRATIC ####

pacman::p_load(dplyr, readr, stringr, readtext, tibble, lexicon, tidytext, quanteda, SnowballC, wordcloud2, ggwordcloud, purrr, fixest, tidyr, modelsummary, kableExtra)
options(modelsummary_factory_default = 'kableExtra')

# Set data directory (adjust this path to where your replication data is stored)
if (!exists("data_dir")) data_dir <- "../../../Legitimacy/Data"

source("scripts/03_models/4_0_Models_output_data_generation.R")

## UNGA Debates
unga_files <- readtext(paste0(data_dir, "/TXT/*"),
                       docvarsfrom = "filenames", 
                       dvsep = "_", 
                       docvarnames = c("country", "session", "year"))

unga_speeches <- unga_files %>%
  mutate(doc_id = str_replace(doc_id, ".txt", "")) %>%
  mutate(doc_id = str_replace(doc_id, "_\\d{2}", "")) %>%
  mutate(text = str_squish(text))

# saveRDS(unga_speeches, file = "../../../Legitimacy/Data/unga_speeches.rds")

unga_topwords <- unga_speeches %>%
  filter(year %in% c(1990:2000)) %>% # Picking years of particular democratic rhetoric
  mutate(text = str_remove_all(text, "[0-9]+")) %>%
  mutate(text = str_remove_all(text, "[[:punct:]]")) %>%
  mutate(text = str_remove_all(text, symbols)) %>%
  unnest_tokens(token, text, token = "words") %>%
  anti_join(tidytext::stop_words %>% rename(token = word), by = "token") %>%
  mutate(token = wordStem(token, language = "english")) %>%
  count(token, sort = TRUE) %>%
  ungroup() %>%
  arrange(desc(n)) %>% # Manually cleaning out noisy words unrelated to democratic justification
  filter(!token %in% c("united", "nations", "council", "president", "secretary", "charter", "session", "organization", "secretariat", "session", "intern", "secretarygener",
                       "congratul", "million", "secretarygener",
                       "sixth", "fiftieth", "fourth", "novemb", "april", "decemb", "octob", "twentyfirst", "thousand", "juli", "august", "june", "anniversari", "septemb", 
                       "israel", "palestine", "namibia", "iraq", "afghanistan", "countries", "africa", 
                       "israeli", "china", "korea", "cyprus", "presidency", "caribbean", "iran", "lebanon", "indonesia", "canada", "guineabissau",
                       "ecuador", "papua", "jordan", "leon", "turkish", "french", "ethiopia", "ukrain", "argentina", "australia",
                       "primarili", "portugues", "kosovo", "vienna", "uruguay", "rhodesia", "timor", "bangladesh", "brazil", "ireland", "taiwan",
                       "syrian", "salvador", "kofi", "annan", "rio", "asean", "spain", "morocco", "guatemala", "vietnames", "turkey", "york",
                       "northsouth", "el", "erad", "libya", "chile", "german", "jerusalem", "nicaragua", "namibian", "yugoslavia", "mexico",
                       "herzegovina", "burundi", "peninsula", "liberia", "portug", "lebanes", "rwanda", "saint", "afghan",
                       "egypt", "mediterranean", "congo", "kuwait", "panama", "israel", "chad", "haiti",
                       "mozambiqu", "cuba", "iraqi", "bosnia", "germani", "india", "sahara", "asian", "africa", "indian",
                       "syria", "pakistan", "ongo", "sudan", "cambodia", "zimbabw", "gulf", "southeast", "japan", "guinea", "somalia", "korean", "african",
                       "kampuchea", "western", "north", "northern", "east", "eastern", "west", "western", "south", "southern", "latin", "american", "america", 
                       "european", "europe", "soviet", "palestinian", "arab",
                       "sir", "de", "oau", "abil", "due", "led")) 

unga_words <- unga_topwords %>% 
  distinct(token, .keep_all = TRUE) %>%
  filter(n > 400) 

wordcloud2(data = unga_words, size = 1, shape = 'pentagon')

dem_ling1 <- tibble(token = c(unga_words %>% pull(token)),
                    dimension = "unga_debates")

# saveRDS(dem_ling1, file = "../../../Legitimacy/Data/democratic_dict_unga.rds")

## CSR

data(key_corporate_social_responsibility)

count_words <- function(text) {
  str_count(text, "\\S+")
}

key_corporate_social_responsibility <- key_corporate_social_responsibility %>%
  select(-regex) %>%
  mutate(token = wordStem(token, language = "english")) %>%
  mutate(word_count = count_words(token)) %>%
  filter(word_count == 1) %>%
  select(-word_count) %>%
  group_by(dimension) %>%
  distinct(token, .keep_all = TRUE) %>%
  ungroup()
 
dem_ling <- key_corporate_social_responsibility %>%
  bind_rows(dem_ling1) %>%
  rowid_to_column() %>%
  spread(dimension, token) %>%
  select(-rowid)

#saveRDS(key_corporate_social_responsibility, file = "../../../Legitimacy/Data/democratic_dict_csr.rds")

## Merge with news text and generate shares

standard_news_token <- standard_news_text %>%
  unnest_tokens(token, text, token = "words") %>%
  anti_join(stopwords %>% rename(token = word), by = "token") 

unga_share <- standard_news_token %>%
  mutate(token = wordStem(token, language = "english")) %>%
  mutate(dem_ling = ifelse(token %in% c(dem_ling1$token), 1, 0)) %>%
  group_by(dem_ling, rowid, year) %>%
  count() %>%
  spread(dem_ling, n) %>%
  mutate(share_unga = `1`/(`0`+`1`)) %>%
  ungroup() %>%
  select(-`1`, -`0`) %>%
  left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))

humanrights_share <- standard_news_token %>%
  mutate(token = wordStem(token, language = "english")) %>%
  mutate(humanrights_ling = ifelse(token %in% c(dem_ling$human_rights), 1, 0)) %>%
  group_by(humanrights_ling, rowid, year) %>%
  count() %>%
  spread(humanrights_ling, n) %>%
  mutate(share_humanrights = `1`/(`0`+`1`)) %>%
  ungroup() %>%
  select(-`1`, -`0`) %>%
  left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))

employee_share <- standard_news_token %>%
  mutate(token = wordStem(token, language = "english")) %>%
  mutate(employee_ling = ifelse(token %in% c(dem_ling$employee), 1, 0)) %>%
  group_by(employee_ling, rowid, year) %>%
  count() %>%
  spread(employee_ling, n) %>%
  mutate(share_employee = `1`/(`0`+`1`)) %>%
  ungroup() %>%
  select(-`1`, -`0`) %>%
  left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))

environment_share <- standard_news_token %>%
  mutate(token = wordStem(token, language = "english")) %>%
  mutate(environment_ling = ifelse(token %in% dem_ling$environment, 1, 0)) %>%
  group_by(environment_ling, rowid, year) %>%
  count() %>%
  spread(environment_ling, n) %>%
  mutate(share_environment = `1`/(`0`+`1`)) %>%
  ungroup() %>%
  select(-`1`, -`0`) %>%
  left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))

community_share <- standard_news_token %>%
  mutate(token = wordStem(token, language = "english")) %>%
  mutate(community_ling = ifelse(token %in% dem_ling$social_and_community, 1, 0)) %>%
  group_by(community_ling, rowid, year) %>%
  count() %>%
  spread(community_ling, n) %>%
  mutate(share_community = `1`/(`0`+`1`)) %>%
  ungroup() %>%
  select(-`1`, -`0`) %>%
  left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))

standard_news_text_shares <- standard_news_text %>%
  left_join(unga_share, by = join_by(year, name, rowid)) %>%
  left_join(humanrights_share, by = join_by(year, name, rowid)) %>%
  left_join(employee_share, by = join_by(year, name, rowid)) %>%
  left_join(environment_share, by = join_by(year, name, rowid)) %>%
  left_join(community_share, by = join_by(year, name, rowid)) %>%
  unique()

mean_share_demleg <- standard_news_text_shares %>%
  group_by(name) %>%
  summarise(mean_share_unga = mean(share_unga),
            mean_share_humanrights = mean(share_humanrights),
            mean_share_employee = mean(share_employee),
            mean_share_environment = mean(share_environment),
            mean_share_community = mean(share_community)) %>%
  ungroup()

output_dem_leg <- standard_news_text_shares %>%
  inner_join(mean_share_demleg, by = join_by(name)) %>%
  ungroup() %>%
  mutate(month = as.factor(substr(date, 6, 7))) %>% 
  distinct(name, stdno, year, date, .keep_all = TRUE)

output_dem_leg %>%
  group_by(physical, societal) %>%
  count()

## Models

mod1 <- feols(mean_share_unga ~ societal + month | year + committee, 
              cluster = "year+committee",
              data = output_dem_leg)

mod2 <- feols(mean_share_humanrights ~ societal + month  | year + committee, 
              cluster = "year+committee",
              data = output_dem_leg)

mod3 <- feols(mean_share_employee ~ societal + month  | year + committee, 
              cluster = "year+committee",
              data = output_dem_leg)

mod4 <- feols(mean_share_environment ~ societal + month  | year + committee,
              cluster = "year+committee",
              data = output_dem_leg)

mod5 <- feols(mean_share_community ~ societal + month  | year + committee, 
               cluster = "year+committee",
               data = output_dem_leg)


mod6_demout_s <- feols(share_democratic ~ societal + month  | year + committee, 
              cluster = "year+committee", 
              data = legitimacy_coding)

# Extract p-values from each model
p1_demout_s <- summary(mod1)$coeftable[, "Pr(>|t|)"]
p2_demout_s <- summary(mod2)$coeftable[, "Pr(>|t|)"]
p3_demout_s <- summary(mod3)$coeftable[, "Pr(>|t|)"]
p4_demout_s <- summary(mod4)$coeftable[, "Pr(>|t|)"]
p5_demout_s <- summary(mod5)$coeftable[, "Pr(>|t|)"]
p6_demout_s <- summary(mod6_demout_s)$coeftable[, "Pr(>|t|)"]
p1_demout_s <- p1_demout_s[grepl("physical|societal", names(p1_demout_s))]
p2_demout_s <- p2_demout_s[grepl("physical|societal", names(p2_demout_s))]
p3_demout_s <- p3_demout_s[grepl("physical|societal", names(p3_demout_s))]
p4_demout_s <- p4_demout_s[grepl("physical|societal", names(p4_demout_s))]
p5_demout_s <- p5_demout_s[grepl("physical|societal", names(p5_demout_s))]
p6_demout_s <- p6_demout_s[grepl("physical|societal", names(p6_demout_s))]

# Physical

mod1_p <- feols(mean_share_unga ~ physical + month | year + committee, 
              cluster = "year+committee",
              data = output_dem_leg)

mod2_p <- feols(mean_share_humanrights ~ physical + month | year + committee, 
              cluster = "year+committee",
              data = output_dem_leg)

mod3_p <- feols(mean_share_employee ~ physical + month | year + committee, 
              cluster = "year+committee",
              data = output_dem_leg)

mod4_p <- feols(mean_share_environment ~ physical + month | year + committee,
              cluster = "year+committee",
              data = output_dem_leg)

mod5_p <- feols(mean_share_community ~ physical + month | year + committee, 
              cluster = "year+committee",
              data = output_dem_leg)

mod6_demout_p <- feols(share_democratic ~ physical + month | year + committee, 
              cluster = "year+committee", 
              data = legitimacy_coding)

# Extract p-values from each model
p1_demout_p <- summary(mod1_p)$coeftable[, "Pr(>|t|)"]
p2_demout_p <- summary(mod2_p)$coeftable[, "Pr(>|t|)"]
p3_demout_p <- summary(mod3_p)$coeftable[, "Pr(>|t|)"]
p4_demout_p <- summary(mod4_p)$coeftable[, "Pr(>|t|)"]
p5_demout_p <- summary(mod5_p)$coeftable[, "Pr(>|t|)"]
p6_demout_p <- summary(mod6_demout_p)$coeftable[, "Pr(>|t|)"]
p1_demout_p <- p1_demout_p[grepl("physical|societal", names(p1_demout_p))]
p2_demout_p <- p2_demout_p[grepl("physical|societal", names(p2_demout_p))]
p3_demout_p <- p3_demout_p[grepl("physical|societal", names(p3_demout_p))]
p4_demout_p <- p4_demout_p[grepl("physical|societal", names(p4_demout_p))]
p5_demout_p <- p5_demout_p[grepl("physical|societal", names(p5_demout_p))]
p6_demout_p <- p6_demout_p[grepl("physical|societal", names(p6_demout_p))]

cm <- c("societal" = "Societal",
        "physical" = "Physical")

models <- list(mod1, mod2, mod3, mod4, mod5, mod6_demout_s)

names(models) <- c("UNGA Debates", "Human Rights CSR", "Labor CSR", "Environment CSR", "Social and Community CSR", "Legitimation Statements")

mod <- modelsummary(models,
                    stars = TRUE,
                    gof_omit = 'AIC|BIC|Within|Std.Errors|RMSE|FE',
                    fmt = 3,
                    coef_map = cm,
                    format = "latex",
                    statistic = c("{std.error}"),
                    notes = c("Fixed effects: Year and committee",
                              "Clustered standard errors by committee and year",
                              "Coverage: 1999 - 2022")) %>%
  kable_styling(font_size = 9, full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Percent democratic legitimation words/statements" = 6))

mod

#save_kable(mod, file = "./Tables/iso_mod2_dem.tex")


models <- list(mod1_p, mod2_p, mod3_p, mod4_p, mod5_p, mod6_demout_p)

names(models) <- c("UNGA Debates", "Human Rights CSR", "Labor CSR", "Environment CSR", "Social and Community CSR", "Legitimation Statements")

mod <- modelsummary(models,
                    stars = TRUE,
                    gof_omit = 'AIC|BIC|Within|Std.Errors|RMSE|FE',
                    fmt = 3,
                    coef_map = cm,
                    format = "latex",
                    statistic = c("{std.error}"),
                    notes = c("Fixed effects: Year and committee",
                              "Clustered standard errors by committee and year",
                              "Coverage: 1999 - 2022")) %>%
  kable_styling(font_size = 9, full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Percent democratic legitimation words/statements" = 6))

mod

#save_kable(mod, file = "./Tables/iso_mod2_dem_physical.tex")

pvalues_demout <- list(p1_demout_p, p2_demout_p, p3_demout_p, p4_demout_p, p5_demout_p, p6_demout_p,
                        p1_demout_s, p2_demout_s, p3_demout_s, p4_demout_s, p5_demout_s, p6_demout_s)


## Coefficient plot

# Societal 

demo_un_societal <- broom::tidy(feols(mean_share_unga ~ societal + month | year + committee, 
                             cluster = "year+committee",
                             data = output_dem_leg %>%
                               mutate(mean_share_unga = scale(mean_share_unga, center = TRUE, scale = TRUE),
                                      societal = scale(societal, center = TRUE, scale = TRUE))),
                       conf.int = TRUE) %>% 
  filter(term == "societal") %>% mutate(term = ifelse(term == "societal", "UNGA", NA))

demo_hum_societal <- broom::tidy(feols(mean_share_humanrights ~ societal + month | year + committee, 
                              cluster = "year+committee",
                              data = output_dem_leg %>%
                                mutate(mean_share_humanrights = scale(mean_share_humanrights, center = TRUE, scale = TRUE),
                                       societal = scale(societal, center = TRUE, scale = TRUE))),
                        conf.int = TRUE) %>% 
  filter(term == "societal") %>% mutate(term = ifelse(term == "societal", "CSR: Human Rights", NA))

demo_emp_societal <- broom::tidy(feols(mean_share_employee ~ societal + month | year + committee, 
                              cluster = "year+committee",
                              data = output_dem_leg %>%
                                mutate(mean_share_employee = scale(mean_share_employee, center = TRUE, scale = TRUE),
                                       societal = scale(societal, center = TRUE, scale = TRUE))),
                        conf.int = TRUE) %>% 
  filter(term == "societal") %>% mutate(term = ifelse(term == "societal", "CSR: Labor", NA))

demo_env_societal <- broom::tidy(feols(mean_share_environment ~ societal + month | year+committee, cluster = "year+committee",
                              data = output_dem_leg %>%
                                mutate(mean_share_environment = scale(mean_share_environment, center = TRUE, scale = TRUE),
                                       societal = scale(societal, center = TRUE, scale = TRUE))),
                        conf.int = TRUE) %>% 
  filter(term == "societal") %>% mutate(term = ifelse(term == "societal", "CSR: Environment", NA))

demo_com_societal <- broom::tidy(feols(mean_share_community ~ societal + month | year+committee, cluster = "year+committee",
                              data = output_dem_leg %>%
                                mutate(mean_share_community = scale(mean_share_community, center = TRUE, scale = TRUE),
                                       societal = scale(societal, center = TRUE, scale = TRUE))),
                        conf.int = TRUE) %>% 
  filter(term == "societal") %>% mutate(term = ifelse(term == "societal", "CSR: Community", NA))

demo_leg_societal <- broom::tidy(feols(share_democratic ~ societal + month | year+committee, cluster = "year+committee",
                                       data = legitimacy_coding %>%
                                         mutate(share_democratic = scale(share_democratic, center = TRUE, scale = TRUE),
                                                societal = scale(societal, center = TRUE, scale = TRUE))),
                                 conf.int = TRUE) %>% 
  filter(term == "societal") %>% mutate(term = ifelse(term == "societal", "Legitimation statements", NA))

# Physical

demo_un_physical <- broom::tidy(feols(mean_share_unga ~ physical | year + committee, 
                             cluster = "year+committee",
                             data = output_dem_leg %>%
                               mutate(mean_share_unga = scale(mean_share_unga, center = TRUE, scale = TRUE),
                                      physical = scale(physical, center = TRUE, scale = TRUE))),
                       conf.int = TRUE) %>% 
  filter(term == "physical") %>% mutate(term = ifelse(term == "physical", "UNGA", NA))

demo_hum_physical <- broom::tidy(feols(mean_share_humanrights ~ physical | year + committee, 
                              cluster = "year+committee",
                              data = output_dem_leg %>%
                                mutate(mean_share_humanrights = scale(mean_share_humanrights, center = TRUE, scale = TRUE),
                                       physical = scale(physical, center = TRUE, scale = TRUE))),
                        conf.int = TRUE) %>% 
  filter(term == "physical") %>% mutate(term = ifelse(term == "physical", "CSR: Human Rights", NA))

demo_emp_physical <- broom::tidy(feols(mean_share_employee ~ physical | year + committee, 
                              cluster = "year+committee",
                              data = output_dem_leg %>%
                                mutate(mean_share_employee = scale(mean_share_employee, center = TRUE, scale = TRUE),
                                       physical = scale(physical, center = TRUE, scale = TRUE))),
                        conf.int = TRUE) %>% 
  filter(term == "physical") %>% mutate(term = ifelse(term == "physical", "CSR: Labor", NA))

demo_env_physical <- broom::tidy(feols(mean_share_environment ~ physical | year+committee, cluster = "year+committee",
                              data = output_dem_leg %>%
                                mutate(mean_share_environment = scale(mean_share_environment, center = TRUE, scale = TRUE),
                                       physical = scale(physical, center = TRUE, scale = TRUE))),
                        conf.int = TRUE) %>% 
  filter(term == "physical") %>% mutate(term = ifelse(term == "physical", "CSR: Environment", NA))

demo_com_physical <- broom::tidy(feols(mean_share_community ~ physical | year+committee, cluster = "year+committee",
                              data = output_dem_leg %>%
                                mutate(mean_share_community = scale(mean_share_community, center = TRUE, scale = TRUE),
                                       physical = scale(physical, center = TRUE, scale = TRUE))),
                        conf.int = TRUE) %>% 
  filter(term == "physical") %>% mutate(term = ifelse(term == "physical", "CSR: Community", NA))

demo_leg_physical <- broom::tidy(feols(share_democratic ~ physical | year+committee, cluster = "year+committee",
                                       data = legitimacy_coding %>%
                                         mutate(share_democratic = scale(share_democratic, center = TRUE, scale = TRUE),
                                                physical = scale(physical, center = TRUE, scale = TRUE))),
                                 conf.int = TRUE) %>% 
  filter(term == "physical") %>% mutate(term = ifelse(term == "physical", "Legitimation statements", NA))



coef_plot_output_dem <- bind_rows(demo_un_societal %>% mutate(dv = "UNGA", model = "Democratic output legitimation", standard = "societal"),
                                  demo_hum_societal %>% mutate(dv = "CSR: Human Rights", model = "Democratic output legitimation", standard = "societal"),
                                  demo_emp_societal %>% mutate(dv = "CSR: Labor", model = "Democratic output legitimation", standard = "societal"),
                                  demo_env_societal %>% mutate(dv = "CSR: Environment", model = "Democratic output legitimation", standard = "societal"),
                                  demo_com_societal %>% mutate(dv = "CSR: Community", model = "Democratic output legitimation", standard = "societal"),
                                  demo_leg_societal %>% mutate(dv = "Legitimation statements", model = "Democratic output legitimation", standard = "societal")) %>%
  bind_rows(demo_un_physical %>% mutate(dv = "UNGA", model = "Democratic output legitimation", standard = "physical"),
            demo_hum_physical %>% mutate(dv = "CSR: Human Rights", model = "Democratic output legitimation", standard = "physical"),
            demo_emp_physical %>% mutate(dv = "CSR: Labor", model = "Democratic output legitimation", standard = "physical"),
            demo_env_physical %>% mutate(dv = "CSR: Environment", model = "Democratic output legitimation", standard = "physical"),
            demo_com_physical %>% mutate(dv = "CSR: Community", model = "Democratic output legitimation", standard = "physical"),
            demo_leg_physical %>% mutate(dv = "Legitimation statements", model = "Democratic output legitimation", standard = "physical"))


## Robustness with different cutoffs

## Cutoffs for robustness models:
# 200
# 600
# 1000

# # ## 200
# unga_words_200 <- unga_topwords %>%
#   filter(n > 200) %>%
#   distinct(token, .keep_all = TRUE)
# 
# dem_ling_200 <- tibble(token = c(unga_words_200 %>% pull(token)),
#                         dimension = "un_debates")
# 
# unga_share_200 <- standard_news_token %>%
#   mutate(token = wordStem(token, language = "english")) %>%
#   mutate(un_ling_200 = ifelse(token %in% dem_ling_200$token, 1, 0)) %>%
#   group_by(un_ling_200, rowid, year) %>%
#   count() %>%
#   spread(un_ling_200, n) %>%
#   mutate(un_ling_200 = `1`/(`0`+`1`)) %>%
#   ungroup() %>%
#   select(-`1`, -`0`) %>%
#   left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))
# 
# # ## 600
# 
# unga_words_600 <- unga_topwords %>%
#   filter(n > 600) %>%
#   distinct(token, .keep_all = TRUE)
# 
# dem_ling_600 <- tibble(token = c(unga_words_600 %>% pull(token)),
#                         dimension = "un_debates")
# 
# unga_share_600 <- standard_news_token %>%
#   mutate(token = wordStem(token, language = "english")) %>%
#   mutate(un_ling_600 = ifelse(token %in% dem_ling_600$token, 1, 0)) %>%
#   group_by(un_ling_600, rowid, year) %>%
#   count() %>%
#   spread(un_ling_600, n) %>%
#   mutate(un_ling_600 = `1`/(`0`+`1`)) %>%
#   ungroup() %>%
#   select(-`1`, -`0`) %>%
#   left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))
# 
# # ## 1200
# 
# unga_words_1200 <- unga_topwords %>%
#   filter(n > 1000) %>%
#   distinct(token, .keep_all = TRUE)
# 
# dem_ling_1200 <- tibble(token = c(unga_words_1200 %>% pull(token)),
#                        dimension = "un_debates")
# 
# unga_share_1200 <- standard_news_token %>%
#   mutate(token = wordStem(token, language = "english")) %>%
#   mutate(un_ling_1200 = ifelse(token %in% dem_ling_1200$token, 1, 0)) %>%
#   group_by(un_ling_1200, rowid, year) %>%
#   count() %>%
#   spread(un_ling_1200, n) %>%
#   mutate(un_ling_1200 = `1`/(`0`+`1`)) %>%
#   ungroup() %>%
#   select(-`1`, -`0`) %>%
#   left_join(news_text %>% select(rowid, name) %>% unique(), by = join_by(rowid))
# 
# # Merge
# 
# robustness_shares_unga <- standard_news_text %>%
#   left_join(unga_share_200, by = join_by(year, name, rowid)) %>%
#   left_join(unga_share_600, by = join_by(year, name, rowid)) %>%
#   left_join(unga_share_1200, by = join_by(year, name, rowid)) %>%
#   unique()
# 
# mean_share_unga_robust <- robustness_shares_unga %>%
#   group_by(name) %>%
#   summarise(mean_share_unga_200 = mean(un_ling_200),
#             mean_share_unga_600 = mean(un_ling_600),
#             mean_share_unga_1200 = mean(un_ling_1200))
# 
# output_dem_leg_robust <- robustness_shares_unga %>%
#   inner_join(mean_share_unga_robust, by = join_by(name)) %>%
#   ungroup() %>%
#   distinct(name, stdno, year, date, .keep_all = TRUE)
# 
# mod1 <- feols(mean_share_unga_200 ~ societal| year + committee,
#               cluster = "year+committee",
#               data = output_dem_leg_robust)
# 
# mod2 <- feols(mean_share_unga_600 ~ societal | year + committee,
#               cluster = "year+committee",
#               data = output_dem_leg_robust)
# 
# mod3 <- feols(mean_share_unga_1200 ~ societal | year + committee,
#               cluster = "year+committee",
#               data = output_dem_leg_robust)
# 
# cm <- c("physical" = "Physical",
#         "societal" = "Societal")
# 
# models <- list(mod1, mod2, mod3)
# 
# names(models) <- c("Cutoff 200", "Cutoff 600", "Cutoff 1200")
# 
# mod <- modelsummary(models,
#                     stars = TRUE,
#                     gof_omit = 'AIC|BIC|Within|Std.Errors|RMSE|FE',
#                     fmt = 3,
#                     coef_map = cm,
#                     format = "latex",
#                     statistic = c("{std.error}"),
#                     notes = c("Fixed effects: Year and committee",
#                               "Clustered standard errors by committee and year",
#                               "Coverage: 1999 - 2022")) %>%
#   kable_styling(font_size = 9, full_width = FALSE) %>%
#   add_header_above(c(" " = 1, "Percent democratic legitimation words" = 3))
# 
# mod

#save_kable(mod, file = "./Tables/iso_mod_robust_unga.tex")

## Clean


rm(list=ls()[! ls() %in% c("coef_plot_input_dem", "coef_plot_input_tech", "coef_plot_output_dem", "coef_plot_output_tech",
                           "liaison_dat_merged_sh", "liaison_dat_merged_frac", "participation_dat_merged_sh", "participation_dat_merged_frac",
                           "tech_leg",
                           "output_tech_leg", "output_dem_leg",
                           "legitimacy_coding",
                           "pvalues_techin", "pvalues_demin", "pvalues_techout", "pvalues_demout",
                           "mod3_techout_p", "mod3_techout_s", "mod6_demout_p", "mod6_demout_s")])
gc()

