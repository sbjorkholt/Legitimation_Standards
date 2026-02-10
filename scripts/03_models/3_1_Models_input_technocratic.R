
#### 3.1 MODEL INPUT LEGITIMATION TECHNOCRATIC ####

## Packages and data

pacman::p_load(DBI, RSQLite, dplyr, readr, stringr, fixest, tidyr, modelsummary, kableExtra, slider)
options(modelsummary_factory_default = 'kableExtra')

con <- dbConnect(RSQLite::SQLite(), "../../../Legitimacy/Data/iso_standards.sqlite") 

standards2 <- readRDS("../../../Legitimacy/Data/standards2.rds")
participation <- dbReadTable(con, "participants")
organzation_type <- read_rds("../../../Legitimacy/Data/organizations_tagged.rds")

dbDisconnect(con)

## Data generation

# oecd_data <- read.csv("../../../Legitimacy/Data/rndoecd.csv") %>%
#   filter(INDICATOR %in% c("GDEXPRD", "RESEARCHER")) %>%
#   filter(SUBJECT %in% c("TOT")) %>%
#   filter(MEASURE %in% c("1000EMPLOYED", "PC_GDP")) %>%
#   select(Country, INDICATOR, TIME_PERIOD, OBS_VALUE) %>%
#   spread(INDICATOR, OBS_VALUE) %>%
#   mutate(country = as.character(countrycode::countrycode(Country,
#                                                          origin = "country.name",
#                                                          destination = "country.name"))) %>%
#   mutate(country = ifelse(Country == "Türkiye", "Turkey", country)) %>%
#   rename(year = TIME_PERIOD)

rndgdp <- read.csv("../../../Legitimacy/Data/rndgdp.csv") %>%
  mutate(country = as.character(countrycode::countrycode(`Country.Name`,
                                                         origin = "country.name",
                                                         destination = "country.name"))) %>%
  gather(5:68, 
         key = "year", value = "rndgdp") %>%
  mutate(year = as.numeric(str_remove_all(year, "X"))) %>%
  select(country, year, rndgdp) %>%
  drop_na(country) %>%
  filter(year %in% 1999:2022)

researchers <- read.csv("../../../Legitimacy/Data/researchers.csv") %>%
  mutate(country = as.character(countrycode::countrycode(`Country.Name`,
                                                         origin = "country.name",
                                                         destination = "country.name"))) %>%
  gather(5:68, 
         key = "year", value = "researchers") %>%
  mutate(year = as.numeric(str_remove_all(year, "X"))) %>%
  select(country, year, researchers) %>%
  drop_na(country) %>%
  filter(year %in% 1999:2022)

gii_data <- readxl::read_excel("../../../Legitimacy/Data/WIPO-GII.xlsx") %>%
  mutate(country = as.character(countrycode::countrycode(`Economy Name`,
                                                         origin = "country.name",
                                                         destination = "country.name"))) %>%
  mutate(country = ifelse(country == "Turkiye", "Turkey", country)) %>%
  filter(`Indicator ID` %in% c("WIPO.GII.103", "WIPO.GII.107", "WIPO.GII.118", "WIPO.GII.159", "WIPO.GII.182", "WIPO.GII.236")) %>%
  filter(`Attribute 1` == "Score") %>%
  gather(`2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`,
         key = "year", value = "score") %>%
  select(country, year, Indicator, score) %>%
  spread(Indicator, score) %>%
  rename("businessgerd" = "Global Innovation Index: GERD performed by business, % GDP",
         "hightechexp" = "Global Innovation Index: High-tech exports, % total trade",
         "knowledgeemp" = "Global Innovation Index: Knowledge-intensive employment, %",
         "scienceart" = "Global Innovation Index: Scientific and technical articles/bn PPP$ GDP",
         "unicollab" = "Global Innovation Index: University-industry R&D collaboration") %>%
  mutate(year = as.numeric(year))

member_bodies <- participation %>%
  filter(membership %in% c("P-member", "Secretariat")) %>%
  mutate(continent = countrycode::countrycode(sourcevar = country,
                                              origin = "country.name",
                                              destination = "country.name"))  %>%
  mutate(committee = str_replace(committee, "(\\d+) /", "\\1/")) %>%
  mutate(committee = str_remove(committee, "ISO/IEC")) %>%
  mutate(committee = str_remove(committee, "ISO/")) %>%
  filter(!committee %in% c("JPC 2", "REMCO", "ASCO", "TMB/WG SR", "TMBG/CCCC", "TMBG/TF 7")) %>%
  filter(!str_detect(committee, "PC ")) %>%
  filter(year <= 2021 & year >= 2004)  %>%
  select(year, country, continent, committee, sector) %>%
  distinct(year, committee, continent, .keep_all = TRUE)  %>%
  group_by(year, committee) %>%
  add_count() %>%
  ungroup()

tech_leg0 <- member_bodies %>%
  #left_join(oecd_data, by = join_by(year, country)) %>%
  left_join(rndgdp, by = join_by(year, country)) %>%
  left_join(researchers, by = join_by(year, country)) %>%
  left_join(gii_data, by = join_by(year, country)) %>%
  left_join(standards2, by = join_by(year, committee)) %>%
  distinct(committee, year, country, .keep_all = TRUE) %>%
  drop_na(physical, societal) 

# tech_leg0 %>% select(country, year, rndgdp) %>% count(country, year) %>% nrow()
# tech_leg0 %>% select(country, year, rndgdp) %>% drop_na(rndgdp) %>% count(country, year) %>% nrow()
# 
# tech_leg0 %>% select(country, year, researchers) %>% count(country, year) %>% nrow()
# tech_leg0 %>% select(country, year, researchers) %>% drop_na(researchers) %>% count(country, year) %>% nrow()
# 
# tech_leg0 %>% select(country, year, hightechexp) %>% count(country, year) %>% nrow()
# tech_leg0 %>% select(country, year, hightechexp) %>% drop_na(hightechexp) %>% count(country, year) %>% nrow()
# 
# tech_leg0 %>% select(country, year, unicollab) %>% count(country, year) %>% nrow()
# tech_leg0 %>% select(country, year, unicollab) %>% drop_na(unicollab) %>% count(country, year) %>% nrow()
# 
# tech_leg0 %>% select(country, year, scienceart) %>% count(country, year) %>% nrow()
# tech_leg0 %>% select(country, year, scienceart) %>% drop_na(scienceart) %>% count(country, year) %>% nrow()
# 
# tech_leg0 %>% select(country, year, businessgerd) %>% count(country, year) %>% nrow()
# tech_leg0 %>% select(country, year, businessgerd) %>% drop_na(businessgerd) %>% count(country, year) %>% nrow()
# 
# tech_leg0 %>% select(country, year, knowledgeemp) %>% count(country, year) %>% nrow()
# tech_leg0 %>% select(country, year, knowledgeemp) %>% drop_na(knowledgeemp) %>% count(country, year) %>% nrow()

tech_leg <- tech_leg0 %>%
  group_by(committee, year, sector, n, physical, societal) %>%
  summarise(GDEXPRD = mean(rndgdp, na.rm = TRUE),
            RESEARCHER = mean(researchers, na.rm = TRUE),
            hightechexp = mean(hightechexp, na.rm = TRUE),
            unicollab = mean(unicollab, na.rm = TRUE),
            scienceart = mean(scienceart, na.rm = TRUE),
            businessgerd = mean(businessgerd, na.rm = TRUE),
            knowledgeemp = mean(knowledgeemp, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na(committee, year, physical, societal)

tech_leg <- tech_leg %>%
  dplyr::group_by(committee) %>%
  dplyr::arrange(year, .by_group = TRUE) %>%
  dplyr::group_modify(~ .x %>%
                        mutate(
                          GDEXPRD_lag      = slide_dbl(.x$GDEXPRD,      ~ mean(.x, na.rm = TRUE), .before = 2, .after = -1, .complete = TRUE),
                          RESEARCHER_lag   = slide_dbl(.x$RESEARCHER,   ~ mean(.x, na.rm = TRUE), .before = 2, .after = -1, .complete = TRUE),
                          hightechexp_lag  = slide_dbl(.x$hightechexp,  ~ mean(.x, na.rm = TRUE), .before = 2, .after = -1, .complete = TRUE),
                          unicollab_lag    = slide_dbl(.x$unicollab,    ~ mean(.x, na.rm = TRUE), .before = 2, .after = -1, .complete = TRUE),
                          scienceart_lag   = slide_dbl(.x$scienceart,   ~ mean(.x, na.rm = TRUE), .before = 2, .after = -1, .complete = TRUE),
                          businessgerd_lag = slide_dbl(.x$businessgerd, ~ mean(.x, na.rm = TRUE), .before = 2, .after = -1, .complete = TRUE),
                          knowledgeemp_lag = slide_dbl(.x$knowledgeemp, ~ mean(.x, na.rm = TRUE), .before = 2, .after = -1, .complete = TRUE)
                        )
  ) %>%
  dplyr::ungroup()

# tech_leg %>% select(RESEARCHER, year) %>% drop_na() %>% count()
# tech_leg %>% select(hightechexp, year) %>% drop_na() %>% count(year)
# tech_leg %>% select(unicollab, year) %>% drop_na() %>% count(year)
# tech_leg %>% select(scienceart, year) %>% drop_na() %>% count(year)
# tech_leg %>% select(businessgerd, year) %>% drop_na() %>% count(year)
# tech_leg %>% select(knowledgeemp, year) %>% drop_na() %>% count(year)

## Models

mod1 <- feols(GDEXPRD ~ physical | year + committee,
               cluster = "committee+year",
               data = tech_leg)
mod2_techin_p <- feols(RESEARCHER ~ physical  | year + committee,
               cluster = "committee+year",
               data = tech_leg)
mod3 <- feols(hightechexp ~ physical | year + committee,
              cluster = "committee+year",
              data = tech_leg)
mod4 <- feols(unicollab ~ physical | year + committee,
              cluster = "committee+year",
              data = tech_leg)
mod5 <- feols(scienceart ~ physical | year + committee,
              cluster = "committee+year",
              data = tech_leg)
mod6 <- feols(businessgerd ~ physical | year + committee,
              cluster = "committee+year",
              data = tech_leg)
mod7 <- feols(knowledgeemp ~ physical | year + committee,
              cluster = "committee+year",
              data = tech_leg)

# Extract p-values from each model
p1_techin_p <- summary(mod1)$coeftable[, "Pr(>|t|)"]
p2_techin_p <- summary(mod2_techin_p)$coeftable[, "Pr(>|t|)"]
p3_techin_p <- summary(mod3)$coeftable[, "Pr(>|t|)"]
p4_techin_p <- summary(mod4)$coeftable[, "Pr(>|t|)"]
p5_techin_p <- summary(mod5)$coeftable[, "Pr(>|t|)"]
p6_techin_p <- summary(mod6)$coeftable[, "Pr(>|t|)"]
p7_techin_p <- summary(mod7)$coeftable[, "Pr(>|t|)"]

models <- list(mod1, mod2_techin_p, mod3, mod4, mod5, mod6, mod7)

cm <- c("physical" = "Committee expertise")
       # "RESEARCHER" = "Committee expertise",
       # "hightechexp" = "Committee expertise",
       # "unicollab" = "Committee expertise",
       # "scienceart" = "Committee expertise",
       # "businessgerd" = "Committee expertise",
       # "knowledgeemp" = "Committee expertise")

names(models) <- c("R&D % of GDP", 
                   "Researchers per million people",
                   "High-tech exports, % total trade",
                   "University-industry R&D collaboration",
                   "Scientific and technical articles/bn PPP$ GDP",
                   "GERD performed by business, % GDP",
                   "Knowledge-intensive employment, %")

mod <- modelsummary(models,
                    stars = TRUE,
                    gof_omit = 'AIC|BIC|Within|Std.Errors|RMSE|FE',
                    fmt = 3,
                    coef_map = cm,
                    format = "latex",
                    statistic = c("{std.error}"),
                    notes = c("Fixed effects: Committee and year",
                              "Clustered standard errors by committee and year",
                              "Coverage: 2004 - 2021")) 


#mod %>%
#  add_header_above(c(" " = 1, "Number of physical standards produced in technical committee" = 7)) %>%
#  kable_styling(font_size = 9, full_width = FALSE)

#mod

#save_kable(mod, file = "./Tables/iso_mod1_tech.tex")

# Models with societal standards

mod1_s <- feols(GDEXPRD ~ societal | year + committee,
              cluster = "committee+year",
              data = tech_leg)
mod2_techin_s <- feols(RESEARCHER ~ societal | year + committee,
              cluster = "committee+year",
              data = tech_leg)
mod3_s <- feols(hightechexp ~ societal | year + committee,
              cluster = "committee+year",
              data = tech_leg)
mod4_s <- feols(unicollab ~ societal | year + committee,
              cluster = "committee+year",
              data = tech_leg)
mod5_s <- feols(scienceart ~ societal | year + committee,
              cluster = "committee+year",
              data = tech_leg)
mod6_s <- feols(businessgerd ~ societal | year + committee,
              cluster = "committee+year",
              data = tech_leg)
mod7_s <- feols(knowledgeemp ~ societal | year + committee,
              cluster = "committee+year",
              data = tech_leg)

# Extract p-values from each model
p1_techin_s <- summary(mod1_s)$coeftable[, "Pr(>|t|)"]
p2_techin_s <- summary(mod2_techin_s)$coeftable[, "Pr(>|t|)"]
p3_techin_s <- summary(mod3_s)$coeftable[, "Pr(>|t|)"]
p4_techin_s <- summary(mod4_s)$coeftable[, "Pr(>|t|)"]
p5_techin_s <- summary(mod5_s)$coeftable[, "Pr(>|t|)"]
p6_techin_s <- summary(mod6_s)$coeftable[, "Pr(>|t|)"]
p7_techin_s <- summary(mod7_s)$coeftable[, "Pr(>|t|)"]

models_s <- list(mod1_s, mod2_techin_s, mod3_s, mod4_s, mod5_s, mod6_s, mod7_s)

cm <- c("societal" = "Committee expertise")
        # "RESEARCHER" = "Committee expertise",
        # "hightechexp" = "Committee expertise",
        # "unicollab" = "Committee expertise",
        # "scienceart" = "Committee expertise",
        # "businessgerd" = "Committee expertise",
        # "knowledgeemp" = "Committee expertise")

names(models_s) <- c("R&D % of GDP", 
                   "Researchers per million people",
                   "High-tech exports, % total trade",
                   "University-industry R&D collaboration",
                   "Scientific and technical articles/bn PPP$ GDP",
                   "GERD performed by business, % GDP",
                   "Knowledge-intensive employment, %")

mod <- modelsummary(models_s,
                    stars = TRUE,
                    gof_omit = 'AIC|BIC|Within|Std.Errors|RMSE|FE',
                    fmt = 3,
                    coef_map = cm,
                    format = "latex",
                    statistic = c("{std.error}"),
                    notes = c("Fixed effects: Committee and year",
                              "Clustered standard errors by committee and year",
                              "Coverage: 2004 - 2021")) 

# mod %>%
#   add_header_above(c(" " = 1, "Number of societal standards produced in technical committee" = 7)) %>%
#   kable_styling(font_size = 9, full_width = FALSE)

#mod

#save_kable(mod, file = "./Tables/iso_mod1_tech_societal.tex")

pvalues_techin <- list(p1_techin_p, p2_techin_p, p3_techin_p, p4_techin_p, p5_techin_p, p6_techin_p, p7_techin_p,
                       p1_techin_s, p2_techin_s, p3_techin_s, p4_techin_s, p5_techin_s, p6_techin_s, p7_techin_s)


## Coefficient plot

# Physical

coef_gdp_physical <- broom::tidy(feols(GDEXPRD ~ physical | year + committee,
                                       cluster = "committee+year",
                                       data = tech_leg %>%
                                         drop_na(GDEXPRD) %>%
                                         mutate(physical = scale(physical, center = TRUE, scale = TRUE),
                                                GDEXPRD = scale(GDEXPRD, center = TRUE, scale = TRUE))),
                             conf.int = TRUE) %>% mutate(term = ifelse(term == "physical", "R&D", NA))

coef_researchers_physical <- broom::tidy(feols(RESEARCHER ~ physical | year + committee,
                                               cluster = "committee+year",
                                               data = tech_leg %>%
                                                 drop_na(RESEARCHER) %>%
                                                  mutate(physical = scale(physical, center = TRUE, scale = TRUE),
                                                         RESEARCHER = scale(RESEARCHER, center = TRUE, scale = TRUE))),
                                          conf.int = TRUE) %>% mutate(term = ifelse(term == "physical", "Researchers", NA))


coef_hightech_physical <- broom::tidy(feols(hightechexp ~ physical | year + committee,
                                               cluster = "committee+year",
                                               data = tech_leg %>%
                                                 drop_na(hightechexp) %>%
                                                 mutate(physical = scale(physical, center = TRUE, scale = TRUE),
                                                        hightechexp = scale(hightechexp, center = TRUE, scale = TRUE))),
                                         conf.int = TRUE) %>% mutate(term = ifelse(term == "physical", "High-tech export", NA))

coef_unicollab_physical <- broom::tidy(feols(unicollab ~ physical | year + committee,
                                               cluster = "committee+year",
                                               data = tech_leg %>%
                                                 drop_na(unicollab) %>%
                                                 mutate(physical = scale(physical, center = TRUE, scale = TRUE),
                                                        unicollab = scale(unicollab, center = TRUE, scale = TRUE))),
                                         conf.int = TRUE) %>% mutate(term = ifelse(term == "physical", "Uni-Industry collab.", NA))

coef_scienceart_physical <- broom::tidy(feols(scienceart ~ physical | year + committee,
                                               cluster = "committee+year",
                                               data = tech_leg %>%
                                                 drop_na(RESEARCHER) %>%
                                                 mutate(physical = scale(physical, center = TRUE, scale = TRUE),
                                                        scienceart = scale(scienceart, center = TRUE, scale = TRUE))),
                                         conf.int = TRUE) %>% mutate(term = ifelse(term == "physical", "Articles", NA))

coef_businessgerd_physical <- broom::tidy(feols(businessgerd ~ physical | year + committee,
                                               cluster = "committee+year",
                                               data = tech_leg %>%
                                                 drop_na(businessgerd) %>%
                                                 mutate(physical = scale(physical, center = TRUE, scale = TRUE),
                                                        businessgerd = scale(businessgerd, center = TRUE, scale = TRUE))),
                                         conf.int = TRUE) %>% mutate(term = ifelse(term == "physical", "Business GERD", NA))

coef_knowledgeemp_physical <- broom::tidy(feols(knowledgeemp ~ physical | year + committee,
                                               cluster = "committee+year",
                                               data = tech_leg %>%
                                                 drop_na(knowledgeemp) %>%
                                                 mutate(physical = scale(physical, center = TRUE, scale = TRUE),
                                                        knowledgeemp = scale(knowledgeemp, center = TRUE, scale = TRUE))),
                                         conf.int = TRUE) %>% mutate(term = ifelse(term == "physical", "Knowledge emp.", NA))

# Societal

coef_gdp_societal <- broom::tidy(feols(GDEXPRD ~ societal | year + committee,
                                       cluster = "committee+year",
                                       data = tech_leg %>%
                                         drop_na(GDEXPRD) %>%
                                         mutate(societal = scale(societal, center = TRUE, scale = TRUE),
                                                GDEXPRD = scale(GDEXPRD, center = TRUE, scale = TRUE))),
                                 conf.int = TRUE) %>% mutate(term = ifelse(term == "societal", "R&D", NA))

coef_researchers_societal <- broom::tidy(feols(RESEARCHER ~ societal | year + committee,
                                               cluster = "committee+year",
                                               data = tech_leg %>%
                                                 drop_na(RESEARCHER) %>%
                                                 mutate(societal = scale(societal, center = TRUE, scale = TRUE),
                                                        RESEARCHER = scale(RESEARCHER, center = TRUE, scale = TRUE))),
                                         conf.int = TRUE) %>% mutate(term = ifelse(term == "societal", "Researchers", NA))

coef_hightech_societal <- broom::tidy(feols(hightechexp ~ societal | year + committee,
                                            cluster = "committee+year",
                                            data = tech_leg %>%
                                              drop_na(hightechexp) %>%
                                              mutate(societal = scale(societal, center = TRUE, scale = TRUE),
                                                     hightechexp = scale(hightechexp, center = TRUE, scale = TRUE))),
                                      conf.int = TRUE) %>% mutate(term = ifelse(term == "societal", "High-tech export", NA))

coef_unicollab_societal <- broom::tidy(feols(unicollab ~ societal | year + committee,
                                             cluster = "committee+year",
                                             data = tech_leg %>%
                                               drop_na(unicollab) %>%
                                               mutate(societal = scale(societal, center = TRUE, scale = TRUE),
                                                      unicollab = scale(unicollab, center = TRUE, scale = TRUE))),
                                       conf.int = TRUE) %>% mutate(term = ifelse(term == "societal", "Uni-Industry collab.", NA))

coef_scienceart_societal <- broom::tidy(feols(scienceart ~ societal | year + committee,
                                              cluster = "committee+year",
                                              data = tech_leg %>%
                                                drop_na(RESEARCHER) %>%
                                                mutate(societal = scale(societal, center = TRUE, scale = TRUE),
                                                       scienceart = scale(scienceart, center = TRUE, scale = TRUE))),
                                        conf.int = TRUE) %>% mutate(term = ifelse(term == "societal", "Articles", NA))

coef_businessgerd_societal <- broom::tidy(feols(businessgerd ~ societal | year + committee,
                                                cluster = "committee+year",
                                                data = tech_leg %>%
                                                  drop_na(businessgerd) %>%
                                                  mutate(societal = scale(societal, center = TRUE, scale = TRUE),
                                                         businessgerd = scale(businessgerd, center = TRUE, scale = TRUE))),
                                          conf.int = TRUE) %>% mutate(term = ifelse(term == "societal", "Business GERD", NA))

coef_knowledgeemp_societal <- broom::tidy(feols(knowledgeemp ~ societal | year + committee,
                                                cluster = "committee+year",
                                                data = tech_leg %>%
                                                  drop_na(knowledgeemp) %>%
                                                  mutate(societal = scale(societal, center = TRUE, scale = TRUE),
                                                         knowledgeemp = scale(knowledgeemp, center = TRUE, scale = TRUE))),
                                          conf.int = TRUE) %>% mutate(term = ifelse(term == "societal", "Knowledge emp.", NA))


coef_plot_input_tech <- bind_rows(
  coef_gdp_physical %>% mutate(dv = "R&D", model = "Technocratic input legitimation", standard = "physical"),
  coef_researchers_physical %>% mutate(dv = "Researchers", model = "Technocratic input legitimation", standard = "physical"),
  coef_hightech_physical %>% mutate(dv = "High-tech export", model = "Technocratic input legitimation", standard = "physical"),
  coef_unicollab_physical %>% mutate(dv = "Uni-Industry collab.", model = "Technocratic input legitimation", standard = "physical"),
  coef_scienceart_physical %>% mutate(dv = "Articles", model = "Technocratic input legitimation", standard = "physical"),
  coef_businessgerd_physical %>% mutate(dv = "Business GERD", model = "Technocratic input legitimation", standard = "physical"),
  coef_knowledgeemp_physical %>% mutate(dv = "Knowledge emp.", model = "Technocratic input legitimation", standard = "physical")) %>%
  bind_rows(coef_gdp_societal %>% mutate(dv = "R&D", model = "Technocratic input legitimation", standard = "societal"),
            coef_researchers_societal %>% mutate(dv = "Researchers", model = "Technocratic input legitimation", standard = "societal"),
            coef_hightech_societal %>% mutate(dv = "High-tech export", model = "Technocratic input legitimation", standard = "societal"),
            coef_unicollab_societal %>% mutate(dv = "Uni-Industry collab.", model = "Technocratic input legitimation", standard = "societal"),
            coef_scienceart_societal %>% mutate(dv = "Articles", model = "Technocratic input legitimation", standard = "societal"),
            coef_businessgerd_societal %>% mutate(dv = "Business GERD", model = "Technocratic input legitimation", standard = "societal"),
            coef_knowledgeemp_societal %>% mutate(dv = "Knowledge emp.", model = "Technocratic input legitimation", standard = "societal"))


## Clean 

rm(list=ls()[! ls() %in% c("coef_plot_input_dem", "coef_plot_input_tech", "coef_plot_output_dem", "coef_plot_output_tech",
                           "liaison_dat_merged_sh", "liaison_dat_merged_frac", "participation_dat_merged_sh", "participation_dat_merged_frac",
                           "tech_leg",
                           "output_tech_leg", "output_dem_leg",
                           "legitimacy_coding",
                           "pvalues_techin", "pvalues_demin", "pvalues_techout", "pvalues_demout",
                           "mod2_techin_s", "mod2_techin_p",
                           "mod2_demin_s_country", "mod4_demin_s_liaison", "mod2_demin_p_country", "mod4_demin_p_liaison")])

gc()
