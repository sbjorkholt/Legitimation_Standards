
#### 3.2 MODEL INPUT LEGITIMATION DEMOCRATIC ####

## Packages and data 

pacman::p_load(DBI, RSQLite, dplyr, readr, stringr, purrr, fixest, tidyr, modelsummary, kableExtra)
options(modelsummary_factory_default = 'kableExtra')

con <- dbConnect(RSQLite::SQLite(), "../../../Legitimacy/Data/iso_standards.sqlite") 

standards2 <- readRDS("../../../Legitimacy/Data/standards2.rds")
participation <- dbReadTable(con, "participants")
liaison <- dbReadTable(con, "liaison")
organzation_type <- read_rds("../../../Legitimacy/Data/organizations_tagged.rds")

dbDisconnect(con)

## Data generation

member_bodies <- participation %>%
  filter(membership %in% c("P-member", "Secretariat")) %>%
  mutate(continent = countrycode::countrycode(sourcevar = country,
                                              origin = "country.name",
                                              destination = "un.region.name")) %>% 
  # un.regionsub.name - use this to estimate with a more fine-grained category scheme
  mutate(committee = str_replace(committee, "(\\d+) /", "\\1/")) %>%
  mutate(committee = str_remove(committee, "ISO/IEC")) %>%
  mutate(committee = str_remove(committee, "ISO/")) %>%
  filter(!committee %in% c("JPC 2", "REMCO", "ASCO", "TMB/WG SR", "TMBG/CCCC", "TMBG/TF 7")) %>%
  filter(!str_detect(committee, "PC ")) %>%
  filter(year <= 2022 & year >= 2004)  %>%
  group_by(year, continent, committee) %>%
  add_count() %>%
  ungroup() %>%
  group_by(year, committee) %>%
  add_count() %>%
  ungroup() %>%
  mutate(share = n/nn) %>% # Estimating number of countries from each region per TC and year
  select(year, continent, committee, sector, share) %>%
  distinct(year, committee, continent, .keep_all = TRUE)

liaison_org <- liaison %>%
  mutate(acronym = str_squish(acronym)) %>%
  left_join(organzation_type %>% mutate(name = str_remove_all(name, "Name: ")),
            by = join_by(name), relationship = "many-to-many") %>%
  mutate(committee = str_replace(committee, "(\\d+) /", "\\1/")) %>%
  mutate(committee = str_remove(committee, "ISO/IEC")) %>%
  mutate(committee = str_remove(committee, "ISO/")) %>%
  filter(!committee %in% c("JPC 2", "REMCO", "ASCO", "TMB/WG SR", "TMBG/CCCC", "TMBG/TF 7")) %>%
  filter(!str_detect(committee, "PC ")) %>%
  filter(year <= 2022 & year >= 2004)  %>%
  group_by(year, category_name, committee) %>%
  add_count() %>%
  ungroup() %>%
  group_by(year, committee) %>%
  add_count() %>%
  ungroup() %>%
  mutate(share = n/nn) %>%
  select(year, category_name, committee, sector, share) %>%
  distinct(year, committee, category_name, .keep_all = TRUE)

## Liaison organizations and member bodies in each category
member_bodies %>%
  group_by(continent) %>%
  count()

liaison_org %>%
  group_by(category_name) %>%
  count()

## Average number of sector and continent represented in each TC
member_bodies %>%
  group_by(committee, year) %>%
  count() %>%
  ungroup() %>%
  summarise(mean = mean(n))

liaison_org %>%
  group_by(committee, year) %>%
  count() %>%
  ungroup() %>%
  summarise(mean = mean(n))

## Fractionalization index

fractionalization <- function(shares, type) {
  
  if (type == "region") {
    # Number of regions
    N <- 5 
  } else if (type == "sector") {
    # Number of organizational types
    N <- 10 
  } else {
    message("Please specify type, either 'region' or 'sector'.")
    return(NULL)
  }
  
  # Compute the normalized Inverse Herfindahl Index
  ni_frac <- (1 - sum(shares^2) - 1/N) / (1 - 1/N)
  
  return(ni_frac)
}

participation_dat_frac <- member_bodies %>%
  group_by(year, committee, sector) %>%
  summarise(frac = fractionalization(share, type = "region")) %>%
  ungroup() %>%
  distinct(year, committee, .keep_all = TRUE)

liaison_dat_frac <- liaison_org %>%
  group_by(year, committee, sector) %>%
  summarise(frac = fractionalization(share, type = "sector")) %>%
  ungroup() %>%
  distinct(year, committee, .keep_all = TRUE)

participation_dat_merged_frac <- participation_dat_frac %>%
  left_join(standards2, by = join_by(year, committee)) %>%
  mutate(year = as.numeric(year)) %>%
  drop_na(physical, societal) %>%
  distinct(year, committee, .keep_all = TRUE)

liaison_dat_merged_frac <- liaison_dat_frac %>%
  left_join(standards2, by = join_by(year, committee)) %>%
  mutate(year = as.numeric(year)) %>%
  drop_na(physical, societal) %>%
  distinct(year, committee, .keep_all = TRUE)


## Shannon index

shannon_index <- function(shares, type) {
  
  if (type == "region") {
    # Number of regions
    N <- 5 
  } else if (type == "sector") {
    # Number of organizational types
    N <- 10
  } else {
    message("Please specify type, either 'region' or 'sector'.")
    return(NULL)
  }
  
  shannon <- -sum(shares * log(shares))
  max_shannon <- log(N)
  normalized_index <- shannon / max_shannon
  
  return(normalized_index)
}

participation_dat_sh <- member_bodies  %>%
  mutate(id = paste0(year, "_", committee)) %>%
  as.data.frame() %>%
  select(year, committee, continent, sector, share, id)

liaison_dat_sh <- liaison_org %>%
  mutate(id = paste0(year, "_", committee)) %>%
  as.data.frame() %>%
  select(year, committee, category_name, sector, share, id)

shannon_index_estimate <- function(df, type){
  shannon_index = shannon_index(df %>% as.data.frame(.) %>% select("share"), type = type)
  id = df %>% pluck("id") %>% head(1)
  data.frame(id, shannon_index)
}

sh_est <- participation_dat_sh %>%
  group_split(id, .keep=TRUE) %>%
  map(shannon_index_estimate, type = "region") %>%
  bind_rows() %>%
  mutate(year = as.numeric(str_remove(str_extract(id, "[0-9]+_"), "_")),
         committee = str_remove(str_extract(id, "_.*"), "_")) %>%
  select(-id)

participation_dat_merged_sh <- sh_est %>%
  ungroup() %>%
  distinct(.keep_all = TRUE) %>%
  left_join(member_bodies %>% select(committee, year, sector), by = join_by(year, committee)) %>%
  left_join(standards2, by = join_by(year, committee)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year <= 2022) %>%
  mutate(share_physical = physical/(physical+societal),
         share_societal = societal/(physical+societal)) %>%
  filter(!is.nan(share_physical)) %>%
  filter(!is.nan(share_societal)) %>%
  distinct(year, committee, .keep_all = TRUE)

sh_est_liaison <- liaison_dat_sh %>%
  group_split(id, .keep=TRUE) %>%
  map(shannon_index_estimate, type = "sector") %>%
  bind_rows() %>%
  mutate(year = as.numeric(str_remove(str_extract(id, "[0-9]+_"), "_")),
         committee = str_remove(str_extract(id, "_.*"), "_")) %>%
  select(-id)

liaison_dat_merged_sh <- sh_est_liaison %>%
  ungroup() %>%
  distinct(.keep_all = TRUE) %>%
  left_join(liaison %>% select(committee, year, sector), by = join_by(year, committee)) %>%
  left_join(standards2, by = join_by(year, committee)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year <= 2022) %>%
  mutate(share_physical = physical/(physical+societal),
         share_societal = societal/(physical+societal)) %>%
  filter(!is.nan(share_physical)) %>%
  filter(!is.nan(share_societal)) %>%
  distinct(year, committee, .keep_all = TRUE)

participation_dat_merged_frac <- participation_dat_merged_frac %>%
  dplyr::group_by(committee) %>%
  dplyr::arrange(year, .by_group = TRUE) %>%
  dplyr::group_modify(~ .x %>%
                        mutate(
                          frac_lag = slide_dbl(.x$frac,      ~ mean(.x, na.rm = TRUE), .before = 2, .after = -1, .complete = TRUE))) %>%
  dplyr::ungroup()

participation_dat_merged_sh <- participation_dat_merged_sh %>%
  dplyr::group_by(committee) %>%
  dplyr::arrange(year, .by_group = TRUE) %>%
  dplyr::group_modify(~ .x %>%
                        mutate(
                          shannon_lag = slide_dbl(.x$shannon_index,      ~ mean(.x, na.rm = TRUE), .before = 2, .after = -1, .complete = TRUE))) %>%
  dplyr::ungroup()

liaison_dat_merged_frac <- liaison_dat_merged_frac %>%
  dplyr::group_by(committee) %>%
  dplyr::arrange(year, .by_group = TRUE) %>%
  dplyr::group_modify(~ .x %>%
                        mutate(
                          frac_lag = slide_dbl(.x$frac,      ~ mean(.x, na.rm = TRUE), .before = 2, .after = -1, .complete = TRUE))) %>%
  dplyr::ungroup()

liaison_dat_merged_sh <- liaison_dat_merged_sh %>%
  dplyr::group_by(committee) %>%
  dplyr::arrange(year, .by_group = TRUE) %>%
  dplyr::group_modify(~ .x %>%
                        mutate(
                          shannon_lag = slide_dbl(.x$shannon_index,      ~ mean(.x, na.rm = TRUE), .before = 2, .after = -1, .complete = TRUE))) %>%
  dplyr::ungroup()



## Models

mod1 <- feols(frac ~ societal + frac_lag | year + committee,
              cluster = "committee+year",
              data = participation_dat_merged_frac)

mod2_demin_s_country <- feols(shannon_index ~ societal | year + committee,
              cluster = "committee+year",
              data = participation_dat_merged_sh)

mod3 <- feols(frac ~ societal + frac_lag | year + committee,
              cluster = "committee+year",
              data = liaison_dat_merged_frac)

mod4_demin_s_liaison <- feols(shannon_index ~ societal | year + committee,
              cluster = "committee+year",
              data = liaison_dat_merged_sh)

# Extract p-values from each model
p1_demin_s <- summary(mod1)$coeftable[, "Pr(>|t|)"]
p2_demin_s <- summary(mod2_demin_s_country)$coeftable[, "Pr(>|t|)"]
p3_demin_s <- summary(mod3)$coeftable[, "Pr(>|t|)"]
p4_demin_s <- summary(mod4_demin_s_liaison)$coeftable[, "Pr(>|t|)"]

cm <- c("societal" = "Committee diversity")
        #"shannon_index" = "Committee diversity")

mods <- list(mod1, mod2_demin_s_country, mod3, mod4_demin_s_liaison)

names(mods) <- c("Rae Index", "Shannon H",
                 "Rae Index", "Shannon H")

mods <- modelsummary(mods,
                    stars = TRUE,
                    gof_omit = 'AIC|BIC|Within|Std.Errors|RMSE|FE',
                    fmt = 5,
                    #coef_map = cm,
                    format = "latex",
                    statistic = c("{std.error}"),
                    notes = c("Fixed effects: Year and committee",
                              "Clustered standard errors by committee and year",
                              "Coverage: 2004 - 2022")) 

#mods %>%
#  kable_styling(font_size = 10, full_width = FALSE) %>%
#  add_header_above(c(" " = 1, "Region" = 2, "Sector" = 2)) %>%
#  add_header_above(c(" " = 1, "Number of societal standards produced in technical committee" = 4))

mods

#save_kable(mods, file = "./Tables/iso_mod1_dem.tex")

## Models with physical standards

mod1 <- feols(frac ~ physical + frac_lag | year + committee,
              cluster = "committee+year",
              data = participation_dat_merged_frac)

mod2_demin_p_country <- feols(shannon_index ~ physical | year + committee,
              cluster = "committee+year",
              data = participation_dat_merged_sh)

mod3_p <- feols(frac ~ physical + frac_lag | year + committee,
              cluster = "committee+year",
              data = liaison_dat_merged_frac)

mod4_demin_p_liaison <- feols(shannon_index ~ physical | year + committee,
              cluster = "committee+year",
              data = liaison_dat_merged_sh)

# Extract p-values from each model
p1_demin_p <- summary(mod1)$coeftable[, "Pr(>|t|)"]
p2_demin_p <- summary(mod2_demin_p_country)$coeftable[, "Pr(>|t|)"]
p3_demin_p <- summary(mod3_p)$coeftable[, "Pr(>|t|)"]
p4_demin_p <- summary(mod4_demin_p_liaison)$coeftable[, "Pr(>|t|)"]

cm <- c("physical" = "Committee diversity")
        #"shannon_index" = "Committee diversity")

mods_p <- list(mod1, mod2_demin_p_country, mod3_p, mod4_demin_p_liaison)

names(mods_p) <- c("Rae Index", "Shannon H",
                 "Rae Index", "Shannon H")

mods_p <- modelsummary(mods_p,
                     stars = TRUE,
                     gof_omit = 'AIC|BIC|Within|Std.Errors|RMSE|FE',
                     fmt = 5,
                     #coef_map = cm,
                     format = "latex",
                     statistic = c("{std.error}"),
                     notes = c("Fixed effects: Year and committee",
                               "Clustered standard errors by committee and year",
                               "Coverage: 2004 - 2022")) 

#mods_p %>%
#  add_header_above(c(" " = 1, "Region" = 2, "Sector" = 2)) %>%
#  add_header_above(c(" " = 1, "Number of physical standards produced in technical committee" = 4)) %>%
#  kable_styling(font_size = 10, full_width = FALSE)

mods_p

#save_kable(mods_p, file = "./Tables/iso_mod1_dem_physical.tex")

pvalues_demin <- list(p1_demin_p, p2_demin_p, p3_demin_p, p4_demin_s,
                       p1_demin_s, p2_demin_s, p3_demin_s, p4_demin_s)



## Coefficient plot

# Societal

coef_frac_societal <- broom::tidy(feols(frac ~ societal | year + committee,
                                        cluster = "committee+year",
                                        data = participation_dat_merged_frac %>%
                                          mutate(societal = scale(societal, center = TRUE, scale = TRUE),
                                                 frac = scale(frac, center = TRUE, scale = TRUE))),
                                  conf.int = TRUE) %>% mutate(term = ifelse(term == "societal", "Rae Index: Region", NA))

coef_frac_societal_liaison <- broom::tidy(feols(frac ~ societal | year + committee,
                                                cluster = "committee+year",
                                                data = liaison_dat_merged_frac %>%
                                                  mutate(societal = scale(societal, center = TRUE, scale = TRUE),
                                                         frac = scale(frac, center = TRUE, scale = TRUE))),
                                          conf.int = TRUE) %>% mutate(term = ifelse(term == "societal", "Rae Index: Sector", NA))

coef_hhi_societal <- broom::tidy(feols(shannon_index ~ societal | year + committee,
                                       cluster = "committee+year",
                                       data = participation_dat_merged_sh %>%
                                         mutate(societal = scale(societal, center = TRUE, scale = TRUE),
                                                shannon_index = scale(shannon_index, center = TRUE, scale = TRUE))), 
                                 conf.int = TRUE) %>% mutate(term = ifelse(term == "societal", "Shannon H: Region", NA))

coef_hhi_societal_liaison <- broom::tidy(feols(shannon_index ~ societal | year + committee,
                                               cluster = "committee+year",
                                               data = liaison_dat_merged_sh %>% 
                                                 mutate(societal = scale(societal, center = TRUE, scale = TRUE),
                                                        shannon_index = scale(shannon_index, center = TRUE, scale = TRUE))),
                                         conf.int = TRUE) %>% mutate(term = ifelse(term == "societal", "Shannon H: Sector", NA))

# Physical

coef_frac_physical <- broom::tidy(feols(frac ~ physical | year + committee,
                                        cluster = "committee+year",
                                        data = participation_dat_merged_frac %>%
                                          mutate(physical = scale(physical, center = TRUE, scale = TRUE),
                                                 frac = scale(frac, center = TRUE, scale = TRUE))),
                                  conf.int = TRUE) %>% mutate(term = ifelse(term == "physical", "Rae Index: Region", NA))

coef_frac_physical_liaison <- broom::tidy(feols(frac ~ physical | year + committee,
                                                cluster = "committee+year",
                                                data = liaison_dat_merged_frac %>%
                                                  mutate(physical = scale(physical, center = TRUE, scale = TRUE),
                                                         frac = scale(frac, center = TRUE, scale = TRUE))),
                                          conf.int = TRUE) %>% mutate(term = ifelse(term == "physical", "Rae Index: Sector", NA))

coef_hhi_physical <- broom::tidy(feols(shannon_index ~ physical | year + committee,
                                       cluster = "committee+year",
                                       data = participation_dat_merged_sh %>%
                                         mutate(physical = scale(physical, center = TRUE, scale = TRUE),
                                                shannon_index = scale(shannon_index, center = TRUE, scale = TRUE))), 
                                 conf.int = TRUE) %>% mutate(term = ifelse(term == "physical", "Shannon H: Region", NA))

coef_hhi_physical_liaison <- broom::tidy(feols(shannon_index ~ physical | year + committee,
                                               cluster = "committee+year",
                                               data = liaison_dat_merged_sh %>% 
                                                 mutate(physical = scale(physical, center = TRUE, scale = TRUE),
                                                        shannon_index = scale(shannon_index, center = TRUE, scale = TRUE))),
                                         conf.int = TRUE) %>% mutate(term = ifelse(term == "physical", "Shannon H: Sector", NA))

coef_plot_input_dem <- bind_rows(
  coef_frac_societal %>% mutate(dv = "Region Rae Index", model = "Democratic input legitimation", standard = "societal"),
  coef_hhi_societal %>% mutate(dv = "Region Shannon H", model = "Democratic input legitimation", standard = "societal"),
  coef_frac_societal_liaison %>% mutate(dv = "Sector Rae Index", model = "Democratic input legitimation", standard = "societal"),
  coef_hhi_societal_liaison %>% mutate(dv = "Sector Shannon H", model = "Democratic input legitimation", standard = "societal")) %>%
  bind_rows(coef_frac_physical %>% mutate(dv = "Region Rae Index", model = "Democratic input legitimation", standard = "physical"),
            coef_hhi_physical %>% mutate(dv = "Region Shannon H", model = "Democratic input legitimation", standard = "physical"),
            coef_frac_physical_liaison %>% mutate(dv = "Sector Rae Index", model = "Democratic input legitimation", standard = "physical"),
            coef_hhi_physical_liaison %>% mutate(dv = "Sector Shannon H", model = "Democratic input legitimation", standard = "physical"))


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

