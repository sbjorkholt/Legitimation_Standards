
library(DBI)
library(RSQLite)
library(tidyverse)
library(fixest)

#### SDG ####

con <- dbConnect(RSQLite::SQLite(), "../../../ISO-standards/iso_standards.sqlite")

sdgs <- dbReadTable(con, "standards_sdgs")
standards3 <- readRDS("../../../Legitimacy/Data/standards3.rds")

sdgs_dat1 <- sdgs %>%
  select(stdno, name, sdg_text, year) %>% 
  mutate(sdg_text = ifelse(is.na(sdg_text), 0, sdg_text),
         social_sdg = ifelse(sdg_text == 0, 0, 1)) %>%
  unique() %>%
  na.omit() %>%
  mutate(year = as.numeric(year)) %>%
  filter(year < 2022) %>%
  inner_join(standards3 %>%
               mutate(year = as.numeric(year)), by = join_by(stdno, name, year)) %>% 
  na.omit() %>% 
  unique() %>%
  mutate(pages = as.numeric(pages))


#### TC ####

standards <- dbReadTable(con, "standards_status")

sectors <- readRDS("../../../Legitimacy/Data/sectors.rds") %>%
  unnest() %>%
  ungroup()

standards <- left_join(standards, sectors, by = join_by(committee))

na_sectors <- standards %>%
  distinct() %>%
  filter(is.na(sector))

na_sectors_filled <- na_sectors %>%
  select(-sector) %>%
  mutate(committee2 = str_remove(committee, "\\/SC.*")) %>%
  left_join(sectors, by = c("committee2" = "committee")) %>%
  select(-committee2)

standards <- standards %>%
  anti_join(na_sectors, by = join_by(committee, title)) %>%
  bind_rows(na_sectors_filled)

standards_tc <- standards %>% 
  unique() %>%
  na.omit() %>%
  mutate(year = as.numeric(year)) %>%
  filter(year < 2022) %>%
  inner_join(standards3 %>%
               select(stdno, name, year, physical, societal) %>%
               mutate(year = as.numeric(year))) %>% 
  na.omit() %>% 
  unique() %>%
  mutate(sector_social = ifelse(sector %in% c("Sustainability and environment",
                                              "Business management and innovation",
                                              "Services",
                                              "Security, safety and risk"), 1, 0))

dbDisconnect(con)

#### MODELS ####

merge <- sdgs_dat1 %>%
  select(stdno, year, name, sdg_text, social_sdg, physical, societal) %>%
  left_join(standards_tc %>% select(stdno, year, name, sector_social), by = join_by(stdno, year, name))

merge <- merge %>% 
  select(social_sdg, sector_social, societal) %>% 
  drop_na(sector_social) %>%
  rename("Social standard" = societal,
         "Social TC origin" = sector_social,
         "Standard answers to SDG" = social_sdg)
melted_corr_mat <- reshape2::melt(round(cor(merge),3))
melted_corr_mat <- melted_corr_mat[which(melted_corr_mat$Var1 != melted_corr_mat$Var2),]

ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=round(value, 2))) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "skyblue", mid = "white") +
  geom_text(aes(Var2, Var1, label = round(value, 2)),
            color = "black", size = 8) + 
  labs(x = "", y = "") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    legend.position = "none",
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

ggsave("./Figures/val_corrplot.pdf", width = 8, height = 8)
