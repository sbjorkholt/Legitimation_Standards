
library(tidyverse)
library(RSelenium)
library(rvest)

# 1. Open Powershell as administrator and start Docker as administrator.
# 2. Ensure that a docker container with chrome image is running. Write "docker ps" to check.
# 3. If "selenium/standalone-chrome" is not there, write "docker start faa1d0124355" (or another image-id, see "docker ps -a").
# (If this does not work, the container is probably gone. Write "docker run -d -p 5983:4444 selenium/standalone-chrome") to make a new one.
# (If this neither works, then the image is probably gone. Then you have to start from scratch by downloading the image. Follow this: https://rpubs.com/johndharrison/RSelenium-Docker)

# 4. Now, the virtual server is up and running. Run the script below.

# CHROME: 5983L -- using Chrome, as it requires less RAM
# FIREFOX: 6496L -- not using Firefox

remDr <- remoteDriver(port = 4445, # Setting up server on this port (Selenium typically runs on 4444) 
                      remoteServerAddr = "localhost", browserName = "chrome") # Not necessary to specify the last two, but doing it here.

remDr$open() # Opening the server.

url <- "https://www.iso.org/news.html"

remDr$navigate(url) # Entering webpage

# Checking that we're on the right spot.

# Clicking away the Cookies button #
remDr$getTitle()
remDr$screenshot(display = TRUE)

click <- remDr$findElements(using = "xpath", 
                           value = "//button[starts-with(@id, 'onetrust')]")

unlist(lapply(click, function(x) {x$getElementText()}))

click[[2]]$clickElement()
Sys.sleep(10) # wait for page loading

remDr$screenshot(display = TRUE)

# Directing the mouse to the area we want to extract using xpath. 
# Go into the webpage, click inspect on the part you want to scrape, find the area you want in the html-code ans select "copy -> XPath". 

get_links <- function(x){
  
  elem <- remDr$findElements(using = "xpath",
                             value = "//div[@class='h5 entry-name mt-0' and a]")
  get_link <- function(x){
    tmp <- x$getElementAttribute("outerHTML")[[1]]
    link <- read_html(tmp) %>% # Extract the links from the HTML
      html_nodes("a") %>% 
      html_attr("href") %>% 
      str_c("https://www.iso.org", .)
    return(link)
  }
  
  all_links <- unlist(lapply(elem, get_link))
  
  return(all_links)
  
}

iso_click <- function(x){
  
  # Make an object of the spot for the "next" button on the webpage
  click <- remDr$findElement(using = "xpath", 
                             value = "//button[starts-with(@id, 'more')]")

  remDr$mouseMoveToLocation(webElement = click) # Put the mouse on the button
  remDr$screenshot(display = TRUE) # Check that the mouse is at the right place.
  
  click$clickElement() # Click the button!
  
  remDr$screenshot(display = TRUE) # Check that the button was clicked.
  
}

idlist <- list()

for (i in 1:2000) { #1000) {
  
  idlist[[i]] <- get_links()
  
  try(iso_click())
  try(iso_click())
  
  print(paste(i))
  
  Sys.sleep(5)

}

saveRDS(idlist, file = "./Data/links.rds")

links <- unlist(idlist)
links <- unique(links)

date <- list()
metadata <- list()
standards <- list()
committees <- list()
headline <- list()
text <- list()

for (i in 1:length(links)) {
  
  link <- read_html(links[i])
  
  date[i] <- link %>%
    html_node("div > time") %>%
    html_text() %>%
    str_trim() 
  
  metadata[i] <- link %>%
    html_nodes("#metadata") %>% 
    html_text() %>%
    str_trim() %>%
    str_split("\n") %>%
    .[[1]] %>%
    str_squish() %>%
    stringi::stri_remove_empty() %>%
    tibble() %>%
    rename("metadata" = ".")
  
  aside <- link %>%
    html_nodes(xpath = "//div[@class='entry-name']") %>% 
    html_text() %>%
    tibble() 
  
  standards[[i]] <- aside %>%
    filter(str_detect(., "ISO [0-9]+")) %>%
    mutate(standard_code = str_squish(.)) %>%
    select(-.)
  
  committees[[i]] <- aside %>%
    filter(str_detect(., "ISO/")) %>%
    mutate(standard_code = str_squish(.)) %>%
    select(-.)
  
  headline[i] <- link %>% 
    html_nodes("h1") %>% 
    html_text()
  
  text[[i]] <- link %>% 
    html_nodes("p") %>% 
    html_text() 
  
  print(paste0(i))
  
  Sys.sleep(2)
  
}

newsdata <- tibble(date = unlist(date),
                   metadata = metadata,
                   standards = standards,
                   committees = committees,
                   headline = unlist(headline),
                   text = text)  %>%
  rowid_to_column()

tags <- newsdata %>%
  select(metadata, rowid) %>%
  unnest(cols = c(metadata)) %>%
  filter(str_detect(metadata, "Tagged as")) %>%
  mutate(tag = str_remove_all(metadata, "Tagged as ")) %>%
  mutate(tag = strsplit(tag, split="(?!^)(?=[[:upper:]][[:lower:]])", perl=T)) %>% 
  unnest(cols = c(tag)) %>%
  select(rowid, tag) %>%
  distinct(.keep_all = TRUE) %>%
  group_by(rowid) %>%
  nest(tags = tag)

newsdata <- newsdata %>%
  left_join(tags, by = "rowid")

saveRDS(newsdata, file = "./Data/newsdata_iso_2.rds")

stopwords <- tibble(word = c(stop_words$word, 
                             c("iso.org", "iso", "standards", "copyright", "international",
                               "iso's", "standard", "including", "iso’s", "isos")))

symbols <- str_c("\\©|\\!|\\@|\\#|\\$|\\%|\\^|\\&|\\*|\\(|\\)|\\{|\\}|\\-|\\=|\\_|\\+|\\:|\\|\\<|\\>|\\?|\\,|\\.|\\/|\\;|\\'|\\[|\\]|\\-=")

newsdata1 <- readRDS("./Data/newsdata_iso.rds") %>%
  rowid_to_column() %>%
  unnest(cols = c(date)) %>%
  mutate(date_chr = str_replace(date, " December ", ".12."),
         date_chr = str_replace(date_chr, " November ", ".11."),
         date_chr = str_replace(date_chr, " October ", ".10."),
         date_chr = str_replace(date_chr, " September ", ".9."),
         date_chr = str_replace(date_chr, " August ", ".8."),
         date_chr = str_replace(date_chr, " July ", ".7."),
         date_chr = str_replace(date_chr, " June ", ".6."),
         date_chr = str_replace(date_chr, " May ", ".5."),
         date_chr = str_replace(date_chr, " April ", ".4."),
         date_chr = str_replace(date_chr, " March ", ".3."),
         date_chr = str_replace(date_chr, " February ", ".2."),
         date_chr = str_replace(date_chr, " January ", ".1.")) %>%
  mutate(year = str_extract(date, "[0-9]{4}")) %>%
  mutate(iso_full = str_extract_all(iso, "ISO [0-9]+:[0-9]+"),
         iso_tc = str_extract_all(iso, "ISO/TC [0-9]+"),
         iso_iec = str_extract_all(iso, "ISO/IEC.*")) %>%
  unnest(cols = c(headline, intro, iso), keep_empty = TRUE) %>%
  mutate(intro = ifelse(is.na(intro), " ", intro),
         headline = ifelse(is.na(headline), " ", headline)) %>%
  group_by(rowid) %>%
  unnest(cols = text) %>%
  mutate(text = paste0(text, collapse = " ")) %>%
  ungroup() %>%
  unique() %>%
  mutate(text = paste(headline, intro, text)) %>%
  unnest(cols = c(date, text)) %>%
  mutate(text = ifelse(text == "", NA, text)) %>%
  drop_na(text) %>%
  mutate(name = str_extract(iso, "ISO.* [0-9]+:[0-9]+")) %>%
  mutate(date = as.Date(date_chr, format = "%d.%m.%Y")) %>%
  select(date, text, name) %>%
  unique() %>%
  mutate(mentioned = str_extract_all(text, "ISO(/IEC)? [0-9]+(\\-[0-9])?:[0-9]+")) %>%
  mutate(text = str_squish(text))

newsdata2 <- readRDS("./Data/newsdata_iso_2.rds") %>%
  mutate(date_chr = str_replace(date, " December ", ".12."),
         date_chr = str_replace(date_chr, " November ", ".11."),
         date_chr = str_replace(date_chr, " October ", ".10."),
         date_chr = str_replace(date_chr, " September ", ".9."),
         date_chr = str_replace(date_chr, " August ", ".8."),
         date_chr = str_replace(date_chr, " July ", ".7."),
         date_chr = str_replace(date_chr, " June ", ".6."),
         date_chr = str_replace(date_chr, " May ", ".5."),
         date_chr = str_replace(date_chr, " April ", ".4."),
         date_chr = str_replace(date_chr, " March ", ".3."),
         date_chr = str_replace(date_chr, " February ", ".2."),
         date_chr = str_replace(date_chr, " January ", ".1.")) %>%
  mutate(year = str_extract(date, "[0-9]{4}")) %>%
  mutate(headline = ifelse(is.na(headline), " ", headline)) %>%
  group_by(rowid) %>%
  unnest(cols = text) %>%
  mutate(text = paste0(text, collapse = " ")) %>%
  ungroup() %>%
  unique() %>%
  mutate(text = paste(headline, text)) %>%
  unnest(cols = c(date, text)) %>%
  mutate(text = ifelse(text == "", NA, text)) %>%
  drop_na(text) %>%
  mutate(date = as.Date(date_chr, format = "%d.%m.%Y")) %>%
  select(date, text, standards) %>%
  unnest(standards) %>%
  rename(name = standard_code) %>%
  select(date, text, name) %>%
  unique() %>%
  mutate(mentioned = str_extract_all(text, "ISO(/IEC)? [0-9]+(\\-[0-9])?:[0-9]+")) %>%
  mutate(text = str_squish(text))

newsdata2_lim <- newsdata2 %>%
  filter(!date %in% newsdata1$date)

news <- bind_rows(newsdata1, newsdata2) %>%
  mutate(name = str_replace_all(name, "\\[Withdrawn\\]", ""),
         name = str_replace_all(name, "\\—.*", ""),
         name = str_replace_all(name, "to .*", ""),
         name = str_squish(name)) %>%
  filter(str_detect(name, "^[ISO]")) %>%
  rowid_to_column()

name_news <- news %>%
  select(date, text, name)

mentioned_news <- news %>%
  select(date, text, mentioned) %>%
  rename(name = mentioned) %>%
  unnest(name) 

news_data <- bind_rows(name_news, mentioned_news) %>%
  distinct(date, name, .keep_all = TRUE) %>%
  nest(data = text) %>%
  rowid_to_column() %>% unnest(cols = c(data))

saveRDS(news_data, file = "./Data/news.rds")
