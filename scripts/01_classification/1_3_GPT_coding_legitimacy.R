
library(tidyverse)
library(tidytext)
library(DBI)
library(RSQLite)
library(httr)

# standard_news_sentences2 <- valid_check %>% 
#   select(-text) %>%
#   inner_join(standard_news_sentences, by = join_by(sentence_id, rowid)) 

create_prompt <- function(text){
  prompts <- purrr::map(standard_news_sentences$text,
                         
                         ~list(
                           
                           list(
                             "role" = "system",
                             "content" = stringr::str_c(
                               
                               "You are a political scientist and expert on standards, legitimacy and legitimation. Your task is to code sentences from text that markets the contribution of international standards. ",
                               "You are to code in two stages: ",
                               
                               "First, consider whether the sentence is a legitimation statement. A legitimation statement seeks to bolster acceptance for the standard by referring to an evaluative argument. ",
                               "For example: 'The standard provides solid steps towards a more sustainable approach to agriculture',
                               'iso 673 improves efficiency in key areas', 
                               'numerous organizations have reported enormous savings in energy thanks to iso 50001's energy management systems', 
                               or 'standards are powerful tools to help organizations implement effective changes'. ",
                               "Sometimes the text contains interviews, and these might be used as legitimation, e.g 'expert John says standards are crucial for enabling the operability of iot', or ",
                               "'being iso 45001 certified helped us improve the safety of our employees, says Jenna.' ",
                               "Be sure that the sentence speaks about a standard, a committee (also called TC, SC or working group, WG) or the standard developers (also called experts). ",
                               "For example: 'the work of sc 42 is important in ensuring global fairness' is a legitimation statement. ",
                               "Sentences referring to other entities are not legitimation statements, e.g. 'an organization's biggest asset is its people'. ",
                               #"When the sentence uses 'it' you need to discern what 'it' refers to. ",
                               "Also make sure to distinguish between legitimation statements and pure statements. Examples of sentences that are pure statements are: ",
                               "'to have any sort of beneficial impact, it is vital that the standards are successfully implemented worldwide' and ",
                               "'the standard sets out guidance and requirements relating to the way in which market research studies are planned'. ",
                               
                               "Second, if the sentence is a legitimation statement, code whether this is a technocratic or a democratic legitimation statement. ",
                               "Democratic legitimation statements justify a standard by alluding to popular democracy, representation, participation, transparency, accountability, engagement, equity, equality, ",
                               "democratic empowerment, non-discrimination, traditional heritage, ethics, social science, human rights, liberty, freedom, economic rights, environmental protection, 
                               green economy, agricultural development, sustainability or climate change. ",
                               "They might also highlight the widely collaborative, diverse and/or stakeholder-focused process of producing standards. ",
                               "Technocratic legitimation statements justify a standard by alluding to objectivity, neutrality, expert knowledge, technical solutions, expertise, scientific, excellent professionals, ",
                               "high quality of staff, well-trained and dedicated personnel, skill, neutrality, hard-working/committed staff, quality of output, innovation, (cost) efficiency or optimization. ",
                               "They might also highlight the skill, talent and expertise of the the standard producers.")
                             
                             # we strive to hire, train and retain the best possible talent from around the world.
                             # as technology and expectations evolve, so does the work of iso/tc 205, in order to ensure continual benefits.
                             
                             # at the heart of the standard is preserving tradition, from specifying what restaurants should look like to how food is to be served to demonstrate that they stay true to their heritage.
                             # today, iso/tc 249 has 20 countries participating in its work with another 16 as observers, including three liaison organizations: the world health organization (who), the world federation of acupuncture-moxibustion societies (wfas) and the world federation of chinese medicine societies (wfcms).
                             # “in this discipline, we believe that social sciences like philosophy, ethics and law should be at the heart of our ideas, in addition to technology and science.
                           ),  
                           
                           list(
                             "role" = "user",
                             "content" = stringr::str_c(
                               
                               "The sentence reads: '", .x, "' ",
                               
                               "Classify it as one of the following: ",
                               
                               "(0) no legitimation statement, (1) democratic legitimation statement, (2) technocratic legitimation statement ",
                               
                               "Use this template to answer the questions: ",
                               
                               "Separate answers using punctuation (|) and line shifts (\n\n). ",
                               
                               "(number) | category | \n\n ",
                               
                               "For example: ",
                               
                               "(1) | democratic legitimation statement | \n\n")
                             
                           )
                         )
  )
  prompts
}

prompts <- create_prompt(standard_news_sentences$text)

api_key <- read_lines("Scripts/ISO/0_API_key_openai.txt")

output_folder <- "../../../Legitimacy/Tests/output_legitimacy5"

for(i in 1:nrow(standard_news_sentences)){
  
  stdno <- standard_news_sentences$sentence_id[i]
  
  destfile <- paste0(output_folder, "/completion_", stdno, ".txt")
  
  if(!file.exists(destfile)){
    
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions", 
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type_json(),
      encode = "json",
      body = list(
        model = "gpt-4.1-mini", #"gpt-3.5-turbo-0125", # gpt-4-turbo-preview
        temperature = 0.1,
        messages = prompts[[i]],
        n = 1))
    
    completion <<- str_trim(content(response)$choices[[1]]$message$content)
    
    Sys.sleep(1)
    
    completion <- tibble(completion = completion)
    
    tokenuse <- tibble(prompt_tokens = str_trim(content(response)$usage$prompt_tokens),
                       completion_tokens = str_trim(content(response)$usage$completion_tokens),
                       total_tokens = str_trim(content(response)$usage$total_tokens))
    
    write.table(completion, file = paste0(output_folder, "/completion_", stdno, ".txt"))
    write_csv(tokenuse, file = paste0(output_folder, "/tokenuse_", stdno, ".csv"))
    
  } else {
    
    message(paste0("File ", stdno, " already exists in folder."))
    
  }
  
  if(file.size(paste0(output_folder, "/completion_", stdno, ".txt")) <= 15L | is.na(paste0(output_folder, "/completion_", stdno, ".txt"))){
    
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type_json(),
      encode = "json",
      body = list(
        model = "gpt-4.1-mini", # gpt-4-turbo-preview # -2025-04-14
        temperature = 0.1,
        messages = prompts[[i]],
        n = 1))
    
    completion <<- str_trim(content(response)$choices[[1]]$message$content)
    
    Sys.sleep(1)
    
    completion <- tibble(completion = completion)
    
    tokenuse <- tibble(prompt_tokens = str_trim(content(response)$usage$prompt_tokens),
                       completion_tokens = str_trim(content(response)$usage$completion_tokens),
                       total_tokens = str_trim(content(response)$usage$total_tokens))
    
    write.table(completion, file = paste0(output_folder, "/completion_", stdno, ".txt"))
    write_csv(tokenuse, file = paste0(output_folder, "/tokenuse_", stdno, ".csv"))
    
  } else {
    
    message(paste0("Finished observation no. ", i))
    
  }
}

################################################################################

completions <- lapply(list.files(paste0(output_folder), full.names = TRUE, pattern = ".txt"), read.table) %>%
  bind_rows()

tokenuse <- lapply(list.files(paste0(output_folder), full.names = TRUE, pattern = ".csv"), read.csv) %>%
  bind_rows()

((0.0005*(sum(tokenuse$prompt_tokens))/1000) + 0.0015*(sum(tokenuse$completion_tokens))/1000)*11 # NOK 3.351701

ids <- str_remove(str_extract(list.files(paste0(output_folder), full.names = TRUE, pattern = ".txt"), "[0-9]+\\.txt"), ".txt")

completions_df <- as_tibble(completions, .name_repair = "universal") %>%
  mutate(sentence_id = as.numeric(ids)) %>%
  mutate(type_code = str_extract(completion, "(\\|)?(\\s*)?\\([0-9]+\\)"),
         type_text = str_extract(completion, "(\\|)(\\s*)?[a-z\\s]+(\\s*)?(\\|)")) %>%
  mutate(type_code = str_extract(type_code, "[0-9]{1}"),
         type_text = str_extract(type_text, "[a-z]+"))

completions_df %>%
  group_by(type_text) %>%
  count() %>%
  ggplot(aes(type_text, n)) +
  geom_bar(stat = "identity")

check_df <- completions_df %>%
  left_join(standard_news_sentences, by = join_by(sentence_id)) %>%
  select(-completion)

saveRDS(check_df, file = "../../../Legitimacy/Data/legitimation_output6.rds")
