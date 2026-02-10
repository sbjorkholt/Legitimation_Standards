
library(tidyverse)
library(tidytext)
library(DBI)
library(RSQLite)
library(httr)
library(jsonlite)

set.seed(42)

con <- dbConnect(RSQLite::SQLite(), "../../../Legitimacy/Data/iso_standards.sqlite")
standards <- dbReadTable(con, "standards_status")
dbDisconnect(con)

setwd("C:/Users/solvebjo/UiO Dropbox/Solveig Bjørkholt/Apps/Overleaf/Legitimacy_standards")

abstracts <- standards %>%
  rename(text = abstract) %>%
  mutate(year = str_extract(publication_date, "[0-9]{4}")) %>%
  select(stdno, text, year, ics_text) %>%
  filter(text != "") %>%
  na.omit() %>%
  distinct(stdno, .keep_all = TRUE)

fetch_responses <- function(data, prompt, version, api_key, output_folder, model = "base") {
  
  for (i in 1:nrow(data)) {
    
    stdno <- data$stdno[i]
    destfile <- paste0(output_folder, "/response_", stdno, ".txt")
    
    if (!file.exists(destfile) || file.size(destfile) < 15L || is.na(file.size(destfile))) {
      
      response <- RETRY(
        "POST",
        times = 3,
        url = "https://api.openai.com/v1/responses",
        add_headers(Authorization = paste("Bearer", api_key)),
        content_type_json(),
        encode = "json",
        body = list(
          prompt = list(
            id = prompt,
            version = version,
            variables = list(
              abstract = data$text[i],
              ics = data$ics_text[i]
            )
          )
        )
      )
      
      content_json <- content(response, as = "parsed")
      Sys.sleep(.5)

            # if (!is.null(content_json$output)) {
      #   if (model == "base" && length(content_json$output) >= 2) {
      #     completion <- content_json$output[[2]]$content[[1]]$text
      #   } else {
      #     completion <- "FAIL"
      #   }
      
      # # Save completion
      # write.table(
      #   tibble(completion = content_json),
      #   file = destfile,
      #   row.names = FALSE,
      #   col.names = FALSE,
      #   quote = FALSE
      # )
      
      completion_text <- content_json$output[[2]]$content[[1]]$text
      writeLines(completion_text, destfile)
      
      # Save token usage (guard if usage is missing)
      if (!is.null(content_json$usage)) {
        write_csv(
          tibble(
            input_tokens = content_json$usage$input_tokens,
            output_tokens = content_json$usage$output_tokens,
            total_tokens = content_json$usage$total_tokens
          ),
          file = paste0(output_folder, "/tokenuse_", stdno, ".csv")
        )
      }
      
      gc()
      message(paste0("Finished post no. ", i))
    } else {
      message(paste0("Finished post no. ", i))
    }
  }
}

api_key <- "YOUR_KEY"

fetch_responses(data = abstracts,
                model = "base",
                prompt = prompt,
                version = "3", # 2
                api_key = api_key,
                output_folder = "./Data/standards2/")

completions <- lapply(list.files(paste0("./Data/standards2/"), full.names = TRUE, pattern = ".txt"), read.table) %>%
  bind_rows()

tokenuse <- lapply(list.files(paste0("./Data/standards2/"), full.names = TRUE, pattern = ".csv"), read.csv) %>%
  bind_rows()

ids <- str_remove(str_extract(list.files(paste0("./Data/standards2/"), full.names = TRUE, pattern = ".txt"), "[0-9]+\\.txt"), ".txt")

completions_df <- as_tibble(completions, .name_repair = "universal") %>%
  mutate(stdno = as.numeric(ids)) %>%
  select(stdno, 
         type_text = V3,
         type_code = V1) %>%
  mutate(type_code = str_extract(type_code, "[0-9]+"))

completions_df %>%
  group_by(type_text) %>%
  count() %>%
  ggplot(aes(type_text, n)) + 
  geom_bar(stat = "identity") + 
  theme_bw()


### COSTS
price_input <- 0.250 / 1e6
price_output <- 2.000 / 1e6
price_cached <- 0.025 / 1e6 

usd_to_nok <- 1 / 0.098   # 1 USD = 10.20408 NOK

costs <- tokenuse %>%
  mutate(
    input_cost_usd  = input_tokens  * price_input,
    output_cost_usd = output_tokens * price_output,
    total_cost_usd  = input_cost_usd + output_cost_usd,
    total_cost_nok  = total_cost_usd * usd_to_nok
  )

total_summary <- costs %>%
  summarise(
    total_input_tokens = sum(input_tokens),
    total_output_tokens = sum(output_tokens),
    total_cost_usd = sum(total_cost_usd),
    total_cost_nok = sum(total_cost_nok)
  )

print(costs)
print(total_summary)

#############################################################

# create_prompt <- function(abstracts){
#   prompts <- purrr::map2(abstracts$text, abstracts$ics_text,
#                          
#                          ~list(
#                            
#                            list(
#                              "role" = "system",
#                              "content" = stringr::str_c(
#                                
#                                "International standards are important regulatory instruments.
#                                Imagine you are an expert on international standards, asked to classify standards into either 'physical' or 'societal'. ",
#                                
#                                "Physical standards: Provide technical specifications, scientific formula or ICT specifications. 
#                                They ensure interchangeability and solve coordination problems. 
#                                Physical standards are specific to products, materials or behaviors and focus primarily on the final results. ",
#                                
#                                "Societal standards: Addresses performance, quality, safety and health in manufacturing processes. 
#                                They aim to promote consistent and responsible practices and ensure the well-being of stakeholders.
#                                Societal standards focus on regulating the organization or system as a whole.")
#                              
#                            ),  
#                            
#                            list(
#                              "role" = "user",
#                              "content" = stringr::str_c(
#                                
#                                "The abstract of the standard reads: '", .x, "' ",
#                                "And the ICS code of the standard is '", .y, "'. ",
#                                
#                                "What type of standard is this? Pick one of the following: ",
#                                
#                                "(1) physical, (2) societal. ",
#                                
#                                "Use this template to answer the questions: ",
#                                
#                                "Include STDNO number and separate answers using punctuation (|) and line shifts (\n\n). ",
#                                
#                                "(number) | category | \n\n ",
#                                
#                                "For example: ",
#                                
#                                "(1) | physical | \n\n")
#                              
#                            )
#                          )
#   )
#   prompts
# }
# 
# 
# prompts <- create_prompt(abstracts)
# 
# api_key <- read_lines("../Credentials/api_key_chatgpt")
# 
# output_folder <- "standard_type/"
# 
# for(i in 1:nrow(abstracts)){
#   
#   stdno <- abstracts$stdno[i]
#   
#   destfile <- paste0(output_folder, "/completion_", stdno, ".txt")
#   
#   if(!file.exists(destfile)){
#     
#     response <- POST(
#       url = "https://api.openai.com/v1/chat/completions",
#       add_headers(Authorization = paste("Bearer", api_key)),
#       content_type_json(),
#       encode = "json",
#       body = list(
#         model = "gpt-3.5-turbo-0125",
#         temperature = 0.2,
#         messages = prompts[[i]],
#         n = 1))
#     
#     completion <<- str_trim(content(response)$choices[[1]]$message$content)
#     
#     Sys.sleep(1)
#     
#     completion <- tibble(completion = completion)
#     
#     tokenuse <- tibble(prompt_tokens = str_trim(content(response)$usage$prompt_tokens),
#                        completion_tokens = str_trim(content(response)$usage$completion_tokens),
#                        total_tokens = str_trim(content(response)$usage$total_tokens))
#     
#     write.table(completion, file = paste0(output_folder, "/completion_", stdno, ".txt"))
#     write_csv(tokenuse, file = paste0(output_folder, "/tokenuse_", stdno, ".csv"))
#     
#   } else {
#     
#     message(paste0("File ", stdno, " already exists in folder."))
#     
#   }
#   
#   if(file.size(paste0(output_folder, "/completion_", stdno, ".txt")) <= 15L | is.na(paste0(output_folder, "/completion_", stdno, ".txt"))){
#     
#     response <- POST(
#       url = "https://api.openai.com/v1/chat/completions",
#       add_headers(Authorization = paste("Bearer", api_key)),
#       content_type_json(),
#       encode = "json",
#       body = list(
#         model = "gpt-3.5-turbo-0125",
#         temperature = 0.2,
#         messages = prompts[[i]],
#         n = 1))
#     
#     completion <<- str_trim(content(response)$choices[[1]]$message$content)
#     
#     Sys.sleep(1)
#     
#     completion <- tibble(completion = completion)
#     
#     tokenuse <- tibble(prompt_tokens = str_trim(content(response)$usage$prompt_tokens),
#                        completion_tokens = str_trim(content(response)$usage$completion_tokens),
#                        total_tokens = str_trim(content(response)$usage$total_tokens))
#     
#     write.table(completion, file = paste0(output_folder, "/completion_", stdno, ".txt"))
#     write_csv(tokenuse, file = paste0(output_folder, "/tokenuse_", stdno, ".csv"))
#     
#   } else {
#     
#     message(paste0("Finished observation no. ", i))
#     
#   }
# }
# 
# ################################################################################
# 
# 
# completions_df %>%
#   left_join(standards %>% mutate(stdno = as.numeric(stdno))) %>%
#   group_by(type_text, year) %>%
#   count() %>%
#   ggplot(aes(as.numeric(year), n, color = type_text)) + 
#   geom_point() + 
#   facet_wrap(~type_text, scales = "free") +
#   geom_smooth()
# 
# # Prompt token: About 375 tokens per call on average
# # Completion token: 6 tokens per call on average
# # Cost for gpt-3.5-turbo-0125: Input: $0.50 / 1M tokens | Output: $1.50/ 1M tokens
# ((375*0.50)/1000000 + (6*1.50)/1000000)*34337
# 
# mean(tokenuse$prompt_tokens)
# 
# table(file.size(list.files("./standard_type_coding2", full.names = TRUE)) <= 15L)

