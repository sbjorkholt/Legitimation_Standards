
pacman::p_load(tidyverse, httr, DBI, RSQLite)

con <- dbConnect(RSQLite::SQLite(), "../../../Legitimacy/Data/iso_standards.sqlite")
liaison <- dbReadTable(con, "liaison")
dbDisconnect(con)

organizations <- liaison %>%
  select(name) %>%
  na.omit() %>%
  mutate(name = str_squish(name)) %>%
  unique() %>%
  rowid_to_column()

categories <- tibble(category_number = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                     category = c("Industry", "Consultant and Registrar", "Standards organization", "Government", "Research", "NGO", "Financial institutions",
                                  "Media", "Intergovernmental organizations", "Other"),
                     description = c("Individual firms and industry associations representing a specific industry or group of professionals.",
                                     "Firms that provide engineering/technical services, or support or training related to standards.",
                                     "Standard development organizations, and accreditation bodies.",
                                     "Governmental agencies/ministries.",
                                     "Professional groups.",
                                     "Research and/or academic institutions.",
                                     "Non-governmental organizations, such as consumer organizations, advocacy groups, or other civil society representatives.",
                                     "Institutions dealing with finance such as banks, insurance companies and real estate firms.",
                                     "Organizations dealing with public media such as TV stations, news outlets and journalistic organizations.",
                                     "Other organizations.")) %>%
  mutate(category_number = as.character(category_number))

# Type	Description
# Industry - Individual firms and industry associations representing a specific industry or group of professionals.
# Consultant and Registrar - Firms that provide engineering/technical services, or support or training related to standards.
# Standards organization - Representatives from national member bodies, standard development organizations, and accreditation bodies.
# Government - Representatives from governmental agencies/ministries.
# Research - Research and/or academic institutions.
# NGO - Non-governmental organizations, such as consumer organizations, advocacy groups, or other civil society representatives.
# Financial institutions - Institutions dealing with finance such as banks, insurance companies and real estate firms.
# Media	- Organizations dealing with public media such as TV stations, news outlets and journalistic organizations.
# Intergovernmental organizations	- Organizations that work on an international level.
# Other	- Other organizations.

create_prompt <- function(organizations){
  prompts <- purrr::map2(organizations$name, organizations$name,
                         
                         ~list(
                           
                           list(
                             "role" = "system",
                             "content" = stringr::str_c(
                               
                               "You are an expert on various organizations, associations, and standard bodies from different industries and sectors. ",
                               
                               "You are to classify organizations into their appropriate category based on the definitions below. ",
                               
                               "1. Industry: Individual firms and industry associations representing a specific industry or group of professionals.",
                               "2. Consultant and Registrar: Firms that provide engineering/technical services, or support or training related to standards.",
                               "3. Standards organization: Standard development organizations and accreditation bodies.",
                               "4. Governmental organization: National or international governmental organizations/agencies/ministries.",
                               "5. Professional association: Professional groups.",
                               "6. Research: Research and/or academic institutions.",
                               "7. NGO: Non-governmental organizations, such as consumer organizations, advocacy groups, or other civil society representatives.",
                               "8. Financial institution: Institutions dealing with finance such as banks, insurance companies and real estate firms.",
                               "9. Media: Organizations dealing with public media such as TV stations, news outlets and journalistic organizations.",
                               "10. Other: Other organizations.")
                           ),
                           
                           list(
                             "role" = "user",
                             "content" = stringr::str_c(
                               
                               "Which category of organization is organization with name ", .x, "?  ",
                               
                               "Answer with one of the following types: ",
                               "1. Industry ",
                               "2. Consultant and Registrar ",
                               "3. Standards organization ",
                               "4. Governmental organization ",
                               "5. Professional association ",
                               "6. Research ",
                               "7. NGO ",
                               "8. Financial institution ",
                               "9. Media ",
                               "10. Other",
                               
                               "Use this template to answer. Follow the template exactly and keep the justification short. ",
                               
                               "Name: ", .x, " | ",
                               
                               "Category: Number from 1 to 10 | ",
                               
                               "Justification \n\n",
                               
                               "For example: ",
                               
                               "Name: European Commission | ",
                               "Category: 4 | ",
                               "The organization maintains governmental relations. \n\n")
                             
                           )
                         )
  )
  prompts
}

prompts <- create_prompt(organizations)

api_key <- read_lines("../../../Legitimacy/credentials/api_key_chatgpt")

output_folder <- "organization_type"

for(i in 1:nrow(organizations)){
  
  no <- organizations$rowid[i]
  
  destfile <- paste0(output_folder, "/completion_", no, ".txt")
  
  if(!file.exists(destfile)){
    
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type_json(),
      encode = "json",
      body = list(
        model = "gpt-4-turbo-preview",
        temperature = 0.2,
        messages = prompts[[i]],
        n = 1))
    
    completion <<- str_trim(content(response)$choices[[1]]$message$content)
    
    Sys.sleep(1)
    
    completion <- tibble(completion = completion)
    
    tokenuse <- tibble(prompt_tokens = str_trim(content(response)$usage$prompt_tokens),
                       completion_tokens = str_trim(content(response)$usage$completion_tokens),
                       total_tokens = str_trim(content(response)$usage$total_tokens))
    
    write.table(completion, file = paste0(output_folder, "/completion_", no, ".txt"))
    write_csv(tokenuse, file = paste0(output_folder, "/tokenuse_", no, ".csv"))
    
  } else {
    
    message(paste0("File ", no, " already exists in folder."))
    
  }
  
  if(file.size(paste0(output_folder, "/completion_", no, ".txt")) <= 15L | is.na(paste0(output_folder, "/completion_", no, ".txt"))){
    
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type_json(),
      encode = "json",
      body = list(
        model = "gpt-4-turbo-preview",
        temperature = 0.2,
        messages = prompts[[i]],
        n = 1))
    
    completion <<- str_trim(content(response)$choices[[1]]$message$content)
    
    Sys.sleep(1)
    
    completion <- tibble(completion = completion)
    
    tokenuse <- tibble(prompt_tokens = str_trim(content(response)$usage$prompt_tokens),
                       completion_tokens = str_trim(content(response)$usage$completion_tokens),
                       total_tokens = str_trim(content(response)$usage$total_tokens))
    
    write.table(completion, file = paste0(output_folder, "/completion_", no, ".txt"))
    write_csv(tokenuse, file = paste0(output_folder, "/tokenuse_", no, ".csv"))
    
  } else {
    
    message(paste0("Finished observation no. ", i))
    
  }
}

completions <- lapply(list.files(paste0(output_folder), full.names = TRUE, pattern = ".txt"), read.table) %>%
  bind_rows()

tokenuse <- lapply(list.files(paste0(output_folder), full.names = TRUE, pattern = ".csv"), read.csv) %>%
  bind_rows()

(((10/1000000)*(mean(tokenuse$prompt_tokens))) + ((30/1000000)*45)*220)*11 # NOK 3.351701
# ids <- str_remove(str_extract(list.files(output_folder, full.names = TRUE, pattern = ".txt"), "[0-9]+\\.txt"), ".txt")

organizations_tagged <- tibble(category = unlist(completions)) %>%
  rowid_to_column() %>%
  separate_wider_delim(category, delim = "|", names = c("name", "category_number", "justification"), too_few = "debug", too_many = "debug") %>%
  mutate(nme = str_remove(name, "Name: "),
         category_number = str_remove(category_number, "Category: "),
         justification = str_remove(justification, "Justification: ")) %>%
  mutate_all(str_squish) %>%
  left_join(categories, by = "category_number") %>%
  filter(category_ok == TRUE) %>%
  mutate(category_number = str_extract(category_number, "[0-9]")) %>%
  rename(category_name = category.y) %>%
  select(rowid, name, category_number, category_name, description, justification)

organizations_tagged %>%
  ggplot(aes(category_name)) +
  geom_histogram(stat = "count") + 
  coord_flip()

saveRDS(organizations_tagged, file = "./organizations_tagged.rds")
