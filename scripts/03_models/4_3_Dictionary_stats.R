
library(kableExtra)

paper_dict <- readRDS("../../../Legitimacy/Data/technocratic_dict_papers.rds")
patent_dict <- readRDS("../../../Legitimacy/Data/technocratic_dict_patents.rds")
unga_dict <- readRDS("../../../Legitimacy/Data/democratic_dict_unga.rds")
csr_dict <- readRDS("../../../Legitimacy/Data/democratic_dict_csr.rds")

## Number of words in each dictionary

paper_dict %>%
  bind_rows(patent_dict) %>%
  bind_rows(unga_dict) %>%
  bind_rows(csr_dict) %>%
  drop_na(token) %>%
  group_by(dimension) %>%
  count()

## Tokens in the dictionaries

# Define the function
format_tokens_as_table <- function(data, column_name, num_columns = 6, col_prefix = "Col") {
  # Ensure column_name is a valid column in the data
  if (!column_name %in% colnames(data)) {
    stop("Column name not found in the data frame.")
  }
  
  # Extract the tokens as a vector
  tokens <- data %>%
    pull({{ column_name }})
  
  # Calculate the number of rows required for the matrix
  num_rows <- ceiling(length(tokens) / num_columns)
  
  # Pad the tokens with NAs to ensure the matrix has a complete rectangle shape
  padded_tokens <- c(tokens, rep(NA, num_rows * num_columns - length(tokens)))
  
  # Replace NAs with empty strings
  padded_tokens[is.na(padded_tokens)] <- ""
  
  # Convert to a matrix
  token_matrix <- matrix(padded_tokens, ncol = num_columns, byrow = TRUE)
  
  # Convert the matrix to a data frame
  token_df <- as.data.frame(token_matrix)
  
  # Rename the columns with the specified prefix
  colnames(token_df) <- paste0(col_prefix, 1:num_columns)
  
  # Use kable to create the table
  kable(token_df, "latex") %>%
    kable_styling()
}

unga_dict_list <- format_tokens_as_table(unga_dict, "token", col_prefix = "UNGA Debates ")
save_kable(unga_dict_list, file = "./Tables/dict_unga.tex")

human_rights_dict_list <- format_tokens_as_table(csr_dict %>% filter(dimension == "human_rights"), "token", col_prefix = "Human Rights ")
save_kable(human_rights_dict_list, file = "./Tables/dict_humanrights.tex")

employee_dict_list <- format_tokens_as_table(csr_dict %>% filter(dimension == "employee"), "token", col_prefix = "Labor ")
save_kable(employee_dict_list, file = "./Tables/dict_employee.tex")

environment_dict_list <- format_tokens_as_table(csr_dict %>% filter(dimension == "environment"), "token", col_prefix = "Environment ")
save_kable(environment_dict_list, file = "./Tables/dict_environment.tex")

community_dict_list <- format_tokens_as_table(csr_dict %>% filter(dimension == "social_and_community"), "token", col_prefix = "Social & Community ")
save_kable(community_dict_list, file = "./Tables/dict_community.tex")

paper_dict_list <- format_tokens_as_table(paper_dict, "token", col_prefix = "Research papers ")
save_kable(paper_dict_list, file = "./Tables/dict_papers.tex")

patent_dict_list <- format_tokens_as_table(patent_dict, "token", col_prefix = "Patents ")
save_kable(patent_dict_list, file = "./Tables/dict_patents.tex")

