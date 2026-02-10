
parse_scopus <- function(entries){
  
  url <- list()
  dc_identifier <- list()
  eid <- list()
  title <- list()
  creator <- list()
  publication_name <- list()
  abstract <- list()
  issn <- list()
  eissn <- list()
  volume <- list()
  issue <- list()
  page_range <- list()
  cover_date <- list()
  cover_display_date <- list()
  doi <- list()
  cited_by_count <- list()
  affiliation_df <- list()
  publication_type <- list()
  subtype <- list()
  subtype_description <- list()
  source_id <- list()
  openaccess <- list()
  openaccess_flag <- list()
  freetoread_df <- list()
  freetoread_label_df <- list()
  call_abstract_df <- list()
  
  for (i in 1:length(entries)) {
    
    url[[i]] <- entries[i][[1]]$`prism:url`
    dc_identifier[[i]] <- entries[i][[1]]$`dc:identifier`
    eid[[i]] <- entries[i][[1]]$eid
    
    title[[i]] <- entries[i][[1]]$`dc:title`
    creator[[i]] <- ifelse(is.null(entries[i][[1]]$`dc:creator`), "", entries[i][[1]]$`dc:creator`)
    publication_name[[i]] <- entries[i][[1]]$`prism:publicationName`
    abstract[[i]] <- ifelse(is.null(entries[i][[1]]$`dc:description`), "", entries[i][[1]]$`dc:description`)
    
    issn[[i]] <- ifelse(is.null(entries[i][[1]]$`prism:issn`), "", entries[i][[1]]$`prism:issn`)
    eissn[[i]] <- ifelse(is.null(entries[i][[1]]$`prism:eIssn`), "", entries[i][[1]]$`prism:eIssn`)
    volume[[i]] <- ifelse(is.null(entries[i][[1]]$`prism:volume`), "", entries[i][[1]]$`prism:volume`)
    
    issue[[i]] <- ifelse(is.null(entries[i][[1]]$`prism:issueIdentifier`), "", entries[i][[1]]$`prism:issueIdentifier`)
    page_range[[i]] <- ifelse(is.null(entries[i][[1]]$`prism:pageRange`), "", entries[i][[1]]$`prism:pageRange`)
    cover_date[[i]] <- entries[i][[1]]$`prism:coverDate`
    cover_display_date[[i]] <- entries[i][[1]]$`prism:coverDisplayDate`
    doi[[i]] <- ifelse(is.null(entries[i][[1]]$`prism:doi`), "", entries[i][[1]]$`prism:doi`)
    
    cited_by_count[[i]] <- entries[i][[1]]$`citedby-count`
    affiliation_df[[i]] <- ifelse(is.null(entries[i][[1]]$affiliation), "", entries[i][[1]]$affiliation)
    
    publication_type[[i]] <- entries[i][[1]]$`prism:aggregationType`
    subtype[[i]] <- entries[i][[1]]$subtype
    subtype_description[[i]] <- entries[i][[1]]$subtypeDescription
    
    source_id[[i]] <- entries[i][[1]]$`source-id`
    openaccess[[i]] <- entries[i][[1]]$openaccess
    openaccess_flag[[i]] <- entries[i][[1]]$openaccessFlag
    freetoread_df[[i]] <- ifelse(is.null(entries[i][[1]]$freetoread), tibble(), entries[i][[1]]$freetoread)
    freetoread_label_df[[i]] <- ifelse(is.null(entries[i][[1]]$freetoreadLabel), tibble(), entries[i][[1]]$freetoreadLabel)
    
  }
  
  df <- tibble(url = url,
               dc_identifier = dc_identifier,
               title = title,
               creator = creator,
               publication_name = publication_name,
               abstract = abstract,
               issn = issn,
               eissn = eissn,
               volume = volume,
               issue = issue,
               page_range = page_range,
               cover_date = cover_date,
               cover_display_date = cover_display_date,
               doi = doi,
               cited_by_count = cited_by_count,
               affiliation_df = affiliation_df,
               publication_type = publication_type,
               subtype = subtype,
               subtype_description = subtype_description,
               source_id = source_id,
               openaccess = openaccess,
               openaccess_flag = openaccess_flag,
               freetoread_df = freetoread_df,
               freetoread_label_df = freetoread_label_df) %>%
    unnest(cols = c(url, dc_identifier, title, creator, publication_name, abstract, issn, eissn, volume, issue, page_range, cover_date, cover_display_date, doi, cited_by_count,
                    publication_type, subtype, subtype_description, source_id, openaccess, openaccess_flag, freetoread_df, freetoread_label_df))
  
  return(df)
}
