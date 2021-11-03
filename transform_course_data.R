
clean_columns <- function(data) {
  colnames(data) <- str_replace_all(colnames(data), " |\\/", "_")
  colnames(data) <- str_replace_all(colnames(data), "\\:|\\?|\\_\\(Rendered_no_HTML\\)|\\(s\\)", "")
  
  # Testing to see whether or not uniting course code and number creates a unique enough column for joining
  data_new_code <- data %>% unite("Course_ID", Prefix:Code, sep = "_", remove = TRUE) %>% 
    distinct(Course_ID, .keep_all = TRUE)
  return(data_new_code)
}


# This is a very simple function that solves a problem with course data received at Florida Poly. Not super generalizable but can be made to 
# be so at some point. 

transform_course_data <- function(data, target_column = "Course_Outline", filter_regex = "", replace_regex = "") {
  require(here)
  require(dplyr)
  require(tidyr)
  require(stringr)
 
  
  # Bit of extra regex to filter down the data to the levels that we need
  filtering_string <- regex(paste0("^Week|\\s{2}|^\\s{1}|^Quiz|^Chapter|^Case|^http|Ch.|^Incoterms|Exam|Presentations|www|", 
                                   filter_regex), 
                            ignore_case = TRUE)
  replace_string <- regex(paste0("^\\d{1}\\. |\\d{2}\\. |^Lab \\d{1}. |^Lab \\d{2}. |^[a-z]. |^\\d{1}.|", 
                                 replace_regex), 
                          ignore_case = TRUE)
  
  
  # This will eventually be its own script
  outl_df <- data %>% 
    mutate(new_col = strsplit(as.character(Course_Description), "[\\\r\\\n\\\t]+")) 
  
  main_outl_df <- outl_df %>% tidyr::separate_rows(new_col, sep = "^[0-9].")  %>% filter(!grepl("^\\d{1}\\. |\\d{2}\\. ", new_col))
  # Getting everything else just in case we need them
  side_outl_df <- outl_df %>% tidyr::separate_rows(new_col, sep = "^[0-9].") %>% filter(!grepl(paste0("^\\d{1}\\. |\\d{2}\\. "), new_col)) 
  
  main_outl_df$new_col <- main_outl_df$new_col %>% 
    str_replace_all(replace_string, "")
  
  
  # Separate 
  side_outl_new <- side_outl_df %>% 
    filter(!grepl(filtering_string, 
                  new_col, ignore.case = TRUE)) %>% 
    filter(!is.na(new_col))
  
  
  side_outl_new$new_col <- side_outl_new$new_col %>% str_replace_all(replace_string, "")
  # Joining the two dataframes for the new_col
  full_outl <- main_outl_df %>% rbind(side_outl_new)
  write.csv(full_outl, here("data/full_outline_data.csv"), row.names = FALSE)
  return(full_outl)
}
