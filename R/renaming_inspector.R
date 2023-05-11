
# CLEAN DATA RENAMER
clean_data_renamer <- function(clean_df, tool_path , no_rename_end = clean_df_cols_no_rename_end, no_rename_start=clean_df_cols_no_rename_start){
  # Start extracting each data frame from above list  
  clean_df <- read_excel(clean_df)
  survey <- read_excel(tool_path, sheet = 1) %>% select(type, name, label = `label::English`)
  choices <- read_excel(tool_path, sheet = 2) %>% select(list_name, name, label = `label::English`)
  
  clean_df_cols <- names(clean_df)
  clean_df_cols_no_rename_start <- clean_df_cols[1:no_rename_start]
  clean_df_cols_no_rename_end <- clean_df_cols[-(1:(length(clean_df_cols) - no_rename_end))]
  clean_df_cols_no_rename_combined <- c(clean_df_cols_no_rename_start, clean_df_cols_no_rename_end)
  clean_df_cols_rename <- clean_df_cols[clean_df_cols %notin% clean_df_cols_no_rename_combined]
  
  # Getting column names of clean data (in clean df choices we see that if it doesnt contains . then we put NA)
  clean_df_vars <- gsub("\\..*", "", clean_df_cols_rename)
  new_clean_df_vars <- clean_df_cols_rename
  clean_df_choices <- ifelse(grepl("\\.", clean_df_cols_rename), clean_df_cols_rename, NA)
  clean_df_choices <- gsub(".*\\.", "", clean_df_choices)
  
  survey_vars <- survey[["name"]]
  survey_type <- survey[["type"]]
  survey_list  <- ifelse(grepl(".* ", survey_type), survey_type, NA)
  survey_list <- gsub(".* ","", survey_list)
  survey_label <- survey[["label"]]
  
  # Getting Choices sheet list_name, name and label
  choices_list <- choices[["list_name"]]
  choices_vars <- choices[["name"]]
  choices_label <- choices[["label"]]
  
  
  # First : Just add variable labels from survey sheet to clean data
  clean_df_var_length <- length(clean_df_vars)
  if(length(clean_df_vars > 0)){
    for(i in 1:length(clean_df_vars)){
      temp_clean_df_var <- clean_df_vars[i]
      temp_clean_df_choice <- clean_df_choices[i]
      temp_var_match <- match(temp_clean_df_var, survey_vars)
      is_select_one <- gsub(" .*","", survey_type[temp_var_match]) =="select_one"
      is_select_multi <- gsub(" .*","", survey_type[temp_var_match]) =="select_multiple"
      temp_var_label <- survey_label[temp_var_match]
      # check that is there a match and is the matched variable has a label or not if yes 2 other if's happens (1. its select_one/mult , 2. its not a select_one_multi)
      if(!is.na(temp_var_match) & !is.na(temp_var_label)){
        # 1. if there is a match and it's not select_one nor select_multiple
        if(!(is_select_one & is_select_multi)){
          new_clean_df_vars[i] <-  temp_var_label
        } #2.if there is a match and matched element is select one or (select_multiple just question [as it starts with question itself and then the question and each choice so this is just the question sorry for long comment])
        #| (is_select_multi & is.na(temp_clean_df_choice))
        if(is_select_one) {
          temp_survey_list <- survey_list[temp_var_match]
          all_choices <- filter(choices, list_name == temp_survey_list) %>% pull(label)
          all_choices[1] <- paste0("\n\n\n", all_choices[1])
          all_choices <- paste0(all_choices, collapse = "\n")
          new_clean_df_vars[i] <- paste(new_clean_df_vars[i], all_choices)
        }
        #3. if there is a match and it's select multiple
        if(is_select_multi & !is.na(temp_clean_df_choice)) { 
          temp_survey_list <- survey_list[temp_var_match]
          distinct_choice <- filter(choices, list_name == temp_survey_list & name == temp_clean_df_choice) %>% pull(label)
          new_clean_df_vars[i] <- paste(new_clean_df_vars[i], "\n\n\n", distinct_choice)
        }
      }
      # cat('\014')
      # cat("Renaming", i, "columns of ", clean_df_var_length, "\n")
      
    }
    # cat('\014') Yup Baby We're done
    cat("Successfully Columns Renamed! \n")
    cat("Renamed", i, "Columns of ", length(clean_df_cols), "Columns")
    
    new_clean_df_vars <- c(clean_df_cols_no_rename_start, new_clean_df_vars, clean_df_cols_no_rename_end)
    names(clean_df) <- new_clean_df_vars
    clean_df <- as.data.frame(clean_df)
    
  } else print("Sorry clean data has no columns!")
  return(clean_df)
}
