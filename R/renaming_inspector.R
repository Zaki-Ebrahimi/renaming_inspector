
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

# DATA MERGE RENAMER
data_merge_renamer <- function(data_merge_path = data_merge_path, tool_path = tool_path){
  #data_merge_path <- "input/Datamerge_AFG_AGORA_KI_2022.xlsx"
  #tool_path <- "input/AGORA_KI_FINAL.xlsx"
  data_merge <- read_excel(data_merge_path)
  data_merge_cols <- names(data_merge)
  
  survey <- read_excel(tool_path, sheet = 1) %>% select(type, name, label = `label::English`)
  choices <- read_excel(tool_path, sheet = 2) %>% select(list_name, name, label = `label::English`)
  
  # Getting column names of clean data (in clean df choices we see that if it doesnt contains . then we put NA)
  data_merge_vars <- gsub("-.*","", data_merge_cols)
  data_merge_choices <- gsub(".*-", "",  data_merge_cols)
  
  # Getting Survey type, variables and label
  # survey_list <- survey[["type"]][grepl("select\\_.*", survey[["type"]])]
  
  survey_vars <- survey[["name"]]
  survey_label <- survey[["label"]]
  survey_type <- survey[["type"]]
  survey_list  <- ifelse(grepl(".* ", survey_type), survey_type, NA)
  survey_list <- gsub(".* ","", survey_list)
  
  # Getting Choices sheet list_name, name and label
  choices_list <- choices[["list_name"]]
  choices_vars <- choices[["name"]]
  choices_label <- choices[["label"]]
  
  # First : Just add variable labels from survey sheet to clean data
  data_merge_vars_length <- length(data_merge_vars)
  new_data_merge_vars <- data_merge_vars
  
  if(length(data_merge_vars_length > 0)){
    
    for(i in 1:length(data_merge_vars)){
      temp_data_merge_var <- data_merge_vars[i]
      temp_data_merge_choice <- data_merge_choices[i]
      is_var_and_choice_same <- match(temp_data_merge_var, temp_data_merge_choice)
      
      temp_var_match <- match(temp_data_merge_var, survey_vars)
      temp_var_label <- survey_label[temp_var_match]
      
      # check that is there a match and is the matched variable has a label or not if yes 2 other if's happens (1. its select_one/mult , 2. its not a select_one_multi)
      if(is.na(temp_var_match)){
        #new_data_merge_vars[i] <- temp_data_merge_var
        next  } 
      if(!is.na(temp_var_match) & !(is.na(is_var_and_choice_same))){
        new_data_merge_vars[i] <-  temp_var_label 
        
      } 
      if(!is.na(temp_var_match) & is.na(is_var_and_choice_same)){
        
        temp_survey_list <- survey_list[temp_var_match]                  
        distinct_choice <- filter(choices, list_name == temp_survey_list & name == temp_data_merge_choice) %>% pull(label)
        new_data_merge_vars[i] <- paste(temp_var_label, "\n", "\n", distinct_choice)
      }
      
      # cat('\014')
      # cat("Renaming", i, "columns of ", data_merge_vars_length, "\n")
    }
    # cat('\014')
    # cat("Yup Baby We're done!")
    cat("Successfully Data Merge Columns Renamed! \n")
    cat("Renamed", i, "Columns of ", data_merge_vars_length, "Columns")
    
    new_data_merge_vars <- new_data_merge_vars
    names(data_merge) <- new_data_merge_vars
    #write.csv(data_merge, "Renamed_Data_Merge.csv")
    return(data_merge)
    
  } else print("Sorry data merge has no columns!")
  
}


# TOP N FUNCTION 
top_n_extractor <- function(analysis_results = analysis_results, survey = survey, top_n){
  #### TOBE REMOVED ####
  #### JUST TO FIND THE QUESTIONS FROM LAST DATAMERGE
  analysis_results <- read.csv(analysis_results)
  data <- analysis_results
  survey <- read_xlsx(survey, sheet = 1) %>% select(type, name)
  #top_n <- 3
  
  ## get the analysis result
  #FOR KI
  survey_type <- gsub(" .*", "", survey$type)
  survey_type[survey_type %notin% c("select_one", "select_multiple")] <- NA
  survey_name <- survey$name[!is.na(survey_type)]
  
  vars <- survey_name
  length_vars <- length(vars)
  ###########################################
  # initializing global vars
  data <- analysis_results #analysisresult the analysis resutls itself
  #filters 1,2,3 ranks and variables
  data <- data[data$rank %in% seq(1:top_n) & data$xi %in% vars,]
  
  data$col_ind <- paste0(data$xi,"--no",data$rank)
  data$val_lab <- paste0(data$xi,"--lab",data$rank)
  
  mergelevel <- unique(data$yi)
  # mergelevel <- c("hh_location")
  
  datamerge <- data.frame()
  rows <- unique(data[data$yi %in% mergelevel, "group"])
  col.names1 <- unique(data$col_ind)
  col.names2 <- unique(data$val_lab)
  # row = 1
  
  
  for(row in 1:length(rows)){
    datamerge[row,"level"] <- unique(data[data$group == rows[row],"yi"])
    datamerge[row,"disaggregation"] <- rows[row]
    
    #Look for values
    # col = "hh_business_network_type_no1"
    for(col in col.names1){
      #print(col)
      temp_val <- data[data$group == rows[row] & data$col_ind == col,"value"]
      if(length(temp_val) > 0){
        datamerge[row, col] <- temp_val
      }else{
        datamerge[row,col] <- NA_real_
      }
    }
    #Look for labels
    for(lab in col.names2){
      temp_lab <- data[data$group == rows[row] & data$val_lab == lab, "variable_label"]
      if(length(temp_lab) > 0){
        datamerge[row, lab] <-  temp_lab
      }else{
        datamerge[row, lab] <- NA_character_
      }
    }
  }
  
  datamerged <- datamerge[,sort(colnames(datamerge))]
  #return(datamerged)
  
  ##############################################
  
  
  output <-  datamerged %>% select(disaggregation,level, everything())
  
  #### Reformating the column names of output
  output[nrow(output)+1,"disaggregation"] <- "Question Label"
  output[nrow(output)+1,"disaggregation"] <- "Ranking" 
  
  # col = 3
  for(col in 3:ncol(output)){
    # print(colnames(output)[col])
    temp_col <- gsub("--lab1|--lab2|--lab3|--lab4|--lab5|--lab6|--lab7|--no1|--no2|--no3|--no4|--no5|--no6|--no7","",colnames(output[col]))
    temp_rank <- sub(".*--", "", colnames(output[col]))
    
    rank_value <- if_else(temp_rank == "no1","Value to Rank 1",
                          if_else(temp_rank == "no2", "Value to Rank 2",
                                  if_else(temp_rank == "no3","Value to Rank 3",
                                          if_else(temp_rank == "no4","Value to Rank 4",
                                                  if_else(temp_rank == "no5","Value to Rank 5",
                                                          if_else(temp_rank == "no6","Value to Rank 6",
                                                                  if_else(temp_rank == "no7","Value to Rank 7",
                                                                          
                                                                          if_else(temp_rank == "lab1","Label of Rank 1",
                                                                                  if_else(temp_rank == "lab2","Label of Rank 2",
                                                                                          if_else(temp_rank == "lab3","Label of Rank 3",        
                                                                                                  if_else(temp_rank == "lab4","Label of Rank 4",        
                                                                                                          if_else(temp_rank == "lab5","Label of Rank 5",        
                                                                                                                  if_else(temp_rank == "lab6","Label of Rank 6", "Label of Rank 7")))))))))))))
    
    col_lab <- unique(data$xi_lab[data$xi == temp_col])
    
    output[nrow(output)-1,col] <- col_lab
    output[nrow(output), col] <- rank_value
    # cat("\014")
    # cat("print for column: ", colnames(output[col]),"\n")
  }
  
  part2 <- output[c(nrow(output)-1,nrow(output)),]
  part1 <- output[1:(nrow(output)-2),]
  
  
  final_output <- bind_rows(part2, part1)
  cat("\014","Successfully TOP ", top_n, " Choices Generated", "\n", "From total of ",length_vars, " Select_one & Select Multiple Questions in your tool!")  
  return(final_output)
}