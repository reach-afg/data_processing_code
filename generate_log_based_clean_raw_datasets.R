
#Log changes
log_recodes <- function(rawDF, cleanedDF, uuid_){
  #Start Log
  # Create empty vectors
  question <- vector()
  old_value <- vector()
  new_value <- vector()
  uuid <- vector()
  
  for (j in 1:length(rawDF)) {
    
    for (rowi in 1:nrow(rawDF)){
      value_raw <- rawDF[rowi, j]
      value_clean <- cleanedDF[rowi, j]
      
      # create logical check temp var
      temp <- value_clean == value_raw
      
      # condition
      if (!temp & !is.na(temp)){
        value_raw <- rawDF[rowi, j]
        value_clean <- cleanedDF[rowi, j]
        
        # append values to vectors
        question <- c(question,names(rawDF[j]))
        old_value <- c(old_value, as.character(value_raw))
        new_value <- c(new_value, as.character(value_clean))
        uuid <- c(uuid, as.character(rawDF[rowi,uuid_]))
      }
      
    }
    # progress
    cat("\014")
    print (paste("Logging Column", j, "of", length(rawDF)))
  }
  
  my_log <- data.frame(question, old_value, new_value, uuid)
  return(my_log)
  #End Log
}


# rename columns - change "/" to "."
rename_vars <- function(df) {
  gsub("/", ".", names(df))
} 

# batch recoding function, requires two parameters, 1. raw data set, 2. recoding plan
recode <- function(rawDF, recodingPlan){
  cleaned_df <- rawDF %>% recode_batch(tos = recodingPlan$to_values,
                                        wheres = recodingPlan$conditions,
                                        targets = recodingPlan$target_variables) %>% 
                                        end_recoding()
  return(cleaned_df)
}

#Overwrite values from recoded columns into original columns
overwrite_recodes <- function(recodedDF, recodingPlan){
  
  distinct_vars <- unique(recodingPlan$target_variables)
  #Extracts origin_var_name from recoded_var_name and replace values from recoded variables to original varialbes 
  for (i in 1:length(distinct_vars)) {
    recoded_var_name <- distinct_vars[i]
    origin_var_name <- substr(recoded_var_name,1, nchar(recoded_var_name)-8)
    recodedDF[[origin_var_name]] <- recodedDF[[recoded_var_name]]
  }
  
  #returns the fial result
  return(recodedDF)
  
}

#Checks recoding
check_recoding <- function(rawDF, cleanedDF, uuid_){
  # Create empty vectors
  question <- vector()
  old_value <- vector()
  new_value <- vector()
  uuid <- vector()
  
  for (j in 1:length(rawDF)) {
    
    for (rowi in 1:nrow(rawDF)){
      value_raw <- rawDF[rowi, j]
      value_clean <- cleanedDF[rowi, j]
      
      # create logical check temp var
      temp <- is.na(value_clean) & !is.na(value_raw)
      
      # condition
      if (temp & !is.na(temp)){
        value_raw <- rawDF[rowi, j]
        value_clean <- cleanedDF[rowi, j]
        
        # append values to vectors
        question <- c(question,names(rawDF[j]))
        old_value <- c(old_value, as.character(value_raw))
        new_value <- c(new_value, as.character(value_clean))
        uuid <- c(uuid, as.character(rawDF[rowi,uuid_]))
      }
      
    }
    # progress
    cat("\014")
    print (paste("Checking Column", j, "of", length(rawDF)))
  }
  
  checks <- data.frame(question, old_value, new_value, uuid)
  return(checks)
}


#Checks if log is correctly applied on the data
check_log <- function(rawDF, cleanedDF, uuid_){
  # Create empty vectors
  question <- vector()
  old_value <- vector()
  new_value <- vector()
  uuid <- vector()
  
  for (j in 1:length(rawDF)) {
    
    for (rowi in 1:nrow(rawDF)){
      value_raw <- rawDF[rowi, j]
      value_clean <- cleanedDF[rowi, j]
      
      # create logical check temp var
     # temp <- is.na(value_clean) & !is.na(value_raw)
      temp <- as.character(value_raw) %in% as.character(value_clean)
      # condition
      if (!isTRUE(temp)){
        value_raw <- rawDF[rowi, j]
        value_clean <- cleanedDF[rowi, j]
        
        # append values to vectors
        question <- c(question,names(rawDF[j]))
        old_value <- c(old_value, as.character(value_raw))
        new_value <- c(new_value, as.character(value_clean))
        uuid <- c(uuid, as.character(rawDF[rowi,uuid_]))
      }
      
    }
    # progress
    cat("\014")
    print(paste("Checking Column", j, "of", length(rawDF)))
  }
  
  checks <- data.frame(question, old_value, new_value, uuid)
  return(checks)
}

# Negage %in% function

`%notin%` <- Negate(`%in%`)





