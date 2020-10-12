
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(lubridate)
source("functions/generate_log_based_clean_raw_datasets.R")

`%notin%` <- Negate(`%in%`)
# Raw Data
raw_data <- read_excel("input/raw_data/AFG2005_WoA_MSNA_2020_-_all_versions_-_False_-_2020-09-17-04-46-51.xlsx")
hh_roster <- read_excel("input/raw_data/AFG2005_WoA_MSNA_2020_-_all_versions_-_False_-_2020-09-17-04-46-51.xlsx", sheet = "c_2")
left <- read_excel("input/raw_data/AFG2005_WoA_MSNA_2020_-_all_versions_-_False_-_2020-09-17-04-46-51.xlsx", sheet = "m1")
died <- read_excel("input/raw_data/AFG2005_WoA_MSNA_2020_-_all_versions_-_False_-_2020-09-17-04-46-51.xlsx", sheet = "m2")


# Deletions log
deletion_log <- read_excel("input/cleaning_log/Cleaning_log_final.xlsx", sheet = "deletions")

# Cleaning log
cleaning_log <- read_excel("input/cleaning_log/Cleaning_log_final.xlsx")

# filter invalid interviews
raw_data_valid <- raw_data %>% filter(`_uuid` %notin% deletion_log$`_uuid`)
raw_data_valid <- raw_data_valid %>% 
  filter(consent == "yes" & pop_group != "CHECK" & respondent_age == "yes")

# Dataframe to check if log is applied correctly
raw_data_valid_temp <- raw_data_valid

# filter out invalid entries form loops
hh_roster_valid <- hh_roster %>% filter(`_submission__uuid` %in% raw_data_valid$`_uuid`)
left_valid <- left %>% filter(`_submission__uuid` %in% raw_data_valid$`_uuid`)
died_valid <- died %>% filter(`_submission__uuid` %in% raw_data_valid$`_uuid` )


# Apply cleaning log on raw data
for (rowi in 1:nrow(cleaning_log)){
  
  uuid_i <- cleaning_log$uuid[rowi]
  var_i <- cleaning_log$question.name[rowi]
  old_i <- cleaning_log$old.value[rowi]
  new_i <- cleaning_log$new.value[rowi]
  print(paste("uuid", uuid_i, "Old value: ", old_i, "changed to", new_i, "for", var_i))
  # Find the variable according to the row of the cleaning log
  raw_data_valid[raw_data_valid$`_uuid` == uuid_i, var_i] <- new_i
}

# Check if log is correctly applied on the raw data - get a cup of coffee and get back in 20 mins
check <- check_log(raw_data_valid_temp, raw_data_valid, uuid_ = "_uuid")
check$key <- paste0(check$question, "_", check$uuid)
cleaning_log$key <- paste0(cleaning_log$question.name,"_",cleaning_log$uuid)

# find if any entry in log is not replaced in the dataset - if the result is 0 obs, then the log is correctly applied on the raw data
find_missing <- anti_join(cleaning_log, check, by = "key")

# write.csv(find_missing, "missing_log_entries2.csv")

# cleaninginspectoR
check_outlier <- cleaninginspectoR::inspect_all(raw_data_valid, uuid.column.name = "_uuid")
# write.csv(check_outlier, "outliers.csv", row.names = F)


df_list <- list(Data = raw_data_valid, roster = hh_roster_valid, left = left_valid, died = died_valid )

openxlsx::write.xlsx(df_list, "output/MSNA_2020_clean_data_final_2020-09-21_final.xlsx")




