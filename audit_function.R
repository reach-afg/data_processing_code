
library(tidyr)
library(dplyr)
library(expss)
# Audit result function
audit_results <- function(audit_dir_path, df_raw, enumerator_code, x_uuid, today){
  
  audit_dir<-audit_dir_path
  uuids<-dir(audit_dir)
  uuid_file<-paste0(audit_dir,"\\","\\audit.csv")
  dfl<-list()
  all_uuids <- length(uuids)
  
  for(i in 1: all_uuids){
    df <-read.csv(paste0(audit_dir, uuids[i],"/audit.csv"))
    df$uuid <- uuids[i]
    dfl[[i]]<- df
    cat("\014","Running audit: ", round((i/all_uuids) * 100,0),"%", sep = "")
  }

  audit_df <- do.call("rbind", dfl)
  audit_df$node <- sub('.*/', '', audit_df$node)
  # only_questions <- audit_df %>% filter(node != "")
  
  audit_joined <- audit_df %>% 
    left_join(select(df_raw, enumerator_code, x_uuid, today), by = c("uuid"="_uuid"))
  
  audit_joined <- audit_joined %>% 
    mutate(
      time_seconds =  round((end - start) * 0.001,1),
      time_minutes =  round(time_seconds / 60,2)
    )
  
  other_events <- audit_joined %>% filter(event != "question")
  only_questions <- audit_joined %>% filter(event == "question")
  
  return(list(question_events = only_questions, other_events = other_events))
  
}


### Reports function
audit_report_wide <- function(audit_results, enumerator_code){
  
  report <- audit_results %>% 
    group_by(Enumerator_Unique_Id = .[,enumerator_code], node) %>%
    summarise(seconds_mean = round(mean(time_seconds, na.rm = T),1)
    )
  
  report_by_date <- audit_results %>% 
    group_by(today, Enumerator_Unique_Id = .[,enumerator_code], node) %>%
    summarise(seconds_mean = round(mean(time_seconds, na.rm = T),1)
    )
  
  report_wide <- report %>%
    select(Enumerator_Unique_Id, node, seconds_mean) %>%
    pivot_wider(names_from = Enumerator_Unique_Id, values_from = seconds_mean)
  
  report_wide_2 <- report %>%
    select(Enumerator_Unique_Id, node, seconds_mean) %>%
    pivot_wider(names_from = node  , values_from = seconds_mean)
  
  return(list(report_wide_question = report_wide, report_wide_by_enum = report_wide_2, report = report,
              report_by_date = report_by_date))
  
}


#time check based on start end time
time_check <- function(df, time_limit){
  df <- df %>% mutate(start = ymd_hms(start), end = ymd_hms(end),
                      interview_duration = difftime(as.POSIXct(end), as.POSIXct(start), units = "mins"),
                      time_validity = case_when( 
                        interview_duration < time_limit ~ "Deleted",
                        interview_duration >= time_limit ~ "Valid"
                        
                      )
              )
  return(df)
}




time_check_audit <- function(df_raw, x_uuid, time_limit, audit_dir_path = "./audit/", today = "today"){
# Audit Checks
audit_dir<-audit_dir_path
uuids<-dir(audit_dir)
uuid_file<-paste0(audit_dir,"\\","\\audit.csv")
dfl<-list()
all_uuids <- length(uuids)

for(i in 1: length(uuids)){
  df <-read.csv(paste0(audit_dir, uuids[i],"/audit.csv"))
  df <- df %>% filter(node != "")
  duration_ms<- sum(df$end - df$start)
  duration_secs<-duration_ms/1000
  duration_minutes<- round(duration_secs/60,1)
  dfl[[i]]<-data.frame(uuid =uuids[i],duration_ms=duration_ms,durations_secs=duration_secs,duration_minutes = duration_minutes)
  cat("\014","Running audit: ", round((i/all_uuids) * 100,0),"%", sep = "")
}
duration_df <- do.call("rbind", dfl)
duration_df <- rename(duration_df, `_uuid` = uuid)


#time check based on start end time
df_no_audit_files <- df_raw %>% mutate(start = ymd_hms(start), 
                                   end = ymd_hms(end),
                                   start_end = round(as.POSIXct(end) - as.POSIXct(start)))

# Join Audit checks and main dataset
df_str_audit_all <- df_raw %>%
  left_join(select(df_no_audit_files, x_uuid, start_end), by = c("_uuid"="_uuid"))

# Calculating time using start - end time for missing audit files
df_str_audit_all <- df_str_audit_all %>%
  left_join(select(duration_df, x_uuid,  duration_audit = duration_minutes), by = c("_uuid"="_uuid"))

# Merging both audit checks
df_str_audit_all <- df_str_audit_all %>% 
  mutate(interview_duration = if_na(duration_audit, start_end),
         time_validity = case_when( 
           interview_duration < time_limit ~ "deleted",
           interview_duration >= time_limit ~ "valid")
  ) 

 
 return(df_str_audit_all)
}

############################################################################
