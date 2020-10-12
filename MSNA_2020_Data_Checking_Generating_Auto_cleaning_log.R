# Process data, logical/GIS checks, Generate Automatic cleaning Log.
library(dplyr)
library(lubridate)
library(readxl)
library(kableExtra)
library(knitr)
library(readr)
library(readxl)
library(tidyr)
### GIS Checks
library(sf)
source("functions/audit_function_full.R")
`%notin%` <- Negate(`%in%`)
# Global vars
assessment_start_date <- as.Date("2020-08-09")
time_limit <- 30
max_interv <- 6


sample_df <- read_excel("input/sample/WoAA_2020_Compiled_Sample_20200721.xlsx")
sample_df <- sample_df %>% mutate(
  pop_group_final = case_when(
    pop_group == "recent_returnees" | pop_group == "non_recent_returnees" ~ "returnees",
    TRUE ~ pop_group
  ),
  prov_key = paste0(province_kobo,"_",pop_group_final),
  dist_key = paste0(district_kobo,"_",pop_group_final)
)
# dist_sample_check <- sample_df %>% group_by(dist_key, pop_group_sample =pop_group_final) %>% summarise(
#   Target = sum(survey_buffer, na.rm = T)
# ) %>%  select(dist_key, pop_group_sample)

dist_sample_check <- read_excel("input/sample/dist_sample_updated.xlsx") %>%  filter(Target != 0) %>% select(dist_key = dist_key, pop_group_sample = pop_group_final ) 
# data from yesterday 
raw_data_yesterday_path <- "input/raw_data/Archive/AFG2005_WoA_MSNA_2020_-_all_versions_-_False_-_2020-09-16-04-10-35.xlsx"
# new data path 
raw_data_path <- "input/raw_data/AFG2005_WoA_MSNA_2020_-_all_versions_-_False_-_2020-09-17-04-46-51.xlsx"

#get uuids of yesterday's data 
raw_data_yesterday <- read_excel(raw_data_yesterday_path) %>% select(`_uuid`)


# import raw data
raw_data <- read_excel(raw_data_path) %>% type.convert(as.is = T)
raw_data$enumerator_uuid <- toupper(raw_data$enumerator_uuid)

# import hoh roster
hh_roster <- read_excel(raw_data_path, sheet = "c_2" ) %>% type.convert(as.is = T)
hh_roster_hoh <- hh_roster %>%  filter(hh_member == 1) %>% select(hoh_joined_roster = hoh_joined_001, `_uuid` = `_submission__uuid`)

# import died roster
hh_died <- read_excel(raw_data_path, sheet = "m2" ) %>% type.convert(as.is = T)
# import Left roster
hh_left <- read_excel(raw_data_path, sheet = "m1" ) %>% type.convert(as.is = T)


data <- raw_data %>% 
  filter(consent == "yes" & pop_group != "CHECK" & respondent_age == "yes")

# join hoh_joined from roster
data <- data %>% left_join(hh_roster_hoh, by = "_uuid")

# invalid interviews
invalid_data <- raw_data %>% filter(consent != "yes" | pop_group == "CHECK" | respondent_age != "yes") %>%
  select(start:pop_group)


# Population groups
data <- data %>% mutate(
  pop_group_national = case_when(
    pop_group == "idp" & idp_displace_hamal == "after_hamal" ~ "recent_idps",
    pop_group == "idp" & idp_displace_hamal == "before_hamal" ~ "non_recent_idps",
    pop_group == "cb_returnee" & cb_return_when == "recent_returnee" ~ "recent_returnees",
    pop_group == "cb_returnee" & cb_return_when == "non_recent_returnee" ~ "non_recent_returnees",
    pop_group == "non_displaced_vulnerable" ~ "vulnerable",
    TRUE ~ pop_group
  ),
  pop_group_final = case_when(
    pop_group == "idp" & idp_displace_hamal == "after_hamal" ~ "recent_idps",
    pop_group == "idp" & idp_displace_hamal == "before_hamal" ~ "non_recent_idps",
    pop_group == "cb_returnee" ~ "returnees",
    pop_group == "non_displaced_vulnerable" ~ "vulnerable",
    TRUE ~ pop_group
  )
)

# key to join data with sample 
data$prov_key <- paste0(data$province,"_", data$pop_group_final)
data$dist_key <- paste0(data$district, "_", data$pop_group_final)

data <- data %>% left_join(dist_sample_check, by = "dist_key")

# Join settlement codes
sett_names <- sample_df  %>%  select(setl_code, settlement) 
names(sett_names) <- c("village", "village_eng")

data <- data %>% 
  left_join(sett_names, by = "village") %>% 
  mutate(
    village_eng = case_when(
      is.na(village_eng) ~ village_other,
      TRUE ~ village_eng
    )
  )


dual_inter_correct <- read_excel("input/dual_interviews/dual_interveiw_corrections.xlsx") %>% select(`_uuid`, correct_dual_intev)
data <- data %>% left_join(dual_inter_correct, by = "_uuid") %>% mutate(
  interview_type = case_when(
    !is.na(correct_dual_intev) ~ correct_dual_intev,
    TRUE ~ interview_type
  )
)

# Audit Time check
data <- time_check_audit(audit_dir_path = "./audit/", data,  "_uuid", time_limit = time_limit)


# Gis flags
district_df <- st_read("shape_file/MSNA_20_Districts.shp", as_tibble = TRUE, quiet=TRUE )
district_df$Dstrct_Eng <- tolower(district_df$Dstrct_Eng)
district_df$Dstrct_Eng <- gsub(x = district_df$Dstrct_Eng, pattern = "\\ ", replacement = "_")
district_df$Dstrct_Eng <- gsub(x = district_df$Dstrct_Eng, pattern = "\\-", replacement = "_")
district_df$Dstrct_Eng <- gsub(x = district_df$Dstrct_Eng, pattern = "\\(", replacement = "")
district_df$Dstrct_Eng <- gsub(x = district_df$Dstrct_Eng, pattern = "\\)", replacement = "")

# filter extra vars 
district_df <- district_df %>% 
  select(OBJECTID, AREA_SQKM, Shape_Area, Prvnce_Eng, Dstrct_Eng, geometry)

surv_data <- data %>% 
  select(village_eng, district, enumerator_uuid, `_geopoint_longitude`, `_geopoint_latitude`, `_uuid`) %>% as_tibble() %>% 
  filter(!is.na(`_geopoint_longitude`))
surv_data$`_geopoint_longitude` <- as.numeric(surv_data$`_geopoint_longitude`)
surv_data$`_geopoint_latitude` <- as.numeric(surv_data$`_geopoint_latitude`)

# Convert CSV points df to sf object
surv_data <- surv_data  %>%   # coordinates must be numeric
  st_as_sf(
    coords = c("_geopoint_longitude", "_geopoint_latitude"),
    agr = "constant",
    crs = 4326,        # coordinate system
    stringsAsFactors = FALSE,
    remove = TRUE
  ) 

is_in_district_data <- st_join(surv_data, district_df, join = st_intersects) %>% mutate(
  
)

is_in_district_data_final <- is_in_district_data %>% 
  filter(Dstrct_Eng != district | is.na(Dstrct_Eng)) %>% mutate(
    GIS_wrong_distric_flag = "Check"
  ) %>% select(GIS_wrong_distric_flag, `_uuid`) %>% st_drop_geometry()

# for GIS team
for_gis_team <- is_in_district_data %>% 
   mutate(
    GIS_wrong_distric_flag = case_when(
      Dstrct_Eng != district | is.na(Dstrct_Eng) ~ "Check",
      TRUE ~ NA_character_
    ) 
  ) %>% st_drop_geometry() %>% left_join(select(data, `_geopoint_longitude`, `_geopoint_latitude`, `_uuid`), by = "_uuid")

openxlsx::write.xlsx(for_gis_team, "output/gis_checks/WoA_2020_GIS_Checks_2020-09-17.xlsx")

data <- data %>% left_join(is_in_district_data_final, by = "_uuid")

# logical flaggs

data <- data %>% 
  mutate(
    disabled_hoh_comm_bin = case_when(
      disabled_hoh_comm == "yes" ~ 1,
      TRUE ~ 0  
    ),
   disabled_hoh_hear_bin = case_when(
     disabled_hoh_hear == "yes" ~ 1,
     TRUE ~ 0
   ),
   disabled_hoh_memory_bin = case_when(
     disabled_hoh_memory == "yes" ~ 1,
     TRUE ~ 0
   ),
   disabled_hoh_see_bin = case_when(
     disabled_hoh_see == "yes" ~ 1,
     TRUE ~ 0
   ),
   disabled_hoh_selfcare_bin = case_when(
     disabled_hoh_selfcare == "yes" ~ 1,
     TRUE ~ 0
   ),
   disabled_hoh_walk_bin = case_when(
     disabled_hoh_walk == "yes" ~ 1,
     TRUE ~ 0
   ),
   disabled_hoh_total = disabled_hoh_comm_bin + disabled_hoh_hear_bin + disabled_hoh_memory_bin + disabled_hoh_see_bin + disabled_hoh_selfcare_bin + disabled_hoh_walk_bin
  ) %>% select(-c(disabled_hoh_comm_bin, disabled_hoh_hear_bin,disabled_hoh_memory_bin, disabled_hoh_see_bin, disabled_hoh_selfcare_bin, disabled_hoh_walk_bin  ))
  


data <- data %>% mutate(
  wrong_pop_group_flag = case_when(
    is.na(pop_group_sample) ~ "Check",
    TRUE ~ NA_character_
  ),
  primary_care_giver_flag = case_when(
    primary_care_giver == "yes" & under_18_total < 1 ~ "Check",
    TRUE ~ NA_character_
  ),
  factors_list_conflict_flag = case_when(
    pop_group_final == "recent_idps" & (idp_push_factors.active_conflict != major_event.conflict) ~ "Check",
    TRUE ~ NA_character_
  ),
  factors_list_earthquake_flag = case_when(
    pop_group_final == "recent_idps" & (idp_push_factors.earthquake != major_event.earthquake) ~ "Check",
    TRUE ~ NA_character_
  ),
  factors_list_floods_flag = case_when(
    pop_group_final == "recent_idps" & (idp_push_factors.floods != major_event.floods) ~ "Check",
    TRUE ~ NA_character_
  ),
  factors_list_avalanche_flag = case_when(
    pop_group_final == "recent_idps" & (idp_push_factors.avalanche != major_event.avalanche) ~ "Check",
    TRUE ~ NA_character_
  ),
  factors_list_precipitation_deficit_flag = case_when(
    pop_group_final == "recent_idps" & (idp_push_factors.precipitation_deficit != major_event.drought) ~ "Check",
    TRUE ~ NA_character_
  ),
  non_displaced_unattending_female_flag = case_when(
    forcibly_displaced == "non_displaced" & girls_unattending_reason.remov_due_disp == 1  ~ "Check",
    TRUE ~ NA_character_
  ),
  non_displaced_unattending_male_flag = case_when(
    forcibly_displaced == "non_displaced" & boys_unattending_reason.remov_due_disp == 1  ~ "Check",
    TRUE ~ NA_character_
  ),
  unattending_reason_no_girls_flag = case_when(
    enrolled_total > 0 & girls_unattending_reason.lack_docu_enrol == 1 ~ "Check",
    TRUE ~ NA_character_
  ),
  unattending_reason_no_boys_flag = case_when(
    enrolled_total > 0 & boys_unattending_reason.lack_docu_enrol == 1 ~ "Check",
    TRUE ~ NA_character_
  ),
  why_child_away_total = seek_employment + study + kidnapped  + migrated + addiction + other,
  
  why_child_away_flagg = case_when(
    why_child_away_total > 8 ~ "Check",
    TRUE ~ NA_character_
  ),
  non_displaced_income_low_flag = case_when(
    forcibly_displaced == "non_displaced" & income_lower_reason.migr_disp == 1 ~ "Check",
    TRUE ~ NA_character_
  ),
  lcsi_married_daughters_check = case_when(
    (lcsi_married_daughters == "yes" | lcsi_married_daughters == "exhausted") & (gone_girl == "no") ~ "Check",
    TRUE ~ NA_character_
  ),
  need_repay_debt_flag = case_when(
    (debt_amount == 0 | is.na(debt_amount)) & data$three_needs.repay_dept == 1 ~ "Check",
    TRUE ~ NA_character_
  ),
  three_needs_edu_child_flag = case_when(
    three_needs.edu_child == 1 &  children_ed < 1 ~ "Check",
    TRUE ~ NA_character_
  ),
  
  fcs_sum = cereals_tubers + pulses_nuts + vegetables + fruit + meat_fish_eggs + dairy + sugars + oils,
  
  fcs_score_low_flag = case_when(
    food_consumption_score < 14 ~ "Check",
    TRUE ~ NA_character_
  ),
  fcs_sum_high_flag = case_when(
    fcs_sum >= 40 ~ "Check",
    TRUE ~ NA_character_
  ),
  
  children_under_12_sum = males_0_2_total + males_3_5_total + males_6_12_total + females_0_2_total + females_3_5_total + females_6_12_total,
  
  restrict_consumption_flag = case_when(
    restrict_consumption > 0 & children_under_12_sum < 1 ~ "Check",
    TRUE ~ NA_character_
  ),
  shelter_type_room_flag = case_when(
    (shelter_type == "tent" | shelter_type == "makeshift_shelter") & rooms > 3 ~ "Check",
    TRUE ~ NA_character_
  ),
  health_exp_covid_related_flag = case_when(
    (is.na(health_exp) | health_exp == 0) & pay == "covid_related"  ~ "Check",
    TRUE ~ NA_character_
  ),
  health_exp_other_illness_flag = case_when(
    (is.na(health_exp) | health_exp == 0) & pay == "other_Illness" ~ "Check",
    TRUE ~ NA_character_
  ),
  changes_in_behavior_bedwetting_flag = case_when(
    changes_in_behavior.Bedwetting == 1 & under_18_total < 1 ~ "Check",
    TRUE ~ NA_character_
  ),
  main_cause_behavior_school_flag = case_when(
    main_cause_behavior == "interruption_of_exclusion_from_school" & children_ed < 1 ~ "Check",
    TRUE ~ NA_character_
  ),
  main_cause_behavior_conflict_flag = case_when(
    main_cause_behavior == "experiencing_conflict_or_violence" & major_event.conflict == 0 ~ "Check",
    TRUE ~ NA_character_
  ),
  share_latrine_flag = case_when(
    how_many_hh_share >= 8 ~ "Check",
    TRUE ~ NA_character_
  ),
  hoh_joined_flag = case_when(
    hoh_joined != hoh_joined_roster ~ "Check",
    TRUE ~ NA_character_
  ),
  time_60_99_flag = case_when(
    start_end > 59 & start_end > 100 ~ "Check",
    TRUE ~ NA_character_
  ),
  major_event_avanlanche_flag = case_when(
    major_event.avalanche == 1 & is.na(lockdown_impact_ways) ~ "Check",
    TRUE ~ NA_character_
  ),
  disa_multi_flag = case_when(
    hh_members_disabled_comm > 0 & hh_members_disabled_hear > 0 & hh_members_disabled_memory > 0 & hh_members_disabled_see > 0 & hh_members_disabled_selfcare > 0 &
      hh_members_disabled_walk > 0 ~ "Check",
    TRUE ~ NA_character_
  ),
  hoh_disability_flag = case_when(
    disabled_hoh_total > 2 ~ "Check",
    TRUE ~ NA_character_
  ),
  lcsi_flag = case_when(
    (lcsi_migrated == lcsi_savings) & (lcsi_savings == lcsi_household) & (lcsi_food == lcsi_income_equipment) & (lcsi_charity == lcsi_delayed_medical_care) &
      (lcsi_sold_land == lcsi_sold_fem_animal) == (lcsi_married_daughters == lcsi_engage_in_illegal_acts) & 
      (lcsi_savings == lcsi_migrated) & (lcsi_savings == lcsi_household) & (lcsi_savings == lcsi_income_equipment) & (lcsi_savings == lcsi_delayed_medical_care) &
      (lcsi_savings == lcsi_sold_fem_animal) == (lcsi_savings == lcsi_engage_in_illegal_acts) &
      (lcsi_household == lcsi_savings) & (lcsi_household == lcsi_savings) & (lcsi_household == lcsi_income_equipment) & (lcsi_household == lcsi_delayed_medical_care) &
      (lcsi_household == lcsi_sold_fem_animal) & (lcsi_household == lcsi_engage_in_illegal_acts) & 
      (lcsi_food == lcsi_savings) & (lcsi_food == lcsi_household) & (lcsi_food == lcsi_income_equipment) & (lcsi_food == lcsi_delayed_medical_care) &
      (lcsi_food == lcsi_sold_fem_animal) & (lcsi_food == lcsi_engage_in_illegal_acts) &
      (lcsi_charity == lcsi_savings) & (lcsi_charity == lcsi_household) & (lcsi_charity == lcsi_income_equipment) & (lcsi_charity == lcsi_delayed_medical_care) &
      (lcsi_charity == lcsi_sold_fem_animal) == (lcsi_charity == lcsi_engage_in_illegal_acts) ~ "Check",
    TRUE ~ NA_character_
  ),
  members_died_left_flag = case_when(
    (members_left > 1 &  members_died > 1) &  (members_left == members_died) ~ "Check",
    TRUE ~ NA_character_
  ),
  members_left_more_2_flag = case_when(
    members_left > 2 ~ "Check",
    TRUE ~ NA_character_
  ),
  total_cash_income_flag = case_when(
    total_cash_income > 0 & total_cash_income < 1000 ~ "check",
    TRUE ~ NA_character_
  ),
  food_exp_flag = case_when(
    food_exp < 500 ~ "check",
    TRUE ~ NA_character_
  ),
  water_exp_flag = case_when(
   (water_exp > 0 & water_exp < 100) | water_exp > 1000 ~ "check",
   TRUE ~ NA_character_
  ),
  rent_exp_flag = case_when(
    (rent_exp > 0 & rent_exp < 100) | rent_exp > 25000 ~ "check",
    TRUE ~ NA_character_
  ),
  health_exp_flag = case_when(
    health_exp > 0 & health_exp < 200 ~ "check",
    TRUE ~ NA_character_
  ),
  fuel_exp_flag = case_when(
    (fuel_exp > 0 & fuel_exp < 200) | fuel_exp > 5000 ~ "check",
    TRUE ~ NA_character_
  ),
  
  debt_exp_flag = case_when(
    debt_exp > 0 & debt_exp < 200 ~ "check",
    TRUE ~ NA_character_
  )
)


# temp <- data %>% select(ends_with("_flag"))
data$sum_checks <- rowSums(data == "Check", na.rm = T)

# extra deletions
deletions <- read_excel("input/extra_deletions/deletions.xlsx")

data <- data %>% left_join(deletions, by = c("_uuid" = "uuid"))

# deletion reasons
data <- data %>% mutate(
  deletion_criteria_time = case_when(
    time_validity == "deleted" ~ "Check",
    TRUE ~ NA_character_
  ),
  deletion_criteria_wrong_population = wrong_pop_group_flag,
  deletion_criteria_low_FCS_score = fcs_score_low_flag,
  deletion_criteria_long_interview = case_when(
    start_end >= 100 ~ "Check",
    TRUE ~ NA_character_
  ),
  deletion_criteria_6_flags = case_when(
    sum_checks > 5 ~ "Check",
    TRUE ~ NA_character_
  ),
  deletion_criteria_other = case_when(
    !is.na(other_deletions) ~ other_deletions,
    TRUE ~ NA_character_
  )
    
)


data <- data %>% 
  mutate(
    validity = case_when(
      time_validity == "deleted" | wrong_pop_group_flag == "Check" | fcs_score_low_flag == "Check" | start_end >= 100 | sum_checks > 5 | deletion_criteria_other != "trump_did_it"  ~ "deleted",
      TRUE ~ time_validity 
    )
  )


# re-include interviews 
re_include <- read_excel("input/re_include/re_include.xlsx")

data <- data %>% 
  mutate(
    validity = case_when(
      `_uuid` %in% re_include$`_uuid` ~ "valid",
      TRUE ~ validity
    )
  )


# Deletion Report
deletion_report <- data %>% select(
                                   start,
                                   end,
                                   `_submission_time`,
                                   partner = organisation_final,
                                   enumerator_uuid,
                                   region,
                                   province,
                                   district,
                                   village_eng,
                                   pop_group_final,
                                   deletion_criteria_time,
                                   deletion_criteria_wrong_population,
                                   deletion_criteria_low_FCS_score,
                                   deletion_criteria_long_interview,
                                   deletion_criteria_6_flags,
                                   deletion_criteria_other,
                                   validity
                                   ) %>% mutate(
                                     deletion_criteria_time = case_when(
                                       deletion_criteria_time == "Check" ~ "Short Interview",
                                       TRUE ~ NA_character_
                                     ),
                                     deletion_criteria_wrong_population = case_when(
                                       deletion_criteria_wrong_population == "Check" ~ "Wrong population group interviewed in this district",
                                       TRUE ~ NA_character_
                                     ),
                                     deletion_criteria_low_FCS_score = case_when(
                                       deletion_criteria_low_FCS_score == "Check" ~ " Household has an food consumption score below 14, indicating that they are literally starving, which is highly unlikely",
                                       TRUE ~ NA_character_
                                     ),
                                     deletion_criteria_long_interview = case_when(
                                       deletion_criteria_long_interview == "Check" ~ "The form has been open for too long or has been edited, it indicates falsification of data"
                                     ),
                                     deletion_criteria_6_flags = case_when(
                                       deletion_criteria_6_flags == "Check" ~ "Too many issues/Flags with data to be reliable, Indication of consistent poor data quality or misunderstanding of data"
                                     )
                                   ) %>% filter(validity == "deleted")


# dual interivew problems
dual_interview <- data %>% filter(`_uuid` %notin% raw_data_yesterday$`_uuid`)
dual_interview <- dual_interview %>% filter(interview_number == "yes") %>% select(start:dual_hh_int_id, `_uuid`, validity, village_eng)
dual_interview$dual_hh_int_id <- toupper(dual_interview$dual_hh_int_id)
duual_interview <- dual_interview %>% arrange(desc(dual_hh_int_id))

openxlsx::write.xlsx(dual_interview, "dual_hh_int_id_2020_09_17.xlsx")


##### Create Cleaning log
# filter new data for log
new_data <- data %>% filter(`_uuid` %notin% raw_data_yesterday$`_uuid`)

uuid <- vector() 
enumerator <- vector()
question.name <- vector()
issue <- vector()
changed <- vector()
old.value <- vector()
new.value <- vector()
feedback <- vector()
province <- vector()
district <- vector()
settlement <- vector()
date <- vector()
partner <- vector()

for (i in 1:nrow(new_data)){
  
  if(new_data$primary_care_giver_flag [i] == "Check" & !is.na(new_data$primary_care_giver_flag [i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "primary_care_giver")
    old.value <- c(old.value,new_data$primary_care_giver[i])
    issue <- c(issue, "Repondent reports to be primary caregiver for a child in the household but no children in the household")
  }
  if(new_data$factors_list_conflict_flag[i] == "Check" & !is.na(new_data$factors_list_conflict_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "major_event.conflict")
    old.value <- c(old.value,new_data$major_event.conflict[i])
    issue <- c(issue, "Displacement factor (conflict) reponse does not match the response in (Has the majority of your household directly experienced any of the following major events in the past?) question")
  }
  if(new_data$factors_list_earthquake_flag[i] == "Check" & !is.na(new_data$factors_list_earthquake_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "major_event.earthquake")
    old.value <- c(old.value,new_data$major_event.earthquake[i])
    issue <- c(issue, "Displacement factor (earthquake) reponse does not match the response in (Has the majority of your household directly experienced any of the following major events in the past?) question")
  }
  if(new_data$factors_list_floods_flag[i] == "Check" & !is.na(new_data$factors_list_floods_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "major_event.floods")
    old.value <- c(old.value,new_data$major_event.floods[i])
    issue <- c(issue, "Displacement factor (floods) reponse does not match the response in (Has the majority of your household directly experienced any of the following major events in the past?) question")
  }
  if(new_data$factors_list_avalanche_flag[i] == "Check" & !is.na(new_data$factors_list_avalanche_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "major_event.avalanche")
    old.value <- c(old.value,new_data$major_event.avalanche[i])
    issue <- c(issue, "Displacement factor (avalanche) reponse does not match the response in (Has the majority of your household directly experienced any of the following major events in the past?) question")
  }
  if(new_data$factors_list_precipitation_deficit_flag[i] == "Check" & !is.na(new_data$factors_list_precipitation_deficit_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "major_event.drought")
    old.value <- c(old.value,new_data$major_event.drought[i])
    issue <- c(issue, "Displacement factor (drought) reponse does not match the response in (Has the majority of your household directly experienced any of the following major events in the past?) question")
  }
  if(new_data$non_displaced_unattending_female_flag[i] == "Check" & !is.na(new_data$non_displaced_unattending_female_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "girls_unattending_reason.remov_due_disp")
    old.value <- c(old.value,new_data$girls_unattending_reason.remov_due_disp[i])
    issue <- c(issue, "Household is non-displaced but reports girls to not be attending school as removed due to displacement")
  }
  if(new_data$non_displaced_unattending_male_flag[i] == "Check" & !is.na(new_data$non_displaced_unattending_male_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "boys_unattending_reason.remov_due_disp")
    old.value <- c(old.value,new_data$boys_unattending_reason.remov_due_disp[i])
    issue <- c(issue, "Household is non-displaced but reports boys to not be attending school as removed due to displacement")
  }
  if(new_data$unattending_reason_no_girls_flag[i] == "Check" & !is.na(new_data$unattending_reason_no_girls_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "girls_unattending_reason.lack_docu_enrol")
    old.value <- c(old.value,new_data$girls_unattending_reason.lack_docu_enrol[i])
    issue <- c(issue, "Household reports all school-aged girls to be enrolled but some not to be attending school because they lack documentation for enrolment")
  }
  if(new_data$why_child_away_flagg[i] == "Check" & !is.na(new_data$why_child_away_flagg[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "why_child_away_total")
    old.value <- c(old.value,new_data$why_child_away_total[i])
    issue <- c(issue, "Respond a number of children away from the home for multiple reasons which seems unlikely AND equal number of children away is unlikely")
  }
  if(new_data$non_displaced_income_low_flag[i] == "Check" & !is.na(new_data$non_displaced_income_low_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "income_lower_reason.migr_disp")
    old.value <- c(old.value,new_data$income_lower_reason.migr_disp[i])
    issue <- c(issue, "Non-displaced household reporting migration/displacement as the reason for reduced income in the last 30 days")
  }
  if(new_data$lcsi_married_daughters_check[i] == "Check" & !is.na(new_data$lcsi_married_daughters_check[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "gone_girl")
    old.value <- c(old.value,new_data$gone_girl[i])
    issue <- c(issue, "Household reported to have married daughters earlier than intended but reported no female children under 18 to no longer be living in the house")
  }
  if(new_data$need_repay_debt_flag[i] == "Check" & !is.na(new_data$need_repay_debt_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "three_needs.repay_dept")
    old.value <- c(old.value,new_data$three_needs.repay_dept[i])
    issue <- c(issue, "Housheold reports no debt, but debt repayment as a primary need")
  }
  if(new_data$three_needs_edu_child_flag[i] == "Check" & !is.na(new_data$three_needs_edu_child_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "three_needs.edu_child")
    old.value <- c(old.value,new_data$three_needs.edu_child[i])
    issue <- c(issue, "Household reports education as a primary need but no school aged children in the household")
  }
  if(new_data$fcs_sum_high_flag[i] == "Check" & !is.na(new_data$fcs_sum_high_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "fcs_sum")
    old.value <- c(old.value,new_data$fcs_sum[i])
    issue <- c(issue, "Household reports to have all food groups 5 times a week or more, which is highly unlikely")
  }
  if(new_data$restrict_consumption_flag[i] == "Check" & !is.na(new_data$restrict_consumption_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "restrict_consumption")
    old.value <- c(old.value,new_data$restrict_consumption[i])
    issue <- c(issue, "Household reports restricting consumption of food by adults for small children to eat but no children under the age of 12 in the household AND Household unlikely to use every strategy an equal number of times")
  }
  if(new_data$shelter_type_room_flag[i] == "Check" & !is.na(new_data$shelter_type_room_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "rooms")
    old.value <- c(old.value,new_data$rooms[i])
    issue <- c(issue, "Household reports to be in a makeshift shelter or tent and has 4 of more rooms")
  }
  if(new_data$health_exp_covid_related_flag[i] == "Check" & !is.na(new_data$health_exp_covid_related_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "pay")
    old.value <- c(old.value,new_data$pay[i])
    issue <- c(issue, "Household reported no health expenditures in the last 30 days but reports paying for healthcare at active health centre for covid related illness")
  }
  if(new_data$health_exp_other_illness_flag[i] == "Check" & !is.na(new_data$health_exp_other_illness_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "pay")
    old.value <- c(old.value,new_data$pay[i])
    issue <- c(issue, "Household reported no health expenditures in the last 30 days but reports paying for healthcare at active health centre for other illnesses")
  }
  if(new_data$changes_in_behavior_bedwetting_flag[i] == "Check" & !is.na(new_data$changes_in_behavior_bedwetting_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "changes_in_behavior.Bedwetting")
    old.value <- c(old.value,new_data$changes_in_behavior.Bedwetting[i])
    issue <- c(issue, "Household reported an individual in the household to be bed wetting, but no children in the household under the age of which is very unlikely")
  }
  if(new_data$main_cause_behavior_school_flag[i] == "Check" & !is.na(new_data$main_cause_behavior_school_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "main_cause_behavior")
    old.value <- c(old.value,new_data$main_cause_behavior[i])
    issue <- c(issue, "Household reports reason for behavioural change to be interruption/ removal from school but no school aged children in the household")
  }
  if(new_data$main_cause_behavior_conflict_flag[i] == "Check" & !is.na(new_data$main_cause_behavior_conflict_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "main_cause_behavior")
    old.value <- c(old.value,new_data$main_cause_behavior[i])
    issue <- c(issue, "Household reports reason for behavioural change to be experience of violence or conflict but that the household has not been afected by active conflict in the last year")
  }
  if(new_data$share_latrine_flag[i] == "Check" & !is.na(new_data$share_latrine_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "how_many_hh_share")
    old.value <- c(old.value,new_data$how_many_hh_share[i])
    issue <- c(issue, "Household reports sharing latrine with 8 or more other families, which is very unlikely")
  }
  if(new_data$hoh_joined_flag[i] == "Check" & !is.na(new_data$hoh_joined_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "hoh_joined_roster")
    old.value <- c(old.value,new_data$hoh_joined_roster[i])
    issue <- c(issue, "If head of household reported to have joined recently, but then in the first of the individual loops for household members (the first is for the HoH), they report that they did not join during or after the middle of ramandan or vice versa")
  }
  if(new_data$major_event_avanlanche_flag[i] == "Check" & !is.na(new_data$major_event_avanlanche_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "lockdown_impact_ways")
    old.value <- c(old.value,new_data$lockdown_impact_ways[i])
    issue <- c(issue, "Question not asked if respondent reports avalanche as the only event experienced - need to follow up and ask")
  }
  if(new_data$disa_multi_flag[i] == "Check" & !is.na(new_data$disa_multi_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "disability questions")
    old.value <- c(old.value, "check responses for disablity questions")
    issue <- c(issue, "Multiple members with disabilities or one member with many disabiliites is unusual")
  }
  if(new_data$hoh_disability_flag[i] == "Check" & !is.na(new_data$hoh_disability_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "disabled_hoh_total")
    old.value <- c(old.value, new_data$disabled_hoh_total[i])
    issue <- c(issue, "Unusual to have a head of household with more than two disabilities")
  }
  if(new_data$lcsi_flag[i] == "Check" & !is.na(new_data$lcsi_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "lcsi questions")
    old.value <- c(old.value, "check lcsi responses")
    issue <- c(issue, "Same response for all coping strategies in one household is unlikely")
  }
  if(new_data$members_died_left_flag[i] == "Check" & !is.na(new_data$members_died_left_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "members_left & members_died")
    old.value <- c(old.value, "check responses for members_left & members_died ")
    issue <- c(issue, "Household unlikely to have same number of missing and dead members")
  }
  if(new_data$members_left_more_2_flag[i] == "Check" & !is.na(new_data$members_left_more_2_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "members_left")
    old.value <- c(old.value, new_data$members_left[i])
    issue <- c(issue, "Unlikely to have more than 2 members left")
  }
  if(new_data$GIS_wrong_distric_flag[i] == "Check" & !is.na(new_data$GIS_wrong_distric_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "district")
    old.value <- c(old.value, new_data$district[i])
    issue <- c(issue, "GIS check: GPS location not in the district reported")
  }
  if(new_data$total_cash_income_flag[i] == "check" & !is.na(new_data$total_cash_income_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "total_cash_income")
    old.value <- c(old.value, new_data$total_cash_income[i])
    issue <- c(issue, "Questionable amount entered")
  }
  if(new_data$food_exp_flag[i] == "check" & !is.na(new_data$food_exp_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "food_exp")
    old.value <- c(old.value, new_data$food_exp[i])
    issue <- c(issue, "Questionable amount entered")
  }
  if(new_data$water_exp_flag[i] == "check" & !is.na(new_data$water_exp_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "water_exp")
    old.value <- c(old.value, new_data$water_exp[i])
    issue <- c(issue, "Questionable amount entered")
  }
  if(new_data$rent_exp_flag[i] == "check" & !is.na(new_data$rent_exp_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "rent_exp")
    old.value <- c(old.value, new_data$rent_exp[i])
    issue <- c(issue, "Questionable amount entered")
  }
  if(new_data$health_exp_flag[i] == "check" & !is.na(new_data$health_exp_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "health_exp")
    old.value <- c(old.value, new_data$health_exp[i])
    issue <- c(issue, "Questionable amount entered")
  }
  if(new_data$fuel_exp_flag[i] == "check" & !is.na(new_data$fuel_exp_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "fuel_exp")
    old.value <- c(old.value, new_data$fuel_exp[i])
    issue <- c(issue, "Questionable amount entered")
  }
  if(new_data$debt_exp_flag[i] == "check" & !is.na(new_data$debt_exp_flag[i])){
    province <- c(province, new_data$province[i])
    district <- c(district, new_data$district[i])
    settlement <- c(settlement, new_data$village_eng[i])
    date <- c(date, new_data$start[i])
    partner <- c(partner, new_data$organisation_final[i])
    uuid <- c(uuid, new_data$`_uuid`[i])
    enumerator <- c(enumerator,new_data$enumerator_uuid[i] )
    question.name <- c(question.name, "debt_exp")
    old.value <- c(old.value, new_data$debt_exp[i])
    issue <- c(issue, "Questionable amount entered")
  }
}


cleaning_log <- data.frame(date, province, district, settlement, partner, uuid,enumerator,question.name,  old.value, issue)
cleaning_log$feedback <- ""
cleaning_log$new.value <- ""
cleaning_log$changed <- ""
cleaning_log <- cleaning_log %>% select(date, province, district,
                                        settlement, partner, uuid,
                                        enumerator,question.name,
                                        issue, feedback, changed,
                                        old.value, new.value) %>% arrange(question.name)

# openxlsx::write.xlsx(deletion_report, "Deletion Report.xlsx")
# Create Directories
processed_raw_ao_path <- paste0("D:/REACH Afghanistan Dropbox/REACH_AFG/01_projects/01_woa_msna/02_WOA 2020/03_Data Processing/01_Datasets/01_Processed_Raw_Data/",today())
dir.create(processed_raw_ao_path)

processed_cleaing_log_ao_path <- paste0("D:/REACH Afghanistan Dropbox/REACH_AFG/01_projects/01_woa_msna/02_WOA 2020/03_Data Processing/01_Datasets/02_Cleaning_log/",today())
dir.create(processed_cleaing_log_ao_path)

processed_raw_field_path <- paste0("D:/REACH Afghanistan Dropbox/REACH_AFG/04_Field_Management/01_woa_msna/01_data/",today())
dir.create(processed_raw_field_path)

tracking_report_field_path <- paste0("D:/REACH Afghanistan Dropbox/REACH_AFG/04_Field_Management/01_woa_msna/02_general_tracking_report/",today())
dir.create(tracking_report_field_path)

general_deletion_report_field_path <- paste0("D:/REACH Afghanistan Dropbox/REACH_AFG/04_Field_Management/01_woa_msna/03_general_deletion_report/",today())
dir.create(general_deletion_report_field_path)

partner_deletion_report_field_path <- paste0("D:/REACH Afghanistan Dropbox/REACH_AFG/04_Field_Management/01_woa_msna/05_partner_deletion_report/",today())
dir.create(partner_deletion_report_field_path)


# write processed data
data_anonym <- data %>% select(-c(geopoint, `_geopoint_latitude`, `_geopoint_longitude`, `_geopoint_altitude`, `_geopoint_precision`, children_under_12_sum))
openxlsx::write.xlsx(data_anonym, "input/processed/msna_2020_processed_raw_data.xlsx")
openxlsx::write.xlsx(data_anonym, paste0(processed_raw_field_path,"/MSNA_2020_processed_raw_data_", today(), ".xlsx"))
openxlsx::write.xlsx(data_anonym, paste0(processed_raw_ao_path,"/MSNA_2020_processed_raw_data_", today(), ".xlsx"))

#write clenaing log
openxlsx::write.xlsx(cleaning_log, paste0(processed_cleaing_log_ao_path,"/MSNA_2020_cleaning_log_", today(), ".xlsx"))

# write Deletion report 
openxlsx::write.xlsx(deletion_report, paste0(general_deletion_report_field_path,"/MSNA_2020_General_Deletion_report_", today(), ".xlsx"))
# openxlsx::write.xlsx(deletion_report, paste0("input/processed/general_deletion_report", today(), ".xlsx"))

deletion_report %>%
  group_by(partner) %>%
  group_walk(~ write_excel_csv(.x, paste0(partner_deletion_report_field_path,"/Deletion_report_",.y$partner,"_",today(), ".csv")))
