# script: logical and others checks

library(tidyverse)
library(lubridate)
library(glue)
source("R/checks_for_other_responses.R")

# read data 
df_tool_data <- readxl::read_excel("inputs/UGA2103_Financial_Service_Providers_Assessment_HH_Tool_June2021.xlsx") %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.start_date = as_date(start),
         i.check.enumerator_id = enumerator_id,
         i.check.district_name = district_name,
         i.check.point_number = point_number,
         start = as_datetime(start),
         end = as_datetime(end)) %>% 
  filter(consent == "yes", respondent_age >= 18, i.check.start_date > as_date("2021-08-29"), i.check.point_number != "13m.", 
         !i.check.start_date %in% c(as_date("2021-09-08"), as_date("2021-09-09"), 
                                    as_date("2021-09-21")),
         !str_detect(string = point_number, pattern = fixed('test', ignore_case = TRUE)),
         !i.check.uuid %in% c("27b0ffe2-8d47-4897-b402-1928fd23cfb3",
                              "40d216de-76db-42b7-9105-aea1ce234489",
                              "f2f648df-55d6-4b9d-93ca-aa87c3bc30c7",
                              "4167b891-b1ff-46b1-856b-532dd28e7a1e",
                              "d7bde578-cdc2-4d77-b31b-a15c4cec9d38",
                              "4f3afa4f-a065-4f6d-bd25-9919902f9ce0",
                              "a89d0010-d626-4a4f-8b8c-e83e8fade349",
                              "27ac9f75-3954-4c55-90a3-b5b09984fe7a",
                              "48ee8073-a0d7-460f-9867-304a47bdb1fc",
                              "b0a1c83dcdb24671b9eed78d7a77786f",
                              "c5c9b13aa6cd43338e113d8c647c04ed",
                              "d8936d35-7e1c-48d9-bafa-b25e758c05eb"
                              )
         ) %>% 
  mutate(across(contains("/"), .fns = ~as.numeric(.x)))

df_survey <- readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "choices")

df_sample_data <- sf::st_read("inputs/dfa_settlement_host_samples.gpkg", quiet = TRUE)

# output holder -----------------------------------------------------------

logic_output <- list()

# Time checks -------------------------------------------------------------

# Time interval for the survey

min_time_of_survey <- 30
max_time_of_survey <- 120

df_c_survey_time <-  df_tool_data %>% 
  mutate(int.survey_time_interval = lubridate::time_length(end - start, unit = "min"),
         int.survey_time_interval = ceiling(int.survey_time_interval),
         
         i.check.type = "remove_survey",
         i.check.name = "point_number",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = case_when(
           int.survey_time_interval < min_time_of_survey ~ "less_survey_time",
           int.survey_time_interval > max_time_of_survey ~ "more_survey_time",
           TRUE ~ "normal_survey_time" ),
         i.check.issue = glue("{int.survey_time_interval} min taken to do the survey"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "")%>% 
  filter(i.check.issue_id %in% c("less_survey_time", "more_survey_time")) %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_survey_time")){
  if(nrow(df_c_survey_time) > 0){
    logic_output$df_c_survey_time <- df_c_survey_time
  }
}

# check the time between surveys

min_time_btn_surveys <- 5

df_c_time_btn_survey <- df_tool_data %>%
  group_by(i.check.start_date, i.check.enumerator_id) %>%
  filter(n()>1) %>% 
  arrange(start, .by_group = TRUE) %>%
  mutate(int.time_between_survey = lubridate::time_length(start - lag(end, default = first(start)), unit = "min"),
         int.time_between_survey = ceiling(int.time_between_survey)) %>%
  filter(int.time_between_survey != 0 & int.time_between_survey < min_time_btn_surveys) %>%
  mutate(i.check.type = "remove_survey",
         i.check.name = "point_number",
         i.check.current_value = "",
         i.check.value = "",
         i.check.issue_id = "less_time_btn_surveys",
         i.check.issue = glue("{int.time_between_survey} min taken between surveys"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_time_btn_survey")){
  if(nrow(df_c_time_btn_survey) > 0){
    logic_output$df_c_time_btn_survey <- df_c_time_btn_survey
  }
}
# Logical checks ----------------------------------------------------------

# Anyone who selected "ugandan" and previously answered community_type = refugee, should be checked.
df_c_nationality <- df_tool_data %>% 
  filter(status == "refugee", nationality == "ugandan") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "nationality",
         i.check.current_value = nationality,
         i.check.value = "",
         i.check.issue_id = "logic_c_nationality",
         i.check.issue = "nationality: ugandan but community_type: refugee",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_nationality")){
  if(nrow(df_c_nationality) > 0){
    logic_output$df_c_nationality <- df_c_nationality
  }
}

# Anyone who selected host for "type of community" and answers "refugee ID" or "beneficiary ID" should be checked.
df_c_id_type <- df_tool_data %>% 
  filter(status == "host_community", str_detect(string = id_type, pattern = "unhcr_refugee_id|ug_refugee_id|benef_id_not_unhcr")) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "id_type",
         i.check.current_value = id_type,
         i.check.value = "",
         i.check.issue_id = "logic_c_status",
         i.check.issue = glue("status: host_community but refugee id_type: {id_type}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_id_type")){
  if(nrow(df_c_id_type) > 0){
    logic_output$df_c_id_type <- df_c_id_type
  }
}

# If respondents have selected a language but have NOT selected the same language that they previously selected for their main language, we need to check the survye.
df_c_language <- df_tool_data %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "main_language",
         i.check.current_value = main_language,
         i.check.value = "",
         i.check.issue_id = ifelse(str_detect(string = language_understand, pattern = main_language, negate = TRUE) , 
                                   "logic_c_main_language", "main_language_also_understood"),
         i.check.issue = glue("main_language: {main_language} not in understood languages: {language_understand}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  filter(i.check.issue_id == "logic_c_main_language") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_language")){
  if(nrow(df_c_language) > 0){
    logic_output$df_c_language <- df_c_language
  }
}

# If respondent has selected "none" in addition to another option, the survey needs to be checked.
# type_phone_owned

df_c_type_phone_owned <- df_tool_data %>% 
  rowwise() %>% 
  mutate(int.type_phone_owned_count = sum(c_across(starts_with("type_phone_owned/")), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "type_phone_owned",
         i.check.current_value = "none",
         i.check.value = "none",
         i.check.issue_id = ifelse(int.type_phone_owned_count > 1 & `type_phone_owned/none` == 1, "logic_c_type_phone_owned", "expected_response"),
         i.check.issue = glue("none option selected with other options: {type_phone_owned}"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  filter(i.check.issue_id == "logic_c_type_phone_owned") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_type_phone_owned")){
  if(nrow(df_c_type_phone_owned) > 0){
    logic_output$df_c_type_phone_owned <- df_c_type_phone_owned
  }
}

# If previously selected "0" in response to "how many mobile phone numbers do you have" the survye needs to be checked.
# walk_top_up
# add this constraint to odk

# df_c_walk_top_up <- df_tool_data %>% 
#   filter(walk_top_up %in% c("no_need_to_walk", "regularly_walk", "walk_specifically") , no_phones_hh_owns == 0) %>% 
#   mutate(i.check.issue_id = "un_expected_response",
#          i.check.type = NA,
#          i.check.name = "walk_top_up",
#          i.check.current_value = walk_top_up,
#          i.check.value = NA,
#          i.check.checked_by = "Mathias",
#          i.check.checked_date = as_date(today()),
#          i.check.comment = NA) %>% 
#   dplyr::select(starts_with("i.check"))%>% 
#   rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# If they previously selected "yes" to having mobile internet coverage (Q56) and now replied "no", the survey needs to be checked.
# mobile_internet == "yes" and internet_awareness == "no"

df_c_internet_awareness <- df_tool_data %>% 
  filter(mobile_internet == "yes", internet_awareness == "no") %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "internet_awareness",
         i.check.current_value = internet_awareness,
         i.check.value = NA,
         i.check.issue_id = "logic_c_internet_awareness",
         i.check.issue = "mobile_internet: yes but internet_awareness: no",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_internet_awareness")){
  if(nrow(df_c_internet_awareness) > 0){
    logic_output$df_c_internet_awareness <- df_c_internet_awareness
  }
}

# # If respondents who previously said they DO NOT have access to a feature phone or smart phone are now selecting uses for their phones that can only be done online (e.g. social media, access to information online etc.), survey needs to be checked
# # mobile_phone_use
# df_c_mobile_phone_use <- df_tool_data %>% 
#   filter(str_detect(string = type_phone_owned, pattern = "none|basic_phone")) %>% 
#   mutate(i.check.issue_id = ifelse(str_detect(string = mobile_phone_use, 
#                                                       pattern = "social_media|online_inform_access|mobile_cash_voucher|mobile_banking|contactless_mobile_pay"), "un_expected_response", "expected_response"),
#          i.check.type = NA,
#          i.check.name = "mobile_phone_use",
#          i.check.current_value = NA,
#          i.check.value = NA,
#          i.check.checked_by = "Mathias",
#          i.check.checked_date = as_date(today()),
#          i.check.comment = NA) %>% 
#   filter(i.check.issue_id == "un_expected_response") %>% 
#   dplyr::select(starts_with("i.check"))%>% 
#   rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
# 
# # If respondents who previously said they DO NOT have access to a feature phone or smart phone are now selecting reasons for using their phones that can only be done online (e.g. online education; looking for specific information etc.), survey needs to be checked
# # phone_use
# df_c_phone_use <- df_tool_data %>% 
#   filter(str_detect(string = type_phone_owned, pattern = "none|basic_phone")) %>% 
#   mutate(i.check.issue_id = ifelse(str_detect(string = phone_use, 
#                                                       pattern = "talking_messaging|social_media|for_security|weather_forecast|receive_aid_information|provide_feedback"), 
#                                            "un_expected_response", "expected_response"),
#          i.check.type = NA,
#          i.check.name = "phone_use",
#          i.check.current_value = NA,
#          i.check.value = NA,
#          i.check.checked_by = "Mathias",
#          i.check.checked_date = as_date(today()),
#          i.check.comment = NA) %>% 
#   filter(i.check.issue_id == "un_expected_response") %>% 
#   dplyr::select(starts_with("i.check"))%>% 
#   rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# If in previous qn "why do you want to have  a mobile money account?" they answered "it is safer than keeping cash at home" and they now asnwered "the system is not safe i am concerned that my money will disappear", survey needs to be checked
# reason_want_mm_acc/safer_than_home == 1 and reason_not_open_mm_acc/unsafe_system
df_c_reason_not_open_mm_acc <- df_tool_data %>% 
  filter(`reason_want_mm_acc/safer_than_home` == 1, `reason_not_open_mm_acc/unsafe_system` == 1) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "reason_not_open_mm_acc",
         i.check.current_value = "unsafe_system",
         i.check.value = "unsafe_system",
         i.check.issue_id = "logic_c_reason_not_open_mm_acc",
         i.check.issue = "reason_want_mm_acc: safer_than_home but reason_not_open_mm_acc: unsafe_system",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_reason_not_open_mm_acc")){
  if(nrow(df_c_reason_not_open_mm_acc) > 0){
    logic_output$df_c_reason_not_open_mm_acc <- df_c_reason_not_open_mm_acc
  }
}

# if in previous question 'why do you want to have a bank account? ' is "Yes, it will allow me to securely store my money" and they now answered "the system isnt safe i am concerned that my money will disappear", survey needs to be checked
# reason_want_bank_acc/safe_storage and reason_not_open_bank_acc/unsafe_system
df_c_reason_not_open_bank_acc <- df_tool_data %>% 
  filter(`reason_want_bank_acc/safe_storage` == 1, `reason_not_open_bank_acc/unsafe_system` == 1) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "reason_not_open_bank_acc",
         i.check.current_value = "unsafe_system",
         i.check.value = "unsafe_system",
         i.check.issue_id = "logic_c_reason_not_open_bank_acc",
         i.check.issue = "reason_want_bank_acc: safer_than_home but reason_not_open_bank_acc: unsafe_system",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_reason_not_open_bank_acc")){
  if(nrow(df_c_reason_not_open_bank_acc) > 0){
    logic_output$df_c_reason_not_open_bank_acc <- df_c_reason_not_open_bank_acc
  }
}

# if in previous question 'Why do you want to have a pre-paid or smart card?' answered "it will allow me to securely store my money" and they now chose "the system is not safe i am concerned that my money will disappear", check survey
# reason_want_card/safe_storage and reason_not_want_card/unsafe_system
df_c_reason_not_want_card <- df_tool_data %>% 
  filter(`reason_want_card/safe_storage` == 1, `reason_not_want_card/unsafe_system` == 1) %>% 
  mutate(i.check.type = "remove_option",
         i.check.name = "reason_not_want_card",
         i.check.current_value = "unsafe_system",
         i.check.value = "unsafe_system",
         i.check.issue_id = "logic_c_reason_not_want_card",
         i.check.issue = "reason_want_card: safer_than_home but reason_not_want_card: unsafe_system",
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_reason_not_want_card")){
  if(nrow(df_c_reason_not_want_card) > 0){
    logic_output$df_c_reason_not_want_card <- df_c_reason_not_want_card
  }
}

# spatial checks ----------------------------------------------------------

sample_pt_nos <- df_sample_data %>% 
  mutate(unique_pt_number = paste0(status, "_", Name)) %>% 
  pull(unique_pt_number) %>% 
  unique()

# duplicate point numbers
df_c_duplicate_pt_nos <- df_tool_data %>% 
  mutate(unique_pt_number = paste0(status, "_", point_number )) %>% 
  group_by(i.check.district_name, status, i.check.point_number) %>% 
  filter(n() > 1, unique_pt_number %in% sample_pt_nos) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "point_number",
         i.check.current_value = point_number,
         i.check.value = "",
         i.check.issue_id = "spatial_c_duplicate_pt_no",
         i.check.issue = glue("point_number: {point_number} is duplicated: check that its not a repeated survey"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  ungroup() %>%
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_duplicate_pt_nos")){
  if(nrow(df_c_duplicate_pt_nos) > 0){
    logic_output$df_c_duplicate_pt_nos <- df_c_duplicate_pt_nos
  }
}

# pt id does not exist in sample

df_c_pt_not_in_sample <- df_tool_data %>% 
  mutate(unique_pt_number = paste0(status, "_", point_number )) %>% 
  filter(!unique_pt_number %in% sample_pt_nos) %>% 
  mutate(i.check.type = "change_response",
         i.check.name = "point_number",
         i.check.current_value = point_number,
         i.check.value = "",
         i.check.issue_id = "spatial_c_pt_no_not_in_sample",
         i.check.issue = glue("point_number: {point_number} not in samples"),
         i.check.other_text = "",
         i.check.checked_by = "",
         i.check.checked_date = as_date(today()),
         i.check.comment = "", 
         i.check.reviewed = "",
         i.check.adjust_log = "",
         i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
         i.check.so_sm_choices = "") %>% 
  dplyr::select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

if(exists("df_c_pt_not_in_sample")){
  if(nrow(df_c_pt_not_in_sample) > 0){
    logic_output$df_c_pt_not_in_sample <- df_c_pt_not_in_sample
  }
}

# threshold distance exceeded

threshold_dist <- 150

df_sample_data_thresh <- df_sample_data %>% 
  mutate(unique_pt_number = paste0(status, "_", Name)) %>% 
  sf::st_transform(4326)

df_tool_data_thresh <- df_tool_data %>% 
  mutate(unique_pt_number = paste0(status, "_", point_number)) %>% 
  sf::st_as_sf(coords = c("_geopoint_longitude","_geopoint_latitude"), crs = 4326)

# sample_data_unique_pts
sample_data_unique_pts <- df_sample_data_thresh %>%  
  pull(unique_pt_number) %>% 
  unique()
# tool_data_unique_pts
tool_data_unique_pts <- df_tool_data_thresh %>% 
  pull(unique_pt_number) %>% 
  unique()

sample_pt_nos_thresh <- sample_data_unique_pts[sample_data_unique_pts %in% tool_data_unique_pts]

if(length(sample_pt_nos_thresh) > 0){
  
  # tibble to hold the data
  df_data_with_distance <- tibble()
  
  for (pt_number in sample_pt_nos_thresh){
    current_sample <- df_sample_data_thresh %>% 
      filter(unique_pt_number == pt_number)
    current_tool_data <- df_tool_data_thresh %>% 
      filter(unique_pt_number == pt_number) 
    
    if(nrow(current_tool_data) > 0){
      current_sample_target_dist <- sf::st_distance(x = current_sample, y = current_tool_data, by_element = TRUE)
      
      current_data_with_dist <- current_tool_data %>% 
        sf::st_drop_geometry() %>% 
        mutate(distance = round(x = current_sample_target_dist, digits = 0))
      
      df_data_with_distance <- bind_rows(df_data_with_distance, current_data_with_dist)
    }
  }
  
  # format the required data
  df_c_greater_thresh_distance <- df_data_with_distance %>% 
    filter(as.numeric(distance) >= threshold_dist) %>% 
    mutate(i.check.type = "remove_survey",
           i.check.name = "point_number",
           i.check.current_value = point_number,
           i.check.value = "",
           i.check.issue_id = "spatial_c_dist_to_sample_greater_than_threshold",
           i.check.issue = glue("{distance} m greater_than_threshold:{threshold_dist} m"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = paste0(i.check.uuid, "_", i.check.type, "_", i.check.name),
           i.check.so_sm_choices = "") %>% 
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

if(exists("df_c_greater_thresh_distance")){
  if(nrow(df_c_greater_thresh_distance) > 0){
    logic_output$df_c_greater_thresh_distance <- df_c_greater_thresh_distance
  }
}

# combine checks ----------------------------------------------------------

df_logic_checks <- bind_rows(logic_output)

# others checks

df_others_data <- extract_other_data(input_tool_data = df_tool_data, input_survey = df_survey, input_choices = df_choices)


# combine logic and others checks
df_combined_checks <- bind_rows(df_logic_checks, df_others_data)

# output the resulting data frame
write_csv(x = df_combined_checks, file = paste0("outputs/", butteR::date_file_prefix(), "_combined_logic_spatial_and_others_checks.csv"), na = "")
