# logical checks

library(tidyverse)
library(lubridate)

# read data 
df_tool_data <- readxl::read_excel("inputs/data_digital_finance.xlsx") %>% 
  mutate(i.check.uuid = `_uuid`,
         i.check.today = today,
         i.check.enumerator_id = enumerator_id) %>% 
  filter(consent=="yes", today > as_date("2021-08-23"))

df_survey <- readxl::read_excel("inputs/UGA2103_Financial_Service_Providers_Assessment_HH_Tool_June2021.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/UGA2103_Financial_Service_Providers_Assessment_HH_Tool_June2021.xlsx", sheet = "choices")


# Time checks -------------------------------------------------------------

# Time interval for the survey

min_time_of_survey <- 40
max_time_of_survey <- 120

df_c_survey_time <-  df_tool_data %>% 
  mutate(int.survey_time_interval = difftime(end,start, units = "mins"),
         int.survey_time_interval = round(int.survey_time_interval,2),
         i.check.identified_issue = case_when(
           int.survey_time_interval < min_time_of_survey ~ "less_survey_time",
           int.survey_time_interval > max_time_of_survey ~ "more_survey_time",
           TRUE ~ "normal_survey_time" ),
         i.check.type = NA,
         i.check.name = NA,
         i.check.current_value = NA,
         i.check.value = NA,
         i.check.checked_by = "Mathias",
         i.check.checked_date = as_date(today()),
         i.check.comment = NA)%>% 
  filter(i.check.identified_issue %in% c("less_survey_time", "more_survey_time")) %>% 
  select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))


# check the time between surveys

min_time_btn_surveys <- 5

df_c_time_btn_survey <- df_tool_data %>%
  group_by( today, enumerator_id) %>%
  arrange(start, .by_group = TRUE) %>% 
  mutate(int.t_between_survey = (start - lag(end, default=first(start))),
         int.time_between_survey = make_difftime(int.t_between_survey, units = "mins"),
         int.time_between_survey = round(int.time_between_survey,2)) %>%
  filter(int.time_between_survey !=0 & int.time_between_survey < min_time_btn_surveys) %>%
  mutate(i.check.identified_issue = "less_time_btn_surveys",
         i.check.type = NA,
         i.check.name = NA,
         i.check.current_value = NA,
         i.check.value = NA,
         i.check.checked_by = "Mathias",
         i.check.checked_date = as_date(today()),
         i.check.comment = NA ) %>% 
  select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# Logical checks ----------------------------------------------------------

# Anyone who selected "ugandan" and previously answered community_type = refugee, should be checked.
df_c_nationality <- df_tool_data %>% 
  filter(status == "refugee", nationality == "ugandan") %>% 
  mutate(i.check.identified_issue = "un_expected_response",
         i.check.type = NA,
         i.check.name = "nationality",
         i.check.current_value = NA,
         i.check.value = nationality,
         i.check.checked_by = "Mathias",
         i.check.checked_date = as_date(today()),
         i.check.comment = NA) %>% 
  select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# Anyone who selected host for "type of community" and answers "refugee ID" or "beneficiary ID" should be checked.
df_c_id_type <- df_tool_data %>% 
  filter(status == "host_community", id_type %in% c("unhcr_refugee_id", "ug_refugee_id", "benef_id_not_unhcr")) %>% 
  mutate(i.check.identified_issue = "un_expected_response",
         i.check.type = NA,
         i.check.name = "id_type",
         i.check.current_value = id_type,
         i.check.value = NA,
         i.check.checked_by = "Mathias",
         i.check.checked_date = as_date(today()),
         i.check.comment = NA) %>% 
  select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# If respondents have selected a language but have NOT selected the same language that they previously selected for their main language, we need to check the survye.
df_c_language <- df_tool_data %>% 
  mutate(i.check.identified_issue = ifelse(glue::glue("language_understand/{main_language}") == 0, "un_expected_response", "main_language_also_understood"),
         i.check.type = NA,
         i.check.name = "language_understand",
         i.check.current_value = NA,
         i.check.value = NA,
         i.check.checked_by = "Mathias",
         i.check.checked_date = as_date(today()),
         i.check.comment = NA) %>% 
  filter(i.check.identified_issue == "un_expected_response") %>% 
  select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# If respondent has selected "none" in addition to another option, the survey needs to be checked.
# type_phone_owned

df_c_type_phone_owned <- df_tool_data %>% 
  rowwise() %>% 
  mutate(int.owned_phone_types_count = sum(c_across(starts_with("type_phone_owned/")), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(i.check.identified_issue = ifelse(int.owned_phone_types_count > 1 & "type_phone_owned/none" == 1, "un_expected_response", "expected_response"),
         i.check.type = NA,
         i.check.name = "type_phone_owned",
         i.check.current_value = NA,
         i.check.value = NA,
         i.check.checked_by = "Mathias",
         i.check.checked_date = as_date(today()),
         i.check.comment = NA) %>% 
  filter(i.check.identified_issue == "un_expected_response") %>% 
  select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# If previously selected "0" in response to "how many mobile phone numbers do you have" the survye needs to be checked.
# walk_top_up

df_c_walk_top_up <- df_tool_data %>% 
  filter(walk_top_up %in% c("no_need_to_walk", "regularly_walk", "walk_specifically") , no_phones_hh_owns == 0) %>% 
  mutate(i.check.identified_issue = "un_expected_response",
         i.check.type = NA,
         i.check.name = "walk_top_up",
         i.check.current_value = walk_top_up,
         i.check.value = NA,
         i.check.checked_by = "Mathias",
         i.check.checked_date = as_date(today()),
         i.check.comment = NA) %>% 
  select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# If they previously selected "yes" to having mobile internet coverage (Q56) and now replied "no", the survey needs to be checked.
# mobile_internet == "yes" and internet_awareness == "no"

df_c_internet_awareness <- df_tool_data %>% 
  filter(mobile_internet == "yes", internet_awareness == "no") %>% 
  mutate(i.check.identified_issue = "un_expected_response",
         i.check.type = NA,
         i.check.name = "walk_top_up",
         i.check.current_value = walk_top_up,
         i.check.value = NA,
         i.check.checked_by = "Mathias",
         i.check.checked_date = as_date(today()),
         i.check.comment = NA) %>% 
  select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# Do you currently use mobile internet (social media, apps, and websites like WhatsApp, Messenger, Facebook, <other locally relevant>, etc)?
# can be constrained in the tool

# If respondents who previously said they DO NOT have access to a feature phone or smart phone are now selecting uses for their phones that can only be done online (e.g. social media, access to information online etc.), survey needs to be checked
# mobile_phone_use
df_c_mobile_phone_use <- df_tool_data %>% 
  rowwise() %>% 
  mutate(int.mobile_phone_use = sum(c_across(starts_with("phone_use/")), na.rm = TRUE)) %>% 
  ungroup() %>%
  filter(int.mobile_phone_use > 0) %>% 
  mutate(i.check.identified_issue = ifelse("type_phone_owned/none" == 1 | "type_phone_owned/basic_phone" == 1, "un_expected_response", "expected_response"),
         i.check.type = NA,
         i.check.name = "mobile_phone_use",
         i.check.current_value = NA,
         i.check.value = NA,
         i.check.checked_by = "Mathias",
         i.check.checked_date = as_date(today()),
         i.check.comment = NA) %>% 
  filter(i.check.identified_issue == "un_expected_response") %>% 
  select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# If respondents who previously said they DO NOT have access to a feature phone or smart phone are now selecting reasons for using their phones that can only be done online (e.g. online education; looking for specific information etc.), survey needs to be checked
# phone_use
df_c_phone_use <- df_tool_data %>% 
  rowwise() %>% 
  mutate(int.phone_use_count = sum(c_across(starts_with("phone_use/")), na.rm = TRUE)) %>% 
  ungroup() %>%
  filter(int.phone_use_count > 0) %>% 
  mutate(i.check.identified_issue = ifelse("type_phone_owned/none" == 1 | "type_phone_owned/basic_phone" == 1, "un_expected_response", "expected_response"),
         i.check.type = NA,
         i.check.name = "phone_use",
         i.check.current_value = NA,
         i.check.value = NA,
         i.check.checked_by = "Mathias",
         i.check.checked_date = as_date(today()),
         i.check.comment = NA) %>% 
  filter(i.check.identified_issue == "un_expected_response") %>% 
  select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# If in previous qn "why do you want to have  a mobile money account?" they answered "it is safer than keeping cash at home" and they now asnwered "the system is not safe i am concerned that my money will disappear", survey needs to be checked


# if in previous question 'why do you want to have a bank account? ' is "Yes, it will allow me to securely store my money" and they now answered "the system isnt safe i am concerned that my money will disappear", survey needs to be checked


# if in previous question 'Why do you want to have a pre-paid or smart card?' answered "it will allow me to securely store my money" and they now chose "the system is not safe i am concerned that my money will disappear", check survey




# spatial checks ----------------------------------------------------------

# duplicate point numbers
# missing sample point numbers from the dataset(few data points from particular area)
# pt id does not exist in sample
