# logical checks

library(tidyverse)
library(lubridate)

# read data 
df_tool_data <- readxl::read_excel("inputs/data_digital_finance.xlsx") %>% 
  mutate(
    i.check.uuid = `_uuid`,
    i.check.today = today,
    i.check.enumerator_id = enumerator_id
  )
df_survey <- readxl::read_excel("inputs/UGA2103_Financial_Service_Providers_Assessment_HH_Tool_June2021.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/UGA2103_Financial_Service_Providers_Assessment_HH_Tool_June2021.xlsx", sheet = "choices")

# Time interval for the survey --------------------------------------------

min_time_of_survey <- 40
max_time_of_survey <- 120

df_c_survey_time <-  df_tool_data %>% 
  mutate(
    int.survey_time_interval = difftime(end,start, units = "mins"),
    int.survey_time_interval = round(int.survey_time_interval,2),
    i.check.identified_issue = case_when(
      int.survey_time_interval < min_time_of_survey ~ "less_survey_time",
      int.survey_time_interval > max_time_of_survey ~ "more_survey_time",
      TRUE ~ "normal_survey_time" ),
    i.check.type = NA,
    i.check.name = NA,
    i.check.value = NA,
    i.check.checked_by = "Mathias",
    i.check.checked_date = as_date(today()),
    i.check.comment = NA
  )%>% 
  filter(i.check.identified_issue %in% c("less_survey_time", "more_survey_time")) %>% 
  select(starts_with("i.check"))%>% 
  rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))

# Anyone who selected "ugandan" and previously answered community_type = refugee, should be checked.
df_c_nationality <- df_tool_data %>% 
# Anyone who selected host for "type of community" and answers "refugee ID" or "beneficiary ID" should be checked.

# If respondents have selected a language but have NOT selected the same language that they previously selected for their main language, we need to check the survye.


# If respondent has selected "none" in addition to another option, the survey needs to be checked.

# If previously selected "0" in response to "how many mobile phone numbers do you have" the survye needs to be checked.


# If they previously selected "yes" to having mobile internet coverage (Q56) and now replied "no", the survey needs to be checked.
 
# Do you currently use mobile internet (social media, apps, and websites like WhatsApp, Messenger, Facebook, <other locally relevant>, etc)?
 
# If respondents who previously said they DO NOT have access to a feature phone or smart phone are now selecting uses for their phones that can only be done online (e.g. social media, access to information online etc.), survey needs to be checked

# If respondents who previously said they DO NOT have access to a feature phone or smart phone are now selecting reasons for using their phones that can only be done online (e.g. online education; looking for specific information etc.), survey needs to be checked
 

# If in previous qn "why do you want to have  a mobile money account?" they answered "it is safer than keeping cash at home" and they now asnwered "the system is not safe i am concerned that my money will disappear", survey needs to be checked

 
# if in previous question 'why do you want to have a bank account? ' is "Yes, it will allow me to securely store my money" and they now answered "the system isnt safe i am concerned that my money will disappear", survey needs to be checked


# if in previous question 'Why do you want to have a pre-paid or smart card?' answered "it will allow me to securely store my money" and they now chose "the system is not safe i am concerned that my money will disappear", check survey
