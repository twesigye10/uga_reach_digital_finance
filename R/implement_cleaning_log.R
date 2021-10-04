# using the cleaning log to clean the data

library(tidyverse)
library(lubridate)

# read data
df_cleaning_log <- read_csv("inputs/combined_logic_spatial_and_others_checks.csv") %>% 
  mutate(sheet = NA, index = NA, relevant = NA) %>% 
  select(uuid, type, name, value, issue_id, sheet, index, relevant, issue)
df_raw_data <- readxl::read_excel("inputs/UGA2103_Financial_Service_Providers_Assessment_HH_Tool_June2021.xlsx")

df_survey <- readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "survey") 
df_choices <- readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "choices") 

# find all new choices to add to choices sheet ----------------------------

# gather choice options based on unique choices list
df_grouped_choices<- df_choices %>% 
  group_by(list_name) %>% 
  summarise(choice_options = paste(name, collapse = " : "))
# get new name and ad_option pairs to add to the choices sheet
new_vars <- df_cleaning_log %>% 
  filter(type %in% c("change_response", "add_option")) %>% 
  left_join(df_survey, by = "name") %>% 
  filter(str_detect(string = type.y, pattern = "select_one|select one|select_multiple|select multiple")) %>% 
  separate(col = type.y, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>% 
  left_join(df_grouped_choices, by = "list_name") %>%
  filter(!str_detect(string = choice_options, pattern = value ) ) %>%
  rename(choice = value ) %>%
  select(name, choice) %>%
  distinct() %>% # to make sure there are no duplicates
  arrange(name)

# create kobold object ----------------------------------------------------

kbo <- kobold::kobold(survey = df_survey, 
                      choices = df_choices, 
                      data = df_raw_data, 
                      cleaning = df_cleaning_log)

# modified choices for the survey tool --------------------------------------
df_choises_modified <- butteR:::xlsform_add_choices(kobold = kbo, new_choices = new_vars)


