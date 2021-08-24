library(tidyverse)
library(lubridate)

# read data 
df_tool_data <- readxl::read_excel("inputs/UGA2103_Financial_Service_Providers_Assessment_HH_Tool_June2021")
df_survey <- readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "choices")

# get questions with other
others_colnames <-  df_tool_data %>% 
  select(ends_with("_other"), -contains("/")) %>% 
  colnames()

# data.frame for holding _other response data
df_other_response_data <- data.frame()

for (cln in others_colnames) {
  df_filtered_data <- df_tool_data %>% 
    select("_uuid", "today", "enumerator_id", other_text = cln) %>% 
    filter(!is.na(other_text), !other_text %in% c(" ", "NA")) %>% 
    mutate( other_name = cln, value = NA)
  df_other_response_data <- rbind(df_other_response_data, df_filtered_data)
}
# arrange the data
df_data_arranged <- df_other_response_data %>% 
  arrange(today, `_uuid`)

# get choices to add to the _other responses extracted
df_grouped_choices <- df_choices %>% 
  group_by(list_name) %>% 
  summarise(choice_options = paste(name, collapse = " : ")) %>% 
  arrange(list_name)

# extract parent question and join survey for extracting list_name
df_data_parent_qns <- df_data_arranged %>% 
  mutate(
    parent_qn = str_replace_all(other_name, "_other", "")
  ) %>% 
  left_join(df_survey %>% select(name, type), by = c("parent_qn"="name")) %>% 
  separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" ) %>% 
  rename(name = parent_qn)

# join other responses with choice options based on list_name

df_join_other_response_with_choices <- df_data_parent_qns %>% 
  left_join(df_grouped_choices, by = "list_name")

# output the resulting data frame
write_csv(x = df_join_other_response_with_choices, file = paste0("outputs/others_responses_",as_date(today()),"_", hour(now()) ,".csv"), na = "")