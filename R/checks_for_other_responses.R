library(tidyverse)
library(lubridate)

# read data ---------------------------------------------------------------
df_tool_data <- readxl::read_excel("inputs/data_digital_finance.xlsx")
df_survey <- readxl::read_excel("inputs/tool.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/tool.xlsx", sheet = "choices")

# get questions with other
others_colnames <-  df_tool_data %>% 
  select(
    ends_with("_other")
  ) %>% colnames()

# data.frame for holding _other response data
df_other_response_data <- data.frame()

for (cln in others_colnames) {
  df_filtered_data <- df_tool_data %>% 
    select("_uuid", "today", "enumerator_id", current_value = cln) %>% 
    filter(!is.na(current_value)) %>% 
    mutate( name = cln, appropriate_choice = NA)
  df_other_response_data <- rbind(df_other_response_data, df_filtered_data)
}
# arrange the data
df_data_arranged <- df_other_response_data %>% 
  arrange(today, `_uuid`)
