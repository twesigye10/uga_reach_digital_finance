library(tidyverse)
library(lubridate)

# read data 
df_tool_data <- readxl::read_excel("inputs/UGA2103_Financial_Service_Providers_Assessment_HH_Tool_June2021.xlsx")
df_survey <- readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "survey")
df_choices <- readxl::read_excel("inputs/UGA2103_Digital_Finace_HH_Tool_June2021.xlsx", sheet = "choices")

extract_other_data <- function(input_tool_data, input_survey, input_choices) {
  
  # add and rename some columns
  df_data <- input_tool_data %>% 
    rename(uuid = `_uuid`) %>% 
    mutate(start_date = as_date(start))
  
  # get questions with other
  others_colnames <-  df_data %>% 
    select(ends_with("_other"), -contains("/")) %>% 
    colnames()
  
  # data.frame for holding _other response data
  df_other_response_data <- data.frame()
  
  for (cln in others_colnames) {
    df_filtered_data <- df_data %>% 
      select(uuid, start_date, enumerator_id, district_name, point_number, other_text = cln) %>% 
      filter(!is.na(other_text), !other_text %in% c(" ", "NA")) %>% 
      mutate( other_name = cln, value = NA)
    df_other_response_data <- rbind(df_other_response_data, df_filtered_data)
  }
  
  # arrange the data
  df_data_arranged <- df_other_response_data %>% 
    arrange(start_date, uuid)
  
  # get choices to add to the _other responses extracted
  df_grouped_choices <- input_choices %>% 
    group_by(list_name) %>% 
    summarise(choice_options = paste(name, collapse = " : ")) %>% 
    arrange(list_name)
  
  # extract parent question and join survey for extracting list_name
  df_data_parent_qns <- df_data_arranged %>% 
    mutate(
      parent_qn = str_replace_all(other_name, "_other", "")
    ) %>% 
    left_join(input_survey %>% select(name, type), by = c("parent_qn"="name")) %>% 
    separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" ) %>% 
    rename(name = parent_qn)
  
  # join other responses with choice options based on list_name
  df_join_other_response_with_choices <- df_data_parent_qns %>% 
    left_join(df_grouped_choices, by = "list_name") %>% 
    mutate(current_value = "other", 
           issue_id = "other_checks",
           issue = "NA",
           checked_by = "NA",
           checked_date = as_date(today()),
           comment = "NA",
           reviewed = "NA",
           adjust_log = "NA"
           )
  
  # care for select_one and select_multiple (change_response, add_option, remove_option)
  output <- list()
  # select_one checks
  output$select_one <- df_join_other_response_with_choices %>% 
    filter(str_detect(select_type, c("select_one|select one"))) %>% 
    mutate(type = "change_response")
  
  # select_multiple checks
  select_mu_data <- df_join_other_response_with_choices %>% 
    filter(str_detect(select_type, c("select_multiple|select multiple")))
  
  select_mu_add_option <- select_mu_data %>% 
    mutate(type = "add_option")
  select_mu_remove_option <- select_mu_data %>% 
    mutate(type = "remove_option")
  
  output$select_multiple <- bind_rows(select_mu_add_option, select_mu_remove_option) %>% 
    arrange(uuid, start_date, enumerator_id, name)
  
  # merge other checks
  merged_other_checks <- bind_rows(output) %>% 
    mutate(uuid_cl = paste0(uuid, "_", type, "_", name),
           so_sm_choices = choice_options) %>% 
    select(uuid,
           start_date,
           enumerator_id,
           district_name,
           point_number,
           type,
           name,
           current_value,
           value,
           issue_id,
           issue,
           other_text,
           checked_by,
           checked_date,
           comment,
           reviewed,
           adjust_log,
           uuid_cl,
           so_sm_choices)
}

# others data

df_others_data <- extract_other_data(input_tool_data = df_tool_data, input_survey = df_survey, input_choices = df_choices)

# output the resulting data frame
write_csv(x = df_others_data, file = paste0("outputs/others_responses_",as_date(today()),"_", hour(now()) ,".csv"), na = "")
