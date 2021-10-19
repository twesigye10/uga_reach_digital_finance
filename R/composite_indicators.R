# using the cleaning log to clean the data

library(tidyverse)
library(janitor)
library(lubridate)

# read data
df_tool_data <- readxl::read_excel("inputs/UGA2103_Financial_Service_Providers_Assessment_HH_Tool_June2021.xlsx") %>% 
  filter(consent == "yes", 
         as_date(start) > as_date("2021-08-29"), 
         point_number != "13m.", 
         !as_date(start) %in% c(as_date("2021-09-08"), as_date("2021-09-09"), 
                                as_date("2021-09-21")),
         !str_detect(string = point_number, pattern = fixed('test', ignore_case = TRUE)),
         !`_uuid` %in% c("27b0ffe2-8d47-4897-b402-1928fd23cfb3",
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
                         "d8936d35-7e1c-48d9-bafa-b25e758c05eb" )) %>% 
  mutate(current_receive_cash = reduce2(.x = c("\\bSCI\\b", "\\bWTI\\b"), .y = c("sci", "wti"), .init = current_receive_cash, .f = str_replace),
         `current_receive_cash/sci` = ifelse(`current_receive_cash/SCI` == 1 & is.na(`current_receive_cash/sci`), `current_receive_cash/SCI`, `current_receive_cash/sci`),
         `current_receive_cash/wti` = ifelse(`current_receive_cash/WTI` == 1 & is.na(`current_receive_cash/wti`), `current_receive_cash/WTI`, `current_receive_cash/wti`)) %>% 
  select(-c(`id_type_refugee/school_ID`, `current_receive_cash/SCI`, `current_receive_cash/WTI`))




create_composite_indicators_dfa <- function(input_df) {
  input_df %>% 
    mutate(
      i.refugee_settlement = ifelse(district_name == "adjumani" & status == "refugee", "adjumani", settlement_name),
      i.region = ifelse(district_name %in% c("isingiro", "kamwenge", "kikuube", "kyegegwa"), "South west", "West nile"),
      i.respondent_age = ifelse(respondent_age < 60, "age_btn_18_59", "age_greater_59"),
      int.disability = paste(vulnerability_see, vulnerability_hear, vulnerability_walk, vulnerability_concentrate, vulnerability_communicate),
      i.disability = ifelse(str_detect(string = int.disability, pattern = "yes_a_lot_of_difficulty|cannot_do_at_all"), "yes_disability", "no_disability"),
      i.education_level = case_when(hh_member_education %in% c("complete_prof_degree", "complete_university") ~ "Higher",
                                    hh_member_education %in% c("incomplete_secondary", "complete_primary", "incomplete_primary") ~ "Low",
                                    hh_member_education %in% c("incomplete_prof_degree", "incomplete_university", "complete_voc_train", "complete_secondary", "incomplte_voc_train") ~ "Middle",
                                    hh_member_education %in% c("no_formal_educ") ~ "None",
                                    TRUE ~ "Other"
      ),
      i.language_understand_number = str_count(string = language_understand, pattern = "[a-z]+.\\b"),
      i.phones_owned_hh = case_when(no_phones_hh_owns == 0 ~ "num_phones_0",
                                    no_phones_hh_owns == 1 ~ "num_phones_1",
                                    no_phones_hh_owns == 2 ~ "num_phones_2",
                                    no_phones_hh_owns == 3 ~ "num_phones_3",
                                    no_phones_hh_owns == 4 ~ "num_phones_4",
                                    no_phones_hh_owns == 5 ~ "num_phones_5",
                                    TRUE ~ "num_phones_greater_5"
      ),
      i.network_type_number = str_count(string = language_understand, pattern = "[a-z]+.\\b")
    ) %>% 
    select(-starts_with("int."))
}

df_updated_data <- create_composite_indicators_dfa(df_tool_data)

df_updated_data %>% 
  tabyl(i.refugee_settlement) 

df_updated_data %>% 
  tabyl(i.region)

df_updated_data %>% 
  tabyl(i.respondent_age) 

df_updated_data %>% 
  tabyl(i.disability)  

df_updated_data %>% 
  tabyl(i.education_level)

df_updated_data %>% 
  tabyl(i.language_understand_number)

df_updated_data %>% 
  tabyl(i.phones_owned_hh)

df_updated_data %>% 
  tabyl(i.network_type_number)
