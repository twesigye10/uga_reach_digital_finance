# function for creating composite indicators

create_composite_indicators_dfa <- function(input_df) {
  input_df %>% 
    mutate(
      i.refugee_settlement = case_when(district_name == "adjumani" & status == "refugee"~ "adjumani", 
                                       settlement_name == "rhino" ~ "rhino_camp",
                                       TRUE ~ settlement_name),
      i.region = ifelse(district_name %in% c("isingiro", "kamwenge", "kikuube", "kyegegwa"), "south_west", "west_nile"),
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