library(tidyverse)
library(srvyr)
library(janitor)
library(glue)

# source some scripts
source("R/composite_indicators.R")
source("R/make_weights.R")

# load data ---------------------------------------------------------------

df_cleaned <- read_csv("outputs/20211022_clean_data.csv")

dap <- read_csv("inputs/r_dap.csv") %>% 
  janitor::clean_names()

start<- Sys.time() 

# load in individual level population data sets
df_ref_pop <- read_csv("inputs/refugee_population.csv")
df_host_pop <- read_csv("inputs/host_population.csv")


# make composite indicator ------------------------------------------------

df_with_composites <- create_composite_indicators_dfa(input_df = df_cleaned) %>% 
  mutate(strata = case_when(status == "refugee" ~ paste0(i.refugee_settlement, "_refugee"),
                            status == "host_community" ~ paste0(i.region,"_host"),
                            TRUE ~ status
  ))

# split data into host and refugee ----------------------------------------

df_ref <- df_with_composites %>% 
  filter(status == "refugee")

df_host <- df_with_composites %>% 
  filter(status == "host_community")

# create weights ----------------------------------------------------------

# refugee weights
ref_weight_table <- make_refugee_weight_table(input_df_ref = df_ref, 
                                              input_refugee_pop = df_ref_pop)
df_ref_with_weights <- df_ref %>% 
  left_join(ref_weight_table, by = "strata")

# host weights
host_weight_table <- make_host_weight_table(input_df_host = df_host, 
                                            input_host_pop = df_host_pop)
df_host_with_weights <- df_host %>% 
  left_join(host_weight_table, by = "strata")

# combined weights
combined_weight_table <- make_combined_weight_table(input_df = df_with_composites,
                                                    input_ref_weight_table = ref_weight_table, 
                                                    input_host_weight_table = host_weight_table)

df_with_combined_weights <- df_with_composites %>% 
  mutate(pop_group = strata) %>% 
  left_join(combined_weight_table, by = "pop_group")

# set up design objects ---------------------------------------------------

ref_svy <- as_survey(.data = df_ref_with_weights, strata = strata, weights = weights )
host_svy <- as_survey(.data = df_host_with_weights, strata = strata, weights = weights )
ref_host_svy <- as_survey(.data = df_with_combined_weights, strata = strata, weights = weights )

# different analyses ---------------------------------------
outputs<-list()

# refugee -----------------------------------------------------------------

# no subsets
dap_refugee <- dap %>% 
  filter(split %in% c("all", "refugee_only"))

refugee_variables_no_subsets <- dap_refugee %>% 
  pull(variable)

outputs$ref_region <- butteR::survey_collapse(df = ref_svy,
                          vars_to_analyze = refugee_variables_no_subsets, 
                          disag="i.region") %>% 
  mutate(population="refugee")

# refugee overall, no additional subset
outputs$ref_overall <- butteR::survey_collapse(df = ref_svy,
                          vars_to_analyze = refugee_variables_no_subsets, ) %>% 
  mutate(population="refugee")

#  subsets
dap_refugee_subset1 <- dap %>% 
  filter( split %in%  c("all","refugee_only"), !is.na(subset_1))

# refugee overall, subset 1
dap_refugee_subset_split <- dap_refugee_subset1 %>% 
  split(.$subset_1)

ref_overall_subset1<-list()

for(i in seq_along(dap_refugee_subset_split)){ #seq_along(dap_single_subset_split)){
  print(i)
  subset_temp<-dap_refugee_subset_split[[i]]
  subset_value<- unique(subset_temp$subset_1)
  vars_temp<- subset_temp %>% pull(variable)
  ref_overall_subset1[[subset_value]]<- butteR::survey_collapse(df = refsvy,
                                                                vars_to_analyze =vars_temp ,
                                                                disag = c( subset_value) 
  )
}

outputs$ref_overall_subset1<- bind_rows(ref_overall_subset1) %>% 
  mutate(population= "refugee")

# refugee overall by district & subset 1
ref_region_subset1 <- list()

for(i in seq_along(dap_refugee_subset_split)){ #seq_along(dap_single_subset_split)){
  print(i)
  subset_temp <-dap_refugee_subset_split[[i]]
  subset_value <- unique(subset_temp$subset_1)
  vars_temp <- subset_temp %>% pull(variable)
  ref_region_subset1[[subset_value]] <- butteR::survey_collapse(df = refsvy,
                                                                 vars_to_analyze = vars_temp ,
                                                                 disag = c( "i.region", subset_value) 
  )
  
}
outputs$ref_region_subset1<- bind_rows(ref_region_subset1) %>% 
  mutate(population = "refugee")

# host -----------------------------------------------------------------

dap_host

# refugee and host -----------------------------------------------------------------

dap_refugee