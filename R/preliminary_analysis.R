library(tidyverse)
library(srvyr)
library(janitor)
library(glue)

# source some scripts
source("R/composite_indicators.R")
source("R/make_weights.R")

# load data ---------------------------------------------------------------

df_cleaned <- read_csv("outputs/20211005_clean_data.csv")

dap <- read_csv("inputs/r_dap.csv") %>% 
  janitor::clean_names()

start<- Sys.time() 

# load in individual level population data sets
df_ref_pop <- read_csv("inputs/refugee_population.csv")
df_host_pop <- read_csv("inputs/host_population.csv")


# make composite indicator ------------------------------------------------

df_with_composites <- create_composite_indicators_dfa(input_df = df_cleaned) %>% 
  mutate(strata = case_when(status == "refugee" ~ paste0(settlement_name, "_refugee"),
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
