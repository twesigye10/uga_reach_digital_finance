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

dap_refugee <- dap %>% 
  filter(split %in% c("all", "refugee_only"))

# no subsets
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

for(i in seq_along(dap_refugee_subset_split)){ 
  print(i)
  subset_temp<-dap_refugee_subset_split[[i]]
  subset_value<- unique(subset_temp$subset_1)
  vars_temp<- subset_temp %>% pull(variable)
  ref_overall_subset1[[subset_value]]<- butteR::survey_collapse(df = ref_svy,
                                                                vars_to_analyze =vars_temp ,
                                                                disag = c( subset_value) 
  )
}

outputs$ref_overall_subset1<- bind_rows(ref_overall_subset1) %>% 
  mutate(population= "refugee")

# refugee overall by region & subset 1
ref_region_subset1 <- list()

for(i in seq_along(dap_refugee_subset_split)){ #seq_along(dap_single_subset_split)){
  print(i)
  subset_temp <-dap_refugee_subset_split[[i]]
  subset_value <- unique(subset_temp$subset_1)
  vars_temp <- subset_temp %>% pull(variable)
  ref_region_subset1[[subset_value]] <- butteR::survey_collapse(df = ref_svy,
                                                                 vars_to_analyze = vars_temp ,
                                                                 disag = c( "i.region", subset_value) 
  )
}
outputs$ref_region_subset1<- bind_rows(ref_region_subset1) %>% 
  mutate(population = "refugee")

# host -----------------------------------------------------------------

dap_host <- dap %>% 
  filter(split %in%  c("all", "host_only"))

# no subsets
host_variables_no_subsets <- dap_host %>% 
  pull(variable)

# host by region, no additional subsets
outputs$host_region <-
  butteR::survey_collapse(df = host_svy,
                          vars_to_analyze = host_variables_no_subsets, 
                          disag = "i.region") %>% 
  mutate(population="host")

# host overall, no additional subset
outputs$host_overall <- 
  butteR::survey_collapse(df = host_svy,
                          vars_to_analyze = host_variables_no_subsets ) %>% 
  mutate(population="host")


# subsets
dap_host_subset1 <- dap %>% 
  filter( split %in%  c("all", "host_only"), !is.na(subset_1))

dap_host_subset_split<-dap_host_subset1 %>% 
  split(.$subset_1)

# host overall, subset 1

host_overall_subset1<-list()

for(i in seq_along(dap_host_subset_split)){
  print(i)
  subset_temp<-dap_host_subset_split[[i]]
  subset_value<- unique(subset_temp$subset_1)
  vars_temp<- subset_temp %>% pull(variable)
  host_overall_subset1[[subset_value]]<- butteR::survey_collapse(df = host_svy,
                                                                 vars_to_analyze =vars_temp ,
                                                                 disag = c(subset_value) 
  )
}

outputs$host_subset1 <- bind_rows(host_overall_subset1) %>% 
  mutate(population="host")

# host region, subset 1

host_region_subset1<-list()

for(i in seq_along(dap_host_subset_split)){
  subset_temp<-dap_host_subset_split[[i]]
  subset_value<- unique(subset_temp$subset_1)
  vars_temp<- subset_temp %>% pull(variable)
  host_region_subset1[[subset_value]]<- butteR::survey_collapse(df = host_svy,
                                                                  vars_to_analyze =vars_temp ,
                                                                  disag = c("i.region", subset_value) 
  )
}
outputs$host_subset1<- bind_rows(host_region_subset1) %>% 
  mutate(population="host")

# refugee and host combined -----------------------------------------------------------------

combined_variables_no_subset <- dap %>% 
  pull(variable)

# no subset

# combined pops  by region, no additional subsets
outputs$combined_pops_region <-
  butteR::survey_collapse(df = ref_host_svy,
                          vars_to_analyze = combined_variables_no_subset, 
                          disag="i.region") %>% 
  mutate(population="combined")

# combined pops overall, no additional subset
outputs$combined_pops_overall <- 
  butteR::survey_collapse(df = ref_host_svy,
                          vars_to_analyze = combined_variables_no_subset, 
  ) %>% 
  mutate(population="combined")

# subset
dap_combined_subset_split <- dap %>% 
  filter(!is.na(subset_1)) %>% 
  split(.$subset_1)

# overall single subset
combined_overall_subset1 <-list()

for(i in seq_along(dap_combined_subset_split)){
  print(i)
  subset_temp <-dap_combined_subset_split[[i]]
  subset_value <- unique(subset_temp$subset_1)
  vars_temp <- subset_temp %>% pull(variable)
  combined_overall_subset1[[subset_value]] <- butteR::survey_collapse(df = refhostsvy,
                                                                     vars_to_analyze =vars_temp ,
                                                                     disag = c( subset_value) 
  )
}
outputs$combined_overall_subset1<- bind_rows(combined_overall_subset1) %>% 
  mutate(population="combined")

# host region, subset 1
combined_region_subset1<-list()

for(i in seq_along(dap_combined_subset_split)){
  print(i)
  subset_temp <-dap_combined_subset_split[[i]]
  subset_value <- unique(subset_temp$subset_1)
  vars_temp <- subset_temp %>% pull(variable)
  combined_region_subset1[[subset_value]] <- butteR::survey_collapse(df = ref_host_svy,
                                                                      vars_to_analyze = vars_temp ,
                                                                      disag = c("district_name", subset_value) 
  )
}
outputs$combined_region_subset1 <- bind_rows(combined_region_subset1) %>% 
  mutate(population="combined")

# merge analysis
full_analysis_long <- bind_rows(outputs)
end<- Sys.time()
end-start

full_analysis_long %>%
  write_csv(paste0(butteR::date_file_prefix(),"_full_analysis_long_format.csv"),na="")

