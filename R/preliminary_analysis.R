library(tidyverse)
library(srvyr)
library(janitor)
library(glue)

# source some scripts
source("R/composite_indicators.R")
source("R/make_weights.R")

# load data ---------------------------------------------------------------

df_cleaned <- read_csv("outputs/20211005_clean_data")

dap <- read_csv("inputs/r_dap.csv") %>% 
  janitor::clean_names()

start<- Sys.time() 

# load in individual level population data sets
df_ref_pop<- read_csv("inputs/refugee_population.csv")
df_host_pop<- read_csv("inputs/host_population.csv")


# make composite indicator ------------------------------------------------

df_with_composites <- create_composite_indicators_dfa(input_df = df_cleaned) 

