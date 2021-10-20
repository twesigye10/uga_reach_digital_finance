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
