library(tidyverse)
library(lubridate)

# read data ---------------------------------------------------------------
df_tool_data <- readxl::read_excel("inputs/data_digital_finance.xlsx")

df_survey <- readxl::read_excel("inputs/tool.xlsx", sheet = "survey")

df_choices <- readxl::read_excel("inputs/tool.xlsx", sheet = "choices")