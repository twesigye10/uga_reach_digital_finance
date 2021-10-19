# using the cleaning log to clean the data

library(tidyverse)
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
