library(readr)
library(dplyr)
library(lubridate)
library(janitor)

crime_df <- read_csv("data/Crime_Data_from_2020_to_Present.csv")

crime_june_df <- crime_df %>%
  clean_names() %>% 
  mutate(date_rptd_lub = mdy_hms(date_rptd),
         date_occ_lub  = mdy_hms(date_occ)) %>% 
  filter(date_occ_lub >= "2020-06-01" & date_occ_lub <= "2020-06-30")

write_csv(x = crime_june_df, path = "data/la_crime_june.csv")
