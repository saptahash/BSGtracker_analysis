library(tidyverse)
library(readr)
library(lubridate)

latestchanges <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest_allchanges.csv")

h7_changes <- 
  latestchanges %>%
  filter(PolicyType == "H7: Vaccination policy") %>%
  group_by(CountryCode) %>%
  arrange(CountryCode, Date) %>%
  mutate(PreviousPolicyValue = lag(PolicyValue),
         Flag = case_when(Flag == 1 ~ "G",
                          Flag == 0 ~ "T",
                          TRUE ~ as.character(NA)),
         PreviousPolicyFlag = lag(Flag)) %>%
  ungroup() %>%
  mutate(CurrentPolicy= paste0(.$PolicyValue, .$Flag),
         PreviousPolicy = paste0(.$PreviousPolicyValue, .$PreviousPolicyFlag),
         CurrentPolicy = ifelse(PolicyValue == 0, "0", 
                                ifelse(is.na(PolicyValue), NA, CurrentPolicy)),
         PreviousPolicy = ifelse(PreviousPolicyValue == 0, "0", 
                                ifelse(is.na(PreviousPolicyValue), NA, PreviousPolicy))) %>%
  select(CountryCode, CountryName, Date, PreviousPolicy, CurrentPolicy, Notes) %>%
  mutate(Date = ymd(Date),
         Date = format(Date, "%d-%b-%y"))
  
write.csv(h7_changes, file = "./data/h7_changes.csv")
  
  
