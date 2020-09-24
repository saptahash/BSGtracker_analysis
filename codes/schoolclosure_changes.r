library(tidyverse)
library(readxl)
library(here)

here()

oxcgrtUS <- read.csv("https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv")

startdate <- "2020-09-01"
enddate <- "2020-09-15"


oxcgrtUS <- 
  oxcgrtUS %>% 
  filter((ymd(Date) == ymd(startdate)) | (ymd(Date) == ymd(enddate))) %>% 
  filter(RegionName != "") %>%
  select(RegionName, RegionCode, CountryName, CountryCode, Date, C1_School.closing, 
         C1_Flag, ConfirmedCases, ConfirmedDeaths)

oxcgrtUS <- 
  oxcgrtUS %>% 
  mutate(period = case_when(ymd(Date) == ymd(startdate) ~ "Start", 
                            ymd(Date) == ymd(enddate) ~ "End")) %>% 
  select(-Date) %>%
  pivot_wider(names_from = period, values_from = c(C1_Flag, C1_School.closing, ConfirmedCases,
                                                   ConfirmedDeaths))


oxcgrtUS <- 
  oxcgrtUS %>% 
  mutate(C1_Flag_Start = case_when(C1_Flag_Start == 0 ~ "T", 
                                   C1_Flag_Start == 1 ~ "G"), 
         C1_Flag_End = case_when(C1_Flag_End == 0 ~ "T", 
                                 C1_Flag_End == 1 ~ "G"), 
         C1_Start = ifelse(!is.na(C1_Flag_Start), paste(C1_School.closing_Start, C1_Flag_Start, sep = ""), 0), 
         C1_End = ifelse(!is.na(C1_Flag_End), paste(C1_School.closing_End, C1_Flag_End, sep = ""), 0), 
         periodcases = ConfirmedCases_End - ConfirmedCases_Start, 
         perioddeaths = ConfirmedDeaths_End - ConfirmedDeaths_Start) %>% 
    select(-starts_with("Confirmed"))

write.csv(oxcgrtUS, file = "../data/output/schoolclosures_changes.csv")