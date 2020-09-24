library(tidyverse)

## COVID economic tracker
economictracker <- read.csv("https://raw.githubusercontent.com/OpportunityInsights/EconomicTracker/main/data/Employment%20Combined%20-%20State%20-%20Daily.csv")

## state FIPS code 
statefips <- read.csv("https://raw.githubusercontent.com/kjhealy/fips-codes/master/state_fips_master.csv")

## USA covid policy
oxcgrtUS <- read.csv("https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv")

##