library(tidyverse)

yougov_path <- "C:/Users/sapta/Downloads/random/oxford cgrt"
list.files(yougov_path)

yougov <- read.csv(paste(yougov_path, "AllDataCombined2020-09-11.csv", sep = "/"))
country_breakdown <- read.csv(paste(yougov_path, "CountryBreakdown_2020-09-11.csv", sep = "/"))


