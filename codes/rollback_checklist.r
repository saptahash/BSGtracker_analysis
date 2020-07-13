#### Lockdown Rollback Checklist

# import packages
library(dplyr)
library(tidyr)
library(RcppRoll)

# define global macros
data_date <- lubridate::today()
pwd <- 'C:/Users/sapta/OneDrive/Desktop/projects/oxfordcgrt/analysis/toby'

# set WD
setwd(pwd)

# read in base csv file
oxcgrtdata <- read.csv(file = paste("./OxCGRT_", data_date, ".csv", sep = ""), stringsAsFactors = FALSE)
#oxcgrtdata <- read.csv("./OxCGRT_2020-06-25.csv", stringsAsFactors = FALSE)

# Filling in gaps in indicators
oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>% group_by(CountryCode) %>%
  mutate(H3_Contact.tracing_1 = H3_Contact.tracing, 
         H2_Testing.policy_1 = H2_Testing.policy, 
         C8_International_1 = C8_International.travel.controls,
         H1_Public.info_1 = H1_Public.information.campaigns) %>% 
  fill(H3_Contact.tracing_1, H2_Testing.policy_1, C8_International_1, H1_Public.info_1)


### Define cases_controlled metric
## Compute 7-day rolling avg of cases
oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>% group_by(CountryCode) %>%
  mutate(moveave_confirmedcases = zoo::rollmean(ConfirmedCases, k = 7, fill = NA, align = 'right')) %>%
  mutate(lag_moveave_cases = lag(moveave_confirmedcases, order_by = Date), 
         newcases = moveave_confirmedcases - lag_moveave_cases, 
         cases_controlled = ifelse((50-newcases)/50 > 0, (50-newcases)/50, 0)) ##FROM TOBY: I can't quite tell if you have handled this elsewhere, but you may need to account for cases that will end up >1. This can occur in uncommon instance where countries revise down their count, giving "negative" newcases.


### define test and trace indicators

#' define test numbers for each country by date 
#' -> by(date): if missing test_data, test_data = latest test data
#' define max_tests -> max(test_data) by Date
oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>% group_by(CountryCode) %>% fill(test_percase)

# CODE BUG ALERT - Inf handling in test_percase, min_tests and max_tests
oxcgrtdata <- oxcgrtdata %>% arrange(Date, CountryCode) %>% group_by(Date) %>%
  mutate(min_tests = min(test_percase, na.rm = T), 
         max_tests = max(test_percase, na.rm = T)) %>%
  mutate(test_score = (log(test_percase) - log(min_tests))/(log(max_tests) - log(min_tests))) 

oxcgrtdata <- oxcgrtdata %>% group_by(Date) %>% mutate(global_mean_test_score = mean(test_score, na.rm = T)) %>% 
       mutate(test_score = ifelse(is.na(test_score) == T, global_mean_test_score, test_score)) %>%
       ungroup() %>%
       mutate(test_score = ifelse(is.na(test_nodata) == T , test_score, 0))

oxcgrtdata <- oxcgrtdata %>% mutate(test_and_trace = 0.25*H3_Contact.tracing_1/3 + 0.25*H2_Testing.policy_1/2 + 0.5*test_score) 


### define imported cases indicator
oxcgrtdata <- oxcgrtdata %>% mutate(manage_imported_cases = C8_International_1/4)


### Behaviour change and community engagement

# ISSUE(FIXED) - no apple_ave or google_ave variable from previous code
# Code correction notes - creating them now for completion - to be removed later
#oxcgrtdata <- oxcgrtdata %>% ungroup() %>% 
#  mutate(apple_ave = rowMeans(oxcgrtdata[,c("week_apple_transit", "week_apple_driving", "week_apple_walking")]), 
#         google_ave = rowMeans(oxcgrtdata[,c("week_goog_retail", "week_goog_transitstations", "week_goog_workplaces")]))

#' CODE CORRECTION NOTE (FIXED) - Taking min at each date for time series purposes. Stata Code takes 
#' global(within country until date) min of google_ave and apple_ave, before taking
#'  min between these 
          ##FROM TOBY: this is also worth noting of the min/max range for tests above.

oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>% group_by(CountryCode) %>% 
  mutate(min_google = roll_min(google_ave, n = 28L, align = "right", fill = NA), 
         min_apple = roll_min(apple_ave, n = 28L, align = "right", fill = NA)) 


oxcgrtdata <- oxcgrtdata %>% mutate(mob = pmin(google_ave, apple_ave, na.rm = T), 
                      mob = case_when(mob < 20 ~ 20,
                                      mob > 20 & mob < 120 ~ mob,
                                      mob > 120 & (is.na(mob) == F) ~ 120))

#NA handling of mob - what happens when mob is 0? codebook says metric is left blank. Which metric? - confirm
##FROM TOBY:  are there cases of mob=0?? If so, then I guess it is <20 and would be treated as 20 for this equation. But 0 seems implausibly low... the absolute zero of mobility! 
##            More likely is a null value, where google_ave and apple_ave are empty for a particular country. In which case we do not report a community_understanding metric, we leave it blank.

oxcgrtdata <- oxcgrtdata %>% mutate(community_understanding = 0.5*cases_controlled + (1-0.5*cases_controlled)*(120-mob)/100) %>%
  mutate(community_understanding = ifelse(H1_Public.info_1!=2, 0, community_understanding)) 


### Final rollback checklist score = mean(4 criterion) ; check NA handling

oxcgrtdata$rollback_score <- rowMeans(oxcgrtdata[c("community_understanding", "test_and_trace",
                      "manage_imported_cases", "cases_controlled")], na.rm = T)



### 



#### Code Rough notebook
#%>%
#    filter(CountryCode == "GBR") %>% select(CountryCode, Date, cases_controlled, ConfirmedCases, lag_moveave_cases, newcases, moveave_confirmedcases))

