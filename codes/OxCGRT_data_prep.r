##################################################################
#### Oxford Coronavirus Response Tracker (OxCGRT) @ BSG #################
##################################################################

### Import and process data exported from OxCGRT database 


library(readr)
library(haven)
library(tidyverse)
library(lubridate)
library(countrycode)
library(zoo)

#pwd <<- "C:\Users\sapta\OneDrive\Desktop\oxcgrtRA\BSGtracker_analysis\codes"
data_date <<- today()
url_oxcgrt <<- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest_withnotes.csv"

#setwd(pwd)
oxcgrtdata <- read_csv(url(url_oxcgrt), col_types = cols(RegionName = col_character(), 
                                                         RegionCode = col_character()))
#notes - save backup at this point 
#subset only national data 
oxcgrtdata <- oxcgrtdata %>% filter(is.na(RegionName))

#Step 2: Bringing in crossnational correlates
correlates <- c("popWB",  "hosp_beds_WB", "total_sars", "sars_deaths",
                "total_mers", "h1n1_death_estimate_25pctile",
                "h1n1_death_estimate_75pctile", "pop_in_cities")

crossnationaldata <- read_dta("../data/input/crossnationaldata.dta") #note - new version stata file

correlatesdata <- crossnationaldata %>% select(countrycode, popWB, hosp_beds_WB,
                                               total_sars, sars_deaths, total_mers,
                                               h1n1_death_estimate_25pctile, 
                                               h1n1_death_estimate_75pctile, pop_in_cities)

oxcgrtdata <- left_join(oxcgrtdata, correlatesdata, by = c("CountryCode" = "countrycode"))

oxcgrtdata <- oxcgrtdata %>% 
  drop_na(Date) %>% 
  mutate(h1n1_midIGR = (h1n1_death_estimate_25pctile + h1n1_death_estimate_75pctile)/2)

#drop notes and M1
oxcgrtdata <- oxcgrtdata %>% select(-starts_with("M1"),-ends_with("Notes"))

#change date variable to date format 
oxcgrtdata <- oxcgrtdata %>% mutate(Date = ymd(Date)) %>% subset(Date < data_date)

# attach region labels

East_Asia_Pacific <<- c("ASM", "AUS", "BRN", "CHN", "FJI", "FSM", "GUM", "HKG", "IDN", "JPN", "KHM", "KIR", "KOR", "LAO", "MAC", "MHL", "MMR", "MNG", "MNP", "MYS", "NCL", "NRU", "NZL", "PHL", "PLW", "PNG", "PRK", "PYF", "SGP", "SLB", "THA", "TLS", "TON", "TUV", "TWN", "VNM", "VUT", "WSM")
Europe_Central_Asia <<- c("ALB", "AND", "ARM", "AUT", "AZE", "BEL", "BGR", "BIH", "BLR", "CHE", "CHI", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "FRO", "GBR", "GEO", "GIB", "GRC", "GRL", "HRV", "HUN", "IMN", "IRL", "ISL", "ITA", "KAZ", "KGZ", "LIE", "LTU", "LUX", "LVA", "MCO", "MDA", "MKD", "MNE", "NLD", "NOR", "POL", "PRT", "ROU", "RUS", "SMR", "SRB", "SVK", "SVN", "SWE", "TJK", "TKM", "TUR", "UKR", "UZB", "RKS")
Latin_America_Caribbean <<- c("ABW", "ARG", "ATG", "BHS", "BLZ", "BOL", "BRA", "BRB", "CHL", "COL", "CRI", "CUB", "CUW", "CYM", "DMA", "DOM", "ECU", "GRD", "GTM", "GUY", "HND", "HTI", "JAM", "KNA", "LCA", "MAF", "MEX", "NIC", "PAN", "PER", "PRI", "PRY", "SLV", "SUR", "SXM", "TCA", "TTO", "URY", "VCT", "VEN", "VGB", "VIR")
Middle_East_North_Africa <<- c("ARE", "BHR", "DJI", "DZA", "EGY", "IRN", "IRQ", "ISR", "JOR", "KWT", "LBN", "LBY", "MAR", "MLT", "OMN", "PSE", "QAT", "SAU", "SYR", "TUN", "YEM")
North_America <<- c("BMU", "CAN", "USA")
South_Asia <<- c("AFG", "BGD", "BTN", "IND", "LKA", "MDV", "NPL", "PAK")
sub_Saharan_Africa <<- c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", "COM", "CPV", "ERI", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "LSO", "MDG", "MLI", "MOZ", "MRT", "MUS", "MWI", "NAM", "NER", "NGA", "RWA", "SDN", "SEN", "SLE", "SOM", "SSD", "STP", "SWZ", "SYC", "TCD", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
region_list <<- c("East_Asia_Pacific", "Europe_Central_Asia", "Latin_America_Caribbean", "Middle_East_North_Africa", "North_America", "South_Asia", "sub_Saharan_Africa")

oxcgrtdata$region <- NA

for(reg in region_list){
  regional <- get(reg)
  oxcgrtdata <- oxcgrtdata %>% mutate(region = ifelse(CountryCode %in% regional, reg, region))
}


write.csv(oxcgrtdata, file = paste("../data/output/OxCGRT_", data_date, ".csv", sep = ""))

#Stata diff: Not pulling JHU data -> confirm w/ Toby

##   Bringing in Apple and Google Mobility Data

url_gmobility <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"

google.mobility <- read_csv(url(url_gmobility), col_types = cols(sub_region_1 = col_character(), 
                                                                 sub_region_2 = col_character(), 
                                                                 metro_area = col_character()))
# BUG - column parsing isn't correct - metro_area isn't being read as text
# Stata diff: Date already stored as date - no need to change

#changing country codes from iso2c to iso3c
crc <- google.mobility %>% pull(country_region_code)
google.mobility$newcountry_code <- NA 
google.mobility$newcountry_code <- unlist(countrycode(crc, origin = "iso2c", destination = "iso3c"))
google.mobility <- google.mobility %>%
  subset(select = -country_region_code) %>%
  rename(country_region_code = newcountry_code)

#google.mobility.play <- google.mobility
#Stata diff: countrycode = "ISO3C" etc find out significance/necessity

#renaming google mob variables
google.mobility <- google.mobility %>% rename(goog_retail = starts_with("retail"),
                                              goog_groceryandpharmacy = starts_with("groceryandpharmacy"),
                                              goog_parks = starts_with("parks"), 
                                              goog_transitstations = starts_with("transit"), 
                                              goog_workplaces = starts_with("workplaces"), 
                                              goog_residential = starts_with("residential"))

google.mobility <- google.mobility %>% filter(is.na(sub_region_1) & is.na(sub_region_2) & is.na(metro_area))

write.csv(google.mobility, file = paste("../data/input/googlemobility_", data_date, ".csv", sep = ""))

# Import Apple Mobility Data

apple.mobility.url <- "https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/apple_reports/applemobilitytrends.csv"
apple.mobility <- read.csv(url(apple.mobility.url))

apple.mobility <- apple.mobility %>% filter(!(geo_type == "city" | 
                            geo_type == "county" | 
                            geo_type == "sub-region"))

country.name.apple <- pull(apple.mobility, var = region)
apple.mobility$countrycode <- NA
apple.mobility$countrycode <- unlist(countrycode(country.name.apple, origin = "country.name", destination = "iso3c"))
#all countries have iso3c codes, no need to drop

apple.mobility <- apple.mobility %>% 
  subset(select = -c(geo_type, region,
                     alternative_name, sub.region, country))

apple.mobility <- apple.mobility %>% pivot_longer(-c(transportation_type, countrycode), names_to = "date", values_to = "apple_") %>%
       separate(date, c("sep", "date"),sep = "X") %>%
       subset(select = -sep) %>% 
       pivot_wider(names_from = transportation_type, values_from =  apple_) %>% 
       mutate(date = ymd(date)) %>% 
       rename(apple_driving = driving, apple_walking = walking, apple_transit = transit) 
#is there a more efficient way to do this without two pivots?

write.csv(apple.mobility, file = paste("../data/input/applemobility_", data_date, ".csv", sep = ""))

#merge with apple and google mobility data

###############
### Use these as back-ups for debugging/testing
###############

#google.mobility <- read.csv(file = paste("../data/input/googlemobility_", data_date, ".csv", sep = ""))
#oxcgrtdata <- read.csv(file = paste("OxCGRT_", data_date, ".csv", sep = ""), stringsAsFactors = FALSE)
#apple.mobility <- read.csv(file = paste("../data/input/applemobility_", data_date, ".csv", sep = ""))

oxcgrtdata <- left_join(oxcgrtdata, google.mobility %>% select(starts_with("goog_"), country_region_code, date), 
               by = c("CountryCode" = "country_region_code", "Date" = "date"))

oxcgrtdata <- left_join(oxcgrtdata, apple.mobility, by = c("CountryCode" = "countrycode", "Date" = "date"))

oxcgrtdata <- oxcgrtdata %>% mutate_at(vars(starts_with("goog")), ~(.+100))

oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>%
  group_by(CountryCode) %>%
     mutate(week_apple_transit = rollmean(apple_transit, k=7, fill = NA))

oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>%
  group_by(CountryCode) %>%
     mutate(week_apple_driving = rollmean(apple_driving, k=7, fill = NA))

oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>%
  group_by(CountryCode) %>%
     mutate(week_apple_walking = rollmean(apple_walking, k=7, fill = NA))

oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>%
  group_by(CountryCode) %>%
     mutate(week_goog_retail = rollmean(goog_retail, k=7, fill = NA))

oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>%
  group_by(CountryCode) %>%
     mutate(week_goog_parks = rollmean(goog_parks, k=7, fill = NA))

oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>%
  group_by(CountryCode) %>%
     mutate(week_goog_transitstations = rollmean(goog_transitstations, k=7, fill = NA))

oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>%
  group_by(CountryCode) %>%
     mutate(week_goog_workplaces = rollmean(goog_workplaces, k=7, fill = NA))

oxcgrtdata <- oxcgrtdata %>% arrange(CountryCode, Date) %>%
  group_by(CountryCode) %>%
     mutate(week_goog_residential = rollmean(goog_residential, k=7, fill = NA))

#Code optimisation Note #2 -  Is there a better way to do this using mutate_at?

oxcgrtdata <- oxcgrtdata %>% ungroup() %>% 
  mutate(apple_ave = rowMeans(oxcgrtdata[,c("week_apple_transit", "week_apple_driving", "week_apple_walking")]), 
         google_ave = rowMeans(oxcgrtdata[,c("week_goog_retail", "week_goog_transitstations", "week_goog_workplaces")])) 

oxcgrtdata <- oxcgrtdata %>% ungroup() %>% mutate(mobility_ave = rowMeans(oxcgrtdata[,c("apple_ave", "google_ave")], na.rm = T))

write.csv(oxcgrtdata, file = paste("../data/output/OxCGRT_", data_date, ".csv", sep = ""))


#Bringing in OWID Testing Data

owid_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv"
owid.data <- read.csv(url(owid_url), stringsAsFactors = FALSE)

owid.data <- owid.data %>% rename(test_total = Cumulative.total, 
                                  test_dailychange = Daily.change.in.cumulative.total,
                                  test_totalperthou = Cumulative.total.per.thousand,
                                  test_dailychangeperthou = Daily.change.in.cumulative.total.per.thousand, 
                                  test_dailychange_7day = X7.day.smoothed.daily.change, 
                                  test_changeperthou_7day = X7.day.smoothed.daily.change.per.thousand, 
                                  countrycode = ISO.code) %>% 
                          subset(select = -c(Source.URL, Notes))

owid.data <- owid.data %>% arrange(countrycode, Date, desc(test_total))
owid.data <- owid.data[!duplicated(owid.data[,c("Date", "countrycode")]), ]
owid.data <- owid.data %>% mutate(Date = as.Date(Date))

write.csv(owid.data, file = paste("../data/input/testing_", data_date, ".csv", sep = ""))

oxcgrtdata <- left_join(oxcgrtdata, owid.data %>% select(countrycode, Date, test_total, test_totalperthou), 
          by = c("Date", "CountryCode"="countrycode"))

oxcgrtdata <- oxcgrtdata %>% mutate(test_percase = test_total/ConfirmedCases)

### new output file with testing data appended
write.csv(oxcgrtdata, file = paste("../data/output/OxCGRT_", data_date, ".csv", sep = ""))  


########### using additional data to qualify gaps in testing data 
testing_data2_url <- "https://drive.google.com/uc?id=13yev97Ua-E-EslhDIX9vQiC2OZIqgGX4&export=download"
testing.data2 <- read.csv(url(testing_data2_url), stringsAsFactors = FALSE)

testing.data2 <- testing.data2 %>% mutate(test_nodata = ifelse(observations_found == 0, 1, NA)) 

no.testing.data <- testing.data2 %>% filter(test_nodata == 1)
no.testing.data.ccode <- countrycode(no.testing.data$country, origin = "country.name", destination = "iso3c")
no.testing.data <- no.testing.data %>% mutate(countrycode = no.testing.data.ccode)
#note Micronesia isn't matched

oxcgrtdata <- left_join(oxcgrtdata, no.testing.data %>% select(countrycode, test_nodata), by = c("CountryCode" = "countrycode"))

write.csv(oxcgrtdata, file = paste("../data/output/OxCGRT_", data_date, ".csv", sep = ""))  
















#################### 
###### ROUGH CODE - DO NOT RUN
####################


# #oxcgrtdata <- 
# 
# #doing rowmeans first 
# oxcgrtdata %>% arrange(CountryCode, Date) %>% rowMeans(apple_transit)
#  
# View(oxcgrtdata %>% ungroup() %>%
# mutate(apple_ave = rowMeans(oxcgrtdata[,c("week_apple_transit", "week_apple_driving", "week_apple_walking")])))
# 
# 
# 
# 
# 
# 
# View(oxcgrtdata %>% arrange(CountryCode, Date) %>%
#        mutate_all(stats::filter(select(starts_with("apple")), rep(1/7,7), sides = 2)))
# 
# View(oxcgrtdata %>% mutate_at(.funs = list(wk = )))
# 
# View(oxcgrtdata %>% arrange(CountryCode, Date) %>% select(apple_transit) %>%
#   mutate(wk_apple_transit = stats::filter(rep(1/7,7), sides = 2)))
# 
# apple.transit <- oxcgrtdata %>% arrange(CountryCode, Date) %>% pull(apple_transit)
# 
# View(oxcgrtdata %>% arrange(CountryCode, Date))
# 
# 
# 
# View(oxcgrtdata %>% arrange(CountryCode, Date) %>%
#      mutate(week_apple_transit = rollmean(apple_transit, k=3, fill = NA)))
# 
# View(stats::filter(oxcgrtdata %>% select(starts_with("apple")), rep(1/7,7), sides = 2))
# #check NA handling for these functions
# 
# glimpse(oxcgrtdata)
# glimpse(google.mobility)

## Testing 








  
  

