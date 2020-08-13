library(tidyverse)

#importing data
oxcgrt_us <- read.csv(url("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_US_states_temp.csv"))

#finding the latest possible date in the data
latest_date <- lubridate::ymd(max(oxcgrt_us$Date))

#finding earliest date - data from last 3 weeks
earliest_date <- lubridate::ymd(latest_date) - 21 

#subsetting only the time period that's needed - i.e. 3 weeks
oxcgrt_regionalchanges <- oxcgrt_us %>% filter(lubridate::ymd(Date) >= earliest_date)

# extracting the columns of indicator values specifically
indicators <- str_subset(names(oxcgrt_us), pattern = "(C|H|E)[0-9][_]")
flags_notes <- str_subset(indicators, pattern = "(Notes|Flag)")
indicators <- indicators[! indicators %in% flags_notes]


temp_tibble <- as_tibble()
agg_tibble <- as_tibble()
for (indc in indicators) {
  ## creating lagged variable for indicator
  lagvar_level <- paste(indc,"_lag", sep = "")
  oxcgrt_regionalchanges <- oxcgrt_regionalchanges %>% arrange(Date) %>%
    group_by(RegionCode) %>% mutate(lagvar_level = lag((!!as.name(indc)), n = 1L))
  ## extracting signal of indicator
  i <- str_sub(indc, start = 1L, end = 3L)
  ## defining the column names mapped to the indicator
  notes <- paste(i, "Notes", sep = "")
  flag <- paste(i, "Flag", sep = "")
  ## also need lagged value of the flag IF flag exists
  if (flag %in% names(oxcgrt_regionalchanges)) {
    lagvar_flag <- paste(i, "_flag_lag", sep = "")
    oxcgrt_regionalchanges <- oxcgrt_regionalchanges %>% arrange(Date) %>%
      group_by(RegionCode) %>% mutate(lagvar_flag = lag((!!as.name(flag)), n = 1L))
    temp_tibble <- oxcgrt_regionalchanges %>% filter((!!as.name(notes)) != "")
    ## select the indicator column, indicator notes, indicator flag and their lags + dates and regions
    temp_tibble <- temp_tibble %>% select(!!as.name(indc), !!as.name(flag), lagvar_level, lagvar_flag, !!as.name(notes), Date, RegionName, RegionCode)
    temp_tibble <- temp_tibble %>% mutate(indicator_type = toString(indc)) %>% 
      rename(present_value = !!as.name(indc),
             past_value = lagvar_level, 
             present_flag = !!as.name(flag), 
             past_flag = lagvar_flag, 
             Notes = !!as.name(notes))
  }
  ## and if flag doesn't exist
  else{
    temp_tibble <- oxcgrt_regionalchanges %>% filter((!!as.name(notes)) != "")
    ## select the indicator column, indicator notes, indicator flag and their lags + dates and regions
    temp_tibble <- temp_tibble %>% select(!!as.name(indc), lagvar_level, !!as.name(notes), Date, RegionName, RegionCode)
    temp_tibble$present_flag <- NA
    temp_tibble$past_flag <- NA
    temp_tibble <- temp_tibble %>% mutate(indicator_type = toString(indc)) %>% 
      rename(present_value = !!as.name(indc),
             past_value = lagvar_level, 
             Notes = !!as.name(notes))
  }
  # finally aggegate to final tibble
  agg_tibble <- bind_rows(agg_tibble, temp_tibble)
}

agg_tibble <- agg_tibble %>% mutate(present_flag = case_when(present_flag == 1 ~ "G", 
                                              present_flag == 0 ~ "T"), past_flag = case_when(past_flag == 1 ~ "G", 
                                    past_flag == 0 ~ "T"))

agg_tibble <- agg_tibble %>% select(RegionName, RegionCode, Date, indicator_type,past_value, past_flag, present_value, present_flag, Notes)

write.csv(agg_tibble, "../data/output/OxCGRT_US_changes.csv")


