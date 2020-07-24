gap.fill <- function(data, var){
  data %>% arrange(CountryCode, Date) %>% group_by(CountryCode) %>%
    mutate()
}