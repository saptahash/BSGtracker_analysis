rollback.hist <- function(data, date, ...) {
  ggplot(data %>% filter(Date == date), aes(x = rollback_score)) + 
    geom_histogram(binwidth = 0.015)
}


twoaxis.line <- function(data, country){ 
  ggplot(data %>% filter(CountryCode == country), aes(x = Date, group = 1)) + 
  geom_line(aes(y = rollback_score)) + 
  geom_line(aes(y = GovernmentResponseIndex/100)) + 
  geom_point(aes(y = rollback_score)) +
  scale_y_continuous(
    name = "Rollback Score", 
    sec.axis = sec_axis(~.*100, name = "GovernmentResponseIndex")
  ) + 
  scale_x_date(breaks = seq.Date(lubridate::ymd(min(lineplot_oxcgrt$Date)), lubridate::ymd(max(lineplot_oxcgrt$Date)),7)) + 
  theme(axis.text.x = element_text(size = 6.5, angle = 90))
}

rollback.tile <- function(data, region){
  ggplot(data %>% filter(region = region), aes(y = CountryCode, x = Date, fill = rollback_score)) + 
    geom_tile(width = 3, height = 1.5) +
    scale_fill_viridis_c(name = "Rollback Score") + 
    scale_x_date(breaks = seq.Date(lubridate::ymd(min(sample$Date)), lubridate::ymd(max(sample$Date)), 7)) + 
    theme(axis.text.y = element_text(size = 3), 
          axis.text.x = element_text(size = 6, angle = 10))
}