rollback.hist <- function(data, date, ...) {
  ggplot(data %>% filter(Date == date), aes(x = rollback_score)) + 
    geom_histogram(binwidth = 0.015)
}


twoaxis.line <- function(data, countrylist){ 
  ggplot(data %>% filter(CountryCode %in% countrylist), aes(x = Date, group = 1)) + 
    geom_line(aes(y = rollback_score)) + 
    geom_line(aes(y = StringencyIndex/100)) + 
    geom_point(aes(y = rollback_score)) +
    scale_y_continuous(
      name = "Rollback Score", 
      sec.axis = sec_axis(~.*100, name = "Stringency Index")
    ) + 
    scale_x_date(breaks = seq.Date(lubridate::ymd(min(lineplot_oxcgrt$Date)), lubridate::ymd(max(lineplot_oxcgrt$Date)),7)) + 
    theme(axis.text.x = element_text(size = 6.5, angle = 0))
}


scatter.SI.rollback <- function(date){
  scatter.plot.subtitle <- paste("Date: ", date, sep = "")
  ggplot(oxcgrtdata %>% filter(Date == date) %>% filter(!is.na(outoflockdown)), aes(x = recoded_rollback, y = StringencyIndex, color = factor(outoflockdown), label = CountryCode)) + 
    geom_point(aes(size = ConfirmedCases)) + 
    lims(colour = c("0", "1")) + 
    #      geom_text_repel(data = subset(oxcgrtdata %>% filter(Date == "2020-06-27"), outoflockdown == 1), 
    #                size = 3) + 
    #    annotate(geom = "text", x = 0.01, y = 37, label = "Countries below this range are scaling back lockdown", 
    #             size = 1.5, hjust = "left") +
    geom_hline(yintercept = 35, size = 0.3, linetype = 2) + 
    labs(x = "Rollback Readiness", 
         y = "Stringency Index", 
         #       title = "Mapping Stringency Index and Rollback readiness", 
         subtitle = scatter.plot.subtitle) + 
    guides(size = F, colour = F) + 
    #    scale_colour_discrete(name = "", breaks = c(1), labels = c("Scaling back lockdown")) +
    scale_y_continuous(breaks = c(25, 35, 50, 75, 100)) 
}

scatter.SI.rollback.detail <- function(date){
  scatter.plot.subtitle <- paste("Date: ", date, sep = "")
  ggplot(oxcgrtdata %>% filter(Date == date) %>% filter(!is.na(outoflockdown)), aes(x = recoded_rollback, y = StringencyIndex, color = factor(outoflockdown), label = CountryCode)) + 
    geom_point(aes(size = newcases)) + 
    lims(colour = c("0", "1")) + 
    geom_text_repel(data = subset(oxcgrtdata %>% filter(Date == date), outoflockdown == 1), 
                    size = 3) + 
    annotate(geom = "text", x = 0.01, y = 37, label = "Countries below this range are scaling back lockdown", 
             size = 2.5, hjust = "left") +
    geom_hline(yintercept = 35, size = 0.3, linetype = 2) + 
    labs(x = "Rollback Readiness", 
         y = "Stringency Index", 
         title = "Mapping Stringency Index and Rollback readiness", 
         subtitle = scatter.plot.subtitle) + 
    guides(size = F) + 
    scale_colour_discrete(name = "", breaks = c(1), labels = c("Scaling back lockdown")) +
    scale_y_continuous(breaks = c(25, 35, 50, 75, 100)) 
}


tilemap.regionwise <- function(region_name){
  ggplot(plot_rollback %>% arrange(CountryCode, Date) %>% ungroup() %>%
           filter(region == region_name) , 
         aes(y = forcats::fct_rev(CountryCode), x = Date, fill = recoded_rollback)) + 
    geom_tile(width = 0.9, height = 0.9) +
    scale_fill_viridis_c(name = "Rollback Readiness Index", na.value = "gray") +
    scale_x_date(breaks = seq.Date(lubridate::ymd(min(plot_rollback$Date)),
                                   lubridate::ymd(max(plot_rollback$Date) - 7), 7), 
                 limits = c(lubridate::ymd("2020-04-01"), max(plot_rollback$Date) - 7), 
                 expand = c(0,0)) + 
    scale_y_discrete(limits = rev(levels(plot_rollback$CountryCode))) +
    theme(axis.text.y = element_text(size = 10), 
          axis.text.x = element_text(size = 6, angle = 10),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme_classic() +
    labs(x = "Date", 
         y = "Country Code (ISO-3)")
}

chloropleth.map.summary <- function(date){
  map.subtitle = paste("Date: ", date, sep = "")
  ggplot(map_df %>% filter(Date == lubridate::ymd(date))) +
    geom_sf(aes(fill = recoded_rollback)) + 
    labs(subtitle = map.subtitle) + 
    scale_fill_viridis_c(option = "viridis", name = "Rollback Readiness Index", na.value = "gray") + 
    ggthemes::theme_map()
}

