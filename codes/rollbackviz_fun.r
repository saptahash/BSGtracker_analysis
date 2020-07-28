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
  ggplot(plot_rollback %>% filter(Date == date), aes(x = openness_risk, y = StringencyIndex, color = factor(lightup_state), label = CountryCode)) + 
    geom_point(aes(size = newcases)) + 
    lims(colour = c("0", "1")) + 
    geom_text_repel(data = subset(plot_rollback %>% filter(Date == date), lightup_state == 1 | key_country == 1), 
                    size = 3, colour = "black") +
    geom_hline(yintercept = 50, size = 0.3, linetype = 2) + 
    geom_vline(xintercept = 0.5, size = 0.3, linetype = 2) +
    labs(x = "Openness Risk", 
         y = "Stringency Index", 
         #       title = "Mapping Stringency Index and Rollback readiness", 
         subtitle = scatter.plot.subtitle) + 
    guides(size = F, colour = F) + 
    #    scale_colour_discrete(name = "", breaks = c(1), labels = c("Scaling back lockdown")) +
    scale_y_continuous(breaks = c(25, 35, 50, 75, 100)) 
}

scatter.SI.rollback.detail <- function(date){
  theme_set(theme_gray())
  scatter.plot.subtitle <- paste("Date: ", date, sep = "")
  ggplot(plot_rollback %>% filter(Date == date), aes(x = openness_risk, color = factor(lightup_state), y = StringencyIndex,label = CountryCode)) +  #color = factor(outoflockdown), 
    geom_point(aes(size = newcases)) + 
    lims(colour = c("0", "1")) + 
    geom_text_repel(data = subset(plot_rollback %>% filter(Date == date), lightup_state == 1 | key_country == 1 | ((openness_risk > 0.4) & (StringencyIndex < 50))), 
                    size = 3, colour = "black") + 
#    annotate(geom = "text", x = 0.01, y = 37, label = "Countries below this range are scaling back lockdown", 
#             size = 2.5, hjust = "left") +
    geom_hline(yintercept = 50, size = 0.3, linetype = 2) + 
    geom_vline(xintercept = 0.5, size = 0.3, linetype = 2) +
    labs(x = "Openness Risk", 
         y = "Stringency Index", 
         title = "Mapping Stringency Index and Openness Risk", 
         subtitle = scatter.plot.subtitle) + 
    guides(size = F) + 
#    viridis::scale_colour_viridis(discrete = T) +
    scale_y_continuous(breaks = c(25, 35, 50, 75, 100)) + 
    scale_x_continuous(breaks = c(seq(from = 0, to = 1, by = 0.2))) +
    scale_colour_discrete(name = "", breaks = c(1), labels = c("Dropped stringency levels in past week")) +
    scale_size(range = c(3,9))
}


tilemap.regionwise <- function(region_name){
  ggplot(plot_rollback %>% arrange(CountryCode, Date) %>% ungroup() %>%
           filter(region == region_name) , 
         aes(y = forcats::fct_rev(CountryCode), x = Date, fill = openness_risk)) + 
    geom_tile(width = 0.9, height = 0.9) +
    scale_fill_viridis_c(name = "Openness Risk", na.value = "gray", begin = 0, end = 1) +
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
    geom_sf(aes(fill = openness_risk)) + 
    labs(subtitle = map.subtitle) + 
    scale_fill_viridis_c(option = "viridis", name = "Openness Risk", na.value = "gray") + 
    ggthemes::theme_map()
}

