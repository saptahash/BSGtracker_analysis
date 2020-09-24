finalplot <- ggplot(plot_rollback %>% filter(Date %in% dateseq_scatter), aes(x = openness_risk, y = StringencyIndex, color = factor(lightup_state), label = CountryCode)) + 
  geom_point(aes(size = newcases)) + 
  lims(colour = c("0", "1")) + 
  geom_text_repel(data = subset(plot_rollback %>% filter(Date %in% dateseq_scatter), lightup_state == 1 | key_country == 1), 
                  size = 3, colour = "black") +
  geom_hline(yintercept = 50, size = 0.3, linetype = 2) + 
  geom_vline(xintercept = 0.5, size = 0.3, linetype = 2) +
  labs(x = "Risk of Openness", 
       y = "Stringency Index", 
#       title = "Stringency Index and Openness Risk over last quarter",
#       subtitle = "(Bubble Size reflects number of new cases)") + 
       caption = "Note: Size of each bubble reflects number of new cases") + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 12.5), 
        plot.subtitle = element_text(hjust = 0.5, size = 10), 
        legend.position = "bottom", 
        legend.text = element_text(size = 15), 
        strip.text.x = element_text(size = 15)) +
  guides(size = F, 
         colour = guide_legend(override.aes = list(size=4))) + 
  scale_colour_discrete(name = "", breaks = c(1), labels = c("Scaling back lockdown")) +
  scale_y_continuous(breaks = c(25, 50, 75, 100)) + 
  scale_x_continuous(breaks = c(seq(from = 0, to = 1, by = 0.2))) +
  scale_colour_discrete(name = "", breaks = c(1), labels = c("Dropped stringency levels in previous week (7 days prior to date)")) +
  scale_size(range = c(2,7)) +
  annotate(geom = "rect", xmin = 0.5, ymin = 0, xmax = 1, ymax = 50, alpha = 0.15) +
  facet_wrap(~ Date)



ggplot(plot_rollback %>% filter(Date %in% dateseq_scatter), aes(x = openness_risk, y = StringencyIndex, color = factor(lightup_state), label = CountryCode)) + 
  geom_point(aes(size = newcases)) + 
  lims(colour = c("0", "1")) + 
  geom_text_repel(data = subset(plot_rollback %>% filter(Date %in% dateseq_scatter), lightup_state == 1 | key_country == 1), 
                  size = 3, colour = "black") +
  geom_hline(yintercept = 50, size = 0.3, linetype = 2) + 
  geom_vline(xintercept = 0.5, size = 0.3, linetype = 2) +
  labs(x = "Risk of Openness", 
       y = "Stringency Index", 
       #         title = "Stringency Index and Openness Risk over last quarter",
       #         subtitle = "(Bubble Size reflects number of new cases)", 
       caption = "Bubble Size reflects number of new cases") + 
  theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 12.5), 
        plot.subtitle = element_text(hjust = 0.5, size = 10),        
        legend.text = element_text(size = 15), 
        strip.text.x = element_text(size = 15)) +
  guides(size = F,
         colour = guide_legend(override.aes = list(size=4))) + 
  scale_colour_discrete(name = "", breaks = c(1), labels = c("Scaling back lockdown")) +
  scale_y_continuous(breaks = c(25, 50, 75, 100)) + 
  scale_x_continuous(breaks = c(seq(from = 0, to = 1, by = 0.2))) +
  scale_colour_discrete(name = "", breaks = c(1), labels = c("Dropped stringency levels in previous week\n (7 days prior to date)")) +
  scale_size(range = c(2,7)) +
  facet_wrap(~ Date) +
  theme(legend.position = "bottom")

ggsave(paste("../graphs/new-score/summary_scatterSIroll_latest", ".png", sep = ""), plot = finalplot,
       width = 12, 
       height = 8)

theme_set(theme_gray())
date <- lubridate::as_date(max(oxcgrtdata$Date)) - 14
scatter.plot.title <- paste("Stringency Index and Risk of Openness as of ", lubridate::as_date(date), sep = "")
ggplot(plot_rollback %>% filter(Date == date), aes(x = openness_risk, color = factor(lightup_state), y = StringencyIndex,label = CountryCode)) +  #color = factor(outoflockdown), 
  geom_point(aes(size = newcases)) + 
  lims(colour = c("0", "1")) + 
  geom_text_repel(data = subset(plot_rollback %>% filter(Date == date), lightup_state == 1 | key_country == 1 | ((openness_risk > 0.4) & (StringencyIndex < 50))), 
                  size = 3, colour = "black") + 
  #    annotate(geom = "text", x = 0.01, y = 37, label = "Countries below this range are scaling back lockdown", 
  #             size = 2.5, hjust = "left") +
  geom_vline(xintercept = 0.5, size = 0.3, linetype = 2) +
  geom_segment(aes(x = 0.5, xend = 1, y = 50, yend = 50), color = "black", linetype = 2) +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 12.5), 
        plot.subtitle = element_text(hjust = 0.5, size = 10), 
        legend.position = "bottom", 
        legend.text = element_text(size = 15)) +
  labs(x = "Risk of Openness", 
       y = "Stringency Index", 
#       subtitle = "(Bubble size reflects number of new cases)", 
#       title = scatter.plot.title,
       caption = "Note: Size of bubble reflects the number of new cases") + 
  guides(size = F, 
         colour = guide_legend(override.aes = list(size=4))) + 
  #    viridis::scale_colour_viridis(discrete = T) +
  scale_y_continuous(breaks = c(25, 50, 75, 100)) + 
  scale_x_continuous(breaks = c(seq(from = 0, to = 1, by = 0.2))) +
  scale_colour_discrete(name = "", breaks = c(1), labels = c("Dropped stringency levels in past week")) +
  scale_size(range = c(3,9)) +
  annotate(geom = "label", x = 1, y = 99, label = "Group 1", 
           color = "black") +
  annotate(geom = "label", x = 1, y = 1, label = "Group 2", 
           color = "red") + 
  geom_label( x = 0, y = 50, label = "Group 3", 
              color = "black", angle = 90) + 
  annotate(geom = "text", x = 0, y = 99, label = "", 
           color = "black") + 
  annotate(geom = "rect", xmin = 0.5, ymin = 50, xmax = 1, ymax = 100, alpha = 0.075) +
  annotate(geom = "rect", xmin = 0.5, ymin = 0, xmax = 1, ymax = 50, alpha = 0.15) +
  facet_wrap(~Date)

ggsave(paste("../graphs/new-score/detail_scatterSIroll_latest", ".png", sep = ""), width = 10, 
       height = 8)

ggplot(lineplot_rollback %>% filter(CountryCode %in% country_lineplot), aes(x = Date, group = 1)) + 
  geom_line(aes(y = openness_risk)) + 
  geom_line(aes(y = StringencyIndex/100), colour = "red") + 
  scale_y_continuous(
    name = "Risk of Openness", 
    sec.axis = sec_axis(~.*100, name = "Stringency Index")) + 
  scale_x_date(breaks = seq.Date(lubridate::ymd(min(lineplot_rollback$Date)), lubridate::ymd(max(lineplot_rollback$Date)),45),
               date_labels = "%d-%b") + 
  theme(axis.text.x = element_text(size = 11), 
        axis.text.y.right = element_text(colour = "red"), 
        axis.title.y.right = element_text(colour = "red"), 
        plot.caption = element_text(hjust = 0.0, face = "italic"), 
        plot.title = element_text(hjust = 0.5), 
        strip.text.x = element_text(size = 15)) + 
#  labs(title = "Openness Risk Index and Stringency Index of twelve countries over time",
#       caption = "Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker 
#       or bsg.ox.ac.uk/covidtracker") + 
  facet_wrap(~ CountryName)

ggsave(paste("../graphs/new-score/lineplot_latest", ".png", sep = ""),
       width = 16, 
       height = 8)

ggplot(map_df %>% filter(Date %in% dateseq_scatter)) +
  geom_sf(aes(fill = openness_risk)) + 
  ggthemes::theme_map() +
#  labs(title = "Chloropleth Map of countries over time",
#       caption = "Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker or bsg.ox.ac.uk/covidtracker" ) + 
  theme(plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.0, face = "italic"), 
        legend.position = "right",
        strip.text.x = element_text(size = 15)) +
  scale_fill_viridis_c(option = "viridis", name = "Risk of Openness", na.value = "gray", direction = -1, 
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) + 
  facet_wrap(~Date)

ggsave(paste("../graphs/new-score/chloropleth_latest", ".png", sep = ""), width = 15, 
       height = 7.5)

