install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(ggrepel)
library(gganimate)
library(plotly)
library(ggpubr)

## Visualise general spread of rollback scores - what does the distribution look like over time? 
date_seq <- seq.Date(from = lubridate::ymd('2020-03-01'), to = max(lineplot_oxcgrt$Date), by = 7)

P <- list()  
for(date in date_seq){
  p <- rollback.hist(lineplot_oxcgrt, zoo::as.Date(date))
  P <- c(p, list(P))
  ggsave(paste("rollback_hist", zoo::as.Date(date), ".png", sep = ""), plot = p)
}

### missing feat - need to save all these in a single page

## Two way line plot of Stringency/Government Response Index v/s rollback_scores
countrylist <- c("CHN", "KOR", "FRA", "ITA", "GBR", "USA", "ESP")

twoaxis.line(lineplot_oxcgrt, "USA")

for(country in countrylist){
  p <- twoaxis.line(lineplot_oxcgrt, country)
  #ggsave
}

## 
p1 = ggplot(sample, aes(y = CountryCode, x = Date, fill = rollback_score)) + 
  geom_tile(width = 3, height = 1.5) +
  scale_fill_viridis_c(name = "Rollback Score") + 
  scale_x_date(breaks = seq.Date(lubridate::ymd(min(sample$Date)), lubridate::ymd(max(sample$Date)), 7)) + 
  theme(axis.text.y = element_text(size = 3), 
        axis.text.x = element_text(size = 6, angle = 10))

lineplot_oxcgrt <- lineplot_oxcgrt %>% 
  mutate(rollback_score = ifelse(rollback_score > 1, 1, rollback_score)) %>% 
           mutate(tilemap_color = case_when(rollback_score < 0.25 ~ 1, 
                                            rollback_score >= 0.25 & rollback_score < 0.5 ~ 2, 
                                            rollback_score >= 0.5 & rollback_score < 0.75 ~ 3, 
                                            rollback_score >= 0.75 ~ 4))

### looks terrible -> pick better colour palette?
ggplot(lineplot_oxcgrt %>% filter(region == "Europe_Central_Asia"),
       aes(y = CountryCode, x = Date, fill = rollback_score)) + 
  geom_tile(width = 3, height = 1.5) +
  scale_fill_fermenter(breaks = c(0, 0.25, 0.5, 0.75, 1), palette = c("#F4EDCA", "#C3D7A4", "#52854C", "#4E84C4")) + 
  scale_x_date(breaks = seq.Date(lubridate::ymd(min(sample$Date)), lubridate::ymd(max(sample$Date)), 7)) + 
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6, angle = 10),
        legend.position = "top") 

### looks better with continuous plot
ggplot(lineplot_oxcgrt %>% filter(region == "North_America"), 
       aes(y = CountryCode, x = Date, fill = rollback_score)) + 
  geom_tile(width = 3, height = 1.5) +
  scale_fill_viridis_c(name = "Rollback Score") +
#  scale_fill_fermenter(breaks = c(0, 0.25, 0.5, 0.75, 1), palette = "Reds") +
  scale_x_date(breaks = seq.Date(lubridate::ymd(min(sample$Date)), lubridate::ymd(max(sample$Date)), 7)) + 
  theme(axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6, angle = 10))

### need to define a lower limit at which lockdowns aren't applicable
### recoding rollback_score 

lineplot_oxcgrt %>% group_by(CountryCode) %>% mutate(lockdown_cutoff = 0.5*max(StringencyIndex)) %>%
  










ggplot(lineplot_oxcgrt %>% filter(CountryCode == "ESP"), aes(x = Date, group = 1)) + 
  geom_line(aes(y = rollback_score)) + 
  geom_line(aes(y = GovernmentResponseIndex/100)) + 
  geom_point(aes(y = rollback_score)) +
  scale_y_continuous(
    name = "Rollback Score", 
    sec.axis = sec_axis(~.*100, name = "GovernmentResponseIndex")
  ) + 
  scale_x_date(breaks = seq.Date(lubridate::ymd(min(lineplot_oxcgrt$Date)), lubridate::ymd(max(lineplot_oxcgrt$Date)),7)) + 
  theme(axis.text.x = element_text(size = 6.5, angle = 90))




rollback.hist(lineplot_oxcgrt, lubridate::ymd(date_seq[4]))
rollback.hist(lineplot_oxcgrt, '2020-03-05')

ggplot(lineplot_oxcgrt %>% filter(Date == '2020-03-05'), aes(x = rollback_score)) + 
  geom_histogram(binwidth = 0.015)

ggplot(lineplot_oxcgrt %>% filter(Date == '2020-06-05'), aes(x = rollback_score)) + 
  geom_histogram(binwidth = 0.015)

ggplot(lineplot_oxcgrt %>% filter(Date == '2020-06-05'), aes(x = StringencyIndex)) + 
  geom_histogram(binwidth = 2, alpha = 0.5)




## WORLD MAP gif
### f1 - need pause 
### f2 - white/greyfor no data

world <- ne_countries(scale = "medium",returnclass = "sf")

animation_oxcgrt <- oxcgrtdata %>% select(CountryCode, Date, rollback_score) %>% 
  filter(Date > "2020-03-01" & Date < "2020-06-28")

rollback_df <- left_join(world, animation_oxcgrt, by = c("iso_a3" = "CountryCode"))

ggplot(rollback_df %>% filter(Date == "2020-03-19")) +
  geom_sf(aes(fill = rollback_score)) + 
  theme_bw() + 
  labs(title = "Contry") + 
  scale_fill_viridis_c(name = "Rollback Scores")

## producing the gif for rollback from Mar 1- latest available data
rollback_plot <- ggplot(rollback_df %>% arrange(Date) %>%
                          mutate(rollback_score = ifelse(rollback_score > 1, 1, rollback_score))) + 
  geom_sf(aes(fill = rollback_score)) + 
  theme_light() + 
  labs(title = "Country rollback-scores", 
       subtitle = "Date: {current_frame}") +
  scale_fill_viridis_c(name = "Rollback Scores") + 
  transition_manual(Date) + 
  ease_aes()

rollback_anim <- animate(rollback_plot, fps = 5, width = 1000, height = 500, renderer = gifski_renderer(loop = F))
save_animation(rollback_anim, file = "rollback_animation_fps5.gif")

## Line plots - two line plots 
## 1. Standard two-y axis line plot
## 2. scatter-connected plot w/ arrowheads

lineplot_oxcgrt <- oxcgrtdata %>% select(CountryCode, GovernmentResponseIndex, Date, region, rollback_score, newcases, StringencyIndex, 
                                         cases_rate, deaths_rate_pct, lag1_ConfirmedDeaths,
                                         ConfirmedDeaths, ConfirmedCases) %>% 
  filter(Date > "2020-03-01" & Date < "2020-06-28") %>%
  mutate(Date = lubridate::ymd(Date))

#lineplot_oxcgrt <- lineplot_oxcgrt %>% arrange(CountryCode, Date) %>% group_by(CountryCode) %>%
#  mutate(lag1_ConfirmedDeaths = lag(ConfirmedDeaths, n=1, order_by = Date))

lineplot_oxcgrt <- lineplot_oxcgrt %>% group_by(CountryCode) %>% 
  mutate(growthrate_deaths = (ConfirmedDeaths - lag1_ConfirmedDeaths)/(lag1_ConfirmedDeaths), 
         new_deaths = ConfirmedDeaths - lag1_ConfirmedDeaths)

## line graph of growth rate of deaths
ggplot(lineplot_oxcgrt %>% filter(CountryCode == "GBR"), aes(x = Date, group = 1)) + 
  geom_line(aes(y = StringencyIndex))

ggplot(lineplot_oxcgrt %>% filter(CountryCode == "IND"), aes(x = Date, group = 1)) + 
  geom_line(aes(y = rollback_score))

## line graph of rollback scores
ggplot(lineplot_oxcgrt %>% filter(CountryCode == "GBR")) + 
  geom_line(aes(x = Date, y = rollback_score, group = 1)) + 
  geom_point(aes(x = Date, y = rollback_score))

## Using Growth rates instead of level - finalising
ggplot(lineplot_oxcgrt %>% filter(CountryCode == "ESP"), aes(x = Date, group = 1)) + 
  geom_line(aes(y = rollback_score)) + 
  geom_line(aes(y = GovernmentResponseIndex/100)) + 
  geom_point(aes(y = rollback_score)) +
  scale_y_continuous(
    name = "Rollback Score", 
    sec.axis = sec_axis(~.*100, name = "GovernmentResponseIndex")
  ) + 
  scale_x_date(breaks = seq.Date(lubridate::ymd(min(lineplot_oxcgrt$Date)), lubridate::ymd(max(lineplot_oxcgrt$Date)),7)) + 
  theme(axis.text.x = element_text(size = 6.5, angle = 90))

#bug - should carryforward stringency index

## Connected scatter plots 
ggplot(lineplot_oxcgrt %>% filter(CountryCode == "IND"), aes(x = ConfirmedDeaths, y = rollback_score, label = Date)) + 
  geom_point() +
  geom_segment(aes(xend = c(tail(ConfirmedDeaths, n= -1), NA), yend = c(tail(rollback_score,n = -1), NA)), 
               arrow = arrow(length = unit(0.3, "cm")))


ggplot(lineplot_oxcgrt %>% filter(CountryCode == "IND"), aes(x = ConfirmedCases, y = rollback_score, label = Date)) + 
  geom_point() +
  geom_segment(aes(xend = c(tail(ConfirmedCases, n= -1), NA), yend = c(tail(rollback_score,n = -1), NA)), 
               arrow = arrow(length = unit(0.3, "cm")))

# possible - improve
ggplot(lineplot_oxcgrt %>% filter(CountryCode == "IND"), aes(x = StringencyIndex, y = rollback_score, label = Date)) + 
  geom_point() +
  geom_segment(aes(xend = c(tail(StringencyIndex, n= -1), NA), yend = c(tail(rollback_score,n = -1), NA)), 
               arrow = arrow(length = unit(0.3, "cm")))

## moving scatter plot over time 

# messy - reject
ggplot(lineplot_oxcgrt %>% filter(CountryCode == "IND"), aes(x = new_deaths, y = rollback_score, label = Date)) + 
  geom_point() +
  geom_segment(aes(xend = c(tail(new_deaths, n= -1), NA), yend = c(tail(rollback_score,n = -1), NA)), 
               arrow = arrow(length = unit(0.3, "cm")))


#incomplete heatmap using ggplot2
ggplot(lineplot_oxcgrt %>% arrange(Date) %>%
         mutate(rollback_score = ifelse(rollback_score > 1, 1, rollback_score)), 
       aes(y = CountryCode, x = Date, fill = rollback_score)) + 
  theme_minimal() + 
  geom_tile(color = "white", width = 1.1, height = 1.1) + 
  scale_fill_viridis_c(name = "Rollback Scores") +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        plot.title = element_text(size = 20, face = 'bold', vjust = 2, hjust = 0.5),
        axis.text.x = element_text(size = 8, hjust = .5, vjust = .5, face = 'plain'),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(axis.text = element_text(size = 2))
  

sample <- lineplot_oxcgrt %>% arrange(Date) %>% mutate(Date = lubridate::ymd(Date)) %>%
  mutate(rollback_score = ifelse(rollback_score > 1, 1, rollback_score)) %>% 
  filter(Date > "2020-03-03") 

#heatmap using heatmap function 
 p1 = ggplot(sample, aes(y = CountryCode, x = Date, fill = rollback_score)) + 
  geom_tile(width = 3, height = 1.5) +
  scale_fill_viridis_c(name = "Rollback Score") + 
  scale_x_date(breaks = seq.Date(lubridate::ymd(min(sample$Date)), lubridate::ymd(max(sample$Date)), 7)) + 
  theme(axis.text.y = element_text(size = 3), 
        axis.text.x = element_text(size = 6, angle = 10))

#heatmap
ggplotly(p1)














+ 
  geom_text_repel(data = sample_dates) + 
  theme_light()



############################################################
############### ROUGH CODE
##########



#save_animation(rollback_anim, file = "rollback.gif")

world <- ne_countries(scale = "medium",returnclass = "sf")
class(world)

ggplot(data = world) + geom_sf()

ggplot(data = world) + geom_sf(color = "black", fill = "lightgreen") +
  xlab("Longitude") + ylab("Latitude") + 
  ggtitle("World Map", subtitle = paste0(length(unique(world$name)), " countries"))

oxcgrt_world <- left_join(world, sample,
                          by = c("iso_a3" = "CountryCode"))

ccmap <- ggplot(data = oxcgrt_world %>% mutate(Date = lubridate::ymd(Date)) %>% arrange(Date)) +
  geom_sf(aes(fill = ConfirmedCases)) + 
  scale_fill_viridis_c(name = "ConfirmedCases") + 
  transition_manual(Date) + 
  ease_aes()

sample <- oxcgrtdata %>% select(CountryCode, Date, ConfirmedCases) %>%
  mutate(Date = lubridate::ymd(Date)) %>% filter(Date > "2020-03-03" & Date < "2020-06-03")

gganimate(cc_map, "cc_map.gif", title_frame = T)
save_animation(cc_map_a, file = "cc_map.gif")


oxcgrt_world <- oxcgrt_world %>% group_by(iso_a3) %>% mutate(Date = as.Date(Date)) 

cc_map <- ggplot(oxcgrt_world %>% arrange(Date)) + 
  geom_sf(aes(fill = ConfirmedCases)) + 
  scale_fill_viridis_c(name = "ConfirmedCases")
transition_time(Date) +
  ease_aes() 




ggplot(oxcgrtdata %>% select(CountryCode, Date, ConfirmedCases, popWB, rollback_score) %>% mutate(casespercapita = ConfirmedCases/popWB) %>% filter(CountryCode == "GBR"), aes(x = Date, y = casespercapita, group = 1)) +
  geom_line() +
  geom_point()

cases_map <- ggplot(oxcgrtdata %>% select(CountryCode, Date, ConfirmedCases)) + 
  geom_sf()

cpc_end <- 
  
  sample_dates <- sample %>% sample_frac(0.1)


ggplot(sample, aes(x = newcases, y = rollback_score, label = Date)) + 
  geom_point() +
  geom_segment(aes(xend = c(tail(newcases, n= -1), NA), yend = c(tail(rollback_score,n = -1), NA)), 
               arrow = arrow(length = unit(0.2, "cm"))) + 
  geom_text_repel(data = sample_dates) + 
  theme_light()


cc_map_a <- animate(ccmap, duration = 10, fps = 2, width = 1000, height = 500, renderer = gifski_renderer())


























