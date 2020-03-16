library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

conf <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
death <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
recov <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

conf1  <- conf %>% 
  pivot_longer(names_to = "Date", values_to = "Counts", cols = -(1:4) ) %>% 
  mutate(Date = mdy(Date),
         label = "confirmed")

death1 <- death %>% 
  pivot_longer(names_to = "Date", values_to = "Counts", cols = -(1:4) ) %>% 
  mutate(Date = mdy(Date),
         label = "death")

recov1  <- recov %>% 
  pivot_longer(names_to = "Date", values_to = "Counts", cols = -(1:4) ) %>% 
  mutate(Date = mdy(Date),
         label = "recovered")

dat <- bind_rows(conf1, death1, recov1)

dat %>% group_by(Date, label) %>% summarise(count = sum(Counts)) %>% 
  ggplot() + aes(x=Date, y=count, col = label) + geom_line() + geom_point() +
  coord_trans(y = "log")


#### Map

library(sf)
library(rnaturalearth)

world <- ne_countries(scale = "small", returnclass = "sf") %>% 
  select(admin, postal)

dat2 <- dat %>% 
  filter(label == "confirmed", Date == max(Date)) %>% 
  mutate(Country = case_when(`Country/Region` == "US" ~ "United States of America",
                             `Country/Region` %in% "Congo" ~ "Congo",
                             TRUE ~ `Country/Region` )) %>% 
  group_by(Country) %>% 
  summarise(cnt = sum(Counts)) 

world2 <- world %>% 
  left_join(dat2, by = c("admin" = "Country"))

ggplot(world2) + geom_sf(aes(fill = cnt)) +
  scale_fill_binned(#low="#FFFFCC", high="#800026", 
                     gradient = RColorBrewer::brewer.pal(6, "Reds"),
                     breaks = c(1000, 2000, 3000, 5000, 10000, 50000, 100000),
                     guide = guide_coloursteps(even.steps = FALSE, 
                                               show.limits = TRUE,
                                               ticks.linewidth = 2,
                                               barheight = unit(3, "in")))

