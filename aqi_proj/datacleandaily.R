library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(usmap)
library(maps)
library(gganimate)
library(transformr)
library(gifski)

data2020 <- fread("aqi_proj/data/daily_aqi_by_county_2020.csv", stringsAsFactors = TRUE)
data2019 <- fread("aqi_proj/data/daily_aqi_by_county_2019.csv", stringsAsFactors = TRUE)
data2018 <- fread("aqi_proj/data/daily_aqi_by_county_2018.csv", stringsAsFactors = TRUE)
data2017 <- fread("aqi_proj/data/daily_aqi_by_county_2017.csv", stringsAsFactors = TRUE)
data2016 <- fread("aqi_proj/data/daily_aqi_by_county_2016.csv", stringsAsFactors = TRUE)
data2015 <- fread("aqi_proj/data/daily_aqi_by_county_2015.csv", stringsAsFactors = TRUE)
data2014 <- fread("aqi_proj/data/daily_aqi_by_county_2014.csv", stringsAsFactors = TRUE)
data2013 <- fread("aqi_proj/data/daily_aqi_by_county_2013.csv", stringsAsFactors = TRUE)
data2012 <- fread("aqi_proj/data/daily_aqi_by_county_2012.csv", stringsAsFactors = TRUE)
data2011 <- fread("aqi_proj/data/daily_aqi_by_county_2011.csv", stringsAsFactors = TRUE)
data2010 <- fread("aqi_proj/data/daily_aqi_by_county_2010.csv", stringsAsFactors = TRUE)
data2009 <- fread("aqi_proj/data/daily_aqi_by_county_2009.csv", stringsAsFactors = TRUE)
data2008 <- fread("aqi_proj/data/daily_aqi_by_county_2008.csv", stringsAsFactors = TRUE)
data2007 <- fread("aqi_proj/data/daily_aqi_by_county_2007.csv", stringsAsFactors = TRUE)
data2006 <- fread("aqi_proj/data/daily_aqi_by_county_2006.csv", stringsAsFactors = TRUE)
data2005 <- fread("aqi_proj/data/daily_aqi_by_county_2005.csv", stringsAsFactors = TRUE)
data2004 <- fread("aqi_proj/data/daily_aqi_by_county_2004.csv", stringsAsFactors = TRUE)
data2003 <- fread("aqi_proj/data/daily_aqi_by_county_2003.csv", stringsAsFactors = TRUE)
data2002 <- fread("aqi_proj/data/daily_aqi_by_county_2002.csv", stringsAsFactors = TRUE)
data2001 <- fread("aqi_proj/data/daily_aqi_by_county_2001.csv", stringsAsFactors = TRUE)
data2000 <- fread("aqi_proj/data/daily_aqi_by_county_2000.csv", stringsAsFactors = TRUE)


raw_data <- rbind(data2020, data2019, data2018, data2017, data2016,data2015, data2014, data2013, data2012, data2011, data2010,
                  data2009, data2008, data2007, data2006,data2005, data2004, data2003, data2002, data2001, data2000)

raw_data <- raw_data %>%
  mutate(date_formatted = as.Date(Date),
         month = as.Date(format(date_formatted, "%Y-%m-01")))

raw_data <- raw_data %>% rename(
  state = `State Name`,
  county = `county Name`,
  state.code = `State Code`,
  county.code = `County Code`,
  defining.parameter = `Defining Parameter`,
  defining.site = `Defining Site`,
  num.rep.sites = `Number of Sites Reporting`
)

raw_data$county <-tolower(raw_data$county)

counties_by_month <- raw_data %>% group_by(county, month) %>% summarise(county.mean = mean(AQI))
states_by_month <- raw_data %>% group_by(state, month) %>% summarise(state.mean = mean(AQI))

states_by_month <- states_by_month %>% group_by(state) %>%
  mutate(delta.aqi.state = state.mean - lag(state.mean))

raw_data <- merge(raw_data, counties_by_month, by=c("county", "month"))
raw_data <- merge(raw_data, states_by_month, by=c("state", "month"))


ggplot(states_by_month, aes(x = month, y = delta.aqi.state, color = state)) +
  geom_line() +
  facet_wrap( ~ state)

ca_map <- rbind(map_data("county","California"), map_data("county", "Nevada"), map_data("county", "Oregon"), map_data("county", "Washington"))

raw_subset <- raw_data %>% select("state", "county", "date_formatted", "AQI", "month")

ca_county_subset <- inner_join(ca_map, raw_subset,by=c('subregion' = 'county'))


california_base <- ggplot(data = ca_county_subset[ca_county_subset$date_formatted >= as.Date("2020-06-01"), ],
                          mapping = aes(x = long, y = lat, group = subregion)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")

preanim <- california_base +
  geom_polygon(aes(fill = AQI), color = "white") +
  scale_fill_gradient(limits = c(0,300), high = "red", low = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  transition_time(date_formatted)

anim <- animate(preanim, nframes = 360, fps = 10, renderer = gifski_renderer(), end_pause = 30)

anim_save("cali6.gif", anim)




