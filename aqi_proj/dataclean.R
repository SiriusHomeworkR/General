library(tidyverse)
library(data.table)
library(lubridate)


data2020 <- fread("aqi_proj/data/annual_aqi_by_county_2020.csv")
data2019 <- fread("aqi_proj/data/annual_aqi_by_county_2019.csv")
data2018 <- fread("aqi_proj/data/annual_aqi_by_county_2018.csv")
data2017 <- fread("aqi_proj/data/annual_aqi_by_county_2017.csv")
data2016 <- fread("aqi_proj/data/annual_aqi_by_county_2016.csv")
data2015 <- fread("aqi_proj/data/annual_aqi_by_county_2015.csv")
data2014 <- fread("aqi_proj/data/annual_aqi_by_county_2014.csv")
data2013 <- fread("aqi_proj/data/annual_aqi_by_county_2013.csv")
data2012 <- fread("aqi_proj/data/annual_aqi_by_county_2012.csv")
data2011 <- fread("aqi_proj/data/annual_aqi_by_county_2011.csv")
data2010 <- fread("aqi_proj/data/annual_aqi_by_county_2010.csv")

raw_data <- rbind(data2020, data2019, data2018, data2017, data2016,
                  data2015, data2014, data2013, data2012, data2011, data2010)

raw_data <- raw_data %>% rename(
  num.days = `Days with AQI`,
  good.days = `Good Days`,
  mod.days = `Moderate Days`,
  sens.group.days = `Unhealthy for Sensitive Groups Days`,
  unhlthy.days = `Unhealthy Days`,
  very.unhlthy.days = `Very Unhealthy Days`,
  haz.days = `Hazardous Days`,
  max.aqi = `Max AQI`,
  med.aqi = `Median AQI`
)

raw_data <- raw_data %>%
  mutate(good.pct = good.days/num.days*100,
         mod.pct = mod.days/num.days*100,
         sens.group.pct = sens.group.days/num.days*100,
         unhlthy.pct = unhlthy.days/num.days*100,
         very.unhlthy.pct = very.unhlthy.days/num.days*100,
         haz.pct = haz.days/num.days*100)

raw_data %>% group_by(state.name, year, month) %>% summarize(
  mean.aqi = mean(AQI)
)