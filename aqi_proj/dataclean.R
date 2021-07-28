library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(usmap)
library(rvest)

data2020 <- fread("aqi_proj/data/annual_aqi_by_county_2020.csv", stringsAsFactors = TRUE)
data2019 <- fread("aqi_proj/data/annual_aqi_by_county_2019.csv", stringsAsFactors = TRUE)
data2018 <- fread("aqi_proj/data/annual_aqi_by_county_2018.csv", stringsAsFactors = TRUE)
data2017 <- fread("aqi_proj/data/annual_aqi_by_county_2017.csv", stringsAsFactors = TRUE)
data2016 <- fread("aqi_proj/data/annual_aqi_by_county_2016.csv", stringsAsFactors = TRUE)
data2015 <- fread("aqi_proj/data/annual_aqi_by_county_2015.csv", stringsAsFactors = TRUE)
data2014 <- fread("aqi_proj/data/annual_aqi_by_county_2014.csv", stringsAsFactors = TRUE)
data2013 <- fread("aqi_proj/data/annual_aqi_by_county_2013.csv", stringsAsFactors = TRUE)
data2012 <- fread("aqi_proj/data/annual_aqi_by_county_2012.csv", stringsAsFactors = TRUE)
data2011 <- fread("aqi_proj/data/annual_aqi_by_county_2011.csv", stringsAsFactors = TRUE)
data2010 <- fread("aqi_proj/data/annual_aqi_by_county_2010.csv", stringsAsFactors = TRUE)

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
  med.aqi = `Median AQI`,
  state = State
)


raw_data <- raw_data %>%
  mutate(good.pct = good.days/num.days*100,
         mod.pct = mod.days/num.days*100,
         sens.group.pct = sens.group.days/num.days*100,
         unhlthy.pct = unhlthy.days/num.days*100,
         very.unhlthy.pct = very.unhlthy.days/num.days*100,
         haz.pct = haz.days/num.days*100)

mean.state.df <- raw_data %>% group_by(state, Year) %>% summarize(mean.state = mean(med.aqi),
                                                                  peak.state = max(max.aqi))

raw_data <- merge(raw_data, mean.state.df, by=c("state", "Year"))



ggplot(raw_data, aes(x = Year, y = mean.state, color = state)) +
  geom_line() +
  geom_point()

plot_usmap(regions = "state",                   
           #regions = "counties", for county level summary
           data = mean.state.df,
           values = "mean.state", exclude = c("District of Columbia", "Country of Mexico",
                                                     "Virgin Islands", "Puerto Rico")
           , color = "black") + 
  scale_fill_gradient(
    low = "white", high = "red", 
    name = "AQI", 
    label = scales::comma) + 
  labs(title = "state Avg AQI") +
  theme(legend.position = "right") +
  facet_wrap(~ Year)


#Pollutant trend graphs that affect air quality by year
library(ggplot2) 
library(dplyr)
library(gridExtra)
 
#EDA

airquality_ozone<- read.csv("/Users/adiay/Downloads/Wharton/Project/OzoneNational.csv", header=T)
airquality_nitrogen<- read.csv("/Users/adiay/Downloads/Wharton/Project/Nitrogen_DioxideNational.csv", header=T)
airquality_sulfur<-read.csv("/Users/adiay/Downloads/Wharton/Project/Sulfur_DioxideNational.csv", header=T)
nitrogen_concentration<- airquality_nitrogen$X10th.Percentile+airquality_nitrogen$X90th.Percentile 
ozone_concentration<- airquality_ozone$X10th.Percentile+airquality_ozone$X90th.Percentile  
sulfur_concentration<- airquality_sulfur$X10th.Percentile+airquality_sulfur$X90th.Percentile 

str( airquality_ozone)# data format
summary( airquality_ozone)# quick summary. missing values may be shown
 
 
str( airquality_nitrogen)# data format
summary( airquality_nitrogen)# quick summary. missing values may be shown

str( airquality_sulfur)# data format
summary( airquality_sulfur)# quick summary. missing values may be shown
  
#Ozone concentration by year
fit1 <- lm(airquality_ozone$Year~ ozone_concentration, data =  airquality_ozone)# model specification response ~ x1,..
ggplot(airquality_ozone , aes(x = Year, y = ozone_concentration, color = Concentration.ppm)) +
  ggtitle(" Ozone Concentration by Year") +
  geom_line() +
  geom_point()
 concentration<-ozone_concentration+nitrogen_concentration
 
 
#Nitrogen concentration by year 
fit1 <- lm(airquality_nitrogen$Year~ nitrogen_concentration, data =  airquality_nitrogen)# model specification response ~ x1,..
ggplot(airquality_nitrogen , aes(x = Year, y = nitrogen_concentration, color = Concentration.ppm)) +
  ggtitle(" Nitrogen Concentration by Year") +
  geom_line() +
  geom_point()
  
#Sulfur concentration by year
 fit1 <- lm(airquality_sulfur$Year~ sulfur_concentration, data =  airquality_sulfur)# model specification response ~ x1,..
 ggplot(airquality_sulfur , aes(x = Year, y = sulfur_concentration, color = Concentration.ppm)) +
  ggtitle(" Sulfur Concentration by Year") +
  geom_line() +
  geom_point()
 
 url <- "https://www.nei.org/resources/statistics/state-electricity-generation-fuel-shares"
 data1 <- read_html(url) %>% html_table()
 state_data <- data.frame(data1[1])
 state_data <- state_data %>% rename(
   state = State,
   Nuclear.pct = Nuclear....,
   Coal.pct = Coal....,
   NaturalGas.pct = Natural.Gas....,
   Petroleum.pct = Petroleum....,
   Hydro.pct = Hydro....,
   Geothermal.pct = Geothermal....,
   Solar.pct = Solar...PV....,
   Wind.pct = Wind....,
   Biomass.and.Other.pct = Biomass.and.Other....
 )
 state_data[state_data$Hydro.pct == "(0.2)", c("Hydro.pct")] <- 0.2
 state_data[state_data$Biomass.and.Other.pct == "(0.0)", c("Biomass.and.Other.pct")] <- 0.0
 state_data[state_data$state == "Iowa1", c("state")] <- "Iowa"
 state_data[state_data$state == "New York2", c("state")] <- "New York"
 state_data$Nuclear.pct <- as.numeric(state_data$Nuclear.pct)
 state_data$Coal.pct <- as.numeric(state_data$Coal.pct)
 state_data$NaturalGas.pct <- as.numeric(state_data$NaturalGas.pct)
 state_data$Petroleum.pct <- as.numeric(state_data$Petroleum.pct)
 state_data$Hydro.pct <- as.numeric(state_data$Hydro.pct)
 state_data$Geothermal.pct <- as.numeric(state_data$Geothermal.pct)
 state_data$Solar.pct <- as.numeric(state_data$Solar.pct)
 state_data$Wind.pct <- as.numeric(state_data$Wind.pct)
 state_data$Biomass.and.Other.pct <- as.numeric(state_data$Biomass.and.Other.pct)
 data_long <- state_data %>%
   tidyr::gather("type", "pct", -state)
 
 energy.aqi.join <- merge(mean.state.df[mean.state.df$Year == 2020, ], state_data, by = c("state"))
