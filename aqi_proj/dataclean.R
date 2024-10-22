library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)
library(usmap)
library(rvest)
library(gridExtra)

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

#Plotting of all 50 States vs Avg AQI
raw_data %>%
ggplot(aes(x = Year, y = mean.state, group=state,color = state)) +
  geom_line() +
  geom_point()+
  geom_smooth(method="lm", formula=y~x, se=F,color = "red")+
  facet_wrap(~state) +
  theme_bw() +
  theme(legend.position = 0) +
  ggtitle("`State`vs`Avg AQI")

#Pollutant trend graphs that affect air quality by year

 
#EDA
airquality_ozone<- read.csv("aqi_proj/data/OzoneNational.csv", header=T)
airquality_nitrogen<- read.csv("aqi_proj/data/Nitrogen_DioxideNational.csv", header=T)
airquality_sulfur<-read.csv("aqi_proj/data/Sulfur_DioxideNational.csv", header=T)
airquality_lead<-read.csv("aqi_proj/data/LeadNational.csv", header=T)
airquality_carbon<-read.csv("aqi_proj/data/Carbon_MonoxideNational.csv", header=T)
airquality_PM10<-read.csv("aqi_proj/data/PM10National.csv", header=T)
airquality_PM25<-read.csv("aqi_proj/data/PM25National.csv", header=T)

str( airquality_nitrogen)# data format
summary( airquality_nitrogen)# quick summary. missing values may be shown
airquality_nitrogen %>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("Nitrogen") +
  xlab("Year") +
  ggtitle("Nitrogen Concentration by year")

str( airquality_ozone)# data format
summary( airquality_ozone)# quick summary. missing values may be shown
 airquality_ozone %>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("Ozone") +
  xlab("Year") +
  ggtitle("Ozone Concentration by year")
 
str( airquality_sulfur)# data format
summary( airquality_sulfur)# quick summary. missing values may be shown
 airquality_sulfur %>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("Sulfur Dioxide") +
  xlab("Year") +
  ggtitle("Sulfur Dioxide Concentration by year")
 
str( airquality_lead)# data format
summary( airquality_lead)# quick summary. missing values may be shown
 airquality_lead %>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("Lead") +
  xlab("Year") +
  ggtitle("Lead Concentration by year")
 
str( airquality_carbon)# data format
summary( airquality_carbon)# quick summary. missing values may be shown
 airquality_carbon%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("Carbon Monoxide") +
  xlab("Year") +
  ggtitle("Carbon Monoxide Concentration by year")
 
str( airquality_PM10)# data format  
summary( airquality_PM10)# quick summary. missing values may be shown
 airquality_PM10%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("PM10") + 
  xlab("Year") +
  ggtitle("Particulate Matter 10 Concentration by year")
 
 str( airquality_PM25)# data format  
summary( airquality_PM25)# quick summary. missing values may be shown
 airquality_PM25%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("PM25") + 
  xlab("Year") +
  ggtitle("Particulate Matter 25 Concentration by year")

#Region wise Ozone Concentration
airquality_centralozone<-read.csv("aqi_proj/data/OzoneCentral.csv", header=T)
airquality_UpperMidwestozone<-read.csv("aqi_proj/data/OzoneUpperMidwest.csv", header=T)
airquality_Northeastozone<-read.csv("aqi_proj/data/OzoneNortheast.csv", header=T)
airquality_Northwestozone<-read.csv("aqi_proj/data/OzoneNorthwest.csv", header=T)
airquality_Southozone<-read.csv("aqi_proj/data/OzoneSouth.csv", header=T)
airquality_Southeastozone<-read.csv("aqi_proj/data/OzoneSoutheast.csv", header=T)
airquality_Southwestozone<-read.csv("aqi_proj/data/OzoneSouthwest.csv", header=T)
airquality_Westozone<-read.csv("aqi_proj/data/OzoneWest.csv", header=T)
airquality_NRPozone<-read.csv("aqi_proj/data/OzoneNorthernRockies.csv", header=T)
 
p1<-airquality_centralozone%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("Ozone") + 
  xlab("Year") +
  ggtitle("Ozone Central")
 
  p2 <- airquality_UpperMidwestozone%>% #upper midwest
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("Ozone") + 
  xlab("Year") +
  ggtitle("Ozone MW")
  
  p3 <- airquality_Northeastozone%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("Ozone") + 
  xlab("Year") +
  ggtitle("Ozone NE")
  
  p4 <- airquality_Northwestozone%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("Ozone") + 
  xlab("Year") +
  ggtitle("Ozone NW")
  
  p5 <- airquality_Southozone%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("Ozone") + 
  xlab("Year") +
  ggtitle("Ozone South")
  
  p6 <- airquality_Southeastozone%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("Ozone") + 
  xlab("Year") +
  ggtitle("Ozone SE")
    
  p7 <- airquality_Southwestozone%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("Ozone") + 
  xlab("Year") +
  ggtitle("Ozone SW")
     
  p8 <- airquality_Westozone%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("Ozone") + 
  xlab("Year") +
  ggtitle("Ozone West")
 
  p9 <- airquality_NRPozone%>% #Northern Rockies and Plains
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("Ozone") + 
  xlab("Year") +
  ggtitle("Ozone NRP") #North Rockie and Plains
 
grid.arrange(p1, p2, p3,p4,p5,p6,p7,p8,p9,  ncol=3)# facet the two plots  side by side

# Region wise Nitrogen Di oxide
airquality_centralnitrogen<-read.csv("aqi_proj/data/Nitrogen_DioxideCentral.csv", header=T)
airquality_UpperMidwestnitrogen<-read.csv("aqi_proj/data/Nitrogen_DioxideUpperMidwest.csv", header=T)
airquality_Northeastnitrogen<-read.csv("aqi_proj/data/Nitrogen_DioxideNortheast.csv", header=T)
airquality_Southnitrogen<-read.csv("aqi_proj/data/Nitrogen_DioxideSouth.csv", header=T)
airquality_Southeastnitrogen<-read.csv("aqi_proj/data/Nitrogen_DioxideSoutheast.csv", header=T)
airquality_Southwestnitrogen<-read.csv("aqi_proj/data/Nitrogen_DioxideSouthwest.csv", header=T)
airquality_Westnitrogen<-read.csv("aqi_proj/data/Nitrogen_DioxideWest.csv", header=T)
airquality_NRPnitrogen<-read.csv("aqi_proj/data/Nitrogen_DioxideNorthernRockies.csv", header=T)

p11<-airquality_centralnitrogen%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("NO2") + 
  xlab("Year") +
  ggtitle("NO2 Central")

p12 <- airquality_UpperMidwestnitrogen%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("NO2") + 
  xlab("Year") +
  ggtitle("NO2 MW")

p13 <- airquality_Northeastnitrogen%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("NO2") + 
  xlab("Year") +
  ggtitle("NO2 NE")

 

p14 <- airquality_Southnitrogen%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("NO2") + 
  xlab("Year") +
  ggtitle("NO2 South")

p15 <- airquality_Southeastnitrogen%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("NO2") + 
  xlab("Year") +
  ggtitle("NO2 SE")



p16 <- airquality_Southwestnitrogen%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("NO2") + 
  xlab("Year") +
  ggtitle("NO2 SW")

p17 <- airquality_Westnitrogen%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("NO2") + 
  xlab("Year") +
  ggtitle("NO2 West")

 

p18 <- airquality_NRPnitrogen%>% #Northern Rockies and Plains
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("NO2") + 
  xlab("Year") +
  ggtitle("NO2 NRP")

grid.arrange(p11, p12, p13,p14,p15,p16,p17,p18,  ncol=3)# facet the two plots  side by side

#r Regionwise Sulphurdioxide
airquality_centralsulfurdioxide<-read.csv("aqi_proj/data/Sulfur_DioxideCentral.csv", header=T)
airquality_UpperMidwestsulfurdioxide<-read.csv("aqi_proj/data/Sulfur_DioxideUpperMidwest.csv", header=T)
airquality_Northeastsulfurdioxide<-read.csv("aqi_proj/data/Sulfur_DioxideNortheast.csv", header=T)
airquality_Southsulfurdioxide<-read.csv("aqi_proj/data/Sulfur_DioxideSouth.csv", header=T)
airquality_Southeastsulfurdioxide<-read.csv("aqi_proj/data/Sulfur_DioxideSoutheast.csv", header=T)
airquality_Southwestsulfurdioxide<-read.csv("aqi_proj/data/Sulfur_DioxideSouthwest.csv", header=T)
airquality_Westsulfurdioxide<-read.csv("aqi_proj/data/Sulfur_DioxideWest.csv", header=T)
airquality_NRPsulfurdioxide<-read.csv("aqi_proj/data/Sulfur_DioxideNorthernRockies.csv", header=T)



p21<-airquality_centralsulfurdioxide%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("SO2") + 
  xlab("Year") +
  ggtitle("SO2 Central")

p22 <- airquality_UpperMidwestsulfurdioxide%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("SO2") + 
  xlab("Year") +
  ggtitle("SO2 MW")

p23 <- airquality_Northeastsulfurdioxide%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("SO2") + 
  xlab("Year") +
  ggtitle("SO2 NE")

 

p24 <- airquality_Southsulfurdioxide%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("SO2") + 
  xlab("Year") +
  ggtitle("SO2 South")

p25 <- airquality_Southeastsulfurdioxide%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("SO2") + 
  xlab("Year") +
  ggtitle("SO2 SE")



p26 <- airquality_Southwestsulfurdioxide%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("SO2") + 
  xlab("Year") +
  ggtitle("SO2 SW")

p27 <- airquality_Westsulfurdioxide%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("SO2") + 
  xlab("Year") +
  ggtitle("SO2 West")

 

p28 <- airquality_NRPsulfurdioxide%>% #Northern Rockies and Plains
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("SO2") + 
  xlab("Year") +
  ggtitle("SO2 NRP")

grid.arrange(p21, p22, p23,p24,p25,p26,p27,p28,  ncol=3)# facet the two plots  side by side

#Regionwise CO
airquality_centralcarbonmonoxide<-read.csv("aqi_proj/data/Carbon_MonoxideCentral.csv", header=T)
airquality_UpperMidwestcarbonmonoxide<-read.csv("aqi_proj/data/Carbon_MonoxideUpperMidwest.csv", header=T)
airquality_Northeastcarbonmonoxide<-read.csv("aqi_proj/data/Carbon_MonoxideNortheast.csv", header=T)
airquality_Northwestcarbonmonoxide<-read.csv("aqi_proj/data/Carbon_MonoxideNorthwest.csv", header=T)
airquality_Southcarbonmonoxide<-read.csv("aqi_proj/data/Carbon_MonoxideSouth.csv", header=T)
airquality_Southeastcarbonmonoxide<-read.csv("aqi_proj/data/Carbon_MonoxideSoutheast.csv", header=T)
airquality_Southwestcarbonmonoxide<-read.csv("aqi_proj/data/Carbon_MonoxideSouthwest.csv", header=T)
airquality_Westcarbonmonoxide<-read.csv("aqi_proj/data/Carbon_MonoxideWest.csv", header=T)
 

p41<-airquality_centralcarbonmonoxide%>% 
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("CO") + 
  xlab("Year") +
  ggtitle("CO Central")

p42 <- airquality_UpperMidwestcarbonmonoxide%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("CO") + 
  xlab("Year") +
  ggtitle("CO MW") #Midwest

p43 <- airquality_Northeastcarbonmonoxide%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("CO") + 
  xlab("Year") +
  ggtitle("CO NE")

p44 <- airquality_Northwestcarbonmonoxide%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("CO") + 
  xlab("Year") +
  ggtitle("CO NW")

p45 <- airquality_Southcarbonmonoxide%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("CO") + 
  xlab("Year") +
  ggtitle("CO South")

p46 <- airquality_Southeastcarbonmonoxide%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("CO") + 
  xlab("Year") +
  ggtitle("CO SE")

p47 <- airquality_Southwestcarbonmonoxide%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("CO") + 
  xlab("Year") +
  ggtitle("CO SW")

p48 <- airquality_Westcarbonmonoxide%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("CO") + 
  xlab("Year") +
  ggtitle("CO West")

grid.arrange(p41, p42, p43,p44,p45,p46,p47,p48, ncol=3)# facet the two plots  side by side

#Regionwise PM10
airquality_centralPM10<-read.csv("aqi_proj/data/PM10Central.csv", header=T)
airquality_UpperMidwestPM10<-read.csv("aqi_proj/data/PM10UpperMidwest.csv", header=T)
airquality_NortheastPM10<-read.csv("aqi_proj/data/PM10Northeast.csv", header=T)
airquality_NorthwestPM10<-read.csv("aqi_proj/data/PM10Northwest.csv", header=T)
airquality_SouthPM10<-read.csv("aqi_proj/data/PM10South.csv", header=T)
airquality_SoutheastPM10<-read.csv("aqi_proj/data/PM10Southeast.csv", header=T)
airquality_SouthwestPM10<-read.csv("aqi_proj/data/PM10Southwest.csv", header=T)
airquality_WestPM10<-read.csv("aqi_proj/data/PM10West.csv", header=T)
airquality_NRPPM10<-read.csv("aqi_proj/data/PM10NorthernRockies.csv", header=T)

p51<-airquality_centralPM10%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("PM10") + 
  xlab("Year") +
  ggtitle("PM10 Central")

p52 <- airquality_UpperMidwestPM10%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("PM10") + 
  xlab("Year") +
  ggtitle("PM10 MW")

p53 <- airquality_NortheastPM10%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("PM10") + 
  xlab("Year") +
  ggtitle("PM10 NE")

p54 <- airquality_NorthwestPM10%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("PM10") + 
  xlab("Year") +
  ggtitle("PM10 NW")

p55 <- airquality_SouthPM10%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("PM10") + 
  xlab("Year") +
  ggtitle("PM10 South")

p56 <- airquality_SoutheastPM10%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("PM10") + 
  xlab("Year") +
  ggtitle("PM10 SE")



p57 <- airquality_SouthwestPM10%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("PM10") + 
  xlab("Year") +
  ggtitle("PM10 SW")

p58 <- airquality_WestPM10%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("PM10") + 
  xlab("Year") +
  ggtitle("PM10 West")
 

p59 <- airquality_NRPPM10%>% #Northern Rockies and Plains
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey60") +
  geom_line(aes(y = Mean)) +
  ylab("PM10") + 
  xlab("Year") +
  ggtitle("PM10 NRP")

grid.arrange(p51, p52, p53,p54,p55,p56,p57,p58,p59, ncol=3)# facet the two plots  side by side

#Regionwise PM25
airquality_centralPM25<-read.csv("aqi_proj/data/PM25Central.csv", header=T)
airquality_UpperMidwestPM25<-read.csv("aqi_proj/data/PM25UpperMidwest.csv", header=T)
airquality_NortheastPM25<-read.csv("aqi_proj/data/PM25Northeast.csv", header=T)
airquality_NorthwestPM25<-read.csv("aqi_proj/data/PM25Northwest.csv", header=T)
airquality_SouthPM25<-read.csv("aqi_proj/data/PM25South.csv", header=T)
airquality_SoutheastPM25<-read.csv("aqi_proj/data/PM25Southeast.csv", header=T)
airquality_SouthwestPM25<-read.csv("aqi_proj/data/PM25Southwest.csv", header=T)
airquality_WestPM25<-read.csv("aqi_proj/data/PM25West.csv", header=T)
airquality_NRPPM25<-read.csv("aqi_proj/data/PM25NorthernRockies.csv", header=T)

p61<-airquality_centralPM25%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey70") +
  geom_line(aes(y = Mean)) +
  ylab("PM25") + 
  xlab("Year") +
  ggtitle("PM25 Central")

p62 <- airquality_UpperMidwestPM25%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey70") +
  geom_line(aes(y = Mean)) +
  ylab("PM25") + 
  xlab("Year") +
  ggtitle("PM25 MW")

p63 <- airquality_NortheastPM25%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey70") +
  geom_line(aes(y = Mean)) +
  ylab("PM25") + 
  xlab("Year") +
  ggtitle("PM25 NE")

p64 <- airquality_NorthwestPM25%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey70") +
  geom_line(aes(y = Mean)) +
  ylab("PM25") + 
  xlab("Year") +
  ggtitle("PM25 NW")

p65 <- airquality_SouthPM25%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey70") +
  geom_line(aes(y = Mean)) +
  ylab("PM25") + 
  xlab("Year") +
  ggtitle("PM25 South")

p66 <- airquality_SoutheastPM25%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey70") +
  geom_line(aes(y = Mean)) +
  ylab("PM25") + 
  xlab("Year") +
  ggtitle("PM25 SE")



p67 <- airquality_SouthwestPM25%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey70") +
  geom_line(aes(y = Mean)) +
  ylab("PM25") + 
  xlab("Year") +
  ggtitle("PM25 SW")

p68 <- airquality_WestPM25%>%
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey70") +
  geom_line(aes(y = Mean)) +
  ylab("PM25") + 
  xlab("Year") +
  ggtitle("PM25 West")
 
p69 <- airquality_NRPPM25%>% #Northern Rockies and Plains
  ggplot(aes(x=Year )) +
  geom_ribbon(aes(ymin = X10th.Percentile, ymax = X90th.Percentile), fill = "grey70") +
  geom_line(aes(y = Mean)) +
  ylab("PM25") + 
  xlab("Year") +
  ggtitle("PM25 NRP")

grid.arrange(p61, p62, p63,p64,p65,p66,p67,p68,p69, ncol=3)# facet the two plots  side by side

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
