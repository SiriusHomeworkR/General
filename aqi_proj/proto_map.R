library(dplyr)
library(tidyverse)
install.packages('geofacet')
library(geofacet)
library(ggplot2)
install.packages('rvest')
library(rvest)

url <- "https://www.nei.org/resources/statistics/state-electricity-generation-fuel-shares"
data1 <- read_html(url) %>% html_table()
state_data <- data.frame(data1[1])
state_data <- state_data %>% rename(
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
state_data[state_data$State == "Iowa1", c("State")] <- "Iowa"
state_data[state_data$State == "New York2", c("State")] <- "New York"
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
  tidyr::gather("type", "pct", -State)

ggplot(data_long, aes(y = pct, x = type, fill = type)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  facet_geo(~ State, label = " ", move_axes = FALSE)