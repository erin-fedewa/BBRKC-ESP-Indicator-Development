# notes ----
# Generate wind stress and chl-a indices for RKC larval stages   

  #Wind stress data (1987-2019) is a product of NOAA Blended Winds and MetOp ASCAP sensors 
      #and is from NOAA NCEI/NOAA NESDIS. 
  #The chlorophyll product (1997-2018) is from MODIS Aqua, VIIRS, MERIS, and SeaWiFS sensors 
      #and is from the Ocean Colour Program, European Space Agency.


# Erin Fedewa
# last updated: 2020/4/25

# load ----
library(tidyverse)

# data ----

wind <- read_csv("./Data/Blended_WindStress.csv")
head(wind)
chl <- read_csv("./Data/ChlA_Merged.csv")
head(chl)

# data mgmt ---

#Avg June wind stress indicator *********************************
wind %>%
  select(YEAR, MONTH, north_bering_wind, south_bering_wind) %>%
  filter(MONTH == 6) %>%
  group_by(YEAR) -> Jwind
  

#Plots 

#June EBS vrs NBS
Jwind %>%
  pivot_longer(c(3:4), names_to = "region", values_to = "windstress") %>%
  ggplot(aes(x = YEAR, y = windstress, group = as.factor(region))) +
    geom_point(aes(colour = region)) +
    geom_line(aes(colour = region))
  #Overall similar trends though more wind stress on average in EBS than NBS

#EBS May/June/July wind stress 
wind %>%
  select(YEAR, MONTH, south_bering_wind) %>%
  filter(MONTH %in% c(5,6,7)) %>%
  ggplot(aes(x = YEAR, y = south_bering_wind, group = as.factor(MONTH))) +
   geom_point(aes(colour = MONTH)) +
   geom_line(aes(colour = MONTH))
  #Very different! Worth looking into May vrs June vrs July for RKC larvae 


#Avg May chla indicator *********************************
chl %>%
  select(YEAR, MONTH, north_bering_chl, south_bering_chl) %>%
  filter(MONTH == 5) %>%
  group_by(YEAR) -> Mchl


#Plots 

#June EBS vrs NBS
Mchl %>%
  pivot_longer(c(3:4), names_to = "region", values_to = "chla") %>%
  ggplot(aes(x = YEAR, y = chla, group = as.factor(region))) +
  geom_point(aes(colour = region)) +
  geom_line(aes(colour = region))
#Trends similar since 2010

#EBS May/June/July chlA 
chl %>%
  select(YEAR, MONTH, south_bering_chl) %>%
  filter(MONTH %in% c(5,6,7)) %>%
  ggplot(aes(x = YEAR, y = south_bering_chl, group = as.factor(MONTH))) +
  geom_point(aes(colour = as.factor(MONTH))) +
  geom_line(aes(colour = as.factor(MONTH)))
#May seems to best capture spring bloom and June to some extent 

# combine datasets now and save output
bind_rows(Jwind, Mchl) %>%
  write_csv("./Output/satellite_timeseries.csv")
