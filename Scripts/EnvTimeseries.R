# notes ----
# Generate avg bottom temp, PDO and cold pool extent indices  
#PDO time series estimates determined via PCA using SST (analysis done by Mike Litzow)

# Erin Fedewa
# last updated: 2020/4/22

# load ----
library(tidyverse)

# data ----

temp <- read_csv("./Data/haul_newtimeseries_ebs.csv")
head(temp)
sta<-read_csv("./Data/stock_stations.csv")
head(sta)
pdo<- read_csv("./Data/PDO_timeseries.csv")
head(pdo)

# data mgmt ---

#BBRKC stations only
sta %>% 
  pull(BBRKC) %>%
  na.omit() -> BBonly

# Timeseries ----

# compute mean bottom temperature
temp %>%
  filter(STATIONID %in% BBonly) %>%
  group_by(SURVEY_YEAR) %>%
  summarise(MEAN_BT = mean(GEAR_TEMPERATURE, na.rm = T))-> avg_bt


# compute cold pool areal extent
temp %>%
  filter(STATIONID %in% BBonly) %>%
  group_by(SURVEY_YEAR) %>%
  summarise(count = sum(GEAR_TEMPERATURE < 2, na.rm = T)) %>%
  mutate(CPA = count * 401) %>%
  select(-count) -> cpa

# compute mean winter PDO 
  #Nov/Dec of prior year included in next consecutive year-disregard 1979 and 2020  
  pdo %>% 
    filter(Year >= 1979,
           Month %in% c("Nov", "Dec", "Jan", "Feb", "Mar")) %>% 
    mutate(pdo_yr = ifelse(Month %in% c("Nov", "Dec"), Year + 1, Year)) %>% 
    group_by(pdo_yr) %>%
    rename(., SURVEY_YEAR = pdo_yr) %>%
    summarize(Mean_PDO = mean(est_PDO)) -> mean_winterPDO


# combine indices and save output
avg_bt %>%
  full_join(cpa, by = "SURVEY_YEAR") %>%
  full_join(mean_winterPDO, by = "SURVEY_YEAR") %>%
  write_csv("./Output/environmental_timeseries.csv")







