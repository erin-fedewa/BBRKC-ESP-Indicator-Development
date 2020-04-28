# notes ----
  #Generate benthic pradator master csv
  #Summarize benthic predator and cod biomass across years

# Erin Fedewa
# last updated: 2020/4/24

# load ----
library(tidyverse)
library(mgcv)


# data ----

#Add all bottom trawl data 
ebs82 <- read_csv("./Data/ebs1982_1984.csv")
ebs85 <- read_csv("./Data/ebs1985_1989.csv")
ebs90 <- read_csv("./Data/ebs1990_1994.csv")
ebs95 <- read_csv("./Data/ebs1995_1999.csv")
ebs00 <- read_csv("./Data/ebs2000_2004.csv")
ebs05 <- read_csv("./Data/ebs2005_2008.csv")
ebs09 <- read_csv("./Data/ebs2009_2012.csv")
ebs13 <- read_csv("./Data/ebs2013_2016.csv")
ebs17 <- read_csv("./Data/ebs2017_2018.csv")
ebs19 <- read_csv("./Data/ebs2019.csv")

#data mgmt ----

#Add benthic predator species guild source code
ben <- read_csv("./Data/ForagingGuildsSource_SID.csv")
ben %>% 
  pull(Benthic_predator)%>%
  na.omit() -> benpred

#Add BB stations 
sta<-read_csv("./Data/stock_stations.csv")
sta %>% 
  pull(BBRKC) %>%
  na.omit() -> BBonly

#Filter for only benthic predators in each dataset
  #Note: YFS not included, which make up lg portion of benthic biomass in BB

ebs82 %>%
  filter(SID %in% benpred) ->ebs82
ebs85 %>%
  filter(SID %in% benpred) ->ebs85
ebs90 %>%
  filter(SID %in% benpred) ->ebs90
ebs95 %>%
  filter(SID %in% benpred) ->ebs95
ebs00 %>%
  filter(SID %in% benpred) ->ebs00
ebs05 %>%
  filter(SID %in% benpred) ->ebs05
ebs09 %>%
  filter(SID %in% benpred) ->ebs09
ebs13 %>%
  filter(SID %in% benpred) ->ebs13
ebs17 %>%
  filter(SID %in% benpred) ->ebs17
ebs19 %>%
  filter(SID %in% benpred) ->ebs19

# combine datasets now and save output

bind_rows(ebs82, ebs85, ebs90, ebs95, ebs00, ebs05, ebs09, ebs13, ebs17, ebs19) %>%
  write_csv("./Output/pred_timeseries.csv")

#upload new csv 
pred <- read_csv("./Output/pred_timeseries.csv")
head(pred)

#Sum biomass for each species guild across years 
  #Note:specifying each species here because stomach contents/diets were validated for most
    #and included if assumed to be benthic predator on crab juv/adult stages (YFS not!)
pred %>%
  filter(STATION %in% BBonly) %>% #Only include BB stations 
  mutate(thoustons = ((WTCPUE*100*1371.9616)/1000000)) %>% #Convert WTCPUE in kg/HA to thoustons/km^2
  group_by(YEAR) %>%
  summarise(Cod_Sab_Hal = sum(thoustons[SID %in% c(21720, 21722, 20510, 10120)], na.rm = T),
            Skates = sum(thoustons[SID %in% c(420,435,440,455,471,472,480,460,485)], na.rm = T),
            Flatfish = sum(thoustons[SID %in% c(10220,10115,10130,10140)], na.rm = T),
            Sculpin = sum(thoustons[SID %in% c(21347,21348,21368,21370,21388,21420,21311,21315,21390,21438,21371)], na.rm = T),
            Eelpout = sum(thoustons[SID %in% c(24184, 24191, 24185)], na.rm = T),  
            Wolffish = sum(thoustons[SID %in% c(20320, 20322)], na.rm = T), 
            Octopus = sum(thoustons[SID %in% c(78010, 78012, 78403)], na.rm = T),
            Total_Pred = sum(thoustons[SID %in% benpred], na.rm = T)) -> BBpred_timeseries
            
write.csv(BBpred_timeseries, file = "./Output/BBpred_timeseries.csv")

#Plots 
BBpred_timeseries %>%
  pivot_longer(c(2:8), names_to = "pred_guild", values_to = "thoustons") %>%
  ggplot(aes(x = YEAR, y = thoustons, group = factor(pred_guild)))+
  geom_point(aes(colour = pred_guild)) +
  geom_line(aes(colour = pred_guild)) +
  labs(y = "Benthic Predator Biomass (1000 t)", x = "") +
  theme_bw()+
  theme(panel.grid = element_blank()) 

BBpred_timeseries %>%
  ggplot(aes(x = YEAR, y = Total_Pred)) +
  geom_point() +
  geom_smooth(method = gam, formula = y~s(x))+
  labs(y = "Benthic Predator Biomass (1000 t)", x = "") +
  theme_bw()+
  theme(panel.grid = element_blank()) 
            
#Now lets just look at cod in EBS vrs BB
  #maybe worth getting biometric data for cod to filter for certain size? 

#EBS
pred %>%
  mutate(thoustons = ((WTCPUE*100*1371.9616)/1000000)) %>% #Convert WTCPUE in kg/HA to thoustons/km^2
  group_by(YEAR) %>%
  summarise(Cod = sum(thoustons[SID == 21720], na.rm = T)) -> EBScod

#Plot
EBScod %>%
  ggplot(aes(x = YEAR, y = Cod)) +
  geom_point() +
  geom_smooth(method = gam, formula = y~s(x))+
  labs(y = "EBS Pacific Cod Biomass (1000 t)", x = "") +
  theme_bw()+
  theme(panel.grid = element_blank()) 

            
#BB only 
pred %>%
  filter(STATION %in% BBonly) %>% #Only include BB stations 
  mutate(thoustons = ((WTCPUE*100*1371.9616)/1000000)) %>% #Convert WTCPUE in kg/HA to thoustons/km^2
  group_by(YEAR) %>%
  summarise(Cod = sum(thoustons[SID == 21720], na.rm = T)) -> BBcod

#Plot
BBcod %>%
  ggplot(aes(x = YEAR, y = Cod)) +
  geom_point() +
  geom_smooth(method = gam, formula = y~s(x))+
  labs(y = "Bristol Bay Pacific Cod Biomass (1000 t)", x = "") +
  theme_bw()+
  theme(panel.grid = element_blank()) 

#Cod biomass trends in BB seem to reflect the EBS as a whole. Let's use BB cod biomass as 
    #an indicator assuming this is only a summer/short term snapshot 
    #Would be worth filtering cod by size or developing a spatial overlap index

BBcod %>%
  print(n=38)

