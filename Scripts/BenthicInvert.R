# notes ----
# Generate benthic invert master csv
#Summarize benthic invert biomass across years in Bristol Bay 

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
  #Filter for only benthic inverts in each dataset

ebs82 %>%
  filter(SID %in% c(41201:99909)) ->ebs82
ebs85 %>%
  filter(SID %in% c(41201:99909)) ->ebs85
ebs90 %>%
  filter(SID %in% c(41201:99909)) ->ebs90
ebs95 %>%
  filter(SID %in% c(41201:99909)) ->ebs95
ebs00 %>%
  filter(SID %in% c(41201:99909)) ->ebs00
ebs05 %>%
  filter(SID %in% c(41201:99909)) ->ebs05
ebs09 %>%
  filter(SID %in% c(41201:99909)) ->ebs09
ebs13 %>%
  filter(SID %in% c(41201:99909)) ->ebs13
ebs17 %>%
  filter(SID %in% c(41201:99909)) ->ebs17
ebs19 %>%
  filter(SID %in% c(41201:99909)) ->ebs19

# combine datasets now and save output

bind_rows(ebs82, ebs85, ebs90, ebs95, ebs00, ebs05, ebs09, ebs13, ebs17, ebs19) %>%
write_csv("./Output/benthic_timeseries.csv")

#upload new csv and stock stations 
benthic <- read_csv("./Output/benthic_timeseries.csv")
head(benthic)
sta<-read_csv("./Data/stock_stations.csv")
head(sta)

#BBRKC stations only
sta %>% 
  pull(BBRKC) %>%
  na.omit() -> BBonly

#Create species guilds and sum biomass for each guild across years 
benthic %>%
  filter(STATION %in% BBonly, #Only include BB stations 
         !(SID %in% c(68560, 68580, 69322, 69323))) %>% #remove commercial crab species 
  mutate(thoustons = ((WTCPUE*100*1371.9616)/1000000)) %>% #Convert WTCPUE in kg/HA to thoustons/km^2
  group_by(YEAR) %>%
  summarise(Gersemia = sum(thoustons[SID %in% c(41201:41221)], na.rm = T), #Benthic guilds 
            Pennatulacea = sum(thoustons[SID %in% c(42000:42999)], na.rm = T),
            Actinaria = sum(thoustons[SID %in% c(43000:43999)], na.rm = T),
            Polychaeta = sum(thoustons[SID %in% c(50000:59099)], na.rm = T),
            Barnacles = sum(thoustons[SID %in% c(65100:65211)], na.rm = T),
            Shrimps = sum(thoustons[SID %in% c(66000:66912)], na.rm = T),
            Crabs = sum(thoustons[SID %in% c(68000:69599)], na.rm = T),
            Gastropods = sum(thoustons[SID %in% c(71000:73999)], na.rm = T),
            Bivalves = sum(thoustons[SID %in% c(74000:75799)], na.rm = T),
            Asteroidea = sum(thoustons[SID %in% c(80000:82499)], na.rm = T),
            Echinoidea = sum(thoustons[SID %in% c(82500:82729)], na.rm = T),
            Ophiuroidea = sum(thoustons[SID %in% c(83000:84999)], na.rm = T),
            Holothuroidea = sum(thoustons[SID %in% c(85000:85999)], na.rm = T),
            Porifera = sum(thoustons[SID %in% c(91000:91999)], na.rm = T),
            Bryozoans = sum(thoustons[SID %in% c(95000:95499)], na.rm = T),
            Ascidians = sum(thoustons[SID %in% c(98000:99909)], na.rm = T),
            Total_Benthic = sum(thoustons[SID %in% c(41201:99909)], na.rm = T)) -> BBbenthic_timeseries
        
write.csv(BBbenthic_timeseries, file = "./Output/BBbenthic_timeseries.csv")
 
#Plots 
BBbenthic_timeseries %>%
  pivot_longer(c(2:17), names_to = "benthic_guild", values_to = "thoustons") %>%
  ggplot(aes(x = YEAR, y = thoustons, group = factor(benthic_guild)))+
    geom_point(aes(colour = benthic_guild)) +
    geom_line(aes(colour = benthic_guild)) +
   # geom_hline(aes(yintercept = mean(thoustons)), linetype = 2)+
    labs(y = "Benthic Invert Biomass (1000 t)", x = "") +
    theme_bw()+
    theme(panel.grid = element_blank()) 

BBbenthic_timeseries %>%
  ggplot(aes(x = YEAR, y = Total_Benthic)) +
  geom_point() +
  geom_smooth(method = gam, formula = y~s(x))+
  labs(y = "Benthic Invert Biomass (1000 t)", x = "") +
  theme_bw()+
  theme(panel.grid = element_blank()) 

#Using total_benthic biomass as benthic indicator, though this is dominated by sea stars in BB
    #Index may represent both prey and competitors- benthic production as a whole?


