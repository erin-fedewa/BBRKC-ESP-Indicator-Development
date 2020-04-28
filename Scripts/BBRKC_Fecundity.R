# notes ----
# Examine fecundity and mean egg size of BBRKC ovigerous females  
# Erin Fedewa
# last updated: 2020/4/22

# load ----
library(tidyverse)

# data ----

data <- read_csv("./Data/BBRKC_fecundity.csv")
head(data)
str(data)

#BBRKC Egg Weight  ----

#Plot
data %>%
  group_by(Year) %>%
  summarise(MEW = mean ( `indv_egg wt_g`)*1000,
            SD_EW = sd( `indv_egg wt_g`)*1000 ) %>%
  ggplot(aes(Year, MEW)) +
    geom_bar(stat = "identity") +
   theme_bw() +
    ylab("Mean individual egg weight (mg)") +
   geom_errorbar(aes(ymin = MEW - SD_EW, ymax = MEW + SD_EW), width=.2,
                position=position_dodge(.9))

#ANOVA
m1 <- aov( `indv_egg wt_g` ~ as.factor(Year), data = data)
  summary(m1)
  TukeyHSD(m1)  
  