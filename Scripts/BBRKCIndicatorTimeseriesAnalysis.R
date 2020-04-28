# notes ----
# Assess collinearity b/w BBRKC indicators 
# Assess appropriate lags b/w indicators and response (Legal male biomass)


# Erin Fedewa
# last updated: 2020/4/22

# load ----
library(tidyverse)
library(corrplot)

# data ----

dat <- read_csv("./Data/BBRKCindicators.csv")
head(dat)

# data mgmt ----

#Assess normality 
df.shapiro <- apply(dat[,-1], 2, shapiro.test)
  df.shapiro
#Benthic invert,CP, Legal Bio, Salmon, Catch Area, Catchdist NOT normal  
plot <- apply(dat[,-1], 2, function(x) {hist(x = x)})
  plot
#Try log transformation
log <- apply(dat[,-1], 2, function(x) {hist(x = log(x))})
  log #Looks better for biomass indicators 
#4th root transformation
4root <- apply(dat[,-1], 2, function(x) {hist(x = x^.25)})
  4root 
  
#Assess collinearity b/w indicators (using only subset of indicators with consistent temporal scale)
dat %>% 
  select(PCOD_BIO, BENINVERT_BIO, BOT_TEMP, RKC_MALERECRUIT_BIO, RKC_MALELEGAL_BIO, PDO, WIND_STRESS, CHL_A) %>% 
  cor(use = "complete.obs") %>%
  corrplot(method="number")
  #Pcod: benthic invert & wind stress > 0.06
  #Bottom temp: PDO > 0.06

#Cross-correlation for lags
ccf(dat$PCOD_BIO, dat$RKC_MALELEGAL_BIO, lag.max = 7, na.action=na.pass) #Max lag for hatch year (though check on this...) 
  #Max lag at 0
ccf(dat$BENINVERT_BIO, dat$RKC_MALELEGAL_BIO, lag.max = 7, na.action=na.pass)
  #Max lags at 0/5
ccf(dat$BOT_TEMP, dat$RKC_MALELEGAL_BIO, lag.max = 7, na.action=na.pass)
  #Max lag at 6
ccf(dat$RKC_MALERECRUIT_BIO, dat$RKC_MALELEGAL_BIO, lag.max = 7, na.action=na.pass)
  #Max lag at 0
ccf(dat$PDO, dat$RKC_MALELEGAL_BIO, lag.max = 7, na.action=na.pass)
  #Max lags at 7/-2
ccf(dat$WIND_STRESS, dat$RKC_MALELEGAL_BIO, lag.max = 7, na.action=na.pass)
  #Max lags at -1/7
ccf(dat$CHL_A, dat$RKC_MALELEGAL_BIO, lag.max = 7, na.action=na.pass)
  #Max lags at 5/6

#E.g. RKC legal male biomass decreases (increases) 3-5 years BEFORE increases in bottom temp (chlA)
  #(i.e. max correlation at positive lags) - Doesn't make much sense biologically!

#Look into calculating 2-3 yr averages for some of these indicators i.e. PDO 

#BBRKC indicators_BAS.csv created for Bayesian Adaptive Sampling- 
  #though still issues with pairwise correlations and normality
  #for now let's pick lags that make biological sense:
  #BBRKC Pre Recruit Biomass lag of -1
  #PDO lag of -2
  #Would expect to lag wind stress/chla by -6 but let's keep as is for now 
  

