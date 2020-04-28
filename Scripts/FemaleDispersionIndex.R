setwd("C:/Users/erin.fedewa/Work/Eco Considerations")
dat<-read.csv("CRAB_CPUE_FOR_WEB.csv")
attach(dat)
dat2<-data.frame(LATITUDE, LONGITUDE, SURVEY_YEAR, STATION_ID, COMMON_NAME, CPUE, MATURITY_NAME)
wtlat<-LATITUDE*CPUE
wtlong<-LONGITUDE*CPUE
dat3<-cbind(dat2, wtlat, wtlong)

BBonly<-subset(dat3, COMMON_NAME=="red king crab" & MATURITY_NAME=="Mature female" & STATION_ID%in%c("A-02", "A-03", "A-04", "A-05",
                                                                                                     "A-06",
                                                                                                     "B-01",
                                                                                                     "B-02",
                                                                                                     "B-03",
                                                                                                     "B-04",
                                                                                                     "B-05",
                                                                                                     "B-06",
                                                                                                     "B-07",
                                                                                                     "B-08",
                                                                                                     "C-01",
                                                                                                     "C-02",
                                                                                                     "C-03",
                                                                                                     "C-04",
                                                                                                     "C-05",
                                                                                                     "C-06",
                                                                                                     "C-07",
                                                                                                     "C-08",
                                                                                                     "C-09",
                                                                                                     "D-01",
                                                                                                     "D-02",
                                                                                                     "D-03",
                                                                                                     "D-04",
                                                                                                     "D-05",
                                                                                                     "D-06",
                                                                                                     "D-07",
                                                                                                     "D-08",
                                                                                                     "D-09",
                                                                                                     "D-10",
                                                                                                     "E-01",
                                                                                                     "E-02",
                                                                                                     "E-03",
                                                                                                     "E-04",
                                                                                                     "E-05",
                                                                                                     "E-06",
                                                                                                     "E-07",
                                                                                                     "E-08",
                                                                                                     "E-09",
                                                                                                     "E-10",
                                                                                                     "E-11",
                                                                                                     "E-12",
                                                                                                     "F-01",
                                                                                                     "F-02",
                                                                                                     "F-03",
                                                                                                     "F-04",
                                                                                                     "F-05",
                                                                                                     "F-06",
                                                                                                     "F-07",
                                                                                                     "F-08",
                                                                                                     "F-09",
                                                                                                     "F-10",
                                                                                                     "F-11",
                                                                                                     "F-12",
                                                                                                     "F-13",
                                                                                                     "F-14",
                                                                                                     "G-01",
                                                                                                     "G-02",
                                                                                                     "G-03",
                                                                                                     "G-04",
                                                                                                     "G-05",
                                                                                                     "G-06",
                                                                                                     "G-07",
                                                                                                     "G-08",
                                                                                                     "G-09",
                                                                                                     "G-10",
                                                                                                     "G-11",
                                                                                                     "G-12",
                                                                                                     "G-13",
                                                                                                     "G-14",
                                                                                                     "G-15",
                                                                                                     "H-01",
                                                                                                     "H-02",
                                                                                                     "H-03",
                                                                                                     "H-04",
                                                                                                     "H-05",
                                                                                                     "H-06",
                                                                                                     "H-07",
                                                                                                     "H-08",
                                                                                                     "H-09",
                                                                                                     "H-10",
                                                                                                     "H-11",
                                                                                                     "H-12",
                                                                                                     "H-13",
                                                                                                     "H-14",
                                                                                                     "H-15",
                                                                                                     "H-16",
                                                                                                     "I-01",
                                                                                                     "I-02",
                                                                                                     "I-03",
                                                                                                     "I-04",
                                                                                                     "I-05",
                                                                                                     "I-06",
                                                                                                     "I-07",
                                                                                                     "I-08",
                                                                                                     "I-09",
                                                                                                     "I-10",
                                                                                                     "I-11",
                                                                                                     "I-12",
                                                                                                     "I-13",
                                                                                                     "I-14",
                                                                                                     "I-15",
                                                                                                     "I-16",
                                                                                                     "J-01",
                                                                                                     "J-02",
                                                                                                     "J-03",
                                                                                                     "J-04",
                                                                                                     "J-05",
                                                                                                     "J-06",
                                                                                                     "J-07",
                                                                                                     "J-08",
                                                                                                     "J-09",
                                                                                                     "J-10",
                                                                                                     "J-11",
                                                                                                     "J-12",
                                                                                                     "J-13",
                                                                                                     "J-14",
                                                                                                     "J-15",
                                                                                                     "J-16",
                                                                                                     "K-01",
                                                                                                     "K-02",
                                                                                                     "K-03",
                                                                                                     "K-04",
                                                                                                     "K-05",
                                                                                                     "K-06",
                                                                                                     "K-07",
                                                                                                     "K-08",
                                                                                                     "K-09",
                                                                                                     "K-10",
                                                                                                     "K-11",
                                                                                                     "K-12",
                                                                                                     "K-13",
                                                                                                     "K-14",
                                                                                                     "Z-05"
))

sumCPUE<-aggregate(CPUE ~ SURVEY_YEAR, data=BBonly, FUN=sum)
Swtlat<-aggregate(wtlat ~ SURVEY_YEAR, data=BBonly, FUN=sum)
WTlat<-Swtlat/sumCPUE
Swtlong<-aggregate(wtlong ~ SURVEY_YEAR, data=BBonly, FUN=sum)
WTlong<-Swtlong/sumCPUE


setwd("//Nmfs/akc-kod/Survey/EBS Shelf/2015/TechMemo/PopulationByMaleSizeGroupAndFemaleMaturity/csv files")
fem<-read.csv("RK_BB_LEG1MALES_LEG3FEMALES_ABUNDANCE_CSV.csv")
attach(fem)
femA<-data.frame(SURVEY_YEAR, NUM_FEMALE_MATURE) 
MF<-femA$NUM_FEMALE_MATURE
cbind(femA, WTlat, WTlong)

lat1<-BBonly$wtlat
long1<-BBonly$wtlong
lat2<-WTlat
long2<-WTlong



library(geosphere)
distm (c(lat1, lon1), c(lat2, Lon2), fun = distHaversine)


# Calculate distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

geodetic.distance <- function(point1, point2) 
{ 
  R <- 6371 
  p1rad <- point1 * pi/180 
  p2rad <- point2 * pi/180 
  d <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))	
  d <- acos(d) 
  R*d 
} 




coldpool<-ifelse(BBonly$GEAR_TEMPERATURE<=2, 1, 0)
dat3<-cbind(BBonly, coldpool)

##################################one way#############################
###this way seems to count NA values when determing the number of rows which leads to erroneous calculations##Be careful###### 


pres<-subset(dat3, SURVEY_YEAR=="1977")
a<-sum(pres$coldpool, na.rm=TRUE)
b<-nrow(pres, na.rm=TRUE)
prop<-a/b
prop


#####################Better way for cold pool proportion##################################

year<-levels(factor(dat3$SURVEY_YEAR))

cold<-aggregate(coldpool ~ SURVEY_YEAR, data=dat3, FUN=sum)

tot<-aggregate(coldpool ~ SURVEY_YEAR, data=dat3, FUN=NROW)
rep.value<-cold$coldpool/tot$coldpool
rep.value

H<-data.frame(cbind(year, rep.value))

#################ave temp and SE and CI################
ave<-data.frame(aggregate(GEAR_TEMPERATURE ~ SURVEY_YEAR, data=dat3, FUN=mean)) ###ave temp by year###

se <- function(x) sqrt(var(x)/length(x))###both functions give st error
std <- function(x) sd(x)/sqrt(length(x))
SE<-aggregate(GEAR_TEMPERATURE ~ SURVEY_YEAR, data=dat3, FUN=se) 
Serror<-SE$GEAR_TEMPERATURE
CI<-SE$GEAR_TEMPERATURE*1.96
cbind(ave, Serror, CI)
#########################END################

##############JUNK CODE#########################

levels(dat3$SURVEY_YEAR)
factor(dat3$SURVEY_YEAR)
for (i in factor((dat3$SURVEY_YEAR))
     {  
       
       
       
       
       df_list <- split(dat3, as.factor(dat3$SURVEY_YEAR))
       sapply(df_list, nrow)
       sapply(df_list, function(x){sum(coldpool, na.rm=TRUE)})
       
       P<-function(dat){
         a<-sum(coldpool, na.rm=TRUE)
         b<-nrow(coldpool)
         prop<-a/b
         prop
         return(prop)
       }
       
       
       
       c<-sum(coldpool, na.rm=TRUE)
       
       <-sum(coldpool, na.rm=TRUE)
       
       
       
       
       calcCP<-function(dat){
         cold<-sum(coldpool, na.rm=TRUE)
         return(cold)
       }
       c<-sum(coldpool, na.rm=TRUE)
       
       
       calcCP<-function(dat){
         prop<-sheader(dat2)
         um(coldpool)
         return(prop)
       }
       
       calcCP(dat2)
       
       
       
       
       BBonly$coldpool<-ifelse(BBonly$GEAR_TEMPERATURE<=2, 1, 0)
       
       aggregate(dat2, by=list(SURVEY_YEAR, coldpool), FUN=sum, na.rm=TRUE)
       
       
       year<-factor(SURVEY_YEAR)
       
       sum(GEAR_TEMPERATURE <= 2)
       
       tapply(dat2$GEAR_TEMPERATURE, dat2$SURVEY_YEAR, GEAR_TEMPERATURE <= 2)
       
       
       aggdat<-aggregate(dat2, by=year, FUN=sum, na.rm=TRUE)
       
       for (i in 1:length(GEAR_TEMPERATURE)){
         
         
         print(paste(sum(GEAR_TEMPERATURE < 2)))
       }
       