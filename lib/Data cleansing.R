# !!ATTENTINO!! Before running this code, please copy motor collision dataset into "data" folder#
setwd("~/Documents/Courses/ADS/Project 2/Fall2016-Proj2-grp14")
library(data.table)
library(chron)
library(timeDate)

#test.df= read.csv("data/NYPD_Motor_Vehicle_Collisions.csv")
#test2.df=read.csv("data/manhattan_bike_injury.csv")
#read data #
###################################################################
collision.auto= fread("data/NYPD_Motor_Vehicle_Collisions.csv",select=c("DATE","TIME","LATITUDE", "LONGITUDE",
                  "NUMBER OF PERSONS INJURED","NUMBER OF PERSONS KILLED","NUMBER OF PEDESTRIANS INJURED", 
                  "NUMBER OF PEDESTRIANS KILLED","NUMBER OF CYCLIST INJURED","NUMBER OF CYCLIST KILLED",
                  "NUMBER OF MOTORIST INJURED", "NUMBER OF MOTORIST KILLED","CONTRIBUTING FACTOR VEHICLE 1",
                  "VEHICLE TYPE CODE 1", "VEHICLE TYPE CODE 2", "VEHICLE TYPE CODE 3"))
collision.bike= fread("data/manhattan_bike_injury.csv",select=c("DATE","TIME","LATITUDE", "LONGITUDE",
                  "NUMBER OF PERSONS INJURED","NUMBER OF PERSONS KILLED","NUMBER OF PEDESTRIANS INJURED", 
                  "NUMBER OF PEDESTRIANS KILLED","NUMBER OF CYCLIST INJURED","NUMBER OF CYCLIST KILLED",
                  "NUMBER OF MOTORIST INJURED", "NUMBER OF MOTORIST KILLED","CONTRIBUTING FACTOR VEHICLE 1",
                  "VEHICLE TYPE CODE 1", "VEHICLE TYPE CODE 2", "VEHICLE TYPE CODE 3"))
weather.df= data.frame(fread("data/weatherdata.csv", select= c("Date","Events")))
#names(collision.df.bike)
collision.df<- data.frame(rbind(collision.auto, collision.bike))
collision.df= collision.df[!is.na(collision.df$LATITUDE) & !is.na(collision.df$LONGITUDE),]

###################################################################
#### Injury summary
#collision.test <- collision.df[290:300,]
collision.df$People.Injured = (collision.df$NUMBER.OF.PERSONS.INJURED !=0)
collision.df$People.Killed = (collision.df$NUMBER.OF.PERSONS.KILLED !=0)
collision.df$People.Injured.or.Killed= (collision.df$NUMBER.OF.PERSONS.INJURED !=0 |collision.df$NUMBER.OF.PERSONS.KILLED !=0 )
#dim(collision.test)

###################################################################
### date/time summary
trans.date <- function(x){
  a=as.Date(as.character.Date(x),"%m/%d/%Y") 
  return(a)
}
#collision.df= subset(collision.df, select=-TIME.2)
collision.df$DATE= trans.date(collision.df$DATE)
collision.df$Weekend = is.weekend(collision.df$DATE)

holiday_list <- c("USChristmasDay","USGoodFriday","USIndependenceDay","USLaborDay", "USNewYearsDay",
           "USThanksgivingDay", "USMemorialDay", "USColumbusDay", "USVeteransDay")        
myholidays  <- dates(as.character(holiday(2012:2016,holiday_list)),format="Y-M-D")
#is.holiday(as.Date("2015-11-26"),myholidays)
collision.df$Holiday= is.holiday(collision.df$DATE,myholidays)
#class(collision.df[1,2])
collision.df$TIME= as.factor(sapply(strsplit(collision.df$TIME,":"), "[[", 1) )

###################################
### Weather summary
weather.df$Date= as.Date(weather.df$Date)
collision.df = merge(collision.df, weather.df, by.x= "DATE", by.y= "Date",all.x= TRUE)
colnames(collision.df)[colnames(collision.df) == "Events"] =c("Weather")

write.csv(collision.df, file="data/collision_dateframe.csv")

