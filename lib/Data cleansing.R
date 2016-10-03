# !!ATTENTINO!! Before running this code, please copy motor collision dataset into "data" folder#
#setwd("~/Documents/Courses/ADS/Project 2/Fall2016-Proj2-grp14")
library(data.table)
library(chron)

#read data #
###################################################################
collision.auto= fread("data/NYPD_Motor_Vehicle_Collisions.csv",select=c("DATE","TIME","LATITUDE", "LONGITUDE",
                  "NUMBER OF PERSONS INJURED","NUMBER OF PERSONS KILLED","NUMBER OF PEDESTRIANS INJURED", 
                  "NUMBER OF PEDESTRIANS KILLED","NUMBER OF CYCLIST INJURED","NUMBER OF CYCLIST KILLED",
                  "NUMBER OF MOTORIST INJURED", "NUMBER OF MOTORIST KILLED"))
collision.bike= fread("data/manhattan_bike_injury.csv",select=c("DATE","TIME","LATITUDE", "LONGITUDE",
                  "NUMBER OF PERSONS INJURED","NUMBER OF PERSONS KILLED","NUMBER OF PEDESTRIANS INJURED", 
                  "NUMBER OF PEDESTRIANS KILLED","NUMBER OF CYCLIST INJURED","NUMBER OF CYCLIST KILLED",
                  "NUMBER OF MOTORIST INJURED", "NUMBER OF MOTORIST KILLED"))
weather.df= data.frame(fread("data/weatherdata.csv", select= c("Date","Events")))
#names(collision.df.bike)
#hospital.df= fread("data/Health_and_Hospitals_Corporation__HHC__Facilities.csv")
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
#collision.test[1,2]
trans.date <- function(x){
  a=as.Date(as.character.Date(x),"%m/%d/%Y") 
  return(a)
}
collision.df= subset(collision.df, select=-TIME.2)
collision.df$DATE= trans.date(collision.df$DATE)
#ISOdate(year, month, day, hour = 12, min = 0, sec = 0, tz = "GMT")
collision.df$Weekend = is.weekend(collision.df$DATE)
collision.df$Holiday= is.holiday(collision.df$DATE)
#class(collision.df[1,2])
collision.df$TIME= as.factor(sapply(strsplit(collision.df$TIME,":"), "[[", 1) )

###################################
### Weather summary
weather.df$Date= as.Date(weather.df$Date)
collision.df = merge(collision.df, weather.df, by.x= "DATE", by.y= "Date",all.x= TRUE)
colnames(collision.df)[18]=c("Weather")


