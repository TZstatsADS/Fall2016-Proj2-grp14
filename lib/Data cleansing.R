setwd("~/Documents/Courses/ADS/Project 2/Fall2016-Proj2-grp14")
library(data.table)
collision.df.auto= fread("data/NYPD_Motor_Vehicle_Collisions.csv",select=c("DATE","TIME","LATITUDE", "LONGITUDE",
                  "NUMBER OF PERSONS INJURED","NUMBER OF PERSONS KILLED","NUMBER OF PEDESTRIANS INJURED", 
                  "NUMBER OF PEDESTRIANS KILLED","NUMBER OF CYCLIST INJURED","NUMBER OF CYCLIST KILLED",
                  "NUMBER OF MOTORIST INJURED", "NUMBER OF MOTORIST KILLED","VEHICLE TYPE CODE 1",
                  "VEHICLE TYPE CODE 2", "VEHICLE TYPE CODE 3"))
collision.df.bike= fread("data/manhattan_bike_injury.csv",select=c("DATE","TIME","LATITUDE", "LONGITUDE",
                  "NUMBER OF PERSONS INJURED","NUMBER OF PERSONS KILLED","NUMBER OF PEDESTRIANS INJURED", 
                  "NUMBER OF PEDESTRIANS KILLED","NUMBER OF CYCLIST INJURED","NUMBER OF CYCLIST KILLED",
                  "NUMBER OF MOTORIST INJURED", "NUMBER OF MOTORIST KILLED", "VEHICLE TYPE CODE 1",
                  "VEHICLE TYPE CODE 2", "VEHICLE TYPE CODE 3"))
names(collision.df.bike)

collision.df<- data.frame(rbind(collision.df.auto, collision.df.bike))