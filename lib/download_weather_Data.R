#ads-proj2
setwd("G:/Columbia/study/3rd semester/5243/project2/data")
library(data.table)
library(dplyr)
library(tidyr)
library(rjson)
library(data.table)
library(rbokeh)
library(plotly)
library(weatherData)
library(rcharts)

year_start=2012
year_end=2015

time_start=10
time_end=15

w2012 <- getWeatherForDate(station_id = "nyc",start_date = "2012-06-30",
                           end_date ="2012-12-31",
                           opt_custom_columns=TRUE,custom_columns=c(1:23))
w2013 <- getWeatherForDate(station_id = "nyc",start_date = "2013-01-31",
                           end_date ="2013-12-31",
                           opt_custom_columns=TRUE,custom_columns=c(1:23))

w2014 <- getWeatherForDate(station_id = "nyc",start_date = "2014-01-01",
                           end_date ="2014-12-31",
                           opt_custom_columns=TRUE,custom_columns=c(1:23))

w2015 <- getWeatherForDate(station_id = "nyc",start_date = "2015-01-01",
                           end_date ="2015-12-31",
                           opt_custom_columns=TRUE,custom_columns=c(1:23))
w2016 <- getWeatherForDate(station_id = "nyc",start_date = "2016-01-01",
                           end_date ="2016-09-15",
                           opt_custom_columns=TRUE,custom_columns=c(1:23))
w2012=select(w2012,-c(EST))
w2013=select(w2013,-c(EST))
w2014=select(w2014,-c(EST))
w2015=select(w2015,-c(EST))
w2016=select(w2016,-c(EDT))
weatherdata=rbind(w2012,w2013,w2014,w2015,w2016)
write.csv(weatherdata,"weatherdata.csv")

