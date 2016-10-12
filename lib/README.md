# Project: NYC Open Data
### Code lib Folder

The lib directory contains various files with function definitions (but only function definitions - no code that actually runs). They are as follows:

TimeSeries.R : This R script contains a time series model. The model extends monthly trends for collision for various parts of NYC. The model chosen for the same is the arima (1,1,1)(0,1,0)[12] as it has the lowest BIC amongst various other models. The other models at various time periods that have been used are included as comments in the code.

Cluster.Test.R : Grouping together areas similar pattern of danger (where danger varies with no of collisions) helps policy makers make more informed decisions in a convenient way. This R script clusters areas of similar danger together and displays the same on the NYC map. The areas have been color coded with red indicating regions with a highest hazard rates and blue with the lowest. 

Download_weather_Data.R : Collision and the dangers that come with it vary with weather. Naturally, weather data plays a pivotal role in predicting and analyzing the same. In this R script we, retreived weather data from 2012 to 2016 and appended the same to our master data file.

Data Cleansing.R : This R script combines data from various data sources like the motor collision, bike collision and the weather data. The cleaning aspect of the data dropped not very useful columns and kept useful ones such as the number of people killed, latitude , longitude and  date and time. We also deleted the NA data. 

Vehicle Type vs Year and Time.R : This script plots the number of collisions over the years by different vehicle times. 

Contributing_Factor_Graph.R : This R script created bar plots for number of accidents for every contributing factor was ploted.

Season_Graph.R : This R script plots the number of accidents against year for every season.

Google api.R : This is a script to translate the address name to longitude and latidute. 







