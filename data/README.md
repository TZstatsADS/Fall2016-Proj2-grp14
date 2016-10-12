# Project: NYC Open Data
### Data folder

The data directory contains data used in the analysis. This is treated as read only; in paricular the R/python files are never allowed to write to the files in here. Depending on the project, these might be csv files, a database, and the directory itself may have subdirectories.


+ collision_dateframe	is partial dataset for the final app, containing columns like Date, Time, Location, Collision type and weather. 
+ manhattan_bike_injury and weatherdata are raw datasets. (Another raw data is Motor collision)
+ cluster_full.csv is the cluster results of collision pattern as of time.
