library(RCurl)
library(RJSONIO)
library(plyr)
reverseGeoCode=function(lng,lat,key=NULL){
  if (is.null(key)){
    url=paste("https://maps.googleapis.com/maps/api/geocode/json?latlng=",lat,",",lng,sep="")}
  else {
    url=paste("https://maps.googleapis.com/maps/api/geocode/json?latlng=",lat,",",lng,"&key=",key,sep="")
  }
  doc <- getURL(url)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {return(x$results[[1]]$formatted_address)}
  else return(NA)
}

