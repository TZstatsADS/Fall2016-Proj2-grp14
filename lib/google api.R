library(RCurl)
library(RJSONIO)
library(plyr)

url <- function(address,key, return.call = "json", sensor = "false") {
  root <- "https://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor,"&key=",key, sep = "")
  return(URLencode(u))
}
geoCode <- function(address,key,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address,key)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}
key="AIzaSyAoiIdkgOrlT67k6kmBo--_8FwQdyrzlv8"
geoCode("Grand Central pkwy",key)