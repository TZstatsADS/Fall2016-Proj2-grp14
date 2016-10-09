####### This is cluster test code
####### Please make sure you set the working directory
setwd("~/Documents/Courses/ADS/Project 2/Fall2016-Proj2-grp14")
collision.test= read.csv(file="data/collision_test.csv")
n=20
la.min= 40.5
la.max= 40.91
lo.min= -74.25
lo.max= -73.70
# suppose we have 4 time slot: 12am- 6am, 6am- 12pm, 12pm- 6pm, 6pm-12am
cluster= matrix(nrow = n^2, ncol=8)
cluster_full=matrix(nrow = n^2, ncol=24)
k=1
for(i in 1:n){
  la.min.temp= qunif((i-1)/n, min = la.min, max = la.max)
  la.max.temp= qunif(i/n, min = la.min, max = la.max)
  for(j in 1:n){
    lo.min.temp= qunif((j-1)/n, min = lo.min, max = lo.max)
    lo.max.temp= qunif(j/n, min = lo.min, max = lo.max)
    temp= subset(collision.test, LATITUDE>la.min.temp & LATITUDE<=la.max.temp
                 &LONGITUDE>lo.min.temp&LONGITUDE<=lo.max.temp,select=c(TIME))
    cluster[k,1]= la.min.temp
    cluster[k,2]= la.max.temp
    cluster[k,3]= lo.min.temp
    cluster[k,4]= lo.max.temp
    cluster[k,5]= sum(temp$TIME==0|temp$TIME==1|temp$TIME==2|temp$TIME==3|temp$TIME==4|temp$TIME==5)
    cluster[k,6]= sum(temp$TIME==6|temp$TIME==7|temp$TIME==8|temp$TIME==9|temp$TIME==10|temp$TIME==11)
    cluster[k,7]= sum(temp$TIME==12|temp$TIME==13|temp$TIME==14|temp$TIME==15|temp$TIME==16|temp$TIME==17)
    cluster[k,8]= sum(temp$TIME==18|temp$TIME==19|temp$TIME==20|temp$TIME==21|temp$TIME==22|temp$TIME==23)
    cluster_full[k,]=table(factor(temp$TIME, 1:24))
    k=k+1
  }
}

cluster_sub = apply(cluster[,5:8], 1, function(row) all(row ==0 ))
cluster= cluster[!cluster_sub,]
#dim(cluster)
cluster_result= kmeans(cluster[,5:8],4)
cluster= as.data.frame(cbind(cluster,cluster_result$cluster))
colnames(cluster)= c("la.min","la.max", "lo.min", "lo.max","1","2","3","4","type")

###########################
#visualization
library(leaflet)
factpal <- colorFactor(topo.colors(4), cluster$type)

m1 <- leaflet(cluster) %>% #cluster[cluster$type==2,]
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addRectangles(lng1=~lo.min, lat1=~la.min, lng2=~lo.max, lat2=~la.max, color = ~factpal(type), weight = 1,  
              fillColor = ~factpal(type), fillOpacity = 0.2, popup = NULL, options = pathOptions())# %>%
  #addRectangles(lng1=~lo.min, lat1=~la.min, lng2=~lo.max, lat2=~la.max, color = "#03F", weight = 1,  
   #             fillColor = "yellow", fillOpacity = 0.2, popup = NULL, options = pathOptions()) %>%
m1

########################
#curve
cluster_full= cluster_full[!cluster_sub,]
cluster_full=cbind(cluster_full,cluster_result$cluster)
c1= apply(cluster_full[cluster_full[,25]==1,],2,mean)
c2= apply(cluster_full[cluster_full[,25]==2,],2,mean)
c3= apply(cluster_full[cluster_full[,25]==3,],2,mean)
c4= apply(cluster_full[cluster_full[,25]==4,],2,mean)
lines(c1)
lines(c2)
lines(c3)
lines(c4)
