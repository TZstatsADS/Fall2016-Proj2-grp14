####### This is cluster test code
####### Please make sure you set the working directory
setwd("~/Documents/Courses/ADS/Project 2/Fall2016-Proj2-grp14")
library(leaflet)
library(plotly)
library(RColorBrewer)
library(data.table)
collision= fread("data/collision_dataframe.csv")

col_sample= sample(1:nrow(collision),100000)
collision.test= collision[col_sample,]
write.csv(collision.test, file="data/collision_test.csv")
n=100
la.min= 40.5
la.max= 40.91
lo.min= -74.25
lo.max= -73.70
# suppose we have 4 time slot: 12am- 6am, 6am- 12pm, 12pm- 6pm, 6pm-12am
cluster= matrix(nrow = n^2, ncol=8)
cluster_full=matrix(nrow = n^2, ncol=28)
k=1
for(i in 1:n){
  la.min.temp= qunif((i-1)/n, min = la.min, max = la.max)
  la.max.temp= qunif(i/n, min = la.min, max = la.max)
  for(j in 1:n){
    lo.min.temp= qunif((j-1)/n, min = lo.min, max = lo.max)
    lo.max.temp= qunif(j/n, min = lo.min, max = lo.max)
    temp= subset(collision.test, LATITUDE>la.min.temp & LATITUDE<=la.max.temp
                 &LONGITUDE>lo.min.temp&LONGITUDE<=lo.max.temp,select=c(TIME))
    #cluster[k,1]= la.min.temp
    #cluster[k,2]= la.max.temp
    #cluster[k,3]= lo.min.temp
    #cluster[k,4]= lo.max.temp
    
    cluster_full[k,1]= la.min.temp
    cluster_full[k,2]= la.max.temp
    cluster_full[k,3]= lo.min.temp
    cluster_full[k,4]= lo.max.temp
    #cluster[k,5]= sum(temp$TIME==0|temp$TIME==1|temp$TIME==2|temp$TIME==3|temp$TIME==4|temp$TIME==5)
    #cluster[k,6]= sum(temp$TIME==6|temp$TIME==7|temp$TIME==8|temp$TIME==9|temp$TIME==10|temp$TIME==11)
    #cluster[k,7]= sum(temp$TIME==12|temp$TIME==13|temp$TIME==14|temp$TIME==15|temp$TIME==16|temp$TIME==17)
    #cluster[k,8]= sum(temp$TIME==18|temp$TIME==19|temp$TIME==20|temp$TIME==21|temp$TIME==22|temp$TIME==23)
    cluster_full[k,5:28]=table(factor(temp$TIME, 0:23))
    k=k+1
  }
}

cluster_sub = apply(cluster_full[,5:28], 1, function(row) all(row ==0 ))
#cluster= cluster[!cluster_sub,]
#dim(cluster)
cluster_full= cluster_full[!cluster_sub,]
cluster_result= kmeans(cluster_full[,5:28],4)
#cluster= as.data.frame(cbind(cluster,cluster_result$cluster))
cluster_full=as.data.frame(cbind(cluster_full,cluster_result$cluster))
colnames(cluster_full)= c("la.min","la.max", "lo.min", "lo.max",0:23,"type")
write.csv(cluster_full, file="data/cluster_full.csv")
###########################
#visualization
cols1=brewer.pal(4,"RdBu")
cols2=c(cols1[4],cols1[2],cols1[3],cols1[1])

factpal <- colorFactor(cols2, cluster_full$type)
m1 <- leaflet(cluster_full) %>% 
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addRectangles(lng1=~lo.min, lat1=~la.min, lng2=~lo.max, lat2=~la.max, stroke=F, 
                weight = 1, fillColor = ~factpal(type), fillOpacity = 1, popup = NULL, options = pathOptions())# %>%
m1

########################
#curve

c1= apply(cluster_full[cluster_full[,29]==1,],2,mean)
c2= apply(cluster_full[cluster_full[,29]==2,],2,mean)
c3= apply(cluster_full[cluster_full[,29]==3,],2,mean)
c4= apply(cluster_full[cluster_full[,29]==4,],2,mean)

plot_cluster= as.data.frame(cbind(c1,c2,c3,c4)[c(-1:-4,-29),])
round(plot_cluster, digits= 2)
xaxis<-list(title="Time")
yaxis<-list(title="Number of Collision")

p <- plot_ly(data= plot_cluster, x=1:24, y=~c4,type="scatter", mode= "lines", 
             line=list(color=cols1[1]), name="Type 1") %>%
  layout(xaxis=xaxis,yaxis=yaxis,title="Averafe ollision numbers for different pattens") %>%
  #add_lines(data= plot_cluster, x=1:24, y=~c1,type="scatter", mode= "lines")
  add_trace(p,y=~c2,mode= "lines",line=list(color=cols1[2]),name="Type 2") %>%
  add_trace(p,y=~c3,mode= "lines",line=list(color=cols1[3]),name="Type 3") %>%
  add_trace(p,y=~c1,mode= "lines",line=list(color=cols1[4]),name="Type 4")
p

