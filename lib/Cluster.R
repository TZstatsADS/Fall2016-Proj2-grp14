####### This is cluster test code
####### Please make sure you set the working directory
collision.test= read.csv(file="data/collision_test.csv")
n=20
la.min= 40.5
la.max= 40.91
lo.min= -74.25
lo.max= -73.70
# suppose we have 4 time slot: 12am- 6am, 6am- 12pm, 12pm- 6pm, 6pm-12am
cluster= matrix(nrow = n^2, ncol=4)
k=1
for(i in 1:n){
  la.min.temp= qunif((i-1)/n, min = la.min, max = la.max)
  la.max.temp= qunif(i/n, min = la.min, max = la.max)
  for(j in 1:n){
    lo.min.temp= qunif((j-1)/n, min = lo.min, max = lo.max)
    lo.max.temp= qunif(j/n, min = lo.min, max = lo.max)
    temp= subset(collision.test, LATITUDE>la.min.temp & LATITUDE<=la.max.temp
                 &LONGITUDE>lo.min.temp&LONGITUDE<=lo.max.temp,select=c(TIME))
    cluster[k,1]= sum(temp$TIME==0|temp$TIME==1|temp$TIME==2|temp$TIME==3|temp$TIME==4|temp$TIME==5)
    cluster[k,2]= sum(temp$TIME==6|temp$TIME==7|temp$TIME==8|temp$TIME==9|temp$TIME==10|temp$TIME==11)
    cluster[k,3]= sum(temp$TIME==12|temp$TIME==13|temp$TIME==14|temp$TIME==15|temp$TIME==16|temp$TIME==17)
    cluster[k,4]= sum(temp$TIME==18|temp$TIME==19|temp$TIME==20|temp$TIME==21|temp$TIME==22|temp$TIME==23)
    k=k+1
  }
}

cluster_sub = apply(cluster, 1, function(row) all(row ==0 ))
cluster= cluster[!cluster_sub,]
#dim(cluster)
kmeans(cluster,4)
