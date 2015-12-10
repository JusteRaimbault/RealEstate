
# spatial and temporal smoothed transactions maps

library(kernlab)
library(rgdal)
library(ggplot2)

# load data
carroyage <- readOGR('carroyage','carroyage_brut_ID')
capital <- as.matrix(read.csv('somme_prix_id.csv'))
years=c(1996,1999,2003,2006,2008,2010,2012)
coords = apply(as.matrix(carroyage@data[,1:3]),c(1,2),as.numeric)
spcapital = merge(capital,coords,by.x=2,by.y=1)

# check time-series regularity

# rough plots -> sample to have lisibility (change size for subsampling)
# normalized curves -> / max : remove /max for unnormalized curves
x=c();y=c();car=c();
for(i in sample.int(nrow(capital),size=nrow(capital),replace=TRUE)){show(i);x=append(x,years);ts=c(capital[i,3:9]);y=append(y,ts/max(ts));car=append(car,rep(capital[i,2],length(years)))}
p = ggplot(data=data.frame(years=x,capital=y,carro_id=car),aes(x,y,group=carro_id))
p+geom_line(aes(x,y,color=carro_id))

# let clusterize these normalized curves

k=5

plotTSClustering<-function(k){
  km = kmeans(x=as.matrix(
    #spcapital[,3:9]),
    t(apply(t(as.matrix(spcapital[,3:9])),2,function(c){c/max(c)}))),
              k,iter.max=1000)
  centers = km$centers
  x=c();y=c();clust=c();for(i in 1:k){x=append(x,years);y=append(y,centers[i,]);clust=append(clust,rep(i,length(years)))}
  p = ggplot(data=data.frame(years=x,capital=y,clust=clust),aes(x,y,group=clust))
  p+geom_line(aes(x,y,color=clust,size=km$size[clust]))

  #  map the clusters dirtily ?
  d=rowSums(km$centers[km$cluster,]^2);palette=colorRampPalette(c("green","yellow","red"))(k)
  plot(x=spcapital$X_LAEA,y=spcapital$Y_LAEA,col=palette[ceiling(k*d/max(d))],main=paste0("k=",k),pch='.',cex=8)
}

# try different cluster number
par(mfrow=c(2,2))
for(k in 3:6){plotTSClustering(k)}







