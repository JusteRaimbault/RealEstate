
# spatial and temporal smoothed transactions maps

library(kernlab)
library(rgdal)
library(ggplot2)

setwd('/Users/Juste/Documents/ComplexSystems/RealEstate//Data')

# load data
carroyage <- readOGR('carroyage','carroyage_brut_ID')
capital <- as.matrix(read.csv('somme_prix_id.csv'))
years=c(1996,1999,2003,2006,2008,2010,2012)
coords = apply(as.matrix(carroyage@data[,1:3]),c(1,2),as.numeric)
spcapital = merge(capital,coords,by.x=2,by.y=1)


# basic stats
hist(log(capital[,3:9]),breaks=100)


# check time-series regularity

#####
## NOT USED

# rough plots -> sample to have lisibility (change size for subsampling)
# normalized curves -> / max : remove /max for unnormalized curves
#x=c();y=c();car=c();
#for(i in sample.int(nrow(capital),size=nrow(capital),replace=TRUE)){show(i);x=append(x,years);ts=c(capital[i,3:9]);y=append(y,ts/max(ts));car=append(car,rep(capital[i,2],length(years)))}
#p = ggplot(data=data.frame(years=x,capital=y,carro_id=car),aes(x,y,group=carro_id))
#p+geom_line(aes(x,y,color=carro_id))

# let clusterize these normalized curves

#k=7

#####


####
# function : k = nombre clusters -> trace carte et renvoie ggplot des trajectoires
plotTSClustering<-function(k){
  
  
  # la fonction kmeans fait le clustering ; x est les données à clusteriser : colomnes : coordonnées, lignes : points
  km = kmeans(x=as.matrix(
    #spcapital[,3:9]),
    t(apply(t(as.matrix(spcapital[,3:9])),2,function(c){c/max(c)}))),# on normalize les séries temporelles par x_n(t) = x(t)/max_t(x(t))
              centers=k,nstart=20,iter.max=1000) # k clusters, autres paramètres techniques
  
  # matrice des centroides des clusters
  centers = km$centers
  
  # on construit les données pour le ggplot (elles doivent être à la suite en vecteur)
  x=c();y=c();clust=c();colors=c();palette=colorRampPalette(c("green","yellow","red"))(k)
  for(i in 1:k){
    x=append(x,years);y=append(y,centers[i,]);clust=append(clust,rep(i,length(years)))
    colors=append(colors,rep(sum(km$centers[i,]^2),length(years)))
  }
  mid = (max(colors) + min(colors)) / 2
  
  # plot ts centroids
  p = ggplot(data=data.frame(years=x,capital=y,cluster=clust,color=colors,size=km$size[clust]),aes(x=years,y=capital,group=cluster))
  #
  
  # on donne les couleurs au clusters en fonction de la norme de son centre (arbitraire mais continu dans l'espace des distances entre points)
  d=rowSums(km$centers[km$cluster,]^2)
  colors=palette[ceiling(k*d/max(d))]
  # dirty map : la carte en plot dégueu
  plot(x=spcapital$X_LAEA,y=spcapital$Y_LAEA,col=colors,main=paste0("k=",k),pch='.',cex=8)
  
  # on retourne le ggplot
  return(p+geom_line(aes(x=years,y=capital,color=color,size=size))+ scale_size(range = c(0.5, 6))+scale_color_gradientn(colours=c("green",  "yellow", "red")))
}

# try different cluster number
par(mfrow=c(3,3))
plots=list()
i=1
# on fait k de 2 à 10 : la fonction trace la carte dans les cases de mfrow successivement et stocke le ggplot dans une liste
for(k in 2:10){plots[[i]]=plotTSClustering(k);i=i+1}

# on affiche les ggplots par une fonction spéciale
# (voir script plots)
multiplot(plotlist=plots,cols=3)







