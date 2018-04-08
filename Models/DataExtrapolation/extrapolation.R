
## test data extrapolation on real data

setwd(paste0(Sys.getenv('CS_HOME'),'/RealEstate/Models/DataExtrapolation/'))

library(dplyr)

source('inverseKernels.R')


### load data

# income iris
income <- as.tbl(read.csv(file='data/revenus11.csv',sep=';',stringsAsFactors = F,header = T,dec = ',' ))

# global distrib
gincome <-as.tbl(read.csv(file='data/revenus_csp_idf.csv',sep=';',stringsAsFactors = F))
# ranks of csp through the years
#apply(gincome[2:nrow(gincome),2:ncol(gincome)],MARGIN = 2,function(x){unlist(gincome[2:nrow(gincome),1])[order(x)]})

# population structure
structure <- as.tbl(read.csv(file='data/structure11.csv',sep=';',stringsAsFactors = F))


## agregation to construct overall income distrib

#iris=956800114
#iris=751062304


for(i in 1:100){
#i=1
  iris=income$IRIS[i]
  show(iris)
distr = c(unlist(income[income$IRIS==iris,c("RFUCD111","RFUCD211","RFUCD311","RFUCD411","RFUCQ211","RFUCD611","RFUCD711","RFUCD811","RFUCD911")]))
shares = c(unlist(structure[structure$IRIS==iris,c("ART","CAD","INT","EMP","OUV")]))#/c(unlist(structure[structure$IRIS==iris,"POPTOT"]))
shares=shares/sum(shares)
gmeds = gincome$X2011[2:6]

#res = inverseKernels(histogram =quantilesToHist(distr),
#                     weights = shares,
#                     ker = fixedSdGaussian(5000),
#                     initialParams = rep(median(distr),length(shares)),
#                     paramsBounds=list(lower=rep(min(distr),length(shares)),upper=rep(max(distr),length(shares))),
#                     iters.max = 100
#)
#plotRes(res)

# test with variable width kernel
#initialParams = c(gmeds[1],2000,gmeds[2],2000,gmeds[3],2000,gmeds[4],2000,gmeds[5],2000)
#paramsBounds = list(lower=c(5000,100,5000,100,5000,100,5000,100,5000,100),upper=c(100000,10000,100000,10000,100000,10000,100000,10000,100000,10000))

initialParams = rep(c(median(distr,na.rm=T),1000),length(shares))
paramsBounds = list(lower=rep(c(min(distr,na.rm=T)-100,100),length(shares)),upper=rep(c(max(distr,na.rm=T)+100,10000),length(shares)))
h=quantilesToHist(distr)
show(paramsBounds)
res = inverseKernels(histogram =h,
                     weights = shares,
                     ker = gaussianKernel(),
                     initialParams = initialParams,
                     paramsBounds=paramsBounds,
                     iters.max = 10
)
#plotRes(res)

}


