
setwd(paste0(Sys.getenv('CS_HOME'),'/RealEstate/Models/DataExtrapolation/'))

library(dplyr)

source('inverseKernels.R')

income <- as.tbl(read.csv(file='data/revenus11.csv',sep=';',stringsAsFactors = F,header = T,dec = ',' ))
structure <- as.tbl(read.csv(file='data/structure11.csv',sep=';',stringsAsFactors = F))

library(doParallel)
cl <- makeCluster(60,outfile='log')
registerDoParallel(cl)

n = length(income$IRIS)

estimations <- foreach(i=1:100) %dopar% {
  #try({
    show(paste0("row : ",i," / ",n))
    iris=income$IRIS[i]
    show(paste0("Estimating iris : ",iris))
    source('inverseKernels.R')
    distr = c(unlist(income[income$IRIS==iris,c("RFUCD111","RFUCD211","RFUCD311","RFUCD411","RFUCQ211","RFUCD611","RFUCD711","RFUCD811","RFUCD911")]))
    if(length(which(is.na(distr)))==0){
    shares = c(unlist(structure[structure$IRIS==iris,c("ART","CAD","INT","EMP","OUV")]))#/c(unlist(structure[structure$IRIS==iris,"POPTOT"]))
    shares=shares/sum(shares)
    initialParams = rep(c(median(distr,na.rm=T),1000),length(shares))
    paramsBounds = list(lower=rep(c(min(distr,na.rm=T),100),length(shares)),upper=rep(c(max(distr,na.rm=T),10000),length(shares)))
    h=quantilesToHist(distr)
    show(h)
    res = inverseKernels(histogram =h,
                         weights = shares,
                         ker = gaussianKernel(),
                         initialParams = initialParams,
                         paramsBounds=paramsBounds,
                         iters.max = 10
    )
    return(res)
    }else{return(NA)}
  #})

}
  
save(estimations,file='res/est_gaussian_2011.RData')


