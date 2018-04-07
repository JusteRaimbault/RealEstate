
setwd(paste0(Sys.getenv('CS_HOME'),'/RealEstate/Models/DataExtrapolation/'))

library(dplyr)

source('inverseKernels.R')

income <- as.tbl(read.csv(file='data/revenus11.csv',sep=';',stringsAsFactors = F,header = T,dec = ',' ))
structure <- as.tbl(read.csv(file='data/structure11.csv',sep=';',stringsAsFactors = F))

library(doParallel)
cl <- makeCluster(60,outfile='log')
registerDoParallel(cl)


estimations <- foreach(iris=income$IRIS) %dopar% {
  try({
    show(paste0("Estimating iris : ",iris))
    source('inverseKernels.R')
    distr = c(unlist(income[income$IRIS==iris,c("RFUCD111","RFUCD211","RFUCD311","RFUCD411","RFUCQ211","RFUCD611","RFUCD711","RFUCD811","RFUCD911")]))
    shares = c(unlist(structure[structure$IRIS==iris,c("ART","CAD","INT","EMP","OUV")]))#/c(unlist(structure[structure$IRIS==iris,"POPTOT"]))
    shares=shares/sum(shares)
    initialParams = rep(c(median(distr),1000),length(shares))
    paramsBounds = list(lower=rep(c(min(distr),100),length(shares)),upper=rep(c(max(distr),10000),length(shares)))
    h=quantilesToHist(distr)
    res = inverseKernels(histogram =h,
                         weights = shares,
                         ker = gaussianKernel(),
                         initialParams = initialParams,
                         paramsBounds=paramsBounds,
                         iters.max = 10000
    )
    return(res)
  })

}
  
save(estimations,file='res/est_gaussian.RData')


