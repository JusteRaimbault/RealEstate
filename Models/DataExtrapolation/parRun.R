
setwd(paste0(Sys.getenv('CS_HOME'),'/RealEstate/Models/DataExtrapolation/'))

source('inverseKernels.R')

years = c('01','02','04','05','06','07','08','09','10','11')

library(doParallel)
cl <- makeCluster(60,outfile='log')
registerDoParallel(cl)

for(year in years){
  
  show(paste0("Estimation for year 20",year))
  
  income <- getIncome(year)
  structure <-getStructure(year)
    
  n = length(income$IRIS)
  
  estimations <- foreach(i=sample.int(n,size=60)) %dopar% {
    show(paste0("row : ",i," / ",n))
    iris=income$IRIS[i]
    show(paste0("Estimating iris : ",iris))
    source('inverseKernels.R')
    return(estimateParameters(iris,income,structure,year,iters.max=10))
  }
  
  #save(estimations,file='res/est_gaussian_2011.RData')
  save(estimations,file=paste0('res/testrem_',year,'.RData'))
  
}

stopCluster(cl)

##
#load('res/est_gaussian_2011.RData')






