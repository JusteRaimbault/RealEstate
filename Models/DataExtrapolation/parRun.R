
setwd(paste0(Sys.getenv('CS_HOME'),'/RealEstate/Models/DataExtrapolation/'))

source('inverseKernels.R')

years = c('01','02','03','04','05','06','07','08','09','10','11')

library(doParallel)
cl <- makeCluster(50,outfile='log')
registerDoParallel(cl)

for(year in years){
  
  show(paste0("Estimation for year 20",year))
  
  idcol = 'COM'
  
  income <- getIncome(year,idcol=idcol)
  structure <-getStructure(year,idcol=idcol)
    
  #n = length(income$IRIS)
  #show(n)
  
  
   estimations <- foreach(i=1:nrow(income)) %dopar% {
     show(paste0("row : ",i," / ",n))
     #iris=income$IRIS[i]
     id=income[[idcol]][i]
     
     show(paste0("Estimating : ",id))
     source('inverseKernels.R')
     return(estimateParameters(id,income,structure,year,iters.max=500,idcol=idcol))
   }
   
   save(estimations,file=paste0('res/est_gaussian_20',year,'.RData'))
   #save(estimations,file=paste0('res/testrem_',year,'.RData'))
   
}

stopCluster(cl)

##
#load('res/est_gaussian_2011.RData')

for(year in years){
  
  
  
}






