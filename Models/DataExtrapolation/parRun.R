
setwd(paste0(Sys.getenv('CS_HOME'),'/RealEstate/Models/DataExtrapolation/'))

source('inverseKernels.R')

years = c('01','02','03','04','05','06','07','08','09','10','11')

csp = c("EMP","OUV","INT","ART","CAD")

library(doParallel)
cl <- makeCluster(50,outfile='log')
registerDoParallel(cl)

for(year in years){
  
  show(paste0("Estimation for year 20",year))
  
  idcol = 'COM'
  
  income <- getIncome(year,idcol=idcol)
  structure <-getStructure(year,idcol=idcol)
    
  n = length(income[[idcol]])
  #show(n)
  
  
   estimations <- foreach(i=1:nrow(income)) %dopar% {
     show(paste0("row : ",i," / ",n))
     #iris=income$IRIS[i]
     id=income[[idcol]][i]
     
     show(paste0("Estimating : ",id))
     source('inverseKernels.R')
     return(estimateParameters(id,income,structure,year,iters.max=500,idcol=idcol))
   }
   
   save(estimations,file=paste0('res/extr_20',year,'_',idcol,'.RData'))
   #save(estimations,file=paste0('res/testrem_',year,'.RData'))
   
}

stopCluster(cl)

##
#load('res/est_gaussian_2011.RData')

fullres=data.frame()
for(year in years){
  load(paste0('res/extr_20',year,'_',idcol,'.RData'))
  fullres = rbind(fullres,matrix(sapply(estimations,function(l){
    c(l$medincome,
      l$avincome,
      l$stdincome,
      l$shares,
      l$distrib,
      l$gaussianvalmax,
      l$lognormalvalmax
      )
    }),ncol=length(estimations[[1]]$medincome)*4+3,
    byrow = T)
  )
}
names(fullres)<-c(paste0('medIncome',csp),paste0('avgIncome',csp),paste0('stdIncome',csp),paste0('share',csp),
                  'distribution','optgaussian','optlognormal'
                  )

write.table(fullres,file='res/extrapolate_allyears_communes.csv',sep = ';',row.names = F,col.names = T)







