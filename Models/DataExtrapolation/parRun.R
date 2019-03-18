
setwd(paste0(Sys.getenv('CS_HOME'),'/RealEstate/Models/DataExtrapolation/'))

source('inverseKernels.R')

#years = c('01','02','03','04','05','06','07','08','09','10','11')
years=c('07','08','09','10','11')

csp = c("EMP","OUV","INT","ART","CAD")

idcol = 'COM'

library(doParallel)
cl <- makeCluster(50,outfile='log')
registerDoParallel(cl)

for(year in years){
  
  show(paste0("Estimation for year 20",year))
  
  income <- getIncome(year,idcol=idcol)
  structure <-getStructure(year,idcol=idcol)
    
  n = length(income[[idcol]])
  #show(n)
  
  
   estimations <- foreach(i=1:nrow(income)) %dopar% {
     show(paste0("row : ",i," / ",n))
     #iris=income$IRIS[i]
     tryCatch({
     id=income[[idcol]][i]
     
     show(paste0("Estimating : ",id))
     source('inverseKernels.R')
     return(estimateParameters(id,income,structure,year,iters.max=500,idcol=idcol))
     },error=function(e){show(e);return(NA)})
   }
   
   save(estimations,file=paste0('res/extr_20',year,'_',idcol,'.RData'))
   #save(estimations,file=paste0('res/testrem_',year,'.RData'))
   
}

stopCluster(cl)

##
#load('res/est_gaussian_2011.RData')

# year = '01'
# load(paste0('res/extr_20',year,'_',idcol,'.RData'))
# sapply(estimations,length)
# estimations[[5]]
# test=list();for(k in 1:6){test[[k]]=estimations[[k]]}
# testc=list();for(k in 1:6){testc=append(testc,flatten(estimations[[k]]))}



fullres=data.frame()
for(year in years){
  show(year)
  load(paste0('res/extr_20',year,'_',idcol,'.RData'))
  firstind = which(sapply(estimations,length)>8)[1]
  cols = length(estimations[[firstind]]$medincome)*4+4
  currentdata = sapply(estimations,function(l){
    #show(length(l))
    res = rep(NA,cols)
    if(length(l)>1){
      res = c(l$id,
              l$medincome,
              l$avincome,
              l$stdincome,
              l$shares,
              l$distrib,
              l$gaussianvalmax,
              l$lognormalvalmax
      )
    }
    return(res)
  }
                       
                       )
  
  fullres = rbind(fullres,matrix(
    currentdata,ncol=cols,
    byrow = T)
  )
}
names(fullres)<-c("id",paste0('medIncome_',csp),paste0('avgIncome_',csp),paste0('stdIncome_',csp),paste0('share_',csp),
                  'distribution','optgaussian','optlognormal'
                  )
for(j in (1:ncol(fullres))[c(-1,-22)]){fullres[,j]<-as.numeric(as.character(fullres[,j]))}
as.tbl(fullres)

# table(as.character(sapply(estimations,length)))
# for year 2001 : more than 1/2 with no data !
#1  13   8 
#1 528 771 

# filter 0 and NAs
#length(which(nchar(as.character(fullres$id))>0))
fullres=fullres[which(nchar(as.character(fullres$id))>0),]

write.table(fullres,file='res/extrapolate_allyears_communes.csv',sep = ';',row.names = F,col.names = T)







