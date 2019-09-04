
setwd(paste0(Sys.getenv('CS_HOME'),'/RealEstate/Models/DataExtrapolation/'))

source('inverseKernels.R')

#years = c('01','02','03','04','05','06','07','08','09','10','11')
#years=c('07','08','09','10','11')
#years=c('11')
years=c('03','08','11')

csp = c("EMP","OUV","INT","ART","CAD")

idcol = 'COM'

library(doParallel)
cl <- makeCluster(60,outfile='log')
registerDoParallel(cl)

for(year in years){
  
  show(paste0("Estimation for year 20",year))
  
  # FIXME change if uc or not
  income <- getIncome(year,idcol=idcol)#,dispo='dispo')
  #income <- getIncome(year,idcol=idcol,dispo='dispo')
  
  structure <-getStructure(year,idcol=idcol)
    
  n = length(income[[idcol]])
  show(n)
  show(names(income))
  
  # FIXME change if uc or not
  income_col_prefixes = c('RFMD1','RFMD2','RFMD3','RFMD4','RFMQ2','RFMD6','RFMD7','RFMD8','RFMD9')
  
   estimations <- foreach(i=1:nrow(income)) %dopar% {
     show(paste0("row : ",i," / ",n))
     #iris=income$IRIS[i]
     tryCatch({
     id=income[[idcol]][i]
     
     show(paste0("Estimating : ",id))
     source('inverseKernels.R')
     return(estimateParameters(id,income,structure,year,iters.max=500,idcol=idcol,income_col_prefixes=income_col_prefixes))
     },error=function(e){show(e);return(NA)})
   }
   
   #save(estimations,file=paste0('res/extr_20',year,'_',idcol,'_dispo.RData'))
   save(estimations,file=paste0('res/extr_20',year,'_',idcol,'.RData'))
   #save(estimations,file=paste0('res/testrem_',year,'.RData'))
   
}

stopCluster(cl)

##
#load('res/est_gaussian_2011.RData')

# year = '01'
# load(paste0('res/extr_20',year,'_',idcol,'.RData'))

#load(paste0('res/extr_20',year,'_',idcol,'_dispo.RData'))

# sapply(estimations,length)
# # estimations[[5]]
# # test=list();for(k in 1:6){test[[k]]=estimations[[k]]}
# # testc=list();for(k in 1:6){testc=append(testc,flatten(estimations[[k]]))}
# 
# 
# 
# 
#  fullres=data.frame()
#  for(year in years){
#    show(year)
#    load(paste0('res/extr_20',year,'_',idcol,'_dispo.RData'))
#    firstind = which(sapply(estimations,length)>8)[1]
#    cols = length(estimations[[firstind]]$medincome)*4+5
#    currentdata = sapply(estimations,function(l){
#      show(length(l))
#      res = c(rep(NA,cols-1),paste0("20",year))
#      if(length(l)>1){
#        res = c(l$id,
#                l$medincome,
#                l$avincome,
#                l$stdincome,
#                l$shares,
#                l$distrib,
#                l$gaussianvalmax,
#                l$lognormalvalmax,
#                paste0("20",year)
#        )
#      }
#      return(res)
#    }
# 
#                         )
# 
#    fullres = rbind(fullres,matrix(
#      currentdata,ncol=cols,
#      byrow = T)
#    )
#  }
#  names(fullres)<-c("idcom",paste0('medIncome_',csp),paste0('avgIncome_',csp),paste0('stdIncome_',csp),paste0('share_',csp),
#                    'distribution','optgaussian','optlognormal','year'
#                    )
#  for(j in (1:ncol(fullres))[c(-1,-22)]){fullres[,j]<-as.numeric(as.character(fullres[,j]))}
# 
#  # as.tbl(fullres)
# #
# # table(as.character(sapply(estimations,length)))
# # for year 2001 : more than 1/2 with no data !
# #1  13   8
# #1 528 771
#
# # filter 0 and NAs
# #length(which(nchar(as.character(fullres$id))>0))

#fullres=fullres[which(nchar(as.character(fullres$id))>0),]

#write.table(fullres,file='res/extrapolate_allyears_communes.csv',sep = ';',row.names = F,col.names = T)

 #write.table(fullres,file='res/extrapolate_allyears_communes_dispo.csv',sep = ';',row.names = F,col.names = T)

# #
#summary(as.tbl(fullres)%>% group_by(year))
#
#
# ######
# # consolidate with input data
#
#extrapolated <- as.tbl(read.csv(file='res/extrapolate_allyears_communes.csv',sep=';'))
#
# # get input data : median rfuc ; nbuc
# inputdata = data.frame()
# for(year in years){
#   income <- getIncome(year,idcol=idcol)
#   inds = !is.na(income[[paste0('RFUCQ2',year)]])
#   inputdata=rbind(inputdata,data.frame(medIncome=income[[paste0('RFUCQ2',year)]][inds],
#                                        nbUC=income[[paste0('NBUC',year)]][inds],
#                                        idcom=income[['COM']][inds],
#                                        year=rep(paste0('20',year),length(which(inds)))
#                                        ))
# }
#
# inputdata=as.tbl(inputdata)
# inputdata$year <- as.numeric(as.character(inputdata$year))
#
# #apply(income,2,function(r){length(which(is.na(r)))})
# # -> median income is RFUCQ211
#
# consolidated <- left_join(extrapolated,inputdata,by=c('idcom','year'))
#
# write.table(consolidated,file='res/extrapolate_allyears_communes_CONSOLIDATED.csv',sep = ';',row.names = F,col.names = T)
#

