setwd(paste0(Sys.getenv('CS_HOME'),'/RealEstate/Models/DataExtrapolation/'))

source('inverseKernels.R')

years = c('01','02','03','04','05')
csp = c("EMP","OUV","INT","ART","CAD")

idcol = 'COM'

extrapolated <- read.csv(file='res/extrapolate_allyears_communes.csv',sep = ';')

#for(year in years){
year='01'

income <- getIncome(year,idcol=idcol,dispo='')
structure <-getStructure(year,idcol=idcol)

shareCAD = structure$CAD/rowSums(structure[,csp])
diff = left_join(extrapolated[extrapolated$year==2001,c('idcom','share_CAD')],data.frame(idcom=as.numeric(structure$COM),share=shareCAD))
summary(abs(diff$share_CAD - diff$share))

shareOUV = structure$OUV/rowSums(structure[,csp])
diff = left_join(extrapolated[extrapolated$year==2001,c('idcom','share_OUV')],data.frame(idcom=as.numeric(structure$COM),share=shareOUV))
summary(abs(diff$share_OUV - diff$share))

#}
