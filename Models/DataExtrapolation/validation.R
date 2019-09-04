setwd(paste0(Sys.getenv('CS_HOME'),'/RealEstate/Models/DataExtrapolation/'))

source('inverseKernels.R')

years = as.numeric(paste0('20',c('01','02','03','04','05','06','07','08','09','10','11')))
#years=c('07','08','09','10','11')

csp = c("EMP","OUV","INT","ART","CAD")

idcol = 'COM'

medincomecols = paste0('medIncome_',csp)

data <- as.tbl(read.csv('res/extrapolate_allyears_communes_CONSOLIDATED.csv',sep=';'))


summary(data)


######
## 1) Stability of medIncomes by csp should be similar to overall med income
# -> find reasonable share filtering parameters
# for rates with left data
ratesMedIncome=data.frame()
for(y in 2:length(years)){
  currentdata = left_join(data[data$year==years[y],c(medincomecols,'medIncome','idcom')],data[data$year==years[y-1],c(medincomecols,'medIncome','idcom')],by='idcom')
  rates = (currentdata[,paste0(c(medincomecols,'medIncome'),'.y')] - currentdata[,paste0(c(medincomecols,'medIncome'),'.x')])/currentdata[,paste0(c(medincomecols,'medIncome'),'.x')]
  names(rates)<-c(medincomecols,'medIncome')
  ratesMedIncome=rbind(ratesMedIncome,cbind(rates,year=rep(years[y],nrow(rates)),idcom=currentdata$idcom))
}

for(year in unique(ratesMedIncome$year)){
  for(j in 1:length(medincomecols)){
    show(summary(ratesMedIncome[ratesMedIncome$year==year,j]/ratesMedIncome[ratesMedIncome$year==year,"medIncome"]))
  }
}

# -> some outliers to filter ; check relation with shares (construct "trust indic" ?)


####
## 2) geographical structure ?
# : can not have a priori, target of the work.






