
setwd(paste0(Sys.getenv('CS_HOME'),'/RealEstate/Models/Consolidation/'))

library(stargazer)
library(stringr)


patrimoine_models = read.csv('models.csv',sep=';')

# export with respriach (meilleurs modeles)

models = as.tbl(patrimoine_models) %>% filter(type=='INSEE'|with_respriach==T) 
#[patrimoine_models$type=='INSEE'|(patrimoine_models$with_respriach==T&&patrimoine_models$type=='BIEN'),]


varnames = c('RESAPO'='Apport personnel',
             'COUTAN_DECL'='Versement mensuel',
             'MONTCLA'='Montant emprunté',
             'DETTX'='Taux d\'emprunt',
             'DUREMP'='Durée de l\'emprunt'
             )
explnames = c('intercept'='constante',
  'AGEPR'='age',
  'RESPRIACH'='Prix du bien',
  'Rev_Disp'='Revenu',
  'CSPRCPIS'='CPIS',
  'CSPRemployes'='Employés',
  'CSPRouvriers'='Ouvriers',
  'CSPRprof_inter'='Prof. Inter.',
  'ZNIVVIE'='Niveau de vie'
)


for(var in unique(models$explainedvar)){
  selmodels = as.data.frame(models[models$explainedvar==var,])
  # df columns : explicative var ; coef ; sd ; signif
  explicative = names(selmodels)[seq(5,(ncol(selmodels)-2),by=3)]
  
  getCol <- function(type) {
  coefsbienraw = unlist(selmodels[selmodels$type==type,seq(5,(ncol(selmodels)-2),by=3)])
  coefsbien = ifelse(is.na(coefsbienraw),'',format(coefsbienraw,scientific=F,trim=T,drop0trailing=T))
  sdbien = ifelse(is.na(coefsbienraw),NA,format(unlist(selmodels[selmodels$type==type,seq(6,(ncol(selmodels)-2),by=3)]),scientific=F,trim=T,drop0trailing=T))
  #sdbien = ifelse(is.na(coefsbienraw),'',paste0(' $\\pm$ ',sdbien))
  sdbien = ifelse(is.na(coefsbienraw),'',sdbien)
  #signifbien =  ifelse(is.na(coefsbienraw),'',paste0(' (p=',format(unlist(selmodels[selmodels$type==type,seq(7,(ncol(selmodels)-2),by=3)]),digits=3),')'))
  pval = unlist(selmodels[selmodels$type==type,seq(7,(ncol(selmodels)-2),by=3)])
  signifbien =  ifelse(is.na(coefsbienraw),'',
                       ifelse(pval>0.1,'',ifelse(pval>0.05,'.',ifelse(pval > 0.01,'*',ifelse(
                         pval>0.001,'**','***'
                       )
                       ))))
                       #paste0(' (p=',format(,digits=3),')'))
  return(list(coefsbien,sdbien,signifbien))
  }
  colbien = getCol('BIEN')
  colinsee = getCol('INSEE')
  
  #modeldf = data.frame()#explicative=explicative,
    #coefsbien=coefsbien,sdbien=sdbien,signifbien=signifbien)
  #modeldf[[paste0(var,' (BIEN)')]] = colbien
  #modeldf[[paste0(var,' (INSEE)')]] = colinsee
  modeldf = data.frame(explnames[explicative],colbien[[1]],colbien[[2]],colbien[[3]],colinsee[[1]],colinsee[[2]],colinsee[[3]],
                       stringsAsFactors = F)
  #names(modeldf)<-c('model',paste0(var,' (BIEN)'),paste0(var,' (INSEE)'))
  
  # !!! math does not work with ^ in first col
  modeldf=rbind(
    c('',varnames[[var]],'','','','',''),
    c('Modèle','','BIEN','','','INSEE',''),
    c('Variable','Coef.','Err. Std.','','Coef.','Err. Std.','')
    ,modeldf,as.character(c('Adj. R2',selmodels$AdjRSquared[selmodels$type=='BIEN'],'','',selmodels$AdjRSquared[selmodels$type=='INSEE'],'','')),stringsAsFactors = F)
  #for(j in 1:ncol(modeldf)){modeldf[,j]=as.factor(modeldf[,j])}
  
  res = stargazer(
    modeldf, 
    summary = F,
    type='latex',
    out=paste0('latex/',var,'.tex'),
    #out.header = T,
    #header = '\\documentclass{article}\\begin{document}'
    rownames = F
  )
  #tab = str_replace_all(res,'NA','')
  #cat(tab,file=paste0('latex/',var,'.tex'))
}
  