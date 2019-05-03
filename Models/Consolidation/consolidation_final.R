


#########
# final consolidation script gathering data from
#
# - best models from the patrimoine database
# - extrapolated csp-level commune-level median incomes (kernel inverse GA optimization)
# - transactions from BIEN database
#
# Objective : 
# => get spatialized and local estimates of "access-to-market" variables, including length of mortgage, total amount of mortgage, mortgage rate
# ; at the csp level (=> map "spatial patterns of accessibility to real estate market" - which can be related to local market regimes ?)

setwd(paste0(Sys.getenv('CS_HOME'),'/RealEstate/Models/Consolidation/'))

# load data

# bien transactions
bien <- as.tbl(read.csv(paste0(Sys.getenv('CS_HOME'),'/RealEstate/Data/Donnees_article_juste/Tableau_BIEN2010_2012.csv'),sep=';'))

# uniformization of CSPs
# uniformize with extrapolated data
extrapolated_insee_znivvie <- as.tbl(read.csv('../DataExtrapolation/res/extrapolate_allyears_communes_CONSOLIDATED.csv',sep=';'))
znivvie = extrapolated_insee_znivvie[extrapolated_insee_znivvie$year==2011,]

# TODO launch revdisp extrapolation
#extrapolated_insee_revdisp <- ...

# Q : median or average income ?

# extrapolate for each csp

biencsps = c('Prof_intermediaires'='INT','Com_art_Chef_entreprises'='ART','Employes'='EMP','CPIS'='CAD','Ouvriers'='OUV')
bien$CSP = biencsps[as.character(bien$acquereurs)]

bien$medianZnivvie = rep(NA,nrow(bien))
for(csp in unique(bien$CSP)){
  show(csp)
  currentMedIncs = znivvie[[paste0('medIncome_',csp)]];names(currentMedIncs)=znivvie$idcom
  bien$medianZnivvie[bien$CSP==csp]=currentMedIncs[as.character(bien$DepCom[bien$CSP==csp])]
}

# summary(bien$medianZnivvie)
# TODO investigate presence of NAs

# TODO do some extreme variuation rate (low freq filter) + extreme values filtering on extrapolated data

# TODO
#bien$medianRevdispo = ...
bien$medianRevdispo = rep(50000,nrow(bien))


#######
# 2) load the models and apply them to the reconstructed database

patrimoine_models = read.csv('models.csv',sep=';')

# correspondance between var names

consolidated = bien[,c('DepCom','Age_acq','REQ_PRIX','CSP','medianZnivvie','medianRevdispo')]
names(consolidated) <- c('IDCOM','AGE','PRIX','CSP','medianZnivvie','medianRevdispo')

# create dummies for discrete var
for(csp in unique(consolidated$CSP)){
  consolidated[[paste0('CSP_',csp)]] = as.numeric(consolidated$CSP==csp)
}

explicative = c('AGE','PRIX',paste0('CSP_',unique(consolidated$CSP)),'medianZnivvie','medianRevdispo')

model_names = c('AGEPR'='AGE','RESPRIACH'='PRIX','CSPRprof_inter'='CSP_INT','CSPRprof_inter.1'='CSP_ART','CSPRouvriers'='CSP_OUV',
                'CSPRemployes'='CSP_EMP','CSPRCPIS'='CSP_CAD','ZNIVVIE'='medianZnivvie','Rev_Disp'='medianRevdispo')
models_coefs = patrimoine_models[,c('AGEPR','RESPRIACH','CSPRprof_inter','CSPRprof_inter','CSPRouvriers','CSPRemployes','CSPRCPIS','ZNIVVIE','Rev_Disp')]
names(models_coefs)<- model_names[names(models_coefs)]
models_coefs[is.na(models_coefs)]=0
rownames(models_coefs)<-paste0(as.character(patrimoine_models$explainedvar),'_',as.character(patrimoine_models$type),'_',as.character(patrimoine_models$with_respriach))

m = data.frame(consolidated[,explicative])
for(j in 1:ncol(m)){m[,j]=as.numeric(as.character(m[,j]))}
#m[['medianZnivvie']]=0;m[['medianRevdispo']]=0
m = as.matrix(m)
m = m[,explicative]
coefs = t(as.matrix(models_coefs))
coefs = coefs[explicative,]

final_consolidated = m%*%coefs
summary(final_consolidated)

# ?
# - check filterings

res = data.frame(final_consolidated[,paste0(unique(as.character(patrimoine_models$explainedvar)),'_BIEN_TRUE')])
summary(res)

#hist(res$MONTCLA_BIEN_TRUE,breaks=100)
# TODO : check distribs ; reasonable depending on explicative variables distributions ?
# OR compare to hist of patrimoine => should have fitted distributions ! - compare summaries

removeOutliers<-function(x){x[x>quantile(x,c(0.05),na.rm=T)&x<quantile(x,c(0.95),na.rm=T)]}

hist(removeOutliers(res$MONTCLA_BIEN_TRUE),breaks=100)
# => too small values in nivvie or rev_disp ?




