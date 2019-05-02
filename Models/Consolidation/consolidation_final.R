


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



#######
# 2) load the models and apply them to the reconstructed database

patrimoine_models = as.tbl(read.csv('models.csv',sep=';')) 

# correspondance between var names

consolidated = bien[,c('DepCom','Age_acq','REQ_PRIX','CSP','medianZnivvie','medianRevdispo')]
names(consolidated) <- c('IDCOM','AGE','PRIX','CSP','medianZnivvie','medianRevdispo')

# create dummies for discrete var







