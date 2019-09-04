
setwd(paste0(Sys.getenv('CS_HOME'),'/RealEstate/Models/Consolidation/'))



###loading

menagesvars = c("IDENT","AGEPR","ALGTU","RESAN","CSPR","RESACH","RESAPO","RESPRIACH","ZNIVVIE") # + ponderation
produitsvar = c("IDENTPROD","IDENT","ALGTU","ANNEE","ALGACQ","DETQUA","NATURE","DUREMP","DETTX","MONTCLA","COUNTAN_DECL")

#menages <- load()
#produits <- load()
menages=menages[,menagesvars]
produits=produitsvar[,produitsvar]


### filtering


# menages

# age
menages = menages[menages$AGEPR<=40,]
# agglo
#menages = menages[menages$ALGTU==6||menages$ALGTU==7,]
#zeat==1 -> not done
# year
menages = menages[menages$RESAN%in%(2010:2012),]
# csp 
# type aq



# produits
# algtu 
# annee = 2010
# nature %in% c(2,6)
ppas de filt sur nature (eq detqua)

prod_immo <- algaqu

prod_fin

###

# TODO 
# - variables a expliquer comme explicative -> duremp explique taux
# - matrices de correlation
# - pvalues
# - autres variables thematiques ? (taux montcla / nvie) 




