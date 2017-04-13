setwd(paste0(Sys.getenv('CS_HOME'),'/RealEstate/Models/DataExtrapolation/'))

source('inverseKernels.R')




## tests
# nkers=6
# x=(1:100)/100;
# y=rep(0,length(x))
# mus=c()
# for(i in 1:nkers){
#   mu=runif(1);mus=append(mus,mu)
#   y=y+sapply(x,function(x){fixedSdGaussian(0.1)(x,mu)})
# }
# 
# 
# res = inverseKernels(histogram = list(mids=x,counts=y),
#                      weights = rep(1,nkers),
#                      ker = fixedSdGaussian(0.1),
#                      initialParams = runif(nkers),
#                      paramsBounds=list(lower=rep(0,nkers),upper=rep(1,nkers)))
# mus
# res$parameters
# 
# plot(x,y,type='l')
# for(p in 1:length(res$parameters)){
#   #abline(v=mus[p],col='blue')
#   abline(v=res$parameters[p],col='red');
# }

# GA seems better in all cases



# test with a lognormal plus unif noise
x=rlnorm(100000);x[x>5] = runif(length(which(x>5)))
nkers = 10

res = inverseKernels(histogram =hist(x,breaks=100,plot=FALSE),
                     weights = (1:nkers)/(nkers*(nkers+1)/2),#rep(1,nkers),
                     ker = fixedSdGaussian(0.2),
                     initialParams = runif(nkers),
                     paramsBounds=list(lower=rep(min(x),nkers),upper=rep(max(x),nkers)),
                     iters.max = 200
)

hist(x,breaks=100,freq=FALSE)
h=hist(x,breaks=100,freq=FALSE)
for(p in 1:length(res$parameters)){
  #abline(v=mus[p],col='blue')
  abline(v=res$parameters[p],col='red');
}
points(x = h$mids,y=res$fittedHist,type='l',col='blue')


###
# TODO
# test with variable sigma ?



## 
# TODO
# -- test on real data --
#




q= c(800,900,1000,1100,1200,1500,1900,2500,5000,10000)

fit = fitdistr(q,densfun = "log-normal")
y=rlnorm(n=100000,fit$estimate[1],fit$estimate[2])
y=y[y<q[length(q)]];
yy=hist(y,plot=FALSE,breaks = 100)

# weights : let say we have five categories
weights = c(0.4,0.35,0.1,0.1,0.05)
# corresponding initial guesses for means (must be inside [0,max(x)])
initial=rep(0,length(weights))

# optimize - let take width 1 gaussian
res = inverseKernels(yy,weights,fixedSdGaussian(500),initial,iters.max = 400)

# let do some stuff with the result : for example plot the hist, the fitted and the obtained gaussian means
# here res$parameters are the means for each category
plot(x = yy$mids,y=yy$density,type='l')
for(p in 1:dim(res$parameters)[2]){show(res$parameters[1,p]);abline(v=res$parameters[1,p],col='red');}
points(x = yy$mids,y=res$fittedHist,type='l',col='blue')




