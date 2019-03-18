
########################
#'
#' Inverse problem Kernel Mixture reconstruction
#'

library(GA)
library(MASS)
library(dplyr)


###############
#' 
#' TODO
#'   * check sparse optimization (remove small csp shares ?)
#'   * mixed kernel types
#'   * restrict/increase number of parameters (10 params -> overfit ?) // add AIC fit criteria ?
#'   * hierarchically conditioned ? (dep data e.g. for reference incomes per csp ?)
#'   * multilevel + gwr in application (Q : spatial stationarity of link incomestructure <-> bien variables) 
#' 
#' Optimize kernels parameters for inverse kernel mixture problem
#' 
#' for all j, f(x_j) = \sum w_c k_c(x_j,\vec{\alpha})
#' to be optimized on \alpha
#'  with 
#'  
#'  Required args :
#'   - histogram : histogram object, or list with slots $density and $mids, corresponding to (x_j) and f(x_j)
#'   - weights : array of weights to be attributed to each kernel, must have \sum weights = 1
#'   - ker : kernel function (first arguments x, other parameters to be optimized) - the function fixedSdGaussian(sigma) provide gaussian kernel
#'   - initialParams : initial values for parameters before optimization (same size as weights) - better be a reasonably thematical guess
#'   
#'   Additional:
#'   - paramsBounds : list with slots $lower and $upper, that are lower and upper bounds for optimization
#'   - costFunction \in {"mse"}
#'   - optimMethod \in {"nlm","ga"} . convex optimization ?
#'   - iter.max = 100 : increase if does not converges well
#'   
#'   Value :
#'     list with slots :
#'       - parameters : optimal parameters
#'       - fittedHist : fitted histogram
#'   
#'   Example :
#'   ------------
#'     #  optmize with fixed width gaussian kernels
#'   
#'     # if q are quantiles of the target distrbution
#'     h = quantilesToHist(q)
#'     # if x is the array whose distrib is targeted,nbreaks arbitrary number of breaks
#'     h=hist(x,nbreaks)
#'     
#'     # weights : let say we have five categories
#'     weights = c(0.4,0.3,0.1,0.1,0.1)
#'     # corresponding initial guesses for means (must be inside [0,max(x)])
#'     # let suppose max(x) = 10
#'     initial = c(1,2,3,6,7)
#'     
#'     # optimize - let take width 1 gaussian
#'     res = inverseKernels(h,weights,fixedSdGaussian(1),initial)
#'     
#'     # let do some stuff with the result : for example plot the hist, the fitted and the obtained gaussian means
#'     # here res$parameters are the means for each category
#'     plot(x = h$mids,y=h$density,type='l')
#'     for(p in 1:length(res$parameters)){abline(v=res$parameters[p],col='red');}
#'     points(x = h$mids,y=res$fittedHist,type='l',col='blue')
#'   
#'   
inverseKernels<-function(histogram,weights,ker,initialParams,paramsBounds = NULL,costFunction="mse",optimMethod="ga",iters.max=100){
  if(is.null(histogram$mids)|is.null(histogram$density)){stop()}
  res=list()
  res$initialParams = initialParams
  res$histogram=histogram
  
  x = histogram$mids
  y = histogram$density
  y = y / sum(y*diff(c(0,x)))
  
  # kernel
  if(is.null(formals(ker)$x)|length(formals(ker))<2){stop("invalid kernel")}
  

  # cost function
  # by default : abs
  cost=function(x,y){return(sum(abs(x-y)))}
  if(costFunction=="mse"){cost=function(x,y){return(sum((x-y)^2))}}
  
  # function to optimize
  vals=function(params){
    vals = c()
    xx=c(0,x)
    for(j in 1:length(x)){
      k = 0
      for(c in 1:length(weights)){
        k = k + weights[c]*ker(x=x[j],params[((c-1)*nparams+1):(c*nparams)])
      }
      vals=append(vals,k)
    }
    return(vals/sum(vals*diff(c(0,x))))
  }
  
  f = function(params){
    return(cost(y,vals(params)))
  }
  
  # bounds
  bounds = paramsBounds
  if(is.null(paramsBounds)){
     bounds=list()
     bounds$lower = rep(0,length(weights))
     bounds$upper = rep(max(x),length(weights))
  }
  
  # get num of args
  nparams = length(bounds$lower)/length(weights)
  #show(nparams)
  
  # optim procedure
  parmin=initialParams
  valmax = -Inf
  
  if(optimMethod=="nlm"){
     optim = nlm(f=f,p=initialParams,print.level=2,iterlim = 1000)
     parmin=optim$estimate
  }
  
  if(optimMethod=="optim"){
    mi = optim(par=initialParams,fn = f,method="SANN"#"L-BFGS-B"
               #,lower = paramsBounds$lower,upper = paramsBounds$upper
               ,control = list(trace=2,maxit=50000))
    parmin = mi$par
  }
  
  if(optimMethod=="ga"){
    show(paste0("Optimisation with GA (",iters.max," iterations)"))
    show(bounds)
    optim = ga(type="real-valued",
               fitness=function(x){-f(x)},
               min=bounds$lower,
               max=bounds$upper,
               maxiter=iters.max,
               popSize = 1000,parallel = 1
    )
    parmin=optim@solution
    valmax = optim@fitness
  }
  
  # return parameters, fit
  
  res$parameters = parmin
  res$fittedHist = vals(parmin)
  res$valmax = max(valmax)
  
  
  return(res)
}



#''
#' General function estimating parameter for homogenous kernels (tested : gaussian and log-normal)
#'
estimateParameters<-function(id,income,structure,year,iters.max=1000,
                             structure_col_names=c("ART","CAD","INT","EMP","OUV"),
                             csp_ordered=c("EMP","OUV","INT","ART","CAD"),
                             idcol='IRIS'
                             ){
  income_col_names=paste0(c("RFUCD1","RFUCD2","RFUCD3","RFUCD4","RFUCQ2","RFUCD6","RFUCD7","RFUCD8","RFUCD9"),year)
  distr = c(unlist(income[income[[idcol]]==id,income_col_names]))
  
  # compute only for full distribs
  if(length(which(is.na(distr)))==0){
    shares = c(unlist(structure[structure[[idcol]]==id,structure_col_names]))
    shares=shares/sum(shares)
    
    # gaussian fit
    initialParams = rep(c(median(distr,na.rm=T),1000),length(shares))
    paramsBounds = list(lower=rep(c(min(distr,na.rm=T),100),length(shares)),upper=rep(c(max(distr,na.rm=T),10000),length(shares)))
    h=quantilesToHist(distr)
    resgaussian = inverseKernels(histogram=h,
                       weights = shares,
                       ker = gaussianKernel(),
                       initialParams = initialParams,
                       paramsBounds=paramsBounds,
                       iters.max = iters.max
    )
    
    # log normal fit
    initialParams = rep(c(median(log(distr),na.rm=T),1),length(shares))
    paramsBounds = list(lower=rep(c(min(log(distr),na.rm=T),0.1),length(shares)),upper=rep(c(max(log(distr),na.rm=T),2),length(shares)))
    h=quantilesToHist(distr)
    reslognormal = inverseKernels(histogram =h,
                         weights = shares,
                         ker = logNormalKernel(),
                         initialParams = initialParams,
                         paramsBounds=paramsBounds,
                         iters.max = iters.max
    )
    
    if(resgaussian$valmax > reslognormal$valmax){
      distrib = "gaussian"
      res = resgaussian
      avincome = resgaussian$parameters[seq(from=1,to=length(resgaussian$parameters),by=2)]
      medincome = resgaussian$parameters[seq(from=1,to=length(resgaussian$parameters),by=2)]
      stdincome = resgaussian$parameters[seq(from=2,to=length(resgaussian$parameters),by=2)]
      ord = order(medincome);
    }else{
      distrib = "lognormal"
      res = reslognormal
      mu = reslognormal$parameters[seq(from=1,to=length(reslognormal$parameters),by=2)]
      sigma = reslognormal$parameters[seq(from=2,to=length(reslognormal$parameters),by=2)]
      avincome = exp(mu + sigma^2/2)
      medincome = exp(mu)
      stdincome = sqrt(exp(2*mu + sigma^2)*(exp(sigma^2)-1))
      ord = order(medincome);
    }
  
   medincome=medincome[ord];avincome=avincome[ord];stdincome=stdincome[ord];shares=shares[ord]
   names(avincome)=csp_ordered;names(medincome)=csp_ordered;names(stdincome)=csp_ordered;names(shares)=csp_ordered

   #show(csp_ordered[ord])
   #show("--------------")
   #show(medincome)
   #show("--------------")
   #show(medincome[csp_ordered])
   #show(ord)
   #show("****************")

    res$avincome = avincome
    res$medincome = medincome
    res$stdincome = stdincome
    res$shares = shares
    res$distrib = distrib
    
    res$gaussianvalmax = resgaussian$valmax
    res$lognormalvalmax = reslognormal$valmax
    
    res$id = id
    
    return(res)
  }else{return(list(avincome=rep(0,5),medincome=rep(0,5),stdincome=rep(0,5),shares=rep(0,5),distrib="",gaussianvalmax=0,lognormalvalmax=0,id=""))}
}



#'
#' gaussian kernel of fixed width
fixedSdGaussian<-function(sigma=1){
  return(function(x,mu){1/sqrt(2*pi*sigma)*exp(-(x-mu)^2/(2*sigma^2))})
}

gaussianKernel<-function(){
  return(function(x,pars){mu=pars[1];sigma=pars[2];return(1/sqrt(2*pi*sigma)*exp(-(x-mu)^2/(2*sigma^2)))})
}

logNormalKernel<-function(){
  return(function(x,pars){
    mu=pars[1];sigma=pars[2];
    return(1/sqrt(2*pi*sigma*x)*exp(-(log(x)-mu)^2/(2*sigma^2)))
  })
}

#plot((1:1000)/100,logNormalKernel()((1:1000)/100,c(9.085910,6.907755)))

#'
#' transform vector of quantiles to an histogram object
#'   ! assuming q > 0
quantilesToHist<-function(q){
  mids=c();density=c()
  a = 1/length(q)
  qq=c(0,q)
  for(i in 1:length(q)){mids = append(mids,(qq[i]+qq[i+1])/2);density=append(density,a/(qq[i+1]-qq[i]))}
  res=list()
  res$mids=mids;res$density=density
  return(res)
}


plotRes <- function(res){
  # if(max(res$histogram$density)>max(res$fittedHist)){
  #   plot(res$histogram$mids,res$histogram$density,type='l')
  #   points(res$histogram$mids,res$fittedHist,type='l',col='blue')
  # }else{
  #   plot(res$histogram$mids,res$fittedHist,type='l',col='blue')
  #   points(res$histogram$mids,res$histogram$density,type='l')
  # }
  ylim = c(min(min(res$histogram$density),min(res$fittedHist)),max(max(res$histogram$density),max(res$fittedHist)))
  xlim = c(min(min(res$histogram$mids),min(res$parameters)),max(max(res$histogram$mids),max(res$parameters)))

  plot(res$histogram$mids,res$fittedHist,type='l',col='blue',xlim=xlim,ylim=ylim)
  points(res$histogram$mids,res$histogram$density,type='l')  
  
  for(p in 1:length(res$parameters)){
    abline(v=res$parameters[p],col='red');
  }
}




#'
#' specific function to load income data
getIncome <- function(year,idcol='IRIS'){
  incfile = paste0('data/revenus',ifelse(idcol=='COM',paste0('_com',year),year),'.csv')
  return(as.tbl(read.csv(file=incfile,sep=';',stringsAsFactors = F,header = T,dec = ',' )))
}

#'
#' structure
getStructure <- function(year,cols=c('ART','CAD','INT','EMP','OUV'),idcol="IRIS"){
  
  # change income file name in case of a commune aggregation
  incfile = paste0('data/revenus',ifelse(idcol=='COM',paste0('_com',year),year),'.csv')
  
  income = as.tbl(read.csv(file=incfile,sep=';',stringsAsFactors = F,header = T,dec = ',' ))
  structure99 = read.csv(file='data/structure99.csv',sep=';',stringsAsFactors = F);
  if(idcol=='COM'){structure99=as.data.frame(as.tbl(structure99)%>%group_by(COM)%>%summarize(ART=sum(ART),CAD=sum(CAD),INT=sum(INT),EMP=sum(EMP),OUV=sum(OUV)))}
  rownames(structure99)=structure99[[idcol]]
  structure07 = read.csv(file='data/structure07.csv',sep=';',stringsAsFactors = F);
  if(idcol=='COM'){structure07=as.data.frame(as.tbl(structure07)%>%group_by(COM)%>%summarize(ART=sum(ART),CAD=sum(CAD),INT=sum(INT),EMP=sum(EMP),OUV=sum(OUV)))}
  rownames(structure07)=structure07[[idcol]]
  structure11 = read.csv(file='data/structure11.csv',sep=';',stringsAsFactors = F);
  if(idcol=='COM'){structure11=as.data.frame(as.tbl(structure11)%>%group_by(COM)%>%summarize(ART=sum(ART),CAD=sum(CAD),INT=sum(INT),EMP=sum(EMP),OUV=sum(OUV)))}
  rownames(structure11)=structure11[[idcol]]
  
  numyear = as.numeric(paste0('20',year))
  
  if(numyear > 1999&numyear <= 2007){
    leftst = structure99[as.character(income[[idcol]]),];rightst = structure07[as.character(income[[idcol]]),]
    leftyear=1999;rightyear = 2007
  }
  if(numyear > 2007&numyear <= 2011){
    leftst = structure07[as.character(income[[idcol]]),];rightst = structure11[as.character(income[[idcol]]),]
    leftyear=2007;rightyear = 2011
  }
  
  rownames(leftst)<-as.character(income[[idcol]])
  leftst[which(apply(leftst,1,function(r){length(which(is.na(r)))>0})),]<-rep(0,ncol(leftst));leftst[[idcol]]=as.character(income[[idcol]])
  rownames(rightst)<-as.character(income[[idcol]])
  rightst[which(apply(rightst,1,function(r){length(which(is.na(r)))>0})),]<-rep(0,ncol(rightst));rightst[[idcol]]=as.character(income[[idcol]])
  
  structure = (rightst[,cols] - leftst[,cols]) / (rightyear - leftyear) * numyear + leftst[,cols] - leftyear * (rightst[,cols] - leftst[,cols]) / (rightyear - leftyear) 
  structure[[idcol]] = as.character(income[[idcol]])
  
  return(structure)
  
}




