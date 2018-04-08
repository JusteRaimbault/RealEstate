
########################
#'
#' Inverse problem Kernel Mixture reconstruction
#'

library(GA)
library(MASS)

###############
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
  show(nparams)
  
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
               popSize = 1000,parallel = 4
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

plot(logNormalKernel()((1:1000)/100,c(0,1)))

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








