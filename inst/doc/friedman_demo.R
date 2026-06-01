## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_chunk$set(fig.width = 6, fig.height = 6)

## ----setup--------------------------------------------------------------------
library(mvBayes)
library(BASS)

## ----data---------------------------------------------------------------------
f<-function(x){
  10*sin(pi*x[,1]*x[,2])+20*(x[,3]-.5)^2+10*x[,4]+5*x[,5]
}

sigma<-1
nfunc = 50
tt = seq(0, 1, length.out = nfunc)  # functional variable grid
n = 500  # sample size
p = 9  # number of predictors other (only 4 are used)
X<-matrix(runif(n*p),n,p) # 9 non-functional variables, only first 4 matter
x<-cbind(rep(tt,each=n),kronecker(rep(1,nfunc),X)) # to get y
Y<-matrix(f(x),nrow=n)+rnorm(n*nfunc,0,sigma)

ntest = 1000
Xtest = matrix(runif(ntest * p), ntest, p)
x<-cbind(rep(tt,each=ntest),kronecker(rep(1,nfunc),Xtest)) # to get y
Ytest = matrix(f(x),nrow=ntest)+rnorm(ntest*nfunc,0,sigma)

## ----bass---------------------------------------------------------------------
mod = mvBayes(
  bass,
  X,
  Y,
  nBasis=3
)
plot(mod)

## ----pca----------------------------------------------------------------------
plot(mod$basisInfo, idxMV = tt, xlabel = "tt") 

## ----traceplot----------------------------------------------------------------
traceplot(mod)

## ----train--------------------------------------------------------------------
plot(mod, idxMV = tt, xlabel = "tt")

## ----test---------------------------------------------------------------------
plot(mod, Xtest = Xtest, Ytest = Ytest, idxMV = tt, xlabel = "tt")

## ----sobol--------------------------------------------------------------------
modSensitivity = mvSobol(mod)
plot(modSensitivity, idxMV = tt, xlabel = "tt")

## ----posterior----------------------------------------------------------------
# All posterior predictive samples
Ytest_postSamples = predict(mod, Xtest)
# Posterior predictive mean
Ytest_postMean = apply(Ytest_postSamples, 2, mean)
# single posterior predictive sample (from MCMC iteration #429)
Ytest_postSample429 = predict(mod, Xtest, idxSamples = 429)

