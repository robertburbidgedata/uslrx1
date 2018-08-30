# artificial HMM with 8 states and 3 emissions
# estimate from multiple observation samples with mhsmm
rm(list=ls())
library(mhsmm)
library(zoo)
#source('~/R/oracle_us.R')

# states: {susceptibility to marketing, received sample, engagement} : 2 x 2 x 2
ns<-8
states<-as.character(1:ns)
# emissions (symbols): {low, medium, high}
ne<-3
symbols<-as.character(1:ne)
# length of each sample from the HMM (number of months)
load("/wps_scratch/usr_scratch/burbidge/us/data_month.RData")
nm<-length(data_month)
nm<-24
# no. samples from HMM (no. HCPs prescribing PRADAXA in Sep17)
prd<-"PRADAXA"
mth<-data_month[nm]
datamonth<-format(as.Date(paste(substr(as.character(mth),1,7),"01",sep="-")),"%Y%m%d")
query<-paste(
  "select count(distinct(IMS_ID)) ",
  "from NAPPS.CORP_RX_DATA ",
  "where DATA_MONTH='",datamonth,"' ",
  paste("and PRODUCT_GROUP_NAME='",prd,"'",sep=""),
  sep=""
)
# res <- dbSendQuery(con, query)
# m <- fetch(res, n = -1)
# (this query was executed in SQL Developer and the result copied)
m<-53874
# sample size of random variates used for generating artificial probabilities
n<-1000
# no. iterations for fitting HMM
maxit<-100
# convergence tolerance for fitting HMM
tol<-1e-8
# no. test cases for viterbi
nv<-m
# no. cores to use in parallel
NCORES<-18
# no. trials to average performance over
nt<-NCORES
# accTrials<-numeric(nt)

library(doParallel)
registerDoParallel(cores=NCORES)

accTrials <- foreach(ti=1:nt, .packages=c('mhsmm','zoo')) %dopar% {
  # construct an initial state distn using quantiles of X~Beta
  p1<-numeric(ns)
  a<-runif(1)
  b<-runif(1)
  x<-rbeta(n,a,b)
  q<-quantile(x,seq(0,1,by=1/ns))
  p1<-q[2:(ns+1)]-q[1:ns]
  p1[ns]<-1-sum(p1[1:(ns-1)])
  # print(round(p1,3))
  
  # construct a transition matrix using quantiles of X~Beta
  tmat<-matrix(NA,ns,ns)
  for (row in 1:ns) {
    a<-runif(1)*2
    b<-runif(1)*2
    x<-rbeta(n,a,b)
    q<-quantile(x,seq(0,1,by=1/ns))
    p<-q[2:(ns+1)]-q[1:ns]
    tmat[row,]<-p
    tmat[row,ns]<-1-sum(tmat[row,1:(ns-1)])
  }
  # print(round(tmat,3))
  
  # construct a Poisson emission distribution 
  # (doesn't correspond to fixed no. emissions ...)
  b <- list(lambda=floor(runif(ns,1,1+ns)))
  
  # artificial HMM
  model <- hmmspec(init=p1, trans=tmat, parms.emis=b, dens.emis = dpois.hsmm)
  
  # simulate observation sequences
  train = simulate(model, rep(nm,m), rand.emis = rpois.hsmm)
  
  # starting state for optimization (random in case of local optima)
  p10<-numeric(ns)
  a<-runif(1)
  b<-runif(1)
  x<-rbeta(n,a,b)
  q<-quantile(x,seq(0,1,by=1/ns))
  p10<-q[2:(ns+1)]-q[1:ns]
  p10[ns]<-1-sum(p10[1:(ns-1)])
  
  tmat0<-matrix(NA,ns,ns)
  for (row in 1:ns) {
    a<-runif(1)*2
    b<-runif(1)*2
    x<-rbeta(n,a,b)
    q<-quantile(x,seq(0,1,by=1/ns))
    p<-q[2:(ns+1)]-q[1:ns]
    tmat0[row,]<-p
    tmat0[row,ns]<-1-sum(tmat0[row,1:(ns-1)])
  }
  
  b0 <- list(lambda=floor(runif(ns,1,1+ns)))
  
  startval <- hmmspec(init=p10, trans=tmat0, parms.emis=b0, dens.emis=dpois.hsmm)
  
  # fit the HMM
  h1 = hmmfit(train, startval, mstep=mstep.pois,tol=tol,maxit=maxit)
  # plot(h1$loglik, type = "b", ylab = "Log-likelihood", xlab = "Iteration")
  
  test <- simulate(model, nsim=rep(nm,nv), rand.emis=rpois.hsmm)
  yhat <- predict(h1, test)
  mean(yhat$s==test$s)
  # (this is the value of the foreach block, and will be added to the output list)
}
save(accTrials,file="/wps_scratch/usr_scratch/burbidge/us/accTrials2.RData")

mean(unlist(accTrials))
sd(unlist(accTrials))/sqrt(nt)

