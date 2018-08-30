# artificial HMM with 8 states and 9 emissions
# estimate from multiple observation samples with HMM by
# concatenating to one observation sample
# --doesn't work
rm(list=ls())
library(HMM)
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
# chunk size for incremental training of Baum-Welch
chunkSize<-1000
# sample size of random variates used for generating artificial probabilities
n<-1000
# no. iterations for Baum-Welch (per chunk)
nIter<-10
# delta for convergence of Baum-Welch (progress after 0.01 is very slow here)
bwDelta<-0.01
# no. test cases for viterbi
nv<-1000
# no. cores to use in parallel
NCORES<-18
# no. trials to average performance over (parallelize this)
nt<-NCORES
#accTrials<-numeric(nt)

library(doParallel)
registerDoParallel(cores=NCORES)

accTrials <- foreach(ti=1:nt, .packages=c('HMM','zoo')) %dopar% {
#for(ti in 1:nt){
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
  
  # construct an emission matrix using quantiles of X~Beta
  emat<-matrix(NA,ns,ne)
  for (row in 1:ns) {
    a<-runif(1)*2
    b<-runif(1)*2
    x<-rbeta(n,a,b)
    q<-quantile(x,seq(0,1,by=1/ne))
    p<-q[2:(ne+1)]-q[1:ne]
    emat[row,]<-p
    emat[row,ne]<-1-sum(emat[row,1:(ne-1)])
  }
  # print(round(emat,3))
  
  # generate samples from HMM
  # obs<-matrix(NA,m,nm)
  # for (i in 1:m) {
  #   # initial state
  #   s<-min((1:ns)[runif(1)<cumsum(p1)])
  #   for (j in 1:nm){
  #     # obs
  #     obs[i,j]<-min((1:ne)[runif(1)<cumsum(emat[s,])])
  #     # next state
  #     s<-min((1:ns)[runif(1)<cumsum(tmat[s,])])
  #   }
  # }
  hmm1<-initHMM(States=states,Symbols=symbols,
                startProbs=p1,transProbs=tmat,emissionProbs=emat)
  obs<-matrix(NA,m,nm)
  for (i in 1:m) {
    obs[i,]<-simHMM(hmm1,nm)$observation
  }
  
  ## random initialization of Baum-Welch (to avoid local optima)
  # construct an initial state distn using quantiles of X~Beta
  p10<-numeric(ns)
  a<-runif(1)
  b<-runif(1)
  x<-rbeta(n,a,b)
  q<-quantile(x,seq(0,1,by=1/ns))
  p10<-q[2:(ns+1)]-q[1:ns]
  p10[ns]<-1-sum(p10[1:(ns-1)])
  
  # construct a transition matrix using quantiles of X~Beta
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
  
  # construct an emission matrix using quantiles of X~Beta
  emat0<-matrix(NA,ns,ne)
  for (row in 1:ns) {
    a<-runif(1)*2
    b<-runif(1)*2
    x<-rbeta(n,a,b)
    q<-quantile(x,seq(0,1,by=1/ne))
    p<-q[2:(ne+1)]-q[1:ne]
    emat0[row,]<-p
    emat0[row,ne]<-1-sum(emat0[row,1:(ne-1)])
  }
  
  hmm10<-initHMM(States=as.character(1:ns),Symbols=as.character(1:ne),
                 startProbs=p10,transProbs=tmat0,emissionProbs=emat0)
  
  # # estimate HMM
  # bw<-list()
  # for (i in 1:m) {
  #   bw[[i]] = baumWelch(hmm10,obs[i,])
  # }
  # # ... fails
  
  
  #baumWelch(hmm10,simHMM(hmm1,m)$observation,maxIterations=10)
  
  # as a hack: chain the samples together by matching end and start obs
  # (and hope that on average the hidden states will match at least some of the time)
  rowsUsed1<-numeric(0)
  obsHack1<-obs[1,]
  rowsUsed1<-c(rowsUsed1,1)
  ok<-T
  while (ok) {
    endState<-obsHack1[length(obsHack1)]
    rowsLeft<-setdiff(1:m,rowsUsed1)
    nRowsLeft<-length(rowsLeft)
    ok<-F
    for (i in 1:nRowsLeft){
      if(obs[rowsLeft[i],1]==endState){
        obsHack1<-c(obsHack1,obs[rowsLeft[i],2:nm])
        rowsUsed1<-c(rowsUsed1,rowsLeft[i])
        ok<-T
        break
      }
    }
  }
  # print(length(rowsUsed1))
  
  # Baum-Welch on the hacked sequence
  # pseudo-online method to use only as many training observations as are
  # required (otherwise this algorithm is very slow)
  for (i in 1:floor(m/chunkSize)){
    bw1<-baumWelch(hmm10,obsHack1[((i-1)*chunkSize+1):(i*chunkSize)],
                   maxIterations=nIter*i,delta=bwDelta)
    if(bw1$difference[length(bw1$difference)]<bwDelta)
      break
    hmm10<-bw1$hmm
  }
  
  # # compare initial state probs
  # print(round(data.frame(actual=p1, start=p10, estimated=bw1$hmm$startProbs, row.names=states),3))
  # 
  # # compare transition probs
  # tmatRes<-data.frame(from=character(0),to=character(0),
  #                     actual=numeric(0),start=numeric(0),estimated=numeric(0))
  # for (i in 1:ns){
  #   for (j in 1:ns){
  #     tmatRes<-rbind(tmatRes,
  #             data.frame(from=as.character(i),to=as.character(j),
  #                        actual=round(tmat[i,j],3),start=round(tmat0[i,j],3),
  #                        estimated=round(bw1$hmm$transProbs[i,j],3)))
  #   }
  # }
  # print(tmatRes,row.names=F)
  # plot(tmatRes$actual,tmatRes$estimated,type="p")
  # abline(lm(estimated~actual-1,data=tmatRes),col="red")
  # 
  # # compare emission probs
  # ematRes<-data.frame(state=character(0),symbol=character(0),
  #                     actual=numeric(0),start=numeric(0),estimated=numeric(0))
  # for (i in 1:ns){
  #   for (j in 1:ne){
  #     ematRes<-rbind(ematRes,
  #                    data.frame(state=as.character(i),symbol=as.character(j),
  #                               actual=round(emat[i,j],3),start=round(emat0[i,j],3),
  #                               estimated=round(bw1$hmm$emissionProbs[i,j],3)))
  #   }
  # }
  # print(ematRes,row.names=F)
  # plot(ematRes$actual,ematRes$estimated,type="p")
  # abline(lm(estimated~actual-1,data=ematRes),col="red")
  
  # # randomly permute the sample and repeat
  # obs<-obs[sample(m,m),]
  # rowsUsed2<-numeric(0)
  # obsHack2<-obs[1,]
  # rowsUsed2<-c(rowsUsed2,1)
  # ok<-T
  # while (ok) {
  #   endState<-obsHack2[length(obsHack2)]
  #   rowsLeft<-setdiff(1:m,rowsUsed2)
  #   nRowsLeft<-length(rowsLeft)
  #   ok<-F
  #   for (i in 1:nRowsLeft){
  #     if(obs[rowsLeft[i],1]==endState){
  #       obsHack2<-c(obsHack2,obs[rowsLeft[i],2:nm])
  #       rowsUsed2<-c(rowsUsed2,rowsLeft[i])
  #       ok<-T
  #       break
  #     }
  #   }
  # }
  
  # generate a state-observation sequence from the true HMM
  # and use Viterbi to infer the states from the observations with the
  # estimated HMM
  acc<-numeric(nv)
  for (i in 1:nv){
    actualSeq<-simHMM(hmm1,nm)
    predStates<-viterbi(bw1$hmm,actualSeq$observation)
    acc[i]<-sum(actualSeq$states==predStates)/nm
  }
  # hist(acc,breaks=c(0,seq(from=1/(2*nm),by=1/nm,length.out=nm),1))
  # tab<-table(acc)
  # plot(as.numeric(names(tab))*12,tab,type="h",
  #      xlab=paste("No. Correct States (out of ",nm,")",sep=""),ylab="Freq")
  # abline(v=nm/ns,col="red")
  # text(nm/ns,max(tab),labels="Random Guessing",adj=c(0.5,1),col="red")
  #accTrials[ti]<-mean(acc)
  #save(accTrials,file="/wps_scratch/usr_scratch/burbidge/us/accTrials.RData")
  mean(acc) 
  # (this is the value of the foreach block, and will be added to the output list)
}
save(accTrials,file="/wps_scratch/usr_scratch/burbidge/us/accTrials.RData")
