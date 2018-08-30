# markov.R 
rm(list=ls())

source('~/R/oracle_us.R')
library(markovchain)
library(igraph)
library(pracma)

# months on LRX
load(file="/wps_scratch/usr_scratch/burbidge/us/data_month.RData")
print(data_month)
(nm<-length(data_month))

# SWITCH_TO matrices by month, normalized to transition probability estimates
marmat<-list()
marmatn<-list()
for (mthi in 1:nm){
  mth<-data_month[mthi]
  
  # products on LRX: FROM
  query<-paste(
    "select distinct(FROM_PROD_BRAND_NAME) FROM_PROD_BRAND_NAME ",
    "from NAPPS.CORP_LRX_CURR_SWITCH_DETAILS ",
    "where DATA_DATE=date '",as.character(mth),"'",
    sep=""
  )
  res <- dbSendQuery(con, query)
  from_prod_brand_name <- fetch(res, n = -1)
  from_prod_brand_name<-from_prod_brand_name[order(from_prod_brand_name),]
  print(from_prod_brand_name)
  (nfp<-length(from_prod_brand_name))
  
  # products on LRX: TO
  query<-paste(
    "select distinct(TO_PROD_BRAND_NAME) TO_PROD_BRAND_NAME ",
    "from NAPPS.CORP_LRX_CURR_SWITCH_DETAILS ",
    "where DATA_DATE=date '",as.character(mth),"'",
    sep=""
  )
  res <- dbSendQuery(con, query)
  to_prod_brand_name <- fetch(res, n = -1)
  to_prod_brand_name<-to_prod_brand_name[order(to_prod_brand_name),]
  print(to_prod_brand_name)
  (ntp<-length(to_prod_brand_name))
  
  # sum(SWITCH_TO) FROM-TO
  query<-paste(
    "select FROM_PROD_BRAND_NAME, TO_PROD_BRAND_NAME, sum(SWITCH_TO) SWITCH_TO ",
    "from NAPPS.CORP_LRX_CURR_SWITCH_DETAILS ",
    "where DATA_DATE=date '",as.character(mth),"' ",
    "group by FROM_PROD_BRAND_NAME, TO_PROD_BRAND_NAME ",
    sep=""
  )
  res <- dbSendQuery(con, query)
  marmat2 <- fetch(res, n = -1)
  
  # rearrange to matrix
  marmat[[mth]]<-matrix(NA,nrow=nfp,ncol=ntp,dimnames=list(from_prod_brand_name,to_prod_brand_name))

  for (i in 1:nfp){
    for (j in 1:ntp){
      val <- marmat2[marmat2$FROM_PROD_BRAND_NAME==from_prod_brand_name[i]&
                       marmat2$TO_PROD_BRAND_NAME==to_prod_brand_name[j],"SWITCH_TO"]
      marmat[[mth]][i,j] <- ifelse(length(val)==0,0,val)
    }
  }
  write.csv(marmat[[mth]],
            paste("/icebox_data/data/tmp/burbidge/us/marmatmthly/",as.character(mth),".csv",sep=""))
  
  # normalize
  marmatn[[mth]]<-matrix(NA,nrow=nfp,ncol=ntp,dimnames=list(from_prod_brand_name,to_prod_brand_name))
  marmatn[[mth]]<-marmat[[mth]]/as.data.frame(rep(as.data.frame(apply(marmat[[mth]],1,sum)),ntp))
  dimnames(marmatn[[mth]])<-list(from_prod_brand_name,to_prod_brand_name)
  write.csv(marmatn[[mth]],
            paste("/icebox_data/data/tmp/burbidge/us/marmatmthly/",as.character(mth),"n.csv",sep=""))
}

save(marmat,file="/wps_scratch/usr_scratch/burbidge/us/marmat.RData")
save(marmatn,file="/wps_scratch/usr_scratch/burbidge/us/marmatn.RData")

load("/wps_scratch/usr_scratch/burbidge/us/marmat.RData")
load("/wps_scratch/usr_scratch/burbidge/us/marmatn.RData")

# JARDIANCE
# tmp1<-marmat2[marmat2$FROM_PROD_BRAND_NAME=="JARDIANCE",c("TO_PROD_BRAND_NAME","SWITCH_TO")]
# head(tmp1[order(-tmp1$SWITCH_TO),])

# overall FROM and TO prod_brand_names
from_prod_brand_name<-vector("character",0)
to_prod_brand_name<-vector("character",0)
for (mthi in 1:nm){
  mth<-data_month[mthi]
  from_prod_brand_name<-union(from_prod_brand_name,rownames(marmatn[[mth]]))
  to_prod_brand_name<-union(to_prod_brand_name,colnames(marmatn[[mth]]))
}
(nfp<-length(from_prod_brand_name))
(ntp<-length(to_prod_brand_name))
prod_brand_name <- union(from_prod_brand_name,to_prod_brand_name)
(np<-length(prod_brand_name))

# tests for constancy of transition probabilities
marmatconst<-matrix(NA,nrow=np,ncol=np,dimnames=list(prod_brand_name,prod_brand_name))
for (i in 1:np) {
  for (j in 1:np) {
    sample1<-numeric(0)
    for (mthi in 1:nm){
      mth<-data_month[mthi]
      if (prod_brand_name[i] %in% rownames(marmatn[[mth]]) & 
          prod_brand_name[j] %in% colnames(marmatn[[mth]]))
        sval<-marmatn[[mth]][prod_brand_name[i],prod_brand_name[j]]
      else
        sval<-0 # would be more accurate to look at TRX to see if this actually 0, or NA
      sample1<-c(sample1,sval)
    }
    x<-1:nm
    marmatconst[i,j]<-summary(lm(sample1~x))$coefficients["x","Pr(>|t|)"]
  }
}

save(marmatconst,file="/wps_scratch/usr_scratch/burbidge/us/marmatconst.RData")
load("/wps_scratch/usr_scratch/burbidge/us/marmatconst.RData")

write.csv(marmatconst,"/icebox_data/data/tmp/burbidge/us/marmatconst.csv")
write.csv(prod_brand_name,"/icebox_data/data/tmp/burbidge/us/prod_brand_name.csv")

# reshape as vector for tests
marmatconst1 <- marmatconst[1:(np*np)]
marmatconst1 <- marmatconst1[!is.nan(marmatconst1)]

ks.test(unique(marmatconst1),"punif",0,1)$p.value
mean(marmatconst1<0.01)
hist(marmatconst1,xlab='p',
 main='Frequency Histogram of Probability (p) \nthat Transition Matrix Probability is non-constant'
 ,sub='Kolmogorov-Smirnoff rejects p ~ U[0,1], ~14% of entries have strong evidence of trend')

# EWMA to estimate Markov transition matrix assuming homogeneous
# (compensates for the fact that matrix is not quite constant and weights more
#  recent events more highly)
# library(qcc)
# marmatnewma<-matrix(NA,nrow=nfp,ncol=ntp,dimnames=list(from_prod_brand_name,to_prod_brand_name))
# for (i in 1:nfp) {
#   for (j in 1:ntp) {
#     sample1<-numeric(0)
#     for (mthi in 1:nm){
#       mth<-data_month[mthi]
#       if (from_prod_brand_name[i] %in% rownames(marmatn[[mth]]) & 
#           to_prod_brand_name[j] %in% colnames(marmatn[[mth]]))
#         sval<-marmatn[[mth]][from_prod_brand_name[i],to_prod_brand_name[j]]
#       else
#         sval<-0
#       sample1<-c(sample1,sval)
#     }
#     x<-1:nm
#     marmatnewma[i,j]<-ewmaSmooth(x,sample1)$y[nm]
#   }
# }
# 
# write.csv(marmatnewma,"/icebox_data/data/tmp/burbidge/us/marmatnewma.csv")

## define a markovchainList with the twelve transition matrices

## NB. since we are looking at SWITCH_TO it is impossible for a product
## to switch to itself
## i.e., all probabilities are conditional on there having been a switch
## and any final estimated probabilities should be conditioned on the prior

# need to add null states from prod_brand_name to the marmatn
# and pad with zeros (or NAs ...)
# this may look like a ridiculously inefficient way to do this
# but R isn't Matlab, or even Python, and is atrocious at handling
# matrices (and I have tried all the documented indexing options)
el<-matrix(NA,nrow=np,ncol=np,dimnames=list(prod_brand_name,prod_brand_name))
marmatnfull<-rep(list(el),nm)
for (i in 1:np) {
  for (j in 1:np) {
    for (mthi in 1:nm){
      mth<-data_month[mthi]
      if (prod_brand_name[i] %in% rownames(marmatn[[mth]]) &
          prod_brand_name[j] %in% colnames(marmatn[[mth]]))
        marmatnfull[[mthi]][prod_brand_name[i],prod_brand_name[j]]<-
          marmatn[[mth]][prod_brand_name[i],prod_brand_name[j]]
    }
  }
}

# fill in for non-existent states each month
for (mthi in 1:nm){
  mth<-data_month[mthi]
  # again, convert matrix to df to do anything then cast back
  tmpdf<-data.frame(marmatnfull[[mthi]])
  tmpdf[is.na(tmpdf)]<-0
  marmatnfull[[mthi]]<-as.matrix(tmpdf)
  # colnames undergo tr ' ' '.' during this conversion, so correct
  dimnames(marmatnfull[[mthi]])<-list(prod_brand_name,prod_brand_name)
  # ensure rows sum to 1 for non-existent states in each month
  # by putting prob 1 on isolated state (SWITCH_TO itself)
  for (i in 1:np) {
    if(sum(marmatnfull[[mthi]][prod_brand_name[i],])==0)
      marmatnfull[[mthi]][prod_brand_name[i],prod_brand_name[i]]<-1
  }
}
  
# homogeneous analysis, examine transition matrix for month 12
mc12<-new("markovchain",states=prod_brand_name,
    byrow=T,transitionMatrix=marmatnfull[[mthi]],name=as.character(mth))

# work through the examples in the package documentation
states(mc12)
names(mc12)
dim(mc12)
name(mc12)
print(mc12)
show(mc12)
plot(mc12) # wrapper for igraph
tiff("mc12.tiff", width = 8, height = 8, units = 'in', res = 300)
plot(mc12)
dev.off()
# should be possible to inspect plot.markovchain to convert markovchain to graph
# and use tkplot ...
tkplot(as(mc12,"igraph"))
# ... but problems exporting display ...
plot(mc12,package="diagram",box.size=0.04)

# communicating classes and blocking
mc12cc<-communicatingClasses(mc12)
ccidx<-(1:length(mc12cc))[sapply(mc12cc,length)>1]
mc12blocks<-list()

# blocking
for (i in 1:length(ccidx)) {
  tmpdf<-as(mc12,"data.frame")
  tmpdf<-tmpdf[tmpdf$t0 %in% mc12cc[[ccidx[i]]] & tmpdf$t1 %in% mc12cc[[ccidx[i]]],]
  
  # rounding errors can lead to sum(p) slightly less than 1, so hack to avoid this
  # add the missing prob to p(i,i)
  for (j in 1:length(mc12cc[[ccidx[i]]])) {
    sump<-sum(tmpdf$prob[tmpdf$t0==mc12cc[[ccidx[i]]][j]])
    if (sump<1)
      tmpdf$prob[tmpdf$t0==mc12cc[[ccidx[i]]][j] & tmpdf$t1==mc12cc[[ccidx[i]]][j]] <-
        tmpdf$prob[tmpdf$t0==mc12cc[[ccidx[i]]][j] & tmpdf$t1==mc12cc[[ccidx[i]]][j]] + (1-sump)
  }
  
  mc12blocks[[i]]<-as(tmpdf,"markovchain")
}

# visualisation
for (i in 1:length(ccidx)) {
  plot(mc12blocks[[i]],edge.arrow.size=0.1)
}

## construct non-homogeneous Markov chain
mc<-list()
for (mthi in 1:nm){
  mth<-data_month[mthi]
  mc[[mthi]]<-new("markovchain",states=prod_brand_name,
          byrow=T,transitionMatrix=marmatnfull[[mthi]],name=as.character(mth))
}
mcList <- new("markovchainList", markovchains = mc, name = "US LRX")

# block over markovchainList
blockList<-list()
for (mthi in 1:nm){
  mth<-data_month[mthi]
  mccc<-communicatingClasses(mcList[[mthi]])
  ccidx<-(1:length(mccc))[sapply(mccc,length)>1]
  blockList[[mthi]]<-mccc[ccidx]
}
  
# no. blocks each month ...
sapply(blockList,length)
# ... three non-trivial blocks each month
nb<-3

# block sizes each month ...
sapply(blockList, function(el) sapply(el,length))
# ... consistent blocks over ordering

# create overall blocks as union over blockList
blocks<-list()
for (bi in 1:nb) {
  blocks[[bi]]<-list()
  for (mthi in 1:nm) {
    blocks[[bi]]<-union(blocks[[bi]],blockList[[mthi]][[bi]])
  }
  blocks[[bi]]<-unlist(blocks[[bi]])
}

# size of unions
sapply(blocks,length)

# disjoint?
intersect(blocks[[1]],blocks[[2]])
intersect(blocks[[1]],blocks[[3]])
intersect(blocks[[2]],blocks[[3]])
# ... yes

save(blocks,file="/wps_scratch/usr_scratch/burbidge/us/blocks.RData")
load("/wps_scratch/usr_scratch/burbidge/us/blocks.RData")

# construct list of markovchainLists corresponding to non-homogeneous chains for each block
mcBlocksList<-list()
for (bi in 1:nb){
  # block for each month
  mcB<-list()
  for (mthi in 1:nm){
    mth<-data_month[mthi]

    tmpdf<-as(mc[[mthi]],"data.frame")
    tmpdf<-tmpdf[tmpdf$t0 %in% blocks[[bi]] & tmpdf$t1 %in% blocks[[bi]],]
    
    # rounding errors can lead to sum(p) slightly less than 1, so hack to avoid this
    # add the missing prob to p(i,i) [i.e., the transition that can't happen]
    for (j in 1:length(blocks[[bi]])) {
      sump<-sum(tmpdf$prob[tmpdf$t0==blocks[[bi]][j]])
      if (sump<1)
        tmpdf$prob[tmpdf$t0==blocks[[bi]][j] & tmpdf$t1==blocks[[bi]][j]] <-
          tmpdf$prob[tmpdf$t0==blocks[[bi]][j] & tmpdf$t1==blocks[[bi]][j]] + (1-sump)
    }
    mcB[[mthi]]<-as(tmpdf,"markovchain")
  }
  mcBlocksList[[bi]]<-new("markovchainList", markovchains = mcB, name = paste("Block",bi))
}

# for each block, estimate the transition matrix over the whole twelve months
# the non-homogeneity is partly due to new products and discontinued products
# but largely due to changes in formulary
tmcy<-list()
for (bi in 1:nb){
  d<-length(blocks[[bi]])
  tmcy[[bi]]<-diag(1,d,d)
  dimnames(tmcy[[bi]])<-list(blocks[[bi]],blocks[[bi]])
  for (mthi in 1:nm){
    mth<-data_month[mthi]
    tmcy[[bi]]<-tmcy[[bi]]*mcBlocksList[[bi]][[mthi]]
  }
}

## NB. each step is conditioned on the event of there having been a switch
## should take 12th root of tmcy to estimate one-month switching probs
## with formulary effects averaged out
## tmcy[[bi]]<-rootm(tmcy[[bi]],nm)
## but the system is computationally singular for blocks 1 and 2 (see notes above)
## tried taking the geometric mean over the chain but this isn't a transition matrix
## tmcy[[bi]]<-tmcy[[bi]]%*%rootm(attributes(mcBlocksList[[bi]][[mthi]])$transitionMatrix,12)$B





# # round up diagonals to make rowsums 1
# # bit more hacky this time as rowsums could be >1 and the
# # original correction is subject to rounding errors itself
# for (bi in 1:nb){
#   for (j in 1:nrow(tmcy[[bi]])){
#     sump<-sum(tmcy[[bi]][j,])
#     if (sump!=1)
#       tmcy[[bi]][j,j]<-tmcy[[bi]][j,j]+(1-sump)
#   }
# }



# for blocks 2 and 3, the final state probabilities are almost independent of 
# starting state, which is to say, conditioned on the event that medication 
# was switched, the chosen medication after 12 months is a random sample

# E.g., anticoagulants
apply(tmcy[[3]],2,mean)
# this suggests that if a medication is switched (i.e., current one not effective)
# then final choice is independent of initial choice, and presumably independent of
# symptons, patient demographics, etc.

## need to discuss this with meds to better elucidate prescribing dynamics ...

## BI vs non-BI products
# there is a table BRAND_FEATURES in ICE DB but names are not the 
# same as in CORP_LRX tables, Plamen has some code that does the
# mapping, there doesn't seem to be any table that maps CORP_LRX
# prod_brand_name to IS_BI_BRAND, so we construct it manually
# !!this is not robust
brand_features<-as.data.frame(prod_brand_name,stringsAsFactors=F)
is_bi_brand<-c(0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	1,	1,	1,	1,	0,	0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	1,	0,	0,	0)
brand_features$is_bi_brand<-is_bi_brand
save(brand_features,file="/wps_scratch/usr_scratch/burbidge/us/brand_features.RData")

# for each block, subset rows of transition matrix on BI products
# subset columns on non-BI products, sum probs over rows
bi2nonbi<-list()
for (bi in 1:nb){
  birows<-rownames(tmcy[[bi]]) %in% brand_features$prod_brand_name[brand_features$is_bi_brand==1]
  bicols<-colnames(tmcy[[bi]]) %in% brand_features$prod_brand_name[brand_features$is_bi_brand==0]
  tmpmat<-matrix(tmcy[[bi]][birows,bicols],nrow=sum(birows),ncol=sum(bicols),
            dimnames=list(rownames(tmcy[[bi]])[birows],colnames(tmcy[[bi]])[bicols]))
  bi2nonbi[[bi]]<-apply(tmpmat,1,sum)
}
  
# do this just for last transition matrix (Sep 17)
bi2nonbi12<-list()
for (bi in 1:nb){
  birows<-states(mcBlocksList[[bi]][[nm]]) %in% brand_features$prod_brand_name[brand_features$is_bi_brand==1]
  bicols<-states(mcBlocksList[[bi]][[nm]]) %in% brand_features$prod_brand_name[brand_features$is_bi_brand==0]
  tmpmat<-matrix(mcBlocksList[[bi]][[nm]][birows,bicols],nrow=sum(birows),ncol=sum(bicols),
                 dimnames=list(states(mcBlocksList[[bi]][[nm]])[birows],
                               states(mcBlocksList[[bi]][[nm]])[bicols]))
  bi2nonbi12[[bi]]<-apply(tmpmat,1,sum)
}
print(bi2nonbi12)

#### CHECK!!
nonbi2bi12<-list()
for (bi in 1:nb){
  bicols<-states(mcBlocksList[[bi]][[nm]]) %in% brand_features$prod_brand_name[brand_features$is_bi_brand==1]
  birows<-states(mcBlocksList[[bi]][[nm]]) %in% brand_features$prod_brand_name[brand_features$is_bi_brand==0]
  tmpmat<-matrix(mcBlocksList[[bi]][[nm]][birows,bicols],nrow=sum(birows),ncol=sum(bicols),
                 dimnames=list(states(mcBlocksList[[bi]][[nm]])[birows],
                               states(mcBlocksList[[bi]][[nm]])[bicols]))
  nonbi2bi12[[bi]]<-apply(tmpmat,1,sum)
}
print(nonbi2bi12)

