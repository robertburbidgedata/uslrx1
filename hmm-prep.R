# hmm-prep.R
rm(list=ls())

library(zoo)
source('~/R/oracle_us.R')

load("/wps_scratch/usr_scratch/burbidge/us/data_month.RData")
nm<-length(data_month)
load("/wps_scratch/usr_scratch/burbidge/us/blocks.RData")
nb<-length(blocks)
load("/wps_scratch/usr_scratch/burbidge/us/brand_features.RData")

# check product names on CORP_RX_DATA
product_group_name_trxList<-list()
for (mthi in 1:nm){
  mth<-data_month[mthi]
  
  # convert mth to DATA_MONTH format
  datamonth<-format(as.Date(paste(substr(as.character(mth),1,7),"01",sep="-")),"%Y%m%d")
  
  query<-paste(
    "select PRODUCT_GROUP_NAME, sum(TOTAL_RX) TRX ",
    "from NAPPS.CORP_RX_DATA ",
    "where DATA_MONTH='",datamonth,"' ",
    "group by PRODUCT_GROUP_NAME ",
    "order by TRX desc",
    sep=""
  )
  res <- dbSendQuery(con, query)
  product_group_name_trxList[[datamonth]] <- fetch(res, n = -1)
}

save(product_group_name_trxList,
     file="/wps_scratch/usr_scratch/burbidge/us/product_group_name_trxList.RData")
load("/wps_scratch/usr_scratch/burbidge/us/product_group_name_trxList.RData")

# union of product_group_name over months
product_group_name<-vector("character",0)
for (mthi in 1:nm){
  mth<-data_month[mthi]
  datamonth<-format(as.Date(paste(substr(as.character(mth),1,7),"01",sep="-")),"%Y%m%d")
  product_group_name<-union(product_group_name
                            ,product_group_name_trxList[[datamonth]]$PRODUCT_GROUP_NAME)
}

# all prods in blocks in TRX?
for (bi in 1:nb)
  print(setdiff(blocks[[bi]],product_group_name))

# manually build a lookup from LRX prods to TRX prods
lrxprods<-list()
for(bi in 1:nb)
  lrxprods<-union(lrxprods,blocks[[bi]])
lrxprods<-unlist(lrxprods)
trxprods<-lrxprods
trxprods[trxprods=="ADVAIR DISKUS 100/50"]<-"ADVAIR DISK 100/50"
trxprods[trxprods=="ADVAIR DISKUS 250/50"]<-"ADVAIR DISK 250/50"
trxprods[trxprods=="ADVAIR DISKUS 500/50"]<-"ADVAIR DISK 500/50"
trxprods[trxprods=="BREO ELLIPTA 100-25 MCG"]<-"BREO ELLIPTA 100-25MCG"
trxprods[trxprods=="BREO ELLIPTA 200-25 MCG"]<-"BREO ELLIPTA 200-25MCG"
# not sure about this one ...
trxprods[trxprods=="PULMICORT FLEXHALER"]<-"PULMICORT"
trxprods[trxprods=="SYMBICORT 160-4.5 MCG"]<-"SYMBICORT 160-4.5MCG"
trxprods[trxprods=="SYMBICORT 80-4.5 MCG"]<-"SYMBICORT 80-4.5MCG"
# can't figure out what these prod names of LRX correspond to on TRX:
# "HUMAN GLP-1 ANALOGS", "NON INSULIN DIABETES - OTHER", "TZD" 
trxprods[trxprods=="HUMAN GLP-1 ANALOGS"]<-NA
trxprods[trxprods=="NON INSULIN DIABETES - OTHER"]<-NA
trxprods[trxprods=="TZD"]<-NA

# look-up table from LRX to TRX
rxlookupnames<-data.frame(lrxprods=lrxprods,trxprods=trxprods)
# NB. there are 80 prods on TRX not on LRX

## distn of no. (propn) of patients on a BI product (i.e., TRX) over HCPs
## by communicating class (block) by month
trx1<-list()
for (bi in 1:2) {
  trx1[[bi]]<-list()
  for (mthi in 1:nm) {
    
    # BI products (LRX) in block bi
    bi.prods.lrx<-
      brand_features[brand_features$prod_brand_name %in% blocks[[bi]]&brand_features$is_bi_brand==1,
                     "prod_brand_name"]
    bi.prods.trx<-as.character(rxlookupnames$trxprods[rxlookupnames$lrxprods %in% bi.prods.lrx])
    
    mth<-data_month[mthi]
    
    # convert mth to DATA_MONTH format
    datamonth<-format(as.Date(paste(substr(as.character(mth),1,7),"01",sep="-")),"%Y%m%d")
    
    # no. patients on a BI product by HCP
    # (CORP_RX_DATA is 1.5B rows with no indexes or partitions ...
    #  prob better to dump the whole thing to parquet/oc and do this in Spark ...)
    query<-paste(
      "select sum(TOTAL_RX) TRX ",
      "from NAPPS.CORP_RX_DATA ",
      "where DATA_MONTH='",datamonth,"' ",
      "and PRODUCT_GROUP_NAME in ", 
      paste("(",paste("'",bi.prods.trx,"'",sep="",collapse=","),") ",sep=""),
      "and IMS_ID in (select distinct(IMS_ID) from NAPPS.CORP_IMSID_TO_CURR_CMID sample(1)) ",      
      "group by IMS_ID",
      sep=""
    )
    res <- dbSendQuery(con, query)
    trx1[[bi]][[datamonth]] <- fetch(res, n = -1)
  }
}
#save(trx1,file="/wps_scratch/usr_scratch/burbidge/us/trx1.RData")
load("/wps_scratch/usr_scratch/burbidge/us/trx1.RData")

# boxplots for each block over months of log1p(TRX)
dfList1<-list()
for (bi in 1:nb) {
  # stack TRX, datamonth as data.frame
  dfList1[[bi]]<-data.frame(TRX=numeric(0),datamonth=character(0))
  for (mthi in 1:nm) {
    mth<-data_month[mthi]
    datamonth<-format(as.Date(paste(substr(as.character(mth),1,7),"01",sep="-")),"%Y%m%d")
    tmpdf<-data.frame(TRX=log1p(trx1[[bi]][[datamonth]]),
                      datamonth=substr(as.character(datamonth),1,6))
    dfList1[[bi]]<-rbind(dfList[[bi]],tmpdf)
  }
  boxplot(TRX~datamonth,dfList1[[bi]],range=1,varwidth=T,las=2,outline=F,
          xlab="",ylab="log(TRx)",
          main=paste("Distribution of No. Patients on a BI Product: Block",bi),
          sub="Median +/-2xIQR, Outliers not shown")
}

# boxplots aggregated over months of log1p(TRX) by block
df.trx.bi<-data.frame(TRX=numeric(0),block=numeric(0))
for (bi in 1:nb) {
  tmpdf<-data.frame(TRX=dfList1[[bi]]$TRX,block=bi)
  df.trx.bi<-rbind(df.trx.bi,tmpdf)
}
boxplot(TRX~block,df.trx.bi,range=1,varwidth=T,las=2,outline=F,
        xlab="",ylab="log(TRx)",
        main="Distribution of No. Patients on a BI Product (All months) by Block",
        sub="Median +/-2xIQR, Outliers not shown")

## distn of no. of patients on a product (i.e., TRX) over HCPs
## by communicating class (block) by month
trx2<-list()
for (bi in 1:nb) {
  trx2[[bi]]<-list()
  for (mthi in 1:nm) {
    mth<-data_month[mthi]
    
    # convert mth to DATA_MONTH format
    datamonth<-format(as.Date(paste(substr(as.character(mth),1,7),"01",sep="-")),"%Y%m%d")
    
    # products (LRX) in block bi
    prods.trx<-as.character(rxlookupnames$trxprods[rxlookupnames$lrxprods %in% blocks[[bi]]])

    # no. patients on a product by HCP
    # (CORP_RX_DATA is 1.5B rows with no indexes or partitions ...
    #  prob better to dump the whole thing to parquet/oc and do this in Spark ...)
    query<-paste(
      "select sum(TOTAL_RX) TRX ",
      "from NAPPS.CORP_RX_DATA ",
      "where DATA_MONTH='",datamonth,"' ",
      "and PRODUCT_GROUP_NAME in ", 
      paste("(",paste("'",prods.trx,"'",sep="",collapse=","),") ",sep=""),
      "and IMS_ID in (select distinct(IMS_ID) from NAPPS.CORP_IMSID_TO_CURR_CMID sample(1)) ",      
      "group by IMS_ID",
      sep=""
    )
    res <- dbSendQuery(con, query)
    trx2[[bi]][[datamonth]] <- fetch(res, n = -1)
  }
}
save(trx2,file="/wps_scratch/usr_scratch/burbidge/us/trx2.RData")
load("/wps_scratch/usr_scratch/burbidge/us/trx2.RData")

# boxplots for each block over months of log1p(TRX)
dfList2<-list()
for (bi in 1:nb) {
  # stack TRX, datamonth as data.frame
  dfList2[[bi]]<-data.frame(TRX=numeric(0),datamonth=character(0))
  for (mthi in 1:nm) {
    mth<-data_month[mthi]
    datamonth<-format(as.Date(paste(substr(as.character(mth),1,7),"01",sep="-")),"%Y%m%d")
    tmpdf<-data.frame(TRX=log1p(trx2[[bi]][[datamonth]]),
                      datamonth=substr(as.character(datamonth),1,6))
    dfList2[[bi]]<-rbind(dfList2[[bi]],tmpdf)
  }
  boxplot(TRX~datamonth,dfList2[[bi]],range=1,varwidth=T,las=2,outline=F,
          xlab="",ylab="log(TRx)",
          main=paste("Distribution of No. Patients on any Product: Block",bi),
          sub="Median +/-2xIQR, Outliers not shown")
}

# boxplots aggregated over months of log1p(TRX) by block
df.trx.any<-data.frame(TRX=numeric(0),block=numeric(0))
for (bi in 1:nb) {
  tmpdf<-data.frame(TRX=dfList2[[bi]]$TRX,block=bi)
  df.trx.any<-rbind(df.trx.any,tmpdf)
}
boxplot(TRX~block,df.trx.any,range=1,varwidth=T,las=2,outline=F,
        xlab="",ylab="log(TRx)",
        main="Distribution of No. Patients on any Product (All months) by Block",
        sub="Median +/-2xIQR, Outliers not shown")

# tertiles
round(t(rbind(sapply(dfList1, function(x) expm1(quantile(x$TRX,probs=seq(0,1,1.0/3.0)))),
sapply(dfList2, function(x) expm1(quantile(x$TRX,probs=seq(0,1,1.0/3.0)))))),2)
