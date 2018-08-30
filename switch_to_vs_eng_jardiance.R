# switch_to_vs_eng_jardiance.R
rm(list=ls())
library(zoo)
prd<-"JARDIANCE"

source('~/R/oracle_us.R')

# MONTHS ON LRX
# query<-paste(
#   "select distinct(DATA_DATE) DATA_MONTH",
#   "from NAPPS.CORP_LRX_CURR_SWITCH_DETAILS"
# )
# res <- dbSendQuery(con, query)
# data_month <- fetch(res, n = -1)
# data_month<-sort(as.Date(
#   sapply(data_month,function(x) paste(substr(x,1,4),substr(x,6,7),substr(x,9,10),sep="-")),
#                     format="%Y-%m-%d"))
# save(data_month,file="/wps_scratch/usr_scratch/burbidge/us/data_month.RData")
load(file="/wps_scratch/usr_scratch/burbidge/us/data_month.RData")
print(data_month)
nm<-length(data_month)

### By MONTH (TO): SWITCH_TO vs ENGAGEMENTS over IMSID
for (mthi in 1:nm) {
  
  mth<-data_month[mthi]
  ### pull SWITCH_TO from LRX table
  query<-paste(
    "select IMSID, SWITCH_TO from NAPPS.CORP_LRX_CURR_SWITCH_DETAILS ",
    "where substr(TO_PROD_BRAND_NAME,1,regexp_instr(TO_PROD_BRAND_NAME,'\\Z|\\s',1,1,0)-1)='",
    prd,"'",
    " and DATA_DATE=date '",as.character(mth),"'",sep=""
  )
  res <- dbSendQuery(con, query)
  switch_to <- fetch(res, n = -1)
  
  # SWITCH_TO summary
  # print(dim(switch_to))
  # summary(switch_to$SWITCH_TO)
  tab1<-table(switch_to$SWITCH_TO)
  x<-as.numeric(names(tab1))
  y<-log10(as.numeric(tab1))
  png(file=paste("~/R/us/jardiance_switchto_",as.character(mth),".png",sep=""))
  plot(x,y,type="h",xlab="SWITCH_TO",ylab="log10(freq)"
       ,main=paste("Switching: ",prd,", ",mth,sep=""))
  dev.off()
  
  ### pull no. engagements
  query<-paste(
    "select sw2.IMS_ID IMSID, count(*) NENG from ",
    "NAPPS.CORP_ENGAGEMENT_INFO it, ", 
    "NAPPS.CORP_IMSID_TO_CURR_CMID sw2, ", 
    "NAPPS.CORP_CMID_TO_CURR_CMID sw1 ",
    "where sw1.CURRENT_CM_ID = sw2.CURRENT_CM_ID ",
    "and sw1.CM_ID = SUBSTR( it.CM_ID, 3 ) ",
    "and CALL_DATE<=date '",as.character(mth),"' ",
    "and CALL_DATE>=date '",as.character(seq(from=mth,length.out=2,by="-1 month")[2]),"' ",
    "and PRODUCT_DETAILED='",prd,"' ",
    "group by sw2.IMS_ID"
    ,sep=""
  )
  res <- dbSendQuery(con, query)
  neng <- fetch(res, n = -1)
  
  # NENG summary
  # print(dim(neng))
  # summary(neng$NENG)
  tab2<-table(neng$NENG)
  x<-as.numeric(names(tab2))
  y<-as.numeric(tab2)
  png(file=paste("~/R/us/jardiance_eng_",as.character(mth),".png",sep=""))
  plot(x,y,type="h",xlab="No. Engagements",ylab="freq"
       ,main=paste("Engagements: ",prd,", ",mth,sep=""))
  dev.off()
  
  ### join and scatterplot (inc. zeros)
  dat<-merge(neng,switch_to,by="IMSID",all.x=T,all.y=T)
  dat$SWITCH_TO[is.na(dat$SWITCH_TO)]<-0
  dat$NENG[is.na(dat$NENG)]<-0
  png(file=paste("~/R/us/jardiance_switchto_eng_",as.character(mth),".png",sep=""))
  plot(dat$NENG,dat$SWITCH_TO,type="p",xlab="No. Engagements",ylab="SWITCH_TO"
       ,main=paste("SWITCH_TO vs. Engagements: ",prd,", ",mth,sep=""))
  lm1<-lm(SWITCH_TO~NENG,dat)
  abline(lm1,col="red")
  slm1<-summary(lm1)
  text(0.8*max(dat$NENG),0.8*max(dat$SWITCH_TO),paste("adjR2 = ",round(slm1$adj.r.squared,4)
                                                      ,", p = ",round(pf(slm1$fstatistic["value"],slm1$fstatistic["numdf"],slm1$fstatistic["dendf"]
                                                                         ,lower.tail=F),4),sep=""))
  dev.off()
}

### Boxplots of mean SWITCH_TO per month by IMS_SPECIALTY
mthi<-nm
mth<-data_month[mthi]
### pull IMS_SPECIALTY, SWITCH_TO from LRX table
query<-paste(
  "select IMS_SPECIALTY, sum(SWITCH_TO) SWITCH_TO from NAPPS.CORP_LRX_CURR_SWITCH_DETAILS ",
  "where substr(TO_PROD_BRAND_NAME,1,regexp_instr(TO_PROD_BRAND_NAME,'\\Z|\\s',1,1,0)-1)='",
  prd,"'",
  " and DATA_DATE=date '",as.character(mth),"'",
  " group by IMS_SPECIALTY",
  " order by SWITCH_TO desc", sep=""
)
res <- dbSendQuery(con, query)
specialty_switch_to <- fetch(res, n = -1)

cutoff<-15
barplot(specialty_switch_to$SWITCH_TO[1:cutoff],names.arg=specialty_switch_to$IMS_SPECIALTY[1:cutoff],
        xlab="",ylab="SWITCH_TO",las=2,main=paste("SWITCH_TO vs IMS_SPECIALTY:",prd,mth))

