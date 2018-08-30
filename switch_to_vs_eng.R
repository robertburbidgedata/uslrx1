# switch_to_vs_eng.R
rm(list=ls())
library(zoo)

source('~/R/oracle_us.R')

# BI PRODUCTS ON LRX
# query<-paste(
#   "select distinct(PROD_BRAND_NAME) from",
#   "(select distinct(substr(FROM_PROD_BRAND_NAME,1,regexp_instr(FROM_PROD_BRAND_NAME,'\\Z|\\s',1,1,0)-1)) PROD_BRAND_NAME",
#     "from NAPPS.CORP_LRX_CURR_SWITCH_DETAILS)",
#   "union",
#   "(select distinct(substr(TO_PROD_BRAND_NAME,1,regexp_instr(TO_PROD_BRAND_NAME,'\\Z|\\s',1,1,0)-1)) PROD_BRAND_NAME ",
#     "from NAPPS.CORP_LRX_CURR_SWITCH_DETAILS)"
# )
# res <- dbSendQuery(con, query)
# prod_brand_name <- fetch(res, n = -1)
# save(prod_brand_name,file="/wps_scratch/usr_scratch/burbidge/us/prod_brand_name.RData")
load(file="/wps_scratch/usr_scratch/burbidge/us/prod_brand_name.RData")
print(prod_brand_name)

# could pull these from ICE BRAND_FEATURES
bi_prods<-c(
  "Jentadueto",
  "Synjardi",
  "Trajenta",
  "Pradax",
  "Jardiance",
  "Inspiolto",
  "Spiriva"
)

prod_brand_name_bi<-prod_brand_name[tolower(sapply(prod_brand_name,function(x)substr(x,1,6))) 
                                    %in% tolower(sapply(bi_prods,function(x)substr(x,1,6))),]
print(prod_brand_name_bi)

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

### By MONTH by PRODUCT by FROM/TO: SWITCH_TO vs ENGAGEMENTS over IMSID
mth<-data_month[1]
prd<-prod_brand_name_bi[1]

# TO
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
print(dim(switch_to))
summary(switch_to$SWITCH_TO)
histsw<-hist(switch_to$SWITCH_TO,plot=F)
plot(histsw$mids,log10(histsw$counts),type="h")
tab1<-table(log10(switch_to$SWITCH_TO))
x<-10^as.numeric(names(tab1))
y<-log10(as.numeric(tab1))
plot(x,y,type="h",xlab="SWITCH_TO",ylab="log10(freq)"
     ,main=paste("Switching: ",prd,", ",mth,sep=""))

### pull no. engagements from RX table -- ID??
query<-paste(
  "select sw2.IMS_ID IMSID, it.NENG from ",
  "NAPPS.CORP_ENGAGEMENT_INFO it, ", 
  "NAPPS.CORP_IMSID_TO_CURR_CMID sw2,", 
  "NAPPS.CORP_CMID_TO_CURR_CMID sw1",
  "where sw1.CURRENT_CM_ID = sw2.CURRENT_CM_ID ",
  "and sw1.CM_ID = SUBSTR( it.CM_ID, 3 ) ",
  "and CALL_DATE<=date '",as.character(mth),"' ",
  "and CALL_DATE>=date '",as.character(seq(from=mth,length.out=2,by="-1 month")[2]),"' ",
  "and PRODUCT_DETAILED='",prd,"' ",
  "group by CM_ID) a ",
  "inner join ",
  "(select IMSID,CM_ID from NAPPS.CORP_BEST_ADDRESS) b ",
  "on a.CM_ID=b.CM_ID"
  ,sep=""
)
res <- dbSendQuery(con, query)
neng <- fetch(res, n = -1)

### join and scatterplot
dat<-merge(neng,switch_to,by="CM_ID",all=F)
