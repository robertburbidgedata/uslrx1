# brand_features.R
rm(list=ls())

############################# ORACLE SET-UP ################################
library("ROracle", lib.loc="/apps/prod/easybuild/software/R/3.3.3-foss-2016b/lib64/R/library")

Sys.setenv(
  DBA = "/admin/cl11210",
  NLS_LANG = "AMERICAN_AMERICA.AL32UTF8",
  ORACLE_HOME = "/u01/app/oracle/product/11.2.0/client_ora11r2_1_0",
  ORACLE_SID = "cl11210",
  ORA_NLS10 = "/u01/app/oracle/product/11.2.0/client_ora11r2_1_0/nls/data"
)

drv <- dbDriver("Oracle",interruptible=TRUE,unicode_as_utf8=FALSE)
user <- "rdggdtsci"
pw <- "Boe#Ing#2011"

host <- "NAHVSMDB57"
port <- 1526
sid <- "MDX57Q03"

connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SID=", sid, ")))", sep = "")

con <- dbConnect(drv, username = user, password = pw,
                 dbname = connect.string)

#################################################################################

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



