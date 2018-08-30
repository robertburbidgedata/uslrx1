# ts.R
rm(list=ls())
library(zoo)

source('~/R/oracle_us.R')

# RESP_2 TRX by MONTH
# query<-paste("select DATA_MONTH, substr(PRODUCT_GROUP_NAME,1",
#              ",regexp_instr(PRODUCT_GROUP_NAME,'\\Z|\\s',1,1,0)-1) as PRODUCT",
#               ",sum(TOTAL_RX) as TRX from NAPPS.CORP_RX_DATA",
#               "where substr(PRODUCT_GROUP_NAME,1",
#               ",regexp_instr(PRODUCT_GROUP_NAME,'\\Z|\\s',1,1,0)-1)",
#               "in ('SPIRIVA','SEEBRI','TUDORZA')",
#               "group by DATA_MONTH",
#               ", substr(PRODUCT_GROUP_NAME,1,regexp_instr(PRODUCT_GROUP_NAME,'\\Z|\\s',1,1,0)-1)")
# res <- dbSendQuery(con, query)
# trx_resp_2 <- fetch(res, n = -1)
# save(trx_resp_2,file="/wps_scratch/usr_scratch/burbidge/us/trx_resp_2.RData")
load(file="/wps_scratch/usr_scratch/burbidge/us/trx_resp_2.RData")

dim(trx_resp_2)
names(trx_resp_2)
head(trx_resp_2)

trx_spiriva<-trx_resp_2[trx_resp_2$PRODUCT=="SPIRIVA",c("DATA_MONTH","TRX")]
names(trx_spiriva)<-c("MONTH","TRX_SPIRIVA")
trx_seebri<-trx_resp_2[trx_resp_2$PRODUCT=="SEEBRI",c("DATA_MONTH","TRX")]
names(trx_seebri)<-c("MONTH","TRX_SEEBRI")
trx_tudorza<-trx_resp_2[trx_resp_2$PRODUCT=="TUDORZA",c("DATA_MONTH","TRX")]
names(trx_tudorza)<-c("MONTH","TRX_TUDORZA")

dat<-merge(trx_spiriva,trx_seebri,by="MONTH",all=T)
dat<-merge(dat,trx_tudorza,by="MONTH",all=T)
dat$MONTH<-as.Date(as.character(dat$MONTH),format="%Y%m%d")
head(dat)

plot(dat$MONTH,dat$TRX_SPIRIVA/1e6,type="l",col="darkblue",xlab="",ylab=""
     ,main="US Total Monthly TRX (000,000s): RESP_2",ylim=c(0,max(dat$TRX_SPIRIVA/1e6)))
lines(dat$MONTH,dat$TRX_TUDORZA/1e6,type="l",col="darkgreen")
lines(dat$MONTH,dat$TRX_SEEBRI/1e6,type="l",col="darkred")
legend("right",legend=c("SPIRIVA","TUDORZA","SEEBRI")
       ,col=c("darkblue","darkgreen","darkred"),lty=1)

# TRX prob only reliable for 2015-01-01 onwards, and for SPIRIVA
trx_spiriva$MONTH<-as.Date(as.character(trx_spiriva$MONTH),format="%Y%m%d")
trx_spiriva<-trx_spiriva[trx_spiriva$MONTH>=as.Date("2015-01-01",format="%Y-%m-%d"),]
trx_spiriva<-trx_spiriva[order(trx_spiriva$MONTH),]
ntrx<-nrow(trx_spiriva)

# interpolate to daily
datd<-data.frame(Date=seq(from=trx_spiriva$MONTH[1],
                          to=seq(from=trx_spiriva$MONTH[ntrx],length.out=2,by="month")[2]-1
                          ,by="day"))
datd$MONTH<-as.Date(paste(format(datd$Date,format="%Y%m"),"01",sep=""),format="%Y%m%d")
datd<-merge(datd,trx_spiriva,by="MONTH")
datd$days<-as.numeric(lapply(datd$MONTH,function(x) seq(from=x,length.out=2,by="month")[2]-x))
datd[,"TRX_SPIRIVA"]<-datd[,"TRX_SPIRIVA"]/datd$days
nd<-length(datd$Date)

xy<-spline(x=datd$Date,y=datd[,"TRX_SPIRIVA"],n=nd/30.5)
plot(xy$x,xy$y,type="l")
datd[,paste("TRX_SPIRIVA","i")]<-spline(xy$x,xy$y,n=nd)$y
plot(datd$Date,datd$`TRX_SPIRIVA i`,type="l")

datd<-datd[,c("Date","TRX_SPIRIVA i")]

# RESP_2 ENGAGEMENTS by MONTH
# query<-paste("select CALL_DATE from NAPPS.CORP_ENGAGEMENT_INFO",
#              "where substr(PRODUCT_DETAILED,1,regexp_instr(PRODUCT_DETAILED,'\\Z|\\s',1,1,0)-1)",
#              "='SPIRIVA'",
#              "and CALL_STATUS='Submitted_vod'",
#              "and CALL_TYPE='Detail Only'")
# res <- dbSendQuery(con, query)
# info_spiriva <- fetch(res, n = -1)
# save(info_spiriva,file="/wps_scratch/usr_scratch/burbidge/us/info_spiriva.RData")
load(file="/wps_scratch/usr_scratch/burbidge/us/info_spiriva.RData")

# engagements by day
info_spiriva_day_tab<-table(info_spiriva)
info_spiriva_day<-data.frame(Date=as.Date(substr(names(info_spiriva_day_tab),1,10)
                                          ,format="%Y-%m-%d"),
                             info=as.numeric(info_spiriva_day_tab))

# merge
dat<-merge(datd,info_spiriva_day,by="Date",all=T)

# adstock Spiriva
adstock<-function(x,rate=0){
  return(as.numeric(filter(x=x,filter=rate,method="recursive")))
}

ys<-dat$`TRX_SPIRIVA i`
xs<-dat$info
xs[is.na(xs)]<-0
dt<-as.numeric(dat$Date-min(dat$Date))
modFit<-nls(ys~a+b*adstock(xs,phi)+bt*dt,start=c(a=1,b=1,bt=-1,phi=0.5),
            control=nls.control(maxiter=500))
summary(modFit)
phis<-summary(modFit)$coefficients["phi","Estimate"]

lm1<-lm(ys~adstock(xs,phis)+dt)
summary(lm1)
plot(lm1)
plot(dat$Date[!is.na(xs)&!is.na(ys)],lm1$residuals,type="p")

# no +ve relationship between engagements and TRX

# ts plot
par(mar=c(5, 4, 4, 4))
idx<-!is.na(dat$`TRX_SPIRIVA i`)
plot(dat$Date[idx],dat$`TRX_SPIRIVA i`[idx],type="l",col="darkblue",xlab="",ylab="TRX_SCRIPTS", 
     main="US: Sales and Engagements: Spiriva",ylim=c(0,max(dat$`TRX_SPIRIVA i`,na.rm=T)*1.25))
lines(dat$Date[idx],dt[idx]*lm1$coefficients["dt"]+lm1$coefficients["(Intercept)"],col="red")
par(new=TRUE)
plot(dat$Date[idx], dat$info[idx], type="h", axes=FALSE, col=3, xlab="", ylab="")
axis(side=4, at=pretty(c(0,dat$info)))
mtext("Engagements", side=4, line=3)
legend("topright",c("TRX_SCRIPTS","Engagements"),col=c("darkblue",3),lty=c(1,0),pch=c(NA,"|"))
