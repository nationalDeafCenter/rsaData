library(tidyverse)
library(openxlsx)
source('R/data.r')
source('R/crossTabsFunctions.r')


### do we have some descriptive analyses showing % of deaf only vs other disability groups being on the waiting list?
### that alone would be a good data point.

### could be of interest to do analysis of deaf-only vs deafdisabled vs one disability only vs multiplydisabled.
### maybe not ALL of these groups but we should account for the difference in service provision for
### deaf ppl w/o other disabilities vs. deafdisabled ppl.

dat <- dat%>%
  mutate(
    exitDate2=as.Date(as.character(exitDate),format="%Y%m%d"),
    OOSExitDate=as.Date(OOSExitDate,format="%Y%m%d"),
    OOSPlacementDate=as.Date(OOSPlacementDate,format="%Y%m%d"),
    OOSendDate=pmin(OOSExitDate,exitDate2,na.rm=TRUE))

dat <- dat%>%
  mutate(
    oos=ifelse(
      is.na(OOSPlacementDate),'neverOOS',
      ifelse(is.na(OOSendDate),'currentOOS','previousOOS')
    ),
    maxDate=max(OOSendDate,na.rm=TRUE),
    oosTime=ifelse(oos=='previousOOS',OOSendDate-OOSPlacementDate, NA),
    censorTime=maxDate-OOSPlacementDate,
    leftOOS=oos=='previousOOS',
    followupTime=ifelse(leftOOS,oosTime,censorTime),
    SSDI=ifelse(appSSDI==1,
      'Applicant receives SSDI',
      'Applicant does not receive SSDI'),
    SSI=ifelse(appSSI==1,
      'Applicant receives SSI',
      'Applicant does not receive SSI')
  )


groupNs <- dat%>%group_by(group)%>%summarize(n=n())%>%column_to_rownames("group")


## OOS
## Demographics: age, gender, race, ssd/i,
## Remained on OOS in 2017
## Exited OOS in 2017 (or ever?)
## Duration on OOS
## Any other OOS data??

kmQuantile <- function(km,p)
  mean(c(max(km$time[km$surv>=p]),min(km$time[km$surv<=p])))

kmDuration <- function(followupTime,leftOOS,oos){
  if(sum(oos!='neverOOS')<20) return('[n too small]')
  survObj <- Surv(followupTime[oos!='neverOOS'],leftOOS[oos!='neverOOS'])
  km <- survfit(survObj~1)
  paste0(kmQuantile(km,0.5),' (',kmQuantile(km,0.75),',',kmQuantile(km,0.25),')')
}

 boxplot(age_app~group,dat)
dat%>%group_by(group)%>%summarize(med_age=median(age_app,na.rm=TRUE))

### % on OOS as of 2017
OOStab <- function(what){
  what <- enquo(what)
  out <- dat%>%group_by(group)%>%summarize(overall=!! what)%>%column_to_rownames("group")%>%t()
  for(demo in c('sex','raceEth','ageApp','SSDI','SSI')){
    out <- rbind(out,NA)
    rownames(out)[nrow(out)] <- demo
    out <- rbind(out,
      dat%>%
        filter(!is.na(!! sym(demo)))%>%
        group_by(group,!! sym(demo))%>%
        summarize(Y=!! what)%>%
        spread(group,Y,fill=NA)%>%
        column_to_rownames(demo)
    )
  }
  as.data.frame(out)
}

OOSstateTab <- function(what){
  what <- enquo(what)
  out <- dat%>%group_by(group)%>%summarize(overall=!! what)%>%column_to_rownames("group")%>%t()
  out <- rbind(out,NA)
    rownames(out)[nrow(out)] <- "By State"
  out <- rbind(out,
    dat%>%
      group_by(group,state)%>%
      summarize(Y=!! what)%>%
      spread(group,Y,fill=NA)%>%
      column_to_rownames("state")
  )
  as.data.frame(out)
}


OOSresults <- list(
  `% Current OOS`=OOStab(mean(oos=='currentOOS',na.rm=TRUE)*100),
  `% Ever OOS`=OOStab(mean(oos=='currentOOS'|oos=='previousOOS',na.rm=TRUE)*100),
  `% Exited OOS by 2017`=OOStab(mean(oos=='previousOOS',na.rm=TRUE)*100),
  `Days on OOS (Exited)`=rbind(OOStab(paste0(round(median(oosTime,na.rm=TRUE)),' (',paste(round(quantile(oosTime,c(.25,.75),na.rm=TRUE)),collapse='-'),')')),c('Median and mid-50% interval',rep(NA,5))),
  `OOS Duration (Estimate)`=rbind(OOStab(kmDuration(followupTime,leftOOS,oos)),c('Median and mid-50% interval',rep(NA,5))),
  `Sample Sizes (# people)`=OOStab(n()),
  `Demographic breakdown`=OOStab(n()/groupNs[group[1],'n']*100)
)

openxlsx::write.xlsx(OOSresults,"OOS.xlsx",rowNames=TRUE)

OOSbyState <- list(
  `% Current OOS`=OOSstateTab(mean(oos=='currentOOS',na.rm=TRUE)*100),
  `% Ever OOS`=OOSstateTab(mean(oos=='currentOOS'|oos=='previousOOS',na.rm=TRUE)*100),
  `% Exited OOS by 2017`=OOSstateTab(mean(oos=='previousOOS',na.rm=TRUE)*100),
  `Days on OOS (Exited)`=rbind(OOSstateTab(paste0(round(median(oosTime,na.rm=TRUE)),' (',paste(round(quantile(oosTime,c(.25,.75),na.rm=TRUE)),collapse='-'),')')),c('Median and mid-50% interval',rep(NA,5))),
  `OOS Duration (Estimate)`=rbind(OOSstateTab(kmDuration(followupTime,leftOOS,oos)),c('Median and mid-50% interval',rep(NA,5))),
  `Sample Sizes (# people)`=OOSstateTab(n()),
  `Demographic breakdown`=OOSstateTab(n()/groupNs[group[1],'n']*100)
)


openxlsx::write.xlsx(OOSbyState,"OOSstate.xlsx",rowNames=TRUE)


