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
    oos=ifelse(
      is.na(OOSPlacementDate),'neverOOS',
      ifelse(is.na(OOSExitDate),'currentOOS','previousOOS')
    ),
    oosTime=ifelse(oos=='previousOOS',
      as.Date(OOSExitDate,format="%Y%m%d")-as.Date(OOSPlacementDate,format="%Y%m%d"),
      NA
    )
  )

groupNs <- dat%>%group_by(group)%>%summarize(n=n())%>%column_to_rownames("group")

## OOS
## Demographics: age, gender, race, ssd/i,
## Remained on OOS in 2017
## Exited OOS in 2017 (or ever?)
## Duration on OOS
## Any other OOS data??

boxplot(age_app~group,dat)
dat%>%group_by(group)%>%summarize(med_age=median(age_app,na.rm=TRUE))

### % on OOS as of 2017
OOStab <- function(what){
  what <- enquo(what)
  out <- dat%>%group_by(group)%>%summarize(overall=!! what)%>%column_to_rownames("group")%>%t()
  for(demo in c('sex','raceEth','ageApp','appSSDI','appSSI')){
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

OOSresults <- list(
  `% Current OOS`=OOStab(mean(oos=='currentOOS',na.rm=TRUE)*100),
  `% Ever OOS`=OOStab(mean(oos=='currentOOS'|oos=='previousOOS',na.rm=TRUE)*100),
  `% Exited OOS by 2017`=OOStab(mean(oos=='previousOOS',na.rm=TRUE)*100),
  `Days on OOS`=rbind(OOStab(paste0(round(median(oosTime,na.rm=TRUE)),' (',paste(round(quantile(oosTime,c(.25,.75),na.rm=TRUE)),collapse='-'),')')),'Median and mid-50% interval'),
  `Sample Sizes (# people)`=OOStab(n()),
  `Demographic breakdown`=OOStab(n()/groupNs[group[1],'n']*100)
)

openxlsx::write.xlsx(OOSresults,"OOS.xlsx",rowNames=TRUE)


