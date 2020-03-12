library(RColorBrewer)
library(tidyverse)
library(openxlsx)
library(survival)
library(usmap)
source('R/data.r')
source('R/crossTabsFunctions.r')

### notes: some weirdness:

capitalize <- function(x) paste0(toupper(substr(x,1,1)),substr(x,2,nchar(x)))

round.data.frame <- function(x,digits=0){
  #require(purr)
  x[,map_lgl(x,is.numeric)] <- lapply(x[,map_lgl(x,is.numeric)],round,digits=digits)
  x
}

  map(unique(dat$group),function(g) xtabs(~is.na(OOSPlacementDate)+is.na(OOSExitDate),data=filter(dat,state=='IN',group==g)))%>%setNames(.,unique(dat$group))



  group_by(group)%>%group_map(~as.data.fr
## almost eeryone on OOS is still on OOS in Indiana (other states too?)

### express percentages as % of people ever on OOS who still are, etc



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

kmQuantile <- function(km,p){
  out <- mean(c(max(km$time[km$surv>=p]),min(km$time[km$surv<=p])))
  if(!is.finite(out)) return('*')
  out
}

kmDuration <- function(followupTime,leftOOS,oos){
  if(sum(oos!='neverOOS')==0) return('')
  if(sum(oos!='neverOOS')<20) return('*')
  survObj <- Surv(followupTime[oos!='neverOOS'],leftOOS[oos!='neverOOS'])
  km <- survfit(survObj~1)
  if(all(km$surv>0.25)|all(km$surv<0.75)) return('*')
  paste0(kmQuantile(km,0.5),' (',kmQuantile(km,0.75),',',kmQuantile(km,0.25),')')
}

rawDuration <- function(oosTime){
  if(all(is.na(oosTime))) return(' ')
  paste0(
    round(median(oosTime,na.rm=TRUE)),
    ' (',paste(round(quantile(oosTime,c(.25,.75),na.rm=TRUE)),collapse='-'),')'
  )
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

OOSstateTab <- function(what,header=TRUE){
  what <- enquo(what)
  out <-  dat%>%
      group_by(group,state)%>%
      summarize(Y=!! what)%>%
      spread(group,Y,fill=NA)%>%
    column_to_rownames("state")
  if(header){
    out <- rbind(
      dat%>%
        group_by(group)%>%
        summarize(overall=!! what)%>%
        column_to_rownames("group")%>%
        t(),
      NA,
      out
    )
    rownames(out)[1:2] <- c('Overall','By State')
  }
  as.data.frame(out)
}

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
  `Days on OOS (Exited)`=rbind(OOStab(rawDuration(oosTime)),c('Median and mid-50% interval',rep(NA,5))),
  `OOS Duration (Estimate)`=rbind(
    OOStab(kmDuration(followupTime,leftOOS,oos)),
    c('Median and mid-50% interval',rep(NA,5)),
    c('* Not enough data to estimate',rep(NA,5))
   ),
  `Sample Sizes (# people)`=OOStab(n()),
  `Demographic breakdown`=OOStab(n()/groupNs[group[1],'n']*100)
)

openxlsx::write.xlsx(OOSresults,"OOS.xlsx",rowNames=TRUE)

OOSbyState <- list(
  `% Current OOS`=OOSstateTab(mean(oos=='currentOOS',na.rm=TRUE)*100),
  `% Ever OOS`=OOSstateTab(mean(oos=='currentOOS'|oos=='previousOOS',na.rm=TRUE)*100),
  `% Exited OOS by 2017`=OOSstateTab(mean(oos=='previousOOS',na.rm=TRUE)*100),
  `Days on OOS (Exited)`=rbind(
    OOSstateTab(rawDuration(oosTime)),
    c('Median and mid-50% interval',rep(NA,5))
  ),
  `OOS Duration (Estimate)`=rbind(
    OOSstateTab(kmDuration(followupTime,leftOOS,oos)),
    c('Median and mid-50% interval',rep(NA,5)),
    c('* Not enough data to estimate',rep(NA,5))
   ),
  `Sample Sizes (# people)`=OOSstateTab(n()),
  `Demographic breakdown`=OOSstateTab(n()/groupNs[group[1],'n']*100)
)


openxlsx::write.xlsx(OOSbyState,"OOSstate.xlsx",rowNames=TRUE)

### maps for state data
current <- OOSstateTab(mean(oos=='currentOOS',na.rm=TRUE),header=FALSE)
ever <- OOSstateTab(mean(oos=='currentOOS'|oos=='previousOOS',na.rm=TRUE),header=FALSE)

current[ever==0] <- NA
ever[ever==0] <- NA
current$fips <- statepop$fips[match(rownames(current),statepop$abbr)]
ever$fips <- statepop$fips[match(rownames(ever),statepop$abbr)]

all.equal(map_chr(na.omit(current$fips),~statepop$abbr[statepop$fips==.]),
  rownames(current)[!is.na(current$fips)])

maxes=map_dbl(list(current=current,ever=ever),
  function(meas) max(do.call('c',meas[c('deafblind','justDeaf','deafDisabled')]),na.rm=TRUE))

for(meas in list('current','ever')){
  res <- get(meas)
  for(who in c('deafblind','justDeaf','deafDisabled')){
    plot_usmap(data=res,values=who,color='black')+
      scale_fill_continuous(
        name=paste0(
          switch(who,
            deafblind='Deafblind',
            justDeaf='Deaf-Nondisabled',
            deafDisabled='Deafdisabled'
          ),'\n',
          capitalize(meas),' OOS'
        ),
        limits=c(0,maxes[meas]),
        label=scales::percent,low='#f7fcf5',high="#00A79D",na.value='white')+
    theme(legend.position='right')
    ggsave(paste0('maps/',who,meas,'.pdf'))
  }
}
