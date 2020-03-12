library(tidyverse)
library(openxlsx)
source('R/data.r')
source('R/crossTabsFunctions.r')

round.data.frame <- function(x,digits=0){
  #require(purr)
  x[,map_lgl(x,is.numeric)] <- lapply(x[,map_lgl(x,is.numeric)],round,digits=digits)
  x
}

## Pre-ETS  (These services can be provided without an IPE. As such, let's just say anyone with an Eligibility Date could potentially qualify)

## Pre-ETS Services
## Job exploration
## WB learning
## Counseling on enrollment
## Workplace readiness
## Self-advocacy

services <- c('jec','wble','ceo','wrt','isa')

for(serv in services)
  dat[[serv]] <- apply(
    dat[,paste0(serv,c('VRAgencyStaff','VRAgencyPurchase','PurchaseProviderType','CompServiceProvider')),],
    1,
    function(x) any(!is.na(x))
  )
dat$anyServ <- apply(dat[,services],1,any)

for(serv in services)
  dat[[paste0(serv,'Current')]] <- apply(
    dat[,paste0(serv,c('VRAgencyStaff','VRAgencyPurchase','PurchaseProviderType','CompServiceProvider'))],
    1,
    function(x) any(!is.na(x)&x==1)
  )
dat$anyServCurrent <- apply(dat[,paste0(services,'Current')],1,any)

dat$petsdate <- !is.na(dat$PETSStartDate)

preEtsTab <- function(serv){

  what <- sym(serv)
  out <- dat%>%
    ##    filter(!is.na(EligibilityDate))%>% ## doesn't seem to exclude VR services
#    (see  tab(~is.na(EligibilityDate)+anyServ,dat)
  filter(
#    ageApp%in%c("<18","18-24"),
    Student>0
    )%>%
  group_by(group)%>%
    summarize(overall=mean(!! what,na.rm=TRUE)*100)%>%
    column_to_rownames("group")%>%
    t()

  for(demo in c('sex','raceEth','ageApp','SSDI','SSI')){
    out <- rbind(out,NA)
    rownames(out)[nrow(out)] <- demo
    out <- rbind(out,
      dat%>%
        filter(
          ageApp%in%c("<18","18-24"),
          !is.na(!! sym(demo))
        )%>%
        group_by(group,!! sym(demo))%>%
        summarize(Y=mean(!! what,na.rm=TRUE)*100)%>%
        spread(group,Y,fill=NA)%>%
        column_to_rownames(demo)
    )
  }
  as.data.frame(out)
}


preEts <-  map(c('petsdate','anyServ',services),preEtsTab)
## preEts <- map(1:length(preEts),
##   ~rbind(c(paste('% Receiving',c('Any pre-ETS Service',toupper(services))[.]),rep(NA,ncol(preEts[[.]]))),preEts[.])
##   )
names(preEts) <- c('Start Date','Any',toupper(services))

preEts <- map(preEts,round,digits=1)

preEts <- map(preEts,function(x) rbind(x,`All numbers are % ever received pre-ETS service`=NA,`Classified as "Student with a Disability"`=NA))


openxlsx::write.xlsx(preEts,"preEts.xlsx",rowNames=TRUE)

preEtsCurrent <-  map(c('petsdate',paste0(c('anyServ',services),'Current')),preEtsTab)
## preEts <- map(1:length(preEts),
##   ~rbind(c(paste('% Receiving',c('Any pre-ETS Service',toupper(services))[.]),rep(NA,ncol(preEts[[.]]))),preEts[.])
##   )
names(preEtsCurrent) <- c('Start Date','Any',toupper(services))

preEtsCurrent <- map(preEtsCurrent,round,digits=1)

preEtsCurrent <- map(preEts,function(x) rbind(x,`All numbers are % currently receiving pre-ETS service`=NA,`Classified as "Student with a Disability"`=NA))


openxlsx::write.xlsx(preEtsCurrent,"preEtsCurrent.xlsx",rowNames=TRUE)


## oos and pre-ets?
dat <- dat%>%
  mutate(
    exitDate2=as.Date(as.character(exitDate),format="%Y%m%d"),
    OOSExitDate=as.Date(OOSExitDate,format="%Y%m%d"),
    OOSPlacementDate=as.Date(OOSPlacementDate,format="%Y%m%d"),
    OOSendDate=pmin(OOSExitDate,exitDate2,na.rm=TRUE),
    oos=ifelse(
      is.na(OOSPlacementDate),'neverOOS',
      ifelse(is.na(OOSendDate),'currentOOS','previousOOS')
    ),
    anyPreEts=ifelse(anyServCurrent,'current',ifelse(anyServ,'past','none'))
  )

oosETS <- function(serv){
  dat$serv <- dat[[serv]]
  dat%>%
    filter(Student>0)%>% #ageApp%in%c("<18","18-24"))%>%
    group_by(group)%>%
    mutate(everOOS=oos%in%c('currentOOS','previousOOS'))%>%
    summarize(
      total=n(),
      `# Ever OOS`=sum(everOOS,na.rm=TRUE),
  `# Ever PETS`=sum(serv,na.rm=TRUE),
  `# OOS & PETS`=sum(serv&everOOS,na.rm=TRUE)
  )%>%
    mutate(
      `% of OOS who PETS`=`# OOS & PETS`/`# Ever OOS`*100,
      `% of PETS who OOS`=`# OOS & PETS`/`# Ever PETS`*100
    )
}

oosEts <-  map(c('petsdate','anyServ',services),oosETS)
## preEts <- map(1:length(preEts),
##   ~rbind(c(paste('% Receiving',c('Any pre-ETS Service',toupper(services))[.]),rep(NA,ncol(preEts[[.]]))),preEts[.])
##   )
names(oosEts) <- c('Has Start Date','Any',toupper(services))

oosEts <- map(oosEts,round,digits=1)

oosEts <- map(oosEts,function(x) rbind(x,`All numbers are % ever received pre-ETS service`=NA,`Classified as "Student with a Disability"`=NA))


openxlsx::write.xlsx(oosEts,"oosEts.xlsx",rowNames=TRUE)


oosETScurrent <- function(serv){
  if(serv=='petsdate') dat$serv <- dat$petsdate else dat$serv <- dat[[paste0(serv,'Current')]]
  dat%>%
    filter(ageApp%in%c("<18","18-24"))%>%
    group_by(group)%>%
    mutate(currentOOS=oos=='currentOOS')%>%
    summarize(
      total=n(),
      `# Current OOS`=sum(currentOOS,na.rm=TRUE),
  `# Current PETS`=sum(serv,na.rm=TRUE),
  `# OOS & PETS`=sum(serv&currentOOS,na.rm=TRUE)
  )%>%
    mutate(
      `% of OOS who PETS`=`# OOS & PETS`/`# Current OOS`*100,
      `% of PETS who OOS`=`# OOS & PETS`/`# Current PETS`*100
    )
}

oosEtsCurrent <-  map(c('petsdate','anyServ',services),oosETScurrent)
## preEts <- map(1:length(preEts),
##   ~rbind(c(paste('% Receiving',c('Any pre-ETS Service',toupper(services))[.]),rep(NA,ncol(preEts[[.]]))),preEts[.])
##   )
names(oosEtsCurrent) <- c('Has Start Date','Any',toupper(services))

oosEtsCurrent <- map(oosEtsCurrent,round,digits=1)

oosEtsCurrent <- map(oosEtsCurrent,function(x) rbind(x,`All numbers are % currently receive pre-ETS service`=NA,`Classified as "Student with a Disability"`=NA))


openxlsx::write.xlsx(oosEtsCurrent,"oosEtsCurrent.xlsx",rowNames=TRUE)













