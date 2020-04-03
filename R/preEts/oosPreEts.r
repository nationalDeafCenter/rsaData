library(tidyverse)
library(openxlsx)

round.data.frame <- function(x,digits=0){
  #require(purr)
  x[,map_lgl(x,is.numeric)] <- lapply(x[,map_lgl(x,is.numeric)],round,digits=digits)
  x
}

demos <- c('sex','raceEth','ageApp','SSDI','SSI')
services <- c('jec','wble','ceo','wrt','isa')


if(!exists("studDat")){
  source('R/preEts/preEtsData.r')
}



## oos and pre-ets?
studDat <- studDat%>%
  mutate(
    exitDate2=as.Date(as.character(exitDate),format="%Y%m%d"),
    OOSExitDate=as.Date(as.character(OOSExitDate),format="%Y%m%d"),
    OOSPlacementDate=as.Date(as.character(OOSPlacementDate),format="%Y%m%d"),
    OOSendDate=pmin(OOSExitDate,exitDate2,na.rm=TRUE),
    oos=ifelse(
      is.na(OOSPlacementDate),'neverOOS',
      ifelse(is.na(OOSendDate),'currentOOS','previousOOS')
    ),
    anyPreEts=ifelse(anyServCurrent,'current',ifelse(anyServ,'past','none'))
  )

oosETS <- function(serv){
  studDat$serv <- studDat[[serv]]
  studDat%>%
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
names(oosEts) <- c('Has Start Date','Any',toupper(services))

oosEts <- map(oosEts,round,digits=1)

oosEts <- map(oosEts,function(x) rbind(x,`All numbers are % ever received pre-ETS service`=NA,`Classified as "Student with a Disability"`=NA))


openxlsx::write.xlsx(oosEts,"results/oosEts.xlsx",rowNames=TRUE)









