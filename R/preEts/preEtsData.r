library(tidyverse)

### save some time: maybe we don't need to load the full data
sdatFile <- 'data/studDat.RData'
if(
(file.exists(sdatFile))& ## the prepared data is available
(file.info(sdatFile)$mtime>file.info('R/data.r')$mtime)& ## and up to date
(file.info(sdatFile)$mtime>file.info('R/preEtsData.r')$mtime)
){
  load(sdatFile)
} else{

  source('R/data.r') ### load in data, standard RSA data prep
  studDat <- filter(dat,Student>0) ## only interested in "students with a disability"
  rm(dat) ## save ram
  gc()


################################################
#### Define Variables of Interest
################################################
  services <- c('jec','wble','ceo','wrt','isa')

  ### several variables for each type of service, depending on how it was delivered
  ### here we just want indicators for whether the student received the service
  ### in the data it is coded as:
  ### 1 if student received service
  ### 2 if student received service and is no longer receiving
  ### NA student never received service

  for(serv in services){
    studDat[[serv]] <- apply(
      studDat[,paste0(serv,c('VRAgencyStaff','VRAgencyPurchase','CompServiceProvider'))],
      1,
      function(x) any(!is.na(x))
    ) ## 1 or 2 (not NA)
    studDat[[paste0(serv,'Current')]] <- apply(
      studDat[,paste0(serv,c('VRAgencyStaff','VRAgencyPurchase','CompServiceProvider'))],
      1,
      function(x) any(!is.na(x)&x==1)
    ) ## only 1 (i.e. "currently")
  }
  studDat$anyServ <- apply(studDat[,services],1,any)
  studDat$anyServCurrent <- apply(studDat[,paste0(services,'Current')],1,any)

  ## students may receive several services at once
  studDat$servCombos <- apply(studDat[,services],1,function(x) paste(services[x],collapse=';'))


  ### age distribution is different for pre-ETS
  studDat$ageApp <- cut(studDat$age_app,c(-Inf,14,18,21,Inf),c('<15','15-18','19-21','22+'))%>%
    as.character()%>%
    replace_na('No Application')

  ### does the student have a valide PETS start date?
  studDat$petsdate <- !is.na(studDat$PETSStartDate)

  save(studDat,file=sdatFile)
}
