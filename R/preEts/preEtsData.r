library(tidyverse)

### save some time: maybe we don't need to load the full data
sdatFile <- 'data/studDat.RData'
if(
(file.exists(sdatFile))& ## the prepared data is available
(file.info(sdatFile)$mtime>file.info('R/data.r')$mtime)& ## and up to date
(file.info(sdatFile)$mtime>file.info('R/preEts/preEtsData.r')$mtime)
){
  load(sdatFile)
} else
{
  fullDatFile <- 'data/dat.RData'
  if(
    (file.exists(fullDatFile))&
    (file.info(fullDatFile)$mtime>file.info('R/data.r')$mtime) ## and up to date
    ){
    load(fullDatFile)
  } else source('R/data.r') ### load in data, standard RSA data prep

  ### define sample of interest (see R/preEts/studentsWithDis.r)

  #### classified as "student with a disability" (SWD)
  ## studDat <- filter(dat,Student>0) ## only interested in "students with a disability"
  ## rm(dat) ## save ram
  ## gc()

  dat$stud <- dat$Student>0

  ### age (at application) within 1 year of the state age range for SWD:

  ## determine state-level age ranges
  ages <- strsplit(unique(dat$StateDisStudentAgeRange),';')%>%
    map_dfr(~tibble(StateDisStudentAgeRange=paste(.,collapse=';'),ageMin=.[1],ageMax=.[2]))

  ## bind to dat (check that we're lining things up right first)
  ages <- ages[match(dat$StateDisStudentAgeRange,ages$StateDisStudentAgeRange),]
  stopifnot(
    all.equal(dat$StateDisStudentAgeRange,ages$StateDisStudentAgeRange,na.rm=TRUE)
    )
  dat <- bind_cols(dat,ages[,-1])

## fill in missing values by state
  dat <- dat%>%
    group_by(state)%>%
    mutate(ageMaxMax=max(ageMax,na.rm=TRUE),ageMinMin=min(ageMin,na.rm=TRUE))%>%
    ungroup()%>%
    mutate(
      ageMax=ifelse(is.na(ageMax),ageMaxMax,ageMax),
      ageMin=ifelse(is.na(ageMin),ageMinMin,ageMin)
    )%>%
    select(-ageMinMin,-ageMaxMax)

## age is in range if it is within +/- 1 year of state's age range
  dat$inRange <-
    dat$age_app<=(as.numeric(dat$ageMax)+1)&
    dat$age_app>=(as.numeric(dat$ageMin)-1)

## when there's no application date, if there's a PETS start date set to TRUE, o.w. FALSE
  dat$inRange[is.na(dat$inRange)&!is.na(dat$PETSStartDate)] <- TRUE
  dat$inRange[is.na(dat$inRange)] <- FALSE

  dat$petsdate <- !is.na(dat$PETSStartDate)

  dat$post2014 <- ifelse(!dat$petsdate, TRUE, dat$PETSStartDate>20131231)

  dat$appPost2013 <- with(dat,ifelse(is.na(ApplicationDate)|petsdate,TRUE,ApplicationDate>20121231))

  fol <- function(x,levF,levT) factor(ifelse(x,levT,levF),levels=c(levF,levT))
  sampleSizes <- dat%>%
    mutate(
      stud=fol(stud,'non-SWD','SWD'),
      inRange=fol(inRange,'Age NOT in Range','Age in Range'),
      post2014=fol(post2014,'PETS b/f 2014','PETS 2014+'),
      appPost2013=fol(appPost2013,'App b/f 2013','App post 2013')
      )%>%
    group_by(stud,inRange,post2014,appPost2013)%>%
    summarize(
      total=n(),
      NonPETS=sum(!petsdate),
      PETS=sum(petsdate),
      totalDeaf=sum(deafAll=='deaf'),
      DeafNonPETS=sum(!petsdate&deafAll=='deaf'),
      DeafPETS=sum(deafAll=='deaf'&petsdate))

  write.csv(sampleSizes,'studResults/sampleSizes.csv',row.names=FALSE)

  studDat <- filter(dat,stud,inRange,post2014)#,appPost2013)

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


#### group numbers (Table 1 as of 4-17)
nums <- studDat%>%
  mutate(N=n())%>%
  group_by(group)%>%
  summarize(n=n(),`% of Total`=n()/N[1]*100) %>%
  ungroup()%>%
  add_case(group='total',n=sum(.$n),`% of Total`=sum(.$`% of Total`),.before=1)%>%
  add_case(group='Numbers of students with disabilities')%>%
  add_case(group=paste('Program Year:',paste(unique(studDat$ProgramYear),collapse=', ')))%>%
  mutate(`% of Total`=round(`% of Total`,1))

write_csv(nums,'results/studentWithDisabilityNumbers.csv')
