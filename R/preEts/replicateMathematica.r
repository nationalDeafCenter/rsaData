library(tidyverse)
source('R/data.r')

## if(!exists("dat")){
##   source('R/preEtsData.r')
## }

services <- c('jec','wble','ceo','wrt','isa')

dat$stud <- dat$Student>0


### does the student have a valide PETS start date?
dat$petsdate <- !is.na(dat$PETSStartDate)


stateSumm <- dat%>%
  group_by(state)%>%
  summarize(nCaseload=n(),nStud=sum(stud,na.rm=TRUE),petsN=sum(petsdate),petsPerTot=petsN/nCaseload,petsPerStud=petsN/nStud,ageRange=StateDisStudentAgeRange[1])%>%
  full_join(tibble(state=state.abb,sname=state.name))%>%
  select(-state)%>%
  select(state=sname,everything())%>%
  filter(!is.na(state))%>%
    arrange(state)

write.csv(stateSumm,file='mathematicaTableB1replicate.csv')

### stud: student and within age range (age_app?)
ages <- strsplit(unique(dat$StateDisStudentAgeRange),';')%>%
  map_dfr(~tibble(StateDisStudentAgeRange=paste(.,collapse=';'),ageMin=.[1],ageMax=.[2]))

ages <- ages[match(dat$StateDisStudentAgeRange,ages$StateDisStudentAgeRange),]#dat,ages,by='StateDisStudentAgeRange')
all.equal(dat$StateDisStudentAgeRange,ages$StateDisStudentAgeRange,na.rm=TRUE)

dat <- bind_cols(dat,ages[,-1])

dat$stud2 <-
  dat$age_app<=dat$ageMax&
  dat$age_app>=dat$ageMin&
  (
    dat$InSecondaryEd>0|
    dat$EnrolledInASE>0|
    dat$EnrolledInPostsecEd>0|
    dat$EnrolledInCTTPNoCred>0|
    dat$EnrolledInCTTPCred
  )

dat$stud3 <-
  dat$age_app<=dat$ageMax&
  dat$age_app>=dat$ageMin&
  stud

stateSumm2 <- dat%>%
  group_by(state)%>%
  summarize(nCaseload=n(),nStud=sum(stud2,na.rm=TRUE),petsN=sum(petsdate),petsPerTot=petsN/nCaseload,petsPerStud=petsN/nStud,ageRange=StateDisStudentAgeRange[1])%>%
  full_join(tibble(state=state.abb,sname=state.name))%>%
  select(-state)%>%
  select(state=sname,everything())%>%
  filter(!is.na(state))%>%
  arrange(state)

stateSumm3 <- dat%>%
  group_by(state)%>%
  summarize(nCaseload=n(),nStud=sum(stud2|stud,na.rm=TRUE),petsN=sum(petsdate),petsPerTot=petsN/nCaseload,petsPerStud=petsN/nStud,ageRange=StateDisStudentAgeRange[1])%>%
  full_join(tibble(state=state.abb,sname=state.name))%>%
  select(-state)%>%
  select(state=sname,everything())%>%
  filter(!is.na(state))%>%
  arrange(state)


#We use students with disabilities to refer to students with disabilities who are within the minimum and maximum age range that the VR agency reported for its state definition for age of students with disabilities.

### several variables for each type of service, depending on how it was delivered
### here we just want indicators for whether the student received the service
### in the data it is coded as:
### 1 if student received service
### 2 if student received service and is no longer receiving
### NA student never received service

for(serv in services){
  dat[[serv]] <- apply(
    dat[,paste0(serv,c('VRAgencyStaff','VRAgencyPurchase','CompServiceProvider'))],
    1,
    function(x) any(!is.na(x))
  ) ## 1 or 2 (not NA)
  dat[[paste0(serv,'Current')]] <- apply(
    dat[,paste0(serv,c('VRAgencyStaff','VRAgencyPurchase','CompServiceProvider'))],
    1,
    function(x) any(!is.na(x)&x==1)
  ) ## only 1 (i.e. "currently")
}
dat$anyServ <- apply(dat[,services],1,any)
dat$anyServCurrent <- apply(dat[,paste0(services,'Current')],1,any)

### does the student have a valide PETS start date?
dat$petsdate <- !is.na(dat$PETSStartDate)

dat$stud <- dat$Student>0

### replicate mathematica numbers
dat <- dat%>%
  mutate(
    mathType=
      ifelse(is.na(ApplicationDate)&is.na(IPEAmendedDate)&stud&petsdate,'I',
      ifelse(!is.na(ApplicationDate)&!is.na(IPEAmendedDate)&stud&petsdate,'II',
      ifelse(!is.na(ApplicationDate)&!is.na(IPEAmendedDate)&stud&!petsdate,'III',#'other')))
      ifelse(!is.na(ApplicationDate)&!is.na(IPEAmendedDate)&!stud&!petsdate,'IV',
      ifelse(!is.na(ApplicationDate)&is.na(IPEAmendedDate)&!petsdate,'V','other')
      ))))
  )

dat%>%
  filter(is.na(age_app)|(age_app>14&age_app<21),is.na(exitDate),anyServCurrent)%>%
  group_by(mathType)%>%
  summarize(n())

math <- dat%>%
  filter(mathType%in%c('I','II'),stud,anyServCurrent,is.na(age_app)|(age_app>=14&age_app<22))%>%
  group_by(mathType)%>%
  summarize_at(paste0(services,'Current'),mean,na.rm=TRUE)
