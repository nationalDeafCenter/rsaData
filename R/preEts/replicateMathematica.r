library(tidyverse)
source('R/data.r')

## if(!exists("dat")){
##   source('R/preEtsData.r')
## }

services <- c('jec','wble','ceo','wrt','isa')

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
