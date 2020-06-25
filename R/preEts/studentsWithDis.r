library(tidyverse)
source('R/data.r')

## if(!exists("dat")){
##   source('R/preEtsData.r')
## }

services <- c('jec','wble','ceo','wrt','isa')

dat$stud <- dat$Student>0


### does the student have a valide PETS start date?
dat$petsdate <- !is.na(dat$PETSStartDate)


##### records of specific PETS
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


pdf('studResults/ageStudBoxplot.pdf')
boxplot(age_app~petsdate,data=dat)
dev.off()

outlier <- function(x){
  iqr <- IQR(x,na.rm=TRUE)
  qq <- quantile(x,c(.25,.75),na.rm=TRUE)
  (x<qq[1]-1.5*iqr)|(x>qq[2]+1.5*iqr)
}

dat%>%filter(petsdate)%>%
  mutate(outlier = outlier(age_app)) %>%
  ggplot(aes('',y=age_app))+geom_boxplot(outlier.shape=NA)+geom_point(data=function(x) filter(x,outlier),position='jitter')+scale_y_continuous(trans='log')

p <- dat%>%filter(petsdate)%>%
  ggplot(aes(age_app))+geom_dotplot()#+scale_x_continuous(trans='log')

pdf('ageStudBarplot.pdf')
plot(table(dat$age_app[dat$petsdate]))
dev.off()

### stud: student and within age range (age_app?)

ages <- strsplit(unique(dat$StateDisStudentAgeRange),';')%>%
  map_dfr(~tibble(StateDisStudentAgeRange=paste(.,collapse=';'),ageMin=.[1],ageMax=.[2]))

ages <- ages[match(dat$StateDisStudentAgeRange,ages$StateDisStudentAgeRange),]#dat,ages,by='StateDisStudentAgeRange')
all.equal(dat$StateDisStudentAgeRange,ages$StateDisStudentAgeRange,na.rm=TRUE)

dat <- bind_cols(dat,ages[,-1])

## fill in missing values by state
dat <- dat%>%group_by(state)%>%mutate(ageMaxMax=max(ageMax,na.rm=TRUE),ageMinMin=min(ageMin,na.rm=TRUE))%>%ungroup()%>%mutate(ageMax=ifelse(is.na(ageMax),ageMaxMax,ageMax),ageMin=ifelse(is.na(ageMin),ageMinMin,ageMin))%>%select(-ageMinMin,-ageMaxMax)

dat$inRange <- dat$age_app<=(as.numeric(dat$ageMax)+1)&
  dat$age_app>=(as.numeric(dat$ageMin)-1)
dat$inRange[is.na(dat$inRange)&!is.na(dat$PETSStartDate)] <- TRUE
dat$inRange[is.na(dat$inRange)] <- FALSE

xtabs(~stud+inRange,data=dat)

dat$deaf <- dat$deafAll=='deaf'

library(lubridate)
dat$PETSStartDate2 <- ymd(dat$PETSStartDate)
dat$PETSStartYear <- year(dat$PETSStartDate2)

dat$numerator <- dat$stud&dat$petsdate&dat$inRange&dat$PETSStartYear>2013

sink('studResults/boundaryTable.csv')
cat('SWD & age below range-1,')
cat(
sum(dat$stud&dat$age_app<(as.numeric(dat$ageMin)-1),na.rm=TRUE),
  sum(dat$stud&dat$age_app<(as.numeric(dat$ageMin)-1),na.rm=TRUE)/sum(dat$stud),
sum(dat$stud&dat$age_app<(as.numeric(dat$ageMin)-1)&dat$deaf,na.rm=TRUE),
  sum(dat$stud&dat$age_app<(as.numeric(dat$ageMin)-1)&dat$deaf,na.rm=TRUE)/sum(dat$stud&dat$deaf),'\n',sep=',')

cat('SWD & age above range+1',
    sum(dat$stud&dat$age_app>(as.numeric(dat$ageMax)+1),na.rm=TRUE),
    sum(dat$stud&dat$age_app>(as.numeric(dat$ageMax)+1),na.rm=TRUE)/sum(dat$stud),
    sum(dat$stud&dat$age_app>(as.numeric(dat$ageMax)+1)&dat$deaf,na.rm=TRUE),
    sum(dat$stud&dat$age_app>(as.numeric(dat$ageMax)+1)&dat$deaf,na.rm=TRUE)/sum(dat$stud&dat$deaf),'\n',sep=',')

cat('SWD & age outside range+/-1',
    sum(dat$stud&!dat$inRange,na.rm=TRUE),
    sum(dat$stud&!dat$inRange,na.rm=TRUE)/sum(dat$stud),
    sum(dat$stud&!dat$inRange&dat$deaf,na.rm=TRUE),
    sum(dat$stud&!dat$inRange&dat$deaf,na.rm=TRUE)/sum(dat$stud&dat$deaf),'\n',sep=',')

cat('PETS Start Date & NOT SWD',
    sum(dat$petsdate&!dat$stud,na.rm=TRUE),
    sum(dat$petsdate&!dat$stud,na.rm=TRUE)/sum(dat$petsdate),
    sum(dat$petsdate&!dat$stud&dat$deaf,na.rm=TRUE),
    sum(dat$petsdate&!dat$stud&dat$deaf,na.rm=TRUE)/sum(dat$petsdate&dat$deaf),'\n',sep=',')

cat('PETS Start Date & age in range +/-1 but NOT SWD',
    sum(dat$petsdate&dat$inRange&!dat$stud,na.rm=TRUE),
    sum(dat$petsdate&dat$inRange&!dat$stud,na.rm=TRUE)/sum(dat$inRange&dat$petsdate),
    sum(dat$petsdate&dat$inRange&!dat$stud&dat$deaf,na.rm=TRUE),
    sum(dat$petsdate&dat$inRange&!dat$stud&dat$deaf,na.rm=TRUE)/sum(dat$inRange&dat$petsdate&dat$deaf),'\n',sep=',')

cat('Documented service & PETS Start Date & age in range +/-1 but NOT SWD',
    sum(dat$anyServ&dat$petsdate&dat$inRange&!dat$stud,na.rm=TRUE),
    sum(dat$anyServ&dat$petsdate&dat$inRange&!dat$stud,na.rm=TRUE)/sum(dat$petsdate&dat$inRange&!dat$stud,na.rm=TRUE),
    sum(dat$anyServ&dat$petsdate&dat$inRange&!dat$stud&dat$deaf,na.rm=TRUE),
    sum(dat$anyServ&dat$petsdate&dat$inRange&!dat$stud&dat$deaf,na.rm=TRUE)/sum(dat$petsdate&dat$inRange&!dat$stud&dat$deaf,na.rm=TRUE),'\n',sep=',')

cat('Documented service & PETS Start Date & age in range +/-1 and YES SWD',
    sum(dat$anyServ&dat$petsdate&dat$inRange&dat$stud,na.rm=TRUE),
    sum(dat$anyServ&dat$petsdate&dat$inRange&dat$stud,na.rm=TRUE)/sum(dat$petsdate&dat$inRange&dat$stud,na.rm=TRUE),
    sum(dat$anyServ&dat$petsdate&dat$inRange&dat$stud&dat$deaf,na.rm=TRUE),
    sum(dat$anyServ&dat$petsdate&dat$inRange&dat$stud&dat$deaf,na.rm=TRUE)/sum(dat$petsdate&dat$inRange&dat$stud&dat$deaf,na.rm=TRUE),'\n',sep=',')


cat('Documented current service & PETS Start Date & age in range +/-1 but NOT SWD',
    sum(dat$anyServCurrent&dat$petsdate&dat$inRange&!dat$stud,na.rm=TRUE),
    sum(dat$anyServCurrent&dat$petsdate&dat$inRange&!dat$stud,na.rm=TRUE)/sum(dat$petsdate&dat$inRange&!dat$stud,na.rm=TRUE),
    sum(dat$anyServCurrent&dat$petsdate&dat$inRange&!dat$stud&dat$deaf,na.rm=TRUE),
    sum(dat$anyServCurrent&dat$petsdate&dat$inRange&!dat$stud&dat$deaf,na.rm=TRUE)/sum(dat$petsdate&dat$inRange&!dat$stud&dat$deaf,na.rm=TRUE),'\n',sep=',')


cat('SWD & PETS Start before 2014',
    sum(dat$stud&dat$PETSStartYear<2014,na.rm=TRUE),
    sum(dat$stud&dat$PETSStartYear<2014,na.rm=TRUE)/sum(dat$petsdate&dat$stud),
    sum(dat$stud&dat$PETSStartYear<2014&dat$deaf,na.rm=TRUE),
    sum(dat$stud&dat$PETSStartYear<2014&dat$deaf,na.rm=TRUE)/sum(dat$petsdate&dat$stud),'\n',sep=',')

cat('PETS service but not SWD or no start date',
    sum(dat$anyServ & (!dat$stud|!dat$petsdate),na.rm=TRUE),
    sum(dat$anyServ & (!dat$stud|!dat$petsdate),na.rm=TRUE)/sum(dat$anyServ,na.rm=TRUE),
    sum(dat$deaf&dat$anyServ & (!dat$stud|!dat$petsdate),na.rm=TRUE),
    sum(dat$deaf&dat$anyServ & (!dat$stud|!dat$petsdate),na.rm=TRUE)/sum(dat$deaf&dat$anyServ,na.rm=TRUE),'\n',sep=',')

cat('SWD and start date>=2014 and age in range but no PETS service',
    sum(!dat$anyServ & dat$numerator,na.rm=TRUE),
    sum(!dat$anyServ & dat$numerator,na.rm=TRUE)/sum(dat$numerator,na.rm=TRUE),
    sum(dat$deaf&!dat$anyServ & dat$numerator,na.rm=TRUE),
    sum(dat$deaf&!dat$anyServ & dat$numerator,na.rm=TRUE)/sum(dat$deaf&dat$numerator,na.rm=TRUE),'\n',sep=',')

sink()

pdf('studResults/startDateVsSWD.pdf')
dat%>%filter(PETSStartYear>2000)%>%boxplot(PETSStartDate2~stud,data=.)
dev.off()


pdf('studResults/petsStartDate.pdf')
plot(table(year(dat$PETSStartDate2)))
dev.off()

### what about ppl who are not SWD but have a PETS start date?
## did they get services?



#### application year vs PETS start year
dat$appYear <- year(ymd(dat$ApplicationDate))

pdf('studResults/appYearVsPETSyear.pdf')
plot(table(dat$appYear-dat$PETSStartYear))
plot(table(dat$appYear[dat$deaf]-dat$PETSStartYear[dat$deaf]),main='Deaf')
dev.off()

for(diff in 0:5){
  cat('diff=',diff,' ',round(mean(abs(dat$appYear-dat$PETSStartYear)<=diff,na.rm=TRUE)*100),'%\n',sep='')
}

for(diff in 0:5){
  cat('diff=',diff,' ',round(mean(abs(dat$appYear[dat$deaf]-dat$PETSStartYear[dat$deaf])<=diff,na.rm=TRUE)*100),'%\n',sep='')
}


mean(year(dat$PETSStartDate2)<2015,na.rm=TRUE)

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
  dat$stud

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



### who are the non-SWDs with start dates?

#if you could put together the demographics on this group-- start dates, ages, whatever info we have... just so we have that handy for review, that would also be good while we wait for Mathematica response?


dat2 <- filter(dat,petsdate)

dat2%>%ggplot(aes(age_app))+geom_bar()+facet_wrap(~stud,ncol=1,scales="free_y")
ggsave('studResults/ageVsSWD.jpg')

dat2%>%ggplot(aes(PETSStartYear))+geom_bar()+facet_wrap(~stud,ncol=1,scales="free_y")+xlim(2000,2020)


xtabs(~is.na(ApplicationDate)+stud,dat2)

dat2%>%group_by(stud)%>%summarize(mean(is.na(ApplicationDate)))

dat2$appYear <- year(ymd(dat2$ApplicationDate))
dat2%>%ggplot(aes(appYear-PETSStartYear))+geom_bar()+facet_wrap(~stud,ncol=1,scales="free_y")
ggsave('studResults/appStartDiffvsSWD.jpg')


dat2$appDate <- ymd(
