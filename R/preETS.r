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

dat$stud <- dat$Student>0

services <- c('jec','wble','ceo','wrt','isa')

## does "provider type" tell us anything new about who's getting services?
for(serv in services)
  xtabs(as.formula(paste0('~',serv,'VRAgencyPurchase+',serv,'PurchaseProviderType')),dat,addNA=TRUE)%>%print()
## eh

for(serv in services)
  dat[[serv]] <- apply(
    dat[,paste0(serv,c('VRAgencyStaff','VRAgencyPurchase','CompServiceProvider'))],
    1,
    function(x) any(!is.na(x))
  )
dat$anyServ <- apply(dat[,services],1,any)

dat$servCombos <- apply(dat[,services],1,function(x) paste(services[x],collapse=';'))

commonCombos <- sapply(unique(dat$group),function(g) xtabs(~servCombos,filter(dat,group==g,anyServ,stud))%>%sort()%>%head(3),simplify=FALSE)

sink('results/commonCombos.txt')
print(commonCombos)
sink()

combos <-
  dat%>%
  filter(stud)%>%
  group_by(group)%>%
  mutate(ngrp=n())%>%
  ungroup()%>%
  filter(anyServ)%>%
  mutate(servCombos=servCombos%>%
           gsub("jec","Job Exploration Counseling",.)%>%
           gsub("wble","Work-Based Learning",.)%>%
           gsub("ceo","Counseling on Postsecondary Opportunities",.)%>%
           gsub("wrt","Workplace Readiness Training",.)%>%
           gsub("isa","Instruction in Self-Advocacy",.)
      )%>%
  group_by(group,servCombos)%>%
  summarize(N=n(),pct=N/ngrp[1]*100,value=paste0(N,' (',round(pct),'%)'))

combosFull <- combos%>%pivot_wider(id_cols='servCombos',names_from='group',values_fill=list(value=0))

combosTop3 <- combos%>%
  group_by(group)%>%
  arrange(desc(N))%>%
  slice(1:3)%>%
  mutate(value=paste0(servCombos,' (',N,'; ',round(pct),'%)'),num=1:n())%>%
  pivot_wider(id_cols='num',names_from='group')

openxlsx::write.xlsx(list(top3=combosTop3,full=combosFull),file='results/combosOfServices.xlsx')

for(serv in services)
  dat[[paste0(serv,'Current')]] <- apply(
    dat[,paste0(serv,c('VRAgencyStaff','VRAgencyPurchase','CompServiceProvider'))],
    1,
    function(x) any(!is.na(x)&x==1)
  )
dat$anyServCurrent <- apply(dat[,paste0(services,'Current')],1,any)

### age distribution is different for pre-ETS
dat$ageApp <- cut(dat$age_app,c(-Inf,14,18,21,Inf),c('<15','15-18','19-21','22+'))%>%
  as.character()%>%
  replace_na('No Application')


dat$petsdate <- !is.na(dat$PETSStartDate)

dat$ageLower <- as.numeric(substr(dat$StateDisStudentAgeRange,1,2))
dat$ageUpper <- as.numeric(substr(dat$StateDisStudentAgeRange,4,5))

### impute missing age ranges

dat <- dat%>%group_by(state)%>%mutate(
  ageLower=ifelse(!is.na(ageLower),ageLower,min(ageLower,na.rm=TRUE)),
  ageUpper=ifelse(!is.na(ageUpper),ageUpper,max(ageUpper,na.rm=TRUE))
)%>%
  ungroup()%>%
  mutate(ageInRange=age_app>=ageLower&age_app<=ageUpper)

dat$ageInRange[dat$age_app<14] <- FALSE

### basic stuff
sink('results/basic.txt')
cat('Age in range vs Student w Disability\n')

xtabs(~ageInRange+stud,dat,addNA=TRUE)
cat('% ppl with age in range who are "students w disability"\n')
print(mean(dat$stud[dat$ageInRange],na.rm=TRUE)*100)
cat('\n\namong "students w disability", % with age in range\n')
print(mean(dat$ageInRange[dat$stud],na.rm=TRUE)*100)
cat('\n\nTotal num with age in range:\n')
print(sum(dat$ageInRange,na.rm=TRUE))
cat('\n\nTotal num student w disability":\n')
print(sum(dat$stud,na.rm=TRUE))
cat('\n\nBy Disability Group:\n')
sink()

#### definitions of "group"
impTab <- groupDef(filter(dat,stud))
impTabPer <- groupDef(filter(dat,stud),percentage=TRUE)
openxlsx::write.xlsx(list(n=impTab,percentage=impTabPer),'results/groupDefSwD.xlsx')

numbers <- bind_rows(
  dat%>%filter(stud)%>%summarize(N=n()),
  dat%>%filter(stud)%>%group_by(group)%>%summarize(N=n()),
  dat%>%filter(stud)%>%group_by(deafAll)%>%summarize(N=n())%>%rename(group=deafAll)
)
for(demo in c('sex','raceEth','ageApp','SSDI','SSI')){
  numbers <- bind_rows(
    numbers,
    tibble(subgroup=demo),
    dat%>%filter(stud,!is.na(!!sym(demo)))%>%group_by(!!sym(demo))%>%summarize(N=n())%>%rename(subgroup=!!demo),
    dat%>%
      filter(stud,!is.na(!! sym(demo)))%>%
      group_by(group,!!sym(demo))%>%
      summarize(N=n())%>%
      rename(subgroup=!!demo),
    dat%>%
      filter(stud,!is.na(!! sym(demo)))%>%
      group_by(deafAll,!!sym(demo))%>%
      summarize(N=n())%>%
      rename(subgroup=!!demo,group=deafAll)
  )
}
numbers <- numbers%>%
  mutate(
    group=ifelse(is.na(group),'All',group),
    group=factor(group,levels=unique(group)),
    subgroup=ifelse(is.na(subgroup),'',subgroup),
    subgroup=factor(subgroup,levels=unique(subgroup)))%>%spread(group,N,fill=NA)


### replicate mathematica numbers
dat <- dat%>%
  mutate(
    mathType=
      ifelse(is.na(ApplicationDate)&is.na(IPEAmendedDate)&stud&petsdate,'I',
      ifelse(!is.na(ApplicationDate)&!is.na(IPEAmendedDate)&stud&petsdate,'II',
      ifelse(!is.na(ApplicationDate)&!is.na(IPEAmendedDate)&stud&!petsdate,'III',
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

## percentages <- bind_rows(
##   dat%>%filter(stud)%>%summarize(percent=100),
##   dat%>%filter(stud)%>%group_by(group)%>%summarize(N=n()),
##   dat%>%filter(stud)%>%group_by(deafAll)%>%summarize(N=n())%>%rename(group=deafAll)
## )
percentages <- tibble()
for(demo in c('sex','raceEth','ageApp','SSDI','SSI')){
  percentages <- bind_rows(
    percentages,
    tibble(subgroup=demo),
    dat%>%
      ungroup()%>%
      filter(stud,!is.na(!!sym(demo)))%>%
      mutate(groupN=n())%>%
      group_by(!!sym(demo))%>%
      summarize(percent=n()/groupN[1]*100)%>%
      rename(subgroup=!!demo),
    dat%>%
      filter(stud,!is.na(!! sym(demo)))%>%
      group_by(group)%>%mutate(groupN=n())%>%ungroup()%>%
      group_by(group,!!sym(demo))%>%
      summarize(percent=n()/groupN[1]*100)%>%
      rename(subgroup=!!demo),
    dat%>%
      filter(stud,!is.na(!! sym(demo)))%>%
      group_by(deafAll)%>%mutate(groupN=n())%>%ungroup()%>%
      group_by(deafAll,!!sym(demo))%>%
      summarize(percent=n()/groupN[1]*100)%>%
      rename(subgroup=!!demo,group=deafAll)
  )
}
percentages <- percentages%>%
  mutate(
    group=ifelse(is.na(group),'All',group),
    group=factor(group,levels=unique(group)),
    subgroup=ifelse(is.na(subgroup),'',subgroup),
    subgroup=factor(subgroup,levels=unique(subgroup)))%>%spread(group,percent,fill=NA)



provider <- tibble()
for(serv in services)
  provider <- bind_rows(
    provider,
    dat%>%filter(stud)%>%group_by(group)%>%
    summarize(
      any=mean(!!sym(serv)),
      inHouse=mean(!is.na(!!sym(paste0(serv,'VRAgencyStaff')))[!!sym(serv)]),
      purchase=mean(!is.na(!!sym(paste0(serv,'VRAgencyPurchase')))[!!sym(serv)]),
      compService=mean(!is.na(!!sym(paste0(serv,'CompServiceProvider')))[!!sym(serv)])
    )%>%
    pivot_longer(-group,names_to="provider")%>%
    mutate(value=value*100)%>%
    pivot_wider(id_cols="provider",names_from="group")%>%
    mutate(service=serv)%>%
    select(service,provider,everything())
  )


preEtsTab <- function(serv){

  what <- sym(serv)
  out <- dat%>%
    ##    filter(!is.na(EligibilityDate))%>% ## doesn't seem to exclude VR services
#    (see  tab(~is.na(EligibilityDate)+anyServ,dat)
  filter(
#    ageApp%in%c("<18","18-24"),
    Student>0#,ageInRange
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

petsdate <- dat%>%
  filter(
    Student>0
    )%>%
  group_by(group)%>%
    summarize(`% Receiving Any`=mean(petsdate,na.rm=TRUE)*100)%>%
    column_to_rownames("group")%>%
  t()%>%
  as.data.frame()%>%
  rownames_to_column("what")%>%
  add_case(what="Of which...")

for(serv in services){
  petsdate <- bind_rows(petsdate,
    dat%>%
      filter(
        Student>0,
        petsdate
      )%>%
      group_by(group)%>%
    summarize(overall=sum(!! sym(serv),na.rm=TRUE)/n()*100)%>%
    column_to_rownames("group")%>%
      t()%>%
      as.data.frame()%>%
      mutate(what=switch(serv,
        jec="Job Exploration Counseling",
        wble="Work-Based Learning",
        ceo="Counseling on Postsecondary Opportunities",
        wrt="Workplace Readiness Training",
        isa="Instruction in Self-Advocacy"
      ))
  )
}

pvalStar <- function(p) ifelse(p<0.001,'***',ifelse(p<0.01,'**',ifelse(p<0.05,'*',ifelse(p<0.1,'.',''))))
petsdate <- cbind(petsdate,pvalStar=c(NA,NA,
  map_chr(services,
    ~chisq.test(xtabs(as.formula(paste('~',.,'+group')),filter(dat,stud,petsdate)))$p.value%>%pvalStar())
  ))


preEts$type <- round(petsdate)

preEts <- map(preEts,function(x) rbind(x,`All numbers are % ever received pre-ETS service`=NA,`Classified as "Student with a Disability"`=NA))

preEts$provider <- bind_rows(provider,tibble(service=#'Within state\'s age range at application and
                                          '% Ever received service among those classified as Student with Disability'))

preEts$percentages=bind_rows(percentages,tibble(subgroup=#'Within state\'s age range at application and
                                          'Subgroup %s by student type, including only Students with Disabilities'))


preEts <- map(preEts,round,digits=1)

preEts$numbers=bind_rows(numbers,tibble(subgroup=#'Within state\'s age range at application and
                                          '# Classified as Student with Disability'))

preEts$numberWithStartDate=dat%>%
  filter(
    Student>0#,ageInRange
    )%>%
  group_by(group)%>%
    summarize(Nreceiving=sum(petsdate))%>%
    column_to_rownames("group")%>%
    t()

openxlsx::write.xlsx(preEts,"results/preEts.xlsx",rowNames=TRUE)

## preEtsCurrent <-  map(c('petsdate',paste0(c('anyServ',services),'Current')),preEtsTab)
## ## preEts <- map(1:length(preEts),
## ##   ~rbind(c(paste('% Receiving',c('Any pre-ETS Service',toupper(services))[.]),rep(NA,ncol(preEts[[.]]))),preEts[.])
## ##   )
## names(preEtsCurrent) <- c('Start Date','Any',toupper(services))

## petsdateCurrent <- dat%>%
##   filter(
##     Student>0
##     )%>%
##   group_by(group)%>%
##     summarize(`% Receiving Any`=mean(anyServCurrent,na.rm=TRUE)*100)%>%
##     column_to_rownames("group")%>%
##   t()%>%
##   as.data.frame()%>%
##   rownames_to_column("what")%>%
##   add_case(what="Of which...")

## for(serv in services){
##   petsdateCurrent <- bind_rows(petsdateCurrent,
##     dat%>%
##       filter(
##         Student>0,
##         anyServCurrent
##       )%>%
##       group_by(group)%>%
##     summarize(overall=sum(!! sym(paste0(serv,'Current')),na.rm=TRUE)/n()*100)%>%
##     column_to_rownames("group")%>%
##       t()%>%
##       as.data.frame()%>%
##   mutate(what=paste("% Currently Receive",serv))
##   )
## }

## preEtsCurrent$type <- petsdateCurrent

## preEtsCurrent <- map(preEtsCurrent,round,digits=1)

## preEtsCurrent <- map(preEtsCurrent,function(x) rbind(x,`All numbers are % currently receiving pre-ETS service`=NA,`Classified as "Student with a Disability"`=NA))


## openxlsx::write.xlsx(preEtsCurrent,"results/preEtsCurrent.xlsx",rowNames=TRUE)




## oos and pre-ets?
dat <- dat%>%
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


openxlsx::write.xlsx(oosEts,"results/oosEts.xlsx",rowNames=TRUE)


## oosETScurrent <- function(serv){
##   if(serv=='petsdate') dat$serv <- dat$petsdate else dat$serv <- dat[[paste0(serv,'Current')]]
##   dat%>%
##     filter(ageApp%in%c("<18","18-24"))%>%
##     group_by(group)%>%
##     mutate(currentOOS=oos=='currentOOS')%>%
##     summarize(
##       total=n(),
##       `# Current OOS`=sum(currentOOS,na.rm=TRUE),
##   `# Current PETS`=sum(serv,na.rm=TRUE),
##   `# OOS & PETS`=sum(serv&currentOOS,na.rm=TRUE)
##   )%>%
##     mutate(
##       `% of OOS who PETS`=`# OOS & PETS`/`# Current OOS`*100,
##       `% of PETS who OOS`=`# OOS & PETS`/`# Current PETS`*100
##     )
## }

## oosEtsCurrent <-  map(c('petsdate','anyServ',services),oosETScurrent)
## ## preEts <- map(1:length(preEts),
## ##   ~rbind(c(paste('% Receiving',c('Any pre-ETS Service',toupper(services))[.]),rep(NA,ncol(preEts[[.]]))),preEts[.])
## ##   )
## names(oosEtsCurrent) <- c('Has Start Date','Any',toupper(services))

## oosEtsCurrent <- map(oosEtsCurrent,round,digits=1)

## oosEtsCurrent <- map(oosEtsCurrent,function(x) rbind(x,`All numbers are % currently receive pre-ETS service`=NA,`Classified as "Student with a Disability"`=NA))


## openxlsx::write.xlsx(oosEtsCurrent,"results/oosEtsCurrent.xlsx",rowNames=TRUE)













