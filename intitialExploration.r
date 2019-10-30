### significance?
## overall:
xtabs(~DisabilitySigCode,dat,addNA=TRUE)
## by deafness:
xtabs(~DisabilitySigCode+deafAll,dat,addNA=TRUE)

dat%>%
  filter(deafAll)%>%
  mutate(deafOrHearingLoss=
           ifelse(PrimDisability%in%c(3,4)|SecondDisability%in%c(3,4),'Deaf',
             ifelse(PrimDisability%in%c(5,6)|SecondDisability%in%c(5,6),'Hearing Loss','Other Deaf')))%>%
  xtabs(~DisabilitySigCode+deafOrHearingLoss,data=.)%>%
  as.data.frame()%>%
  ggplot(aes(deafOrHearingLoss,Freq,fill=DisabilitySigCode))+
  geom_col(position='dodge')

## "deaf" is more "significant" than "hearing loss"



dat%>%
  filter(hasIPE)%>%
  mutate(deafAll=factor(ifelse(deafAll,'deaf','not deaf')))%>%
  group_by(deafAll)%>%summarize(propEnrolledPostSec=mean(EnrolledInPostsecEd,na.rm=TRUE),age=mean(age_IPE,na.rm=TRUE))


ageAdjustEnrolled <- lm(EnrolledInPostsecEd~deafAll+as.factor(age_IPE),dat)

enrolledByAgeDeaf <-
  dat%>%
  filter(hasIPE)%>%
  group_by(age_IPE,deafAll)%>%
  summarize(propEnrolledPostSec=mean(EnrolledInPostsecEd,na.rm=TRUE),n=n())

ggplot(filter(enrolledByAgeDeaf,age_IPE>17),aes(age_IPE,propEnrolledPostSec,color=deafAll))+geom_point(aes(size=n))+geom_line()
ggsave('postSecEnrollmentByAgeDeaf.png')


group_by(age_IPE)%>%
  summarize(propDeaf=propEnrolledPostSec[deafAll],propHear=propEnrolledPostSec[!deafAll],nDeaf=n[deafAll],nHear=n[!deafAll])

enrolledDiff <- lm(EnrolledInPostsecEd~deafAll,dat)

dat%>%filter(hasIPE)%>%mutate(deafAll=factor(ifelse(deafAll,'deaf','not deaf')))%>%ggplot(aes(age_IPE))+geom_bar()+facet_wrap(~deafAll,ncol=1,scales='free_y')
ggsave('ageDistributions.png')

#### PETS
pets <-
  dat%>%
  filter(!is.na(PETSStartDate))%>%
  mutate(deafAll=factor(ifelse(deafAll,'deaf','not deaf')))%>%
  select(
    deafAll,
    PETSStartDate,
    starts_with('jec'),
    starts_with('wble'),
    starts_with('ceo'),
    starts_with('wrt'),
    starts_with('isa')
  )%>%
  select(
    deafAll,
    PETSStartDate,
    ends_with('VRAgencyStaff'),
    ends_with('VRAgencyPurchase'),
    ends_with('CompServiceProvider'))

pets2 <- data.frame(startDate=pets$PETSStartDate,deafAll=pets$deafAll)
for(ss in c('jec','wble','ceo','wrt','isa'))
  pets2[[ss]] <- apply(pets%>%select(starts_with(ss)),1,function(x) any(!is.na(x)))

pets2%>%
  mutate(
    anyType=jec|wble|ceo|wrt|isa,
    multiple=jec+wble+ceo+wrt+isa>1
  )%>%
  group_by(deafAll)%>%
  summarize_at(vars(-startDate),mean)
