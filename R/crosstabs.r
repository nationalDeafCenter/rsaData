library(tidyverse)
library(openxlsx)
source('R/data.r')
source('R/crossTabsFunctions.r')


#Crosstab for our groups, race, sex, age!?, SSDI, SSI, Highest Educational Level Completed

### look at only people with IPE?
# dat <- filter(dat,hasIPE)


crossTabs <- list()


#############################
## IPE
#############################
crossTabs$IPE <- ct('hasIPE',dat)
### look at only people with IPE?
# dat <- filter(dat,hasIPE)

#############################
##sex
#############################
## CodeDescription
## 1Individual indicates that he is male.
## 2Individual indicates that she is female.
## 9Individual did not self-identify their sex.
dat <- mutate(dat,sex=ifelse(Sex==1,'Male',ifelse(Sex==2,'Female',NA)))

crossTabs$sex <- ct('sex',dat)

#############################
# race
#############################
raceVars <- c(
  'AmerIndian',
  'Asian',
  'Black',
  'Hawaiian',
  'White',
  'Hispanic'
)

for(rv in raceVars) dat[[rv]][dat[[rv]]==9] <- NA

### how much overlap is there?
for(i in 1:(length(raceVars)-1))
  for(j in (i+1):length(raceVars)){
    print(paste(raceVars[i],raceVars[j]))
    print(xtabs(as.formula(paste('~',raceVars[i],'+',raceVars[j])),dat,addNA=TRUE))
  }
## a lot

dat$nrace <- rowSums(dat[,setdiff(raceVars,'White')],na.rm=TRUE)
table(dat$nrace)

for(rv in raceVars){
  print(rv)
  dat%>%
    filter(deafAll,!!sym(rv)==1)%>%
    mutate(N=n())%>%
    group_by(nrace)%>%
    summarize(n=n(),per=n/mean(N)*100)%>%
    ungroup()%>%
    print()
  }

for(rv in raceVars){
  print(rv)
  dat%>%filter(deafAll,!!sym(rv)==1)%>%
    select(AmerIndian:Hispanic)%>%
    colMeans(na.rm=TRUE)%>%
    print()
}



crossTabs$race <- lapply(raceVars,ct,dat=dat)

## a combined race variable (i'm sure what i'm doing here is problematic...this is first try)
dat$raceCombined <- ifelse(dat$nrace>1,'multi',NA)
for(rv in sort(raceVars))
  dat <- within(dat,raceCombined[is.na(raceCombined)&dat[[rv]]==1] <- rv)

### check:
sapply(raceVars,function(rv) c(sum(dat[[rv]],na.rm=TRUE),sum(dat$raceCombined==rv,na.rm=TRUE)))

crossTabs$race$combined <- ct('raceCombined',data=dat)

#### Age
dat%>%
  mutate(deaf=ifelse(deafAll,'deaf','hearing'))%>%
  ggplot(aes(age_IPE))+
  geom_bar()+
  facet_wrap(~deaf,ncol=1,scales='free_y')
ggsave('ageByDeaf.jpg')

dat%>%
  filter(deafAll)%>%
  ggplot(aes(age_IPE))+
  geom_bar()+
  facet_wrap(~deafType,ncol=1,scales='free_y')
ggsave('ageByDeafType.jpg')

crossTabs$age <-
  dat%>%
  mutate(ageCat=
           ifelse(age_IPE<18,'<18',
             ifelse(age_IPE<25,'18-24',
               ifelse(age_IPE<35,'25-35',
                 ifelse(age_IPE<45,'35-44',
                   ifelse(age_IPE<55,'45-54',
                     ifelse(age_IPE<65,'55-64',
                       ifelse(age_IPE<75,'65-74','75+'))))))))%>%
  ct('ageCat',.)


### SSDI,
crossTabs$SSDI <-
  dat%>%
  mutate(`Receives SSDI at Application`=ifelse(appSSDI==1,'Yes','No'))%>%
  ct('`Receives SSDI at Application`',.)


### SSI,
crossTabs$SSI <-
  dat%>%
  mutate(`Receives SSI at Application`=ifelse(appSSI==1,'Yes','No'))%>%
  ct('`Receives SSI at Application`',.)




### Highest Educational Level Completed

## 1 Individual attained a secondary school diploma.
## 2 Individual attained a secondary school equivalent.
## 3 Individual has a disability and attained a certificate of attendence/completion as a result of succesfully completing an Individualized Education Program (IPE).
## 4 Individual completed one or more years of postsecondary education.
## 5 Individual attained a postsecondary certification, license, or educational certificate (non-degree).
## 6 Individual attained an Associate's Degree.
## 7 Individual attained a Bachelor's Degree.
## 8 Individual attained a degree beyond a Bachelor's Degree.
## 9 No educational level was completed.

dat <- dat%>%
  mutate(attain=
           factor(
             ifelse(EdLevelCompleted%in%c(1,2),'HS',
               ifelse(EdLevelCompleted==3,'HScert',
                 ifelse(EdLevelCompleted==4, 'Some College',
                   ifelse(EdLevelCompleted==5,'College Cert',
                     ifelse(EdLevelCompleted==6,'AA',
                       ifelse(EdLevelCompleted==7,'BA',
                         ifelse(EdLevelCompleted==8,'>BA','none'))))))),
             levels=c('none','HScert','HS','Some College','College Cert','AA','BA','>BA'),
             ordered=TRUE
           )
  )

crossTabs$edAttainment <- ct('attain',dat)

crossTabs$edAttainment$type[[3]]%>%
  as.data.frame()%>%
  rownames_to_column('Attainment')%>%
  mutate(Attainment=factor(Attainment,levels=levels(dat$attain)))%>%
  gather(disabilityType,percent,-Attainment)%>%
  ggplot(aes(Attainment,percent,group=disabilityType,color=disabilityType))+
  geom_line(size=1.2)+
  scale_color_manual(values=subwayPalette)+
  xlab('% Attained At Least...')
ggsave('edAttainmentByDisabilityType.jpg')


### OOS

## check:
with(dat,sum(!is.na(OOSExitDate)&is.na(OOSPlacementDate)))

dat <- dat%>%
  mutate(
    oos=ifelse(
      is.na(OOSPlacementDate),'neverOOS',
      ifelse(is.na(OOSExitDate),'currentOOS','previousOOS')
    ),
    oosTime=ifelse(oos=='previousOOS',
      as.Date(OOSExitDate,format="%Y%m%d")-as.Date(OOSPlacementDate,format="%Y%m%d"),
      NA
    )
  )

crossTabs$oos <- ct('oos',dat)

pdf('oosBoxplots.pdf')

dat%>%
  mutate(deaf=ifelse(deafAll,'deaf','hearing'))%>%
  select(oosTime,deaf,deafType,type)%>%
  gather('grouping','group',-oosTime)%>%
  ggplot(aes(group,oosTime))+
  geom_boxplot()+
  scale_y_continuous("Days on OOS",trans="log",breaks=c(10,100,500,1000,3000))+
  facet_wrap(~grouping,scales="free_x")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('timeOnOOS.jpg')


write.xlsx(lapply(setdiff(names(crossTabs),'race'),toExcel,crossTabs=crossTabs),
  file='rsaCrossTabs.xlsx',
  rowNames=FALSE,colNames=FALSE)

write.xlsx(lapply(names(crossTabs$race),toExcel,crossTabs=crossTabs$race),file='rsaRaceCrossTabs.xlsx',colNames=FALSE)
