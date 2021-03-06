library(tidyverse)
options(stringsAsFactors=FALSE)
library(openxlsx)
options("openxlsx.wrapText"=TRUE)
## Deaf (no secondary, exclude deafblind)
## Deaf (two disabilities at least 1 is deaf,  exclude deafblind)
## DeafBlind (primary or secondary or either)

## Comparison groups:
## Everyone else
## Mobility
## Cognitive

dat <- read_csv('../../data/PY17annual_public_disabilitycode.csv',col_types = cols(.default = col_character())) %>% type_convert()

### application ->  eligibility -> (OOSPlacement-> OOSExit->) ipe

impairments <- read.csv('impairments.csv',sep=';')

dat <- dat%>%
    mutate(
      hasIPE =!is.na(dat$age_IPE),
      SecondDisability = as.numeric(dat$SecondDisability),
      primCat=
        ifelse(PrimDisability%in%3:7,'deaf',
          ifelse(PrimDisability==8,'deafblind',
            ifelse(PrimDisability%in%10:16,'mobility',
              ifelse(PrimDisability>16,'cognitive',
                ifelse(PrimDisability%in%c(1,2),'visual','communicative'))))),
      SecondCat=
        ifelse(is.na(SecondDisability)|SecondDisability==0,'none',
          ifelse(SecondDisability%in%3:7,'deaf',
            ifelse(SecondDisability==8,'deafblind',
              ifelse(SecondDisability%in%10:16,'mobility',
                ifelse(SecondDisability>16,'cognitive',
                  ifelse(SecondDisability%in%c(1,2),'visual','communicative')))))),
      group=
        ifelse(primCat=='deafblind'|SecondCat=='deafblind','deafblind',
          ifelse(SecondCat=='none',
            ifelse(primCat=='deaf','deafNonDisabled',primCat),
            ifelse(primCat=='deaf'|SecondCat=='deaf','deafDisabled',primCat)
          )),
      deafAll=ifelse(group%in%c('deafblind','deafNonDisabled','deafDisabled'),'deaf','hearing')
    )
dat$group[dat$group=='deafDisabled'&(dat$primCat=='visual'|dat$SecondCat=='visual')] <- 'deafblind'

### impairments by group table
sink('results/groupDefs.txt')
print(xtabs(~primCat+SecondCat+group,dat,addNA=TRUE,drop=TRUE))
sink()


dat$imp1 <- impairments$imp[dat$PrimDisability+1]
dat$imp2 <- impairments$imp[dat$SecondDisability+1]
dat$imp2 <- ifelse(dat$imp2%in%c(
  "No Impairment",
  "Other Hearing Impairments (Tinnitus, Meniere's Disease, hyperacusis, etc.)",
  "Deafness, Primary Communication Auditory",
  "Hearing Loss, Primary Communication Auditory",
  "Hearing Loss, Primary Communication Visual",
  "Deafness, Primary Communication Visual",
  "Deaf-Blindness"),
  dat$imp2,
  "Other Non-Hearing-Related Impairment")

dat$imp1 <- factor(dat$imp1,levels=impairments$imp)
dat$imp2 <- factor(dat$imp2,levels=c(impairments$imp,"Other Non-Hearing-Related Impairment"))


#sink('impTab.tsv')
#cat('Group\tPrimary\tSecondary\tDefinition\n')
groupDef <- function(dat,percentage=FALSE){
  impTab <- tibble()
  for(g in sort(unique(dat$group))){
    #cat(g)
    denom <- ifelse(percentage,sum(dat$group==g)/100,1)
    prim <- xtabs(~imp1,data=filter(dat,group==g),drop=TRUE)/denom
    prim <- paste0(names(prim),' (',round(unname(prim)),')')
    sec <- xtabs(~imp2,data=filter(dat,group==g),drop=TRUE)/denom
    sec <- paste0(names(sec),' (',round(unname(sec)),')')

    for(i in 1:max(c(length(prim),length(sec)))){
      # cat('\t')
      if(i==1){
        impTab <- bind_rows(impTab,tibble(Group=g,Primary=prim[1],Secondary=sec[1],
          Definition=switch(g,
            cognitive="Non-deaf; primary disability classifed as \"Mental Impairment\"",
            deafblind="Primary or secondary disability is \"Deaf-Blindness\"",
            deafNonDisabled="Primary disability is deafness or hearing loss/impairment; no secondary disability",
            deafDisabled="Primary or secondary disability is deafness or hearing loss/impairment; a secondary disability is listed",
            mobility="Non-deaf; primary disability classified as \"Physical Impairment\"",
            other="Non-deaf; primary disability is another \"Sensory/Communicative Impairment\"")
        )
        )
      } else impTab <- bind_rows(impTab,tibble(Primary=prim[i],Secondary=sec[i]))
    }
  }
  impTab
}

## impTab <- groupDef(dat)
## impTabPer <- groupDef(dat,percentage=TRUE)
## openxlsx::write.xlsx(list(n=impTab,percentage=impTabPer),'results/groupDefFull.xlsx')
#,alignment=Alignment(wrapText=TRUE))


## compute numbers:
total <- c(table(dat$group),total=nrow(dat))
total <- rbind(n=total,percent=total/nrow(dat)*100)
total <- rbind(
  n=prettyNum(total[1,],big.mark=',',digits=0),
  percent=prettyNum(total[2,],digits=2))

withIPE <- c(table(filter(dat,hasIPE)$group),withIPE=sum(dat$hasIPE))
withIPE <- rbind(n=withIPE,percent=withIPE/sum(dat$hasIPE)*100)
withIPE <- rbind(
  n=prettyNum(withIPE[1,],big.mark=',',digits=0),
  percent=prettyNum(withIPE[2,],digits=2))


###### demos
#############################
##sex
#############################
## CodeDescription
## 1Individual indicates that he is male.
## 2Individual indicates that she is female.
## 9Individual did not self-identify their sex.
dat <- mutate(dat,sex=ifelse(Sex==1,'Male',ifelse(Sex==2,'Female',NA)))


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

dat$nrace <- rowSums(dat[,setdiff(raceVars,'White')],na.rm=TRUE)
dat$nrace2 <- rowSums(dat[,setdiff(raceVars,c('White','Hispanic'))],na.rm=TRUE)
#table(dat$nrace)/nrow(dat)

## a combined race variable modeled after attainment/employment report
dat <- mutate(dat,
  sex=ifelse(Sex==1,'Male',ifelse(Sex==2,'Female',NA)),
  raceEth=
    ifelse(!is.na(Hispanic)&Hispanic==1,"Latinx",
      ifelse(nrace>1,"Multiracial",
        ifelse(!is.na(Black)&Black==1,"African American",
          ifelse(!is.na(Asian)&Asian==1,"Asian",
            ifelse(!is.na(Hawaiian)&Hawaiian==1,"Hawaiian",
              ifelse(!is.na(AmerIndian)&AmerIndian==1,"American Indian",
                ifelse(!is.na(White)&White==1,"White","Other/NA"))))))),
  ageApp=ifelse(age_app<18,'<18',
             ifelse(age_app<25,'18-24',
               ifelse(age_app<35,'25-35',
                 ifelse(age_app<45,'35-44',
                   ifelse(age_app<55,'45-54',
                     ifelse(age_app<65,'55-64',
                       ifelse(age_app<75,'65-74','75+'))))))),
  ageIPE=ifelse(age_IPE<18,'<18',
             ifelse(age_IPE<25,'18-24',
               ifelse(age_IPE<35,'25-35',
                 ifelse(age_IPE<45,'35-44',
                   ifelse(age_IPE<55,'45-54',
                     ifelse(age_IPE<65,'55-64',
                       ifelse(age_IPE<75,'65-74','75+')))))))
)


states <- read.csv('states.csv')
states$abb <- gsub(' ','',states$abb)
dat <- mutate(dat,
  acode=ifelse(AgencyCode>56,AgencyCode-56,AgencyCode)
)
dat$state <- states$abb[dat$acode]


### ssi/ssri
dat <- mutate(dat,
  SSDI=ifelse(appSSDI==1,
    'Applicant receives SSDI',
    'Applicant does not receive SSDI'),
  SSI=ifelse(appSSI==1,
    'Applicant receives SSI',
    'Applicant does not receive SSI')
)


### Status at IEP: employment, hourly wage, highestEdcompleted,

edLevs <-
  ## copied from CASE SERVICE REPORT (RSA-911) p. 64
  c(
    "Individual attained a secondary school diploma.",
    "Individual attained a secondary school equivalency.",
    "Individual has a disability and attained a certificate of attendance/completion as a result of successfully completing an Individualized Education Program (IEP).",
    "Individual completed one or more years of postsecondary education.",
    "Individual attained a postsecondary certification, license, or educational certificate (non-degree).",
    "Individual attained an Associate's Degree.",
    "Individual attained a Bachelor's Degree.",
    "Individual attained a degree beyond a Bachelor's Degree.",
    "No educational level was completed."
  )

dat <- mutate(dat,
  education=#edLevs[EdLevelCompleted],
    cut(EdLevelCompleted,c(0,3,4,5,6,7,8,9),levels=c('HS','Some College','Certificate','AA','BA','>BA','no HS')),
  employment=
    cut(ipeEmpStatus,c(0,6,9,10),labels=c('Employed','Student','Not Employed')),
  hourlyWage= cut(ipeHourlyWage, c(0, 1, 7.25, 9, 11, 15, Inf), include.lowest = TRUE, right = FALSE),
  fulltime=ipeWeeklyHoursWorked>=35
)

save(dat,file='data/dat.RData')
