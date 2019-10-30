library(tidyverse)

dat <- read_csv('../../data/PY17annual_public_disabilitycode.csv')

### application ->  eligibility -> (OOSPlacement-> OOSExit->) ipe

dat$hasIPE <- !is.na(dat$age_IPE) ## only include people w/ IPE

dat$SecondDisability <- as.numeric(dat$SecondDisability)

dat$deafAll <- dat$PrimDisability%in%c(3:8)| dat$SecondDisability%in%c(3:8)## any deafness
dat$deafVis <- dat$PrimDisability%in%c(3,5)|dat$SecondDisability%in%c(3,5) ## deaf + hearing loss, primary communication visual
dat$deafAud <- dat$PrimDisability%in%c(4,6,7)|dat$SecondDisability%in%c(4,6,7) ## deaf + hearing loss+other, primary communication auditory
dat$deafBlind <- dat$PrimDisability==8|dat$SecondDisability==8
dat$physical <- dat$PrimDisability%in%c(10:16)&!dat$deafAll
dat$mental <- dat$PrimDisability%in%c(17:19)&!dat$deafAll


dat$deafAllPrim <- dat$PrimDisability%in%c(3:8)

dat <-
  mutate(dat,deafType=
               ifelse(PrimDisability%in%c(3,5),'Visual',
                 ifelse(PrimDisability%in%c(4,6,7),'Auditory',
                   ifelse(PrimDisability==8,'deafBlind',
                     ifelse(is.na(SecondDisability),'not deaf',
                       ifelse(SecondDisability%in%c(3,5),'Visual',
                         ifelse(SecondDisability%in%c(4,6,7),'Auditory',
                           ifelse(SecondDisability==8,'deafBlind','not deaf')))))))
    )

