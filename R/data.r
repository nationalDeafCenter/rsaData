library(tidyverse)
options(stringsAsFactors=FALSE)

## Deaf (no secondary, exclude deafblind)
## Deaf (two disabilities at least 1 is deaf,  exclude deafblind)
## DeafBlind (primary or secondary or either)

## Comparison groups:
## Everyone else
## Physical
## Mental

dat <- read_csv('../../data/PY17annual_public_disabilitycode.csv')

### application ->  eligibility -> (OOSPlacement-> OOSExit->) ipe



dat <- dat%>%
    mutate(
      hasIPE =!is.na(dat$age_IPE),
      SecondDisability = as.numeric(dat$SecondDisability),
      primCat=
        ifelse(PrimDisability%in%3:7,'deaf',
          ifelse(PrimDisability==8,'deafblind',
            ifelse(PrimDisability%in%10:16,'physical',
              ifelse(PrimDisability>16,'mental','other')))),
      SecondCat=
        ifelse(is.na(SecondDisability)|SecondDisability==0,'none',
          ifelse(SecondDisability%in%3:7,'deaf',
            ifelse(SecondDisability==8,'deafblind',
              ifelse(SecondDisability%in%10:16,'physical',
                ifelse(SecondDisability>16,'mental','other'))))),
      group=
        ifelse(primCat=='deafblind'|SecondCat=='deafblind','deafblind',
          ifelse(SecondCat=='none',
            ifelse(primCat=='deaf','justDeaf',primCat),
            ifelse(primCat=='deaf'|SecondCat=='deaf','deafDisabled',primCat)
          )),
      deafAll=ifelse(group%in%c('deafblind','justDeaf','deafDisabled'),'deaf','hearing')
    )

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
