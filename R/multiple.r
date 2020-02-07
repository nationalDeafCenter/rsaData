library(tidyverse)
options(stringsAsFactors=FALSE)

dat <- read_csv('../../data/PY17annual_public_disabilitycode.csv')

dat <- dat%>%
    mutate(
        SecondDisability=as.numeric(SecondDisability),
        hasIPE=!is.na(dat$age_IPE),
        ## broad disabilty categories
        primCat=
            ifelse(PrimDisability%in%3:7,'deaf',
            ifelse(PrimDisability==8,'deafblind',
            ifelse(PrimDisability%in%10:16,'physical',
            ifelse(PrimDisability%in%1:2,'visual',
            ifelse(PrimDisability==9,'communicative',
            ifelse(PrimDisability%in%1:2,'visual','mental')))))),
        secondCat=
            ifelse(is.na(SecondDisability)|SecondDisability==0,'none',
            ifelse(SecondDisability%in%3:7,'deaf',
            ifelse(SecondDisability==8,'deafblind',
            ifelse(SecondDisability%in%10:16,'physical',
            ifelse(SecondDisability%in%1:2,'visual',
            ifelse(SecondDisability==9,'communicative',
            ifelse(SecondDisability%in%1:2,'visual','mental'))))))),
        secondCat=fct_relevel(secondCat,'none'),
        ## defining deafness:
        deafAll=primCat=='deaf'|secondCat=='deaf',
        deaf=ifelse(deafAll,'deaf','not deaf'),
        ## alternative definitions of multiple disabilities (logical):
        hasSecDisLog=!is.na(SecondDisability)&(SecondDisability!=0),
        hasDistinctSecDisLog=hasSecDisLog&(primCat!=secondCat),
        multipleLog=hasDistinctSecDisLog|primCat=='deafblind'|secondCat=='deafblind',
        ## alternative definitions of multiple disabilities (categorical):
        hasSecDis=ifelse(hasSecDisLog,'has 2nd Disability','no 2nd Disability'),
        hasDistinctSecDis=ifelse(hasDistinctSecDisLog,'has distinct 2nd Disability','no distinct 2nd Disability'),
        multiple=ifelse(multipleLog,'multiple','single')
        )

##### with and without IPE
### cross-tabs
dat%>%
    xtabs(~primCat+secondCat,.)%>%
    addmargins()%T>%
    print()%>%
    sweep(1,.[,ncol(.)]/100,'/')

## has 2nd Disability
dat%>%
xtabs(~hasSecDis+deaf,.)%>%
    addmargins()%T>%
    print()%>%
    sweep(2,.[nrow(.),]/100,'/')

## has distinct 2nd Disability
dat%>%
    xtabs(~hasDistinctSecDis+deaf,.)%>%
    addmargins()%T>%
    print()%>%
    sweep(2,.[nrow(.),]/100,'/')

## has distinct 2nd Disability OR is deafblind
dat%>%
    xtabs(~multiple+deaf,.)%>%
    addmargins()%T>%
    print()%>%
    sweep(2,.[nrow(.),]/100,'/')

#### now the same, but only people w/ IPE
### cross-tabs
dat%>%
    filter(hasIPE)%>%
    xtabs(~primCat+secondCat,.)%>%
    addmargins()%T>%
    print()%>%
    sweep(1,.[,ncol(.)]/100,'/')

## has 2nd Disability
dat%>%
    filter(hasIPE)%>%
    xtabs(~hasSecDis+deaf,.)%>%
    addmargins()%T>%
    print()%>%
    sweep(2,.[nrow(.),]/100,'/')

## has distinct 2nd Disability
dat%>%
    filter(hasIPE)%>%
    xtabs(~hasDistinctSecDis+deaf,.)%>%
    addmargins()%T>%
    print()%>%
    sweep(2,.[nrow(.),]/100,'/')

## has distinct 2nd Disability OR is deafblind
dat%>%
    filter(hasIPE)%>%
    xtabs(~multiple+deaf,.)%>%
    addmargins()%T>%
    print()%>%
    sweep(2,.[nrow(.),]/100,'/')

## has distinct 2nd Disability OR is deafblind
dat%>%
    filter(hasIPE)%>%
    xtabs(~multiple+deaf,.)%>%
    addmargins()%T>%
    print()%>%
    sweep(2,.[nrow(.),]/100,'/')


#### non-deaf people who have 2nd disability are much more likely to have 2nd disability in same category:
dat%>%
    filter(hasSecDisLog)%>%
    xtabs(~hasDistinctSecDis+deaf,.)%>%
    addmargins()%T>%
    print()%>%
    sweep(2,.[nrow(.),]/100,'/')
