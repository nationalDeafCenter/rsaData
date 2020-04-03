library(tidyverse)
source('R/data.r')

## if(!exists("studDat")){
##   source('R/preEtsData.r')
## }


### replicate mathematica numbers
studDat <- studDat%>%
  mutate(
    mathType=
      ifelse(is.na(ApplicationDate)&is.na(IPEAmendedDate)&petsdate,'I',
      ifelse(!is.na(ApplicationDate)&!is.na(IPEAmendedDate)&petsdate,'II',
      ifelse(!is.na(ApplicationDate)&!is.na(IPEAmendedDate)&!petsdate,'III','other')))
      #ifelse(!is.na(ApplicationDate)&!is.na(IPEAmendedDate)&!stud&!petsdate,'IV',
      #ifelse(!is.na(ApplicationDate)&is.na(IPEAmendedDate)&!petsdate,'V','other')
     # ))))
  )

studDat%>%
  filter(is.na(age_app)|(age_app>14&age_app<21),is.na(exitDate),anyServCurrent)%>%
  group_by(mathType)%>%
  summarize(n())

math <- studDat%>%
  filter(mathType%in%c('I','II'),stud,anyServCurrent,is.na(age_app)|(age_app>=14&age_app<22))%>%
  group_by(mathType)%>%
  summarize_at(paste0(services,'Current'),mean,na.rm=TRUE)
