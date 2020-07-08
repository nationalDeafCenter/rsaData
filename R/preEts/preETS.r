library(tidyverse)
library(openxlsx)

round.data.frame <- function(x,digits=0){
  #require(purr)
  x[,map_lgl(x,is.numeric)] <- lapply(x[,map_lgl(x,is.numeric)],round,digits=digits)
  x
}

demos <- c('sex','raceEth','ageApp','SSDI','SSI')
services <- c('jec','wble','ceo','wrt','isa')


if(!exists("studDat")){
  source('R/preEts/preEtsData.r')
}

stopifnot(all(studDat$Student>0))

#### definitions of "group"
## impTab <- groupDef(studDat)
## impTabPer <- groupDef(studDat,percentage=TRUE)
## openxlsx::write.xlsx(list(n=impTab,percentage=impTabPer),'results/groupDefSwD.xlsx')


################################
####### by subgroup
################################
source('R/preEts/preEtsSubgroupFunction.r')
preEts <-  map(c('petsdate','anyServ',services),~preEtsTab(mean(!! sym(.),na.rm=TRUE)*100))
names(preEts) <- c('Start Date','Any',toupper(services))
preEts <- map(preEts,function(x) bind_rows(x,tibble(subgroup="All numbers are % ever received pre-ETS service")))

preEts$numbers <- preEtsTab(n())
preEts$percentages <- preEts$numbers%>%
  mutate_if(is.numeric,function(x) {
    i <- 1
    ppp <- rep(NA,length(x))
    for(j in seq_along(x)) if(is.na(x[j])) i <- i+1 else ppp[j] <- i
    tibble(x,ppp)%>%group_by(ppp)%>%mutate(N=sum(x))%>%ungroup()%>%mutate(x=x/N*100)%>%pull(x)
  })%>%bind_rows(tibble(subgroup='Subgroup size as % of group'))

#########################################
### providers of service
#########################################
source('R/preEts/preEtsProvider.r')

preEts$provider <- bind_rows(provider,
  tibble(service='% Ever received service among those classified as Student with Disability'))

#########################################
### % receiving each type of service
#########################################
source('R/preEts/preEtsType.r')

preEts$type <- round(type)


#### a bit more processing

preEts <- map(preEts,round,digits=1)

for(nn in names(preEts)){
  nnn <- names(preEts[[nn]])[1]
  preEts[[nn]] <- bind_rows(preEts[[nn]],tibble(!! sym(nnn):= "Universe: students with disability"))
}


openxlsx::write.xlsx(preEts,"results/preEts.xlsx",rowNames=TRUE)

############################################
### OOS
############################################

source('R/preEts/oosPreEts.r')

############################################
### by state
############################################
source('R/preEts/preEtsState.r')
preEts$byState <- states
openxlsx::write.xlsx(preEts,"results/preEts.xlsx",rowNames=FALSE)
