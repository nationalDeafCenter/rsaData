library(tidyverse)
library(openxlsx)

source('R/subgroupFunction.r')

round.data.frame <- function(x,digits=0){
  #require(purr)
  x[,map_lgl(x,is.numeric)] <- lapply(x[,map_lgl(x,is.numeric)],round,digits=digits)
  x
}

fullDatFile <- 'data/dat.RData'
if(
  (file.exists(fullDatFile))&
  (file.info(fullDatFile)$mtime>file.info('R/data.r')$mtime) ## and up to date
  ){
  load(fullDatFile)
} else source('R/data.r') ### load in data, standard RSA data prep

toPercentage <- function(nums){
  nums <- as.data.frame(nums,stringsAsFactors=FALSE)
  nums[1,-1] <- nums[1,-1]/nums[1,'All']*100
  nums$subgroup <- fct_recode(nums$subgroup,"% of Total"="")
  nums%>%
    mutate_if(is.numeric,function(x) {
                x1 <- x[1]
                x <- x[-1]
                i <- 1
                ppp <- rep(NA,length(x))
                for(j in seq_along(x)) if(is.na(x[j])) i <- i+1 else ppp[j] <- i
                tibble(x,ppp)%>%group_by(ppp)%>%mutate(N=sum(x))%>%ungroup()%>%mutate(x=x/N*100)%>%pull(x)%>%c(x1,.)
              })%>%
      bind_rows(tibble(subgroup='Subgroup size as % of group'))
}

addElement <- function(demos,dat,name){
  N <- subgroupTab(n(),dat)
  demos[[paste0(name,'N')]] <- N
  demos[[paste0(name,'Per')]] <- toPercentage(N)
  demos
}

demos <- list()

demos <- addElement(demos,dat,'Total')

### pre-ets people
load('data/studDat.RData')
demos <- addElement(demos,filter(studDat,!is.na(PETSStartDate)),'PreETS')
rm(studDat)

#### VA applicants

demos <- addElement(demos,filter(dat,!is.na(ApplicationDate)),'applicant')

### OOS

demos <- addElement(demos,filter(dat,!is.na(OOSPlacementDate)),'everOOS')

### TWE
demos <- addElement(demos,filter(dat,!is.na(TWEStartDate)),'TWE')

### IPE
demos <- addElement(demos,filter(dat,hasIPE),'hasIPE')


write.xlsx(demos,'results/demographicTables.xlsx')
