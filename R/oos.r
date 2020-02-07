library(tidyverse)
library(openxlsx)
source('R/data.r')
source('R/crossTabsFunctions.r')


### do we have some descriptive analyses showing % of deaf only vs other disability groups being on the waiting list?
### that alone would be a good data point.

### could be of interest to do analysis of deaf-only vs deafdisabled vs one disability only vs multiplydisabled.
### maybe not ALL of these groups but we should account for the difference in service provision for
### deaf ppl w/o other disabilities vs. deafdisabled ppl.

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

oos <- ct('oos',dat)


