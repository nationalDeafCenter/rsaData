library(tidyverse)
library(openxlsx)
source('R/data.r')

### (average) hourly wage at IPE

xtabs(~I(ipeHourlyWage==0)+I(ipeWeeklyHoursWorked==0),filter(dat,hasIPE))
## _almost_ equiv: 9 ppl with 0 hours worked but with pos wage, 27 with pos hours worked, 0 wage

wageDat <- dat%>%
  filter(hasIPE)%>%
  mutate(deaf=ifelse(deafAll,'deaf','hearing'))%>%
  select(ipeHourlyWage,ipeWeeklyHoursWorked,deaf,deafType,type)%>%
  gather('grouping','group',-ipeHourlyWage,-ipeWeeklyHoursWorked)

gt40 <- wageDat%>%
  filter(group!='not deaf',ipeWeeklyHoursWorked>0)%>%
  group_by(grouping,group)%>%
  summarize(pgt40=paste0('+',round(mean(ipeHourlyWage>40)*100),'%'))

wageDat%>%
  filter(group!='not deaf',ipeWeeklyHoursWorked>0)%>%
  bind_rows(gt40)%>%
  ggplot(aes(group,ipeHourlyWage))+
#  geom_violin()+
  geom_boxplot()+
  geom_label(aes(group,40,label=pgt40))+
  scale_y_continuous("Hourly Wage at IPE (if positive)",limits=c(0,40),breaks=seq(0,40,5))+
  facet_wrap(~grouping,scales="free_x")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('ipeHourlyWage.jpg')

wageDat%>%
  filter(group!='not deaf')%>%
  group_by(grouping,group)%>%
  summarize(
    n=n(),
    nobs=sum(!is.na(ipeHourlyWage)),
    propObs=nobs/n,
    propEarning=mean(ipeHourlyWage>0,na.rm=TRUE),
    med=median(ipeHourlyWage[ipeWeeklyHoursWorked>0],na.rm=TRUE),
    avg=mean(ipeHourlyWage[ipeWeeklyHoursWorked>0],na.rm=TRUE)
  )

wageDat%>%
  filter(group!='not deaf')%>%
  group_by(grouping,group)%>%
  summarize(
    propEarning=mean(ipeHourlyWage>0,na.rm=TRUE),
    txt=paste0(round(propEarning*100),'%')
  )%>%
  ggplot(aes(group,propEarning))+
  geom_col(position='dodge')+
  facet_wrap(~grouping,scales="free_x")+
  scale_y_continuous('% Positive Weekly Earnings',labels=scales::percent)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
