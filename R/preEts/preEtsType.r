require(tidyverse)
if(!exists("studDat")){
  source('R/preEts/preEtsData.r')
}

type <- studDat%>%
  group_by(group)%>%
    summarize(`% Receiving Any`=mean(petsdate,na.rm=TRUE)*100)%>%
    column_to_rownames("group")%>%
  t()%>%
  as.data.frame()%>%
  rownames_to_column("what")%>%
  add_case(what="Of which...")

for(serv in services){
  type <- bind_rows(type,
    studDat%>%
      filter(
        petsdate
      )%>%
      group_by(group)%>%
    summarize(overall=sum(!! sym(serv),na.rm=TRUE)/n()*100)%>%
    column_to_rownames("group")%>%
      t()%>%
      as.data.frame()%>%
      mutate(what=switch(serv,
        jec="Job Exploration Counseling",
        wble="Work-Based Learning",
        ceo="Counseling on Postsecondary Opportunities",
        wrt="Workplace Readiness Training",
        isa="Instruction in Self-Advocacy"
      ))
  )
}

pvalStar <- function(p) ifelse(p<0.001,'***',ifelse(p<0.01,'**',ifelse(p<0.05,'*',ifelse(p<0.1,'.',''))))
type <- cbind(type,pvalStar=c(NA,NA,
  map_chr(services,
    ~chisq.test(xtabs(as.formula(paste('~',.,'+group')),filter(studDat,petsdate)))$p.value%>%pvalStar())
  ))
