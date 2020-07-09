require(tidyverse)



subgroupTab <- function(
  what, # what to calculate for each subgroup, goes in summarize() e.g. n(), mean(age_app), etc.
  dat, # dataset
  demos=c('sex','raceEth','ageApp','SSDI','SSI') ## demographics to
  ){

  what <- enquo(what)

  FUN <- function(.data) summarize(.data=.data,Y=!! what)

  out <- bind_rows(
    dat%>%FUN(),
    dat%>%group_by(group)%>%FUN(),
    dat%>%mutate(group=deafAll)%>%group_by(group)%>%FUN()
  )
  for(demo in demos){
    demoDat <- filter(dat,!is.na(!!sym(demo)))%>%
      group_by(group)%>%
      rename(subgroup=!!demo)%>%
      group_by(subgroup)

    out <- bind_rows(
      out,
      tibble(subgroup=demo),
      demoDat%>%FUN(),
      demoDat%>%group_by(group,add=TRUE)%>%FUN(),
      demoDat%>%group_by(deafAll,add=TRUE)%>%FUN()%>%rename(group=deafAll),
      )
    rm(demoDat)
  }

  out%>%
    mutate(
      group=ifelse(is.na(group),'All',group),
      group=factor(group,levels=unique(group)),
      subgroup=ifelse(is.na(subgroup),'',subgroup),
      subgroup=factor(subgroup,levels=unique(subgroup))
    )%>%spread(group,Y,fill=NA)
}
