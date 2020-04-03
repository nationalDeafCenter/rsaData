require(tidyverse)

if(!exists("studDat")) source('R/preEts/preEtsData.r')

preEtsTab <- function(what){

  what <- enquo(what)

  FUN <- function(.data) summarize(.data=.data,Y=!! what)

  out <- bind_rows(
    studDat%>%FUN(),
    studDat%>%group_by(group)%>%FUN(),
    studDat%>%mutate(group=deafAll)%>%group_by(group)%>%FUN()
  )
  for(demo in demos){
    demoDat <- filter(studDat,!is.na(!!sym(demo)))%>%
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
