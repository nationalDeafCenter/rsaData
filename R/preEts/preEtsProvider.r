if(!exists("studDat")){
  source('R/preEts/preEtsData.r')
}

provider <- tibble()
for(serv in services)
  provider <- bind_rows(
    provider,
    studDat%>%group_by(group)%>%
    summarize(
      any=mean(!!sym(serv)),
      inHouse=mean(!is.na(!!sym(paste0(serv,'VRAgencyStaff')))[!!sym(serv)]),
      purchase=mean(!is.na(!!sym(paste0(serv,'VRAgencyPurchase')))[!!sym(serv)]),
      compService=mean(!is.na(!!sym(paste0(serv,'CompServiceProvider')))[!!sym(serv)])
    )%>%
    pivot_longer(-group,names_to="provider")%>%
    mutate(value=value*100)%>%
    pivot_wider(id_cols="provider",names_from="group")%>%
    mutate(service=serv)%>%
    select(service,provider,everything())
  )
