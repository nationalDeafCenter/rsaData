lapply <- function(X,FUN,...) sapply(X,FUN,...,simplify=FALSE,USE.NAMES=TRUE)

TAB <- function(v1,v2,data){
  form <-
    as.formula(
     # ifelse(
     #   n_distinct(data[[v1]])>=n_distinct(data[[gsub("`","",v2)]]),
     #   paste('~',v1,'+',v2),
        paste('~',v2,'+',v1)
      #)
    )

  xtabs(form,data,addNA=TRUE)
}

## v2 is ordered factor
TABord <- function(v1,v2,data){
  levs <- levels(data[[v2]])
  names(levs) <- levs
  #data[[v2]] <- fct_explicit_na(data[[v2]],'NA')
  tab <- data%>%
    group_by(!!sym(v1))%>%
    group_map(~map_dfr(levs,function(ll) sum(.x[[v2]]>=ll,na.rm=TRUE)))%>%
    as.data.frame()%>%
    column_to_rownames(var=v1)%>%
    t()
}


ct1 <- function(v1,v2,data){

  if(is.ordered(dat[[v2]])){
    tab <- TABord(v1,v2,data)
    sums2 <- tab[1,]
  } else{
    tab <- TAB(v1,v2,data)
    sums2 <- colSums(tab)
  }

  tab1 <- rbind(
    sweep(tab,1,rowSums(tab),'/'),
    overall=sums2/sum(sums2)
  )*100
  tab2 <- cbind(
    sweep(tab,2,sums2,'/'),
    overall=rowSums(tab)/sum(sums2)
  )*100

  tab <- cbind(tab,Sum=rowSums(tab))
  tab <- rbind(tab,Sum=c(sums2,sum(sums2)))

  list(tab,tab1,tab2)
}

ct <- function(vv,data,ord=FALSE){
  if(is.logical(data[[vv]]))
    data[[vv]] <- ifelse(data[[vv]],paste0(vv,':TRUE'),paste0(vv,':FALSE'))
  list(
    deaf=ct1('deaf',vv,data),
    deafType=ct1('deafType',vv,filter(data,deafAll)),
    type=ct1('type',vv,data)
  )
}


toExcel <- function(name,crossTabs){

  onect <- crossTabs[[name]]

  onect <- lapply(onect, function(x) lapply(x,function(y) rbind(colnames(y),round(y,1))))
  onect <- lapply(onect, function(x) list(x[[1]],cbind(x[[2]],''),x[[3]]))

  onect <- lapply(onect,function(x) lapply(x, function(y) cbind(ifelse(is.na(rownames(y)),'NA',rownames(y)),y)))

  out <-
    lapply(1:3,
      function(i)
        with(onect,
          cbind(
            rbind(c('By Deafness',rep('',ncol(deaf[[i]])-1)), deaf[[i]]),'',
            rbind(c('By Deaf Type',rep('',ncol(deafType[[i]])-1)),deafType[[i]]),'',
            rbind(c('By Disability Type',rep('',ncol(type[[i]])-1)),type[[i]])
          )
        )
    )


  rbind(
    c('Total N',rep('',ncol(out[[1]])-1)),
    out[[1]],
    '',
    c(paste('% Disability Category by',name),rep('',ncol(out[[1]])-1)),
    out[[2]],
    '',
    c(paste('%',name,'by Disability Category'),rep('',ncol(out[[1]])-1)),
    out[[3]]
  )
}

