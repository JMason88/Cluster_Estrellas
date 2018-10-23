require(dplyr)
run_dbscan <- function(data, eps, minPts) {
  tbl<-NULL
  k<-1
  for (i in eps) {
    for (j in minPts) {
      set.seed(123)
      db <- fpc::dbscan(data, eps = i, MinPts = j)
      tbl<-rbind(tbl,data.frame(cls=as.factor(db$cluster),hyad=hip$Symbad_Hyades) %>%
                   mutate(hyad=as.factor(ifelse(hyad,"Hyades","No_Hyades"))) %>% 
                   group_by(cls,hyad) %>% count() %>%
                   tidyr::spread(hyad,n,fill = 0) %>% 
                   mutate(eps=i,minPts=j,test=as.factor(k)))
      k<-k+1
    }
  }
  sorted.test<-order(levels(as.factor(tbl$test)))
  tbl$test=factor(tbl$test,levels = sorted.test )
  return(tbl)
}