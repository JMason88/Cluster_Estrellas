require(dplyr)
require(tidyr)
run_fuzzy<-function(data,k_cls) {
  
  test<-1
  coef<-NULL
  fuzz.sal<-NULL
  for (i in k_cls) {
    for (j in seq(1.1,2,.1)) {
      set.seed(123)
      fuzz1<-cluster::fanny(data,k=i,memb.exp = j, diss = T)
      coef<-rbind(coef,c(round(fuzz1$coeff,2),test))
      out<-as.data.frame(fuzz1$membership) %>% mutate(obs=1:nrow(.)) %>% select(obs, everything())
      colnames(out)<-c("obs",1:i)
      out<-out %>% gather(key="Cluster", value="Membership",-obs) %>% mutate(Membership=round(Membership,2)) %>% 
        group_by(obs) %>% filter(Membership==max(Membership)) %>% distinct(obs, .keep_all = T) %>% 
        arrange(obs) %>% ungroup()
      out<-out %>% mutate(hyad=hip$Symbad_Hyades, hyad=as.factor(ifelse(hyad,"Hyades","No_Hyades"))) %>% 
        filter(Membership>.5) %>% group_by(Cluster, hyad) %>% summarize(tot=n()) %>% 
        spread(hyad,tot,fill = 0) %>% mutate(k_n=i, memb.exp=j, test=as.factor(test))
      fuzz.sal<-rbind(fuzz.sal,out)
      test<-test+1
    }
  }
  coef<-as.data.frame(coef)
  colnames(coef)<-c("dunn_coeff","normalized","test")
  coef$test<-as.factor(coef$test)
  
  sorted.test<-order(levels(as.factor(coef$test)))
  fuzz.sal$test=factor(fuzz.sal$test,levels = sorted.test)
  coef$test=factor(coef$test,levels = sorted.test )
  return(list(coef=as.data.frame(coef),clustering=fuzz.sal))
}

run_fuzzy_one<-function(data,k,memb) {
  set.seed(123)
  fuzz1<-cluster::fanny(data,k=k,memb.exp = memb, diss = T)
  out<-as.data.frame(fuzz1$membership) %>% mutate(hyad=hip$Symbad_Hyades,HIP=hip$HIP) %>% 
    select(HIP, hyad, everything())
  colnames(out)<-c("HIP","Hyades",1:k)
  out<-out %>% gather(key="Cluster", value="Membership",-Hyades,-HIP) %>% mutate(Membership=round(Membership,2)) %>% 
    group_by(HIP) %>% filter(Membership==max(Membership)) %>% distinct(HIP, .keep_all = T) %>% 
    arrange(HIP) %>% ungroup()
#  out<-out %>% mutate(hyad=hip$Symbad_Hyades, hyad=as.factor(ifelse(hyad,"Hyades","No_Hyades"))) %>% 
#    filter(Membership>.5) %>% group_by(Cluster, hyad) %>% summarize(tot=n()) %>% 
#    spread(hyad,tot,fill = 0) %>% mutate(k_n=i, memb.exp=j, test=as.factor(test))
  return(out) 
}
