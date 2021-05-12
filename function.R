unwithinme <- function(data,intra = NULL,inter = NULL,id = NULL,y = NULL){
  
  data$id<-  data[, id]
  subject<-unique(data$id)
  # within_difference----------
  beta_list<-list()
  # list to fill with the individual beta coefficients for 
  # each within subject variable
  for(var in 1:length(intra)){
    varloop<-  intra[var]
    sub_beta_list<-list()
    for(i in subject){
      d2 <- data[data$id == i,]
      beta<-lm(as.formula(paste(y, "~", varloop)), d2)
      sub_beta_list[i]<-beta$coefficients[2]
    }
    d3<-data.frame(beta = do.call(rbind, args = sub_beta_list))
    beta_list[intra[var]]<-list(d3)
  }
  Wdiff<-data.frame(do.call(cbind, args = beta_list))
  # within_average----------
  myformula<-as.formula(paste0(y, "~", id))
  Wave<- as.data.frame(aggregate(myformula, data, mean)[,2])
  # export-----------
  colnames(Wdiff)<-paste0("Wdiff_",intra)
  colnames(Wave)<-paste0("Wave_",y)
  W<-cbind(subject, Wdiff, Wave)
  return(W)
}
