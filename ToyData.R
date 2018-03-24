toydata<-data.frame(rep(1,30), sample(1:100, 30, replace=T), sample(c(0,1), 30, replace=T))
colnames(toydata)<-c("DocIDi", "DocIDj", "Choose")
table(toydata$DocIDj)

lambdai<-runif(30)
?sample
bradleyterry<-function(a,b,lambdai,dataset){
  summationterm<-0
  for (i in nrow(dataset$DocIDj)){
    summationterm<-summationterm+(1/(lambdai[i]+dataset$DocIDj[i]))
    }
    output<-(a-1+sum(dataset$Choose))/(b+summationterm)
    return(output)
}

bradleyterry(1,2,lambdai,toydata)

