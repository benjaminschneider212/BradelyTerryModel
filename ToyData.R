toydata<-data.frame(rep(1,30), sample(1:100, 30, replace=T), sample(c(0,1), 30, replace=T))
colnames(toydata)<-c("DocIDi", "DocIDj", "Choose")
toydata
table(toydata$DocIDj)

lambdai<-runif(30)
?sample
bradleyterry<-function(a,b,lambdai,dataset){
  sum<-0
  for (i in nrow(dataset$DocIDj)){
    sum<-sum+(1/(lambdai[i]+dataset$DocIDj))
    }
    output<-(a-1+sum(dataset$Choose))/(b+sum)
    return(output)
}

bradleyterry(1,2,lambdai,toydata)

