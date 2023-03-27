library(utils)
df=read.csv(choose.files(),sep = ';')

#split data into train and test
library(caTools)

#set a Seed


set.seed(101)

#splitup sample

sample<-sample.split(df$G3,SplitRatio=0.7)
#70%of data
train<-subset(df,sample == TRUE)
#30% test

test<-subset(df,sample==FALSE)


#train and build model4


model<- lm(G3 ~.,data = train)

#predictions

G3.predictions<-predict(model,test)

results<-cbind(G3.predictions,test$G3)
colnames(results)<-c('predicted','actual')


results<-as.data.frame(results)
print(head(results))

to_zero<-function(x)
{
  if(x<0){
    return(0)
  }else
  {
    return(x)
  }
    
}

#APLLY ZERO FUNCTION
results$predicted<-sapply(results$predicted,to_zero)


mse<-mean((results$actual-results$predicted)^2)

print('MSE')
print(mse)

print("Squared Root of MSE")

print(mse^0.5)

SSE<-sum((results$predicted-results$actual)^2)


SST<-sum((mean(df$G3)-results$actual)^2)




R2<-1 - SSE/SST

print('R2')
print(R2)


 