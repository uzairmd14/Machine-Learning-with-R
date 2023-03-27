#   GET THE DATA
library(ISLR)
any(is.na(Caravan))
var(Caravan[,1])
var(Caravan[,2])
purchase<-Caravan[,86]
#
standardized.Caravan<- scale(Caravan[,-86])
#
print(var(standardized.Caravan[,1]))
print(var(standardized.Caravan[,2]))
# TRAIN TEST SPLIT
test.index<-1:1000

test.data<-standardized.Caravan[test.index,]
test.purchase<-purchase[test.index]
#TRAIN DATA
train.data<-standardized.Caravan[-test.index,]
train.purchase<-purchase[-test.index]

################
####################
##############

#KNN MODEL

library(class)
set.seed(101)
predicted.purchase<-knn(train.data,test.data,train.purchase,k=5)
print(head(predicted.purchase))

misclass.error<-mean(test.purchase != predicted.purchase)
print(misclass.error)

# CHOOSING A K VALUE


predicted.purchase<-NULL
error.rate<-NULL

for(i in 1:20){
  set.seed(101)
  predicted.purchase<-knn(train.data,test.data,train.purchase,k=i)
  error.rate[i]<-mean(test.purchase!=predicted.purchase)
  
  
}
print(error.rate)

#####
#VISUALIZE K ELBOW METHOD
library(ggplot2)
k.values<-1:20
error.df<-data.frame(error.rate,k.values)
print(error.df)

l1<-ggplot(error.df,aes(k.values,error.rate)) + geom_point()  +  geom_line(lty='dotted',color='red')
print(l1)
h
























