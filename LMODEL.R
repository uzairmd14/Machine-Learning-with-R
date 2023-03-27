library(utils)
library(Amelia)
library(dplyr)
library(caTools)
df.train<-read.csv(choose.files())


print(head(df.train))
print(str(df.train))
missmap(df.train,main='Missing Map',col=c('yellow','black'),legend= FALSE)
library(ggplot2)
p1<-ggplot(df.train,aes(Survived)) + geom_bar()
print(p1)
#DATA CLEANING

ggplot(df.train,aes(SibSp))+geom_bar()
 ggplot(df.train,aes(Fare))+geom_histogram()
 ggplot(df.train,aes(Fare))+geom_histogram(fill='red',color='black',alpha=0.5)
 pl<-ggplot(df.train,aes(Pclass,Age))
  pl<-pl+geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4))
  pl+scale_y_continuous(breaks = seq(min(0),max(80),by=2))+theme_bw()
  
  pl+scale_y_continuous(breaks = seq(min(0),max(80),by=2))+theme_bw()
  
## IMPUTATION OF AGE BASED ON CLASS
  impute_age <- function(age,class){
    out <- age
    for (i in 1:length(age)){
      
      if (is.na(age[i])){
        
        if (class[i] == 1){
          out[i] <- 37
          
        }else if (class[i] == 2){
          out[i] <- 29
          
        }else{
          out[i] <- 24
        }
      }else{
        out[i]<-age[i]
      }
    }
    return(out)
    
    
    }
  
 fixed.ages<-impute_age(df.train$Age,df.train$Pclass) 
#######
df.train$Age<-fixed.ages
  
print(missmap(df.train,main='Imputation Check',col=c('yellow','black'),legend=FALSE))
  
df.train<-select(df.train,-PassengerId,-Name,-Ticket,-Cabin)

print(head(df.train))
  
  
  
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)
#TRAIN THE MODEL
log.model<-glm(Survived~. , family=binomial(link='logit'),data=df.train)
set.seed(101)
split = sample.split(df.train$Survived, SplitRatio = 0.70)

final.train = subset(df.train, split == TRUE)
final.test = subset(df.train, split == FALSE)
final.log.model <- glm(formula=Survived ~ . , family = binomial(link='logit'),data = final.train)
summary(final.log.model)
fitted.probabilities <- predict(final.log.model,newdata=final.test,type='response')

fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClasificError <- mean(fitted.results != final.test$Survived)
print(paste('Accuracy',1-misClasificError))
table(final.test$Survived, fitted.probabilities > 0.5)
  
  
  
  
  
  
  
  
  
  