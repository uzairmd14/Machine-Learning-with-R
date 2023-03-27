library(utils)
library(ggplot2)
library(dplyr)
bike<-read.csv(choose.files())


print(head(bike))

sp<-ggplot(bike,aes(x=temp,y=count))+geom_point(alpha=0.3,aes(color=temp))+theme_bw()

print(sp)

#CONVERT TO POSIxcr()

bike$datetime<-as.POSIXct(bike$datetime)



pl<-ggplot(bike,aes(datetime,count))+geom_point(aes(color=temp),alpha=0.5)
print(pl)
pl2<-pl+scale_color_continuous(low='#55D8CE',high='#FF6E2E')
print(pl2)



#XCORRELATIOn

cor(bike[,c('temp','count')])

ggplot(bike,aes(factor(season),count))+geom_boxplot(aes(color=factor(season)))+theme_bw()



#FEATURE ENGINEERING
time.stamp <- bike$datetime[4]
format(time.stamp, "%H")




#BUILD MODEL
temp.model<-lm(count~temp,bike)

print(summary(temp.model))















