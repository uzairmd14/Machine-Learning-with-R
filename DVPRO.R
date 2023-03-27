library(ggplot2)
library(data.table)
library(utils)
Csvfile=read.csv(file.choose())
View(Csvfile)



pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")



df<-Csvfile
print(head(df))

pl<-ggplot(df,aes(x=CPI,y=HDI,color=Region)) +geom_point(size=4,shape=1)
p2<-pl+geom_smooth(aes(group=1),method='lm',formula=y~log(x),se=F,color='red')


pl3 <- p2 + geom_text(aes(label = Country), color = "gray20", 
                       data = subset(df, Country %in% pointsToLabel),check_overlap = TRUE)

print(pl3)
