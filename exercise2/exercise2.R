rm(list = ls())
#clean the current workspace
mydata <- read.csv("/cloud/project/ecology/exercise2/xy.csv") 
head(mydata)
#read data
plot(mydata$y ~ mydata$x, data = mydata, main="y ~ x")
#check what relationship between x and y by scatter plot
par(mfrow=c(1, 2)) # set outplay of figure pannel 
boxplot(mydata$x, main="x", sub=paste("Outlier rows: ", boxplot.stats(mydata$x)$out)) 
boxplot(mydata$y, main="y", sub=paste("Outlier rows: ", boxplot.stats(mydata$y)$out))
#check whether there are outliers
library(e1071) 
par(mfrow=c(1, 2)) 
plot(density(mydata$x), main="Density Plot: x", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(mydata$x), 2))) 
polygon(density(mydata$x), col="red") 
plot(density(mydata$y), main="Density Plot: y", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2))) 
polygon(density(mydata$y), col="red")
#check whether data meet normal distribution
cor(mydata$x, mydata$y)
#calculate coeﬃcient 
linearMod <- lm(y ~ x, data= mydata) 
print(linearMod)
#build a linear model
summary(linearMod)
#check of statistic signiﬁcance

