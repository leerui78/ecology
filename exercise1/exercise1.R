rm(list = ls())
#cleaning the current environment
library(ade4)
#加载数据包ade4
data(package = "ade4") 
data("doubs") 
str(doubs) 
#structure函数显示数据结构
mydata <- doubs$env 
head(mydata)
#head函数显示数据前6行
mydata <- mydata[,-1] 
head(mydata)
#mydata[,-1]剔除mydata数据第一列
hist(mydata$alt, col = "green", main = "altitude distribution", xlab = "altitude")
#直方图检查变量分布
library(corrplot)
#加载corrplot库，用于分析相关性
res1 <- cor(mydata) 
res1 
library(psych) 
pairs.panels(mydata[,2:6])



