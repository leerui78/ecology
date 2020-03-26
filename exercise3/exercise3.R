rm(list = ls())
mydata <- read.csv("exercise3/xy.csv")
x <- mydata$x
y <- mydata$y

#确定代价函数或损失函数
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

alpha <- 0.00001 # 指定学习效率
num_iters <- 1000 # 指定迭代次数
cost_history <- rep(0,num_iters) #用于每次迭代后保存代价函数值
theta_history <- list(num_iters) # 用于每次迭代后保存theta值
theta <-  matrix(c(0,0), nrow = 2) # 初始化theta值
X <- cbind(1,x) # 在x中增加一列，所有值为1，使得假设函数具有截距 

for (i in 1:num_iters) { # 梯度下降（重复）
  theta[1] <- theta[1] - alpha * (1/length(y)) * sum(((X%*%theta)- y))
  theta[2] <- theta[2] - alpha * (1/length(y)) * sum(((X%*%theta)- y)*X[,2])
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
} 
print(theta)

# 绘制训练集数据图
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')

# 绘制收敛过程中所有直线
for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) { 
  abline(coef=theta_history[[i]], col=rgb(0.8,0,0,0.3))
}
abline(coef=theta, col='blue') # 绘制截距为theta[1]和斜率为theta[2]的直线