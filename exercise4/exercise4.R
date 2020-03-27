rm(list = ls())
data <- read.csv("exercise4/percep_data.csv")
mydata <- data[1:1000,c(1, 2, 3)]
names(mydata) <- c("X1","X2","y")
str(mydata)

mydata <- mydata[order(mydata$y),]
for (i in 1:1000){
  if (mydata[i,3] > 0){
    num = i
    break
  }
}

g1_x = mydata[1:num-1,1]
g1_y = mydata[1:num-1,2]
g2_x = mydata[num:1000,1]
g2_y = mydata[num:1000,2]
g_x = c(g1_x, g2_x)
g_y = c(g1_y, g2_y)
group = mydata$y
print(g_x)
print(g_y)
print(group)
plot(g_x, g_y, type='n',xlab='X', ylab='Y')
points(g1_x, g1_y, col='red')
points(g2_x, g2_y, col='blue')

theta0 = 0.1 # initial weitht
theta1 = 0.2 # initial weight
theta2 = 0.3 # initial weitht

M = 15            # number of epochs to run
eta = 0.005       # learning rate
th = 0.9          # threshold to stop
verbose = F   # whether detailed weight update info is printed

for (i in 1:M){
  print(paste('Epoch starts: ', i))
  
  ## We reshuffle the order of the datapoint for each epoch.
  index = 1:1000
  index = sample(index)
  
  for (j in index){
    y_j = theta0 + theta1*g_x[j] + theta2*g_y[j]
    
    if (y_j >= 0){
      pred_j = 1
      
    }else{
      pred_j = -1}
    
    theta0 = theta0 + eta*(group[j] - pred_j)*1.0
    theta1 = theta1 + eta*(group[j] - pred_j)*g_x[j]
    theta2 = theta2 + eta*(group[j] - pred_j)*g_y[j]
    
    if (verbose == T){
      print(paste('  -> updating data point ', j, ' : '))
      print(paste('     -> theta0: ' ,theta0))
      print(paste('     -> theta0: ' ,theta1))
      print(paste('     -> theta0: ' ,theta2))
    }
  }  
  
  y_all = theta0 + theta1*g_x + theta2*g_y
  y_pred = y_all
  y_pred[y_all >= 0] = 1
  y_pred[y_all< 0] = -1
  
  acc = sum(y_pred == group)/length(group)
  print(paste('Epoch ends: ', i, ' WITH accuracy: ', acc))
  
  if (acc >= th){
    break
  }
}    

y_all = theta0 + theta1*g_x + theta2*g_y
print(y_all)

y_pred = y_all
y_pred[y_all >= 0] = 1
y_pred[y_all< 0] = -1
print(y_pred)

acc = sum(y_pred == group)/length(group)
print(acc)

plot(g_x, g_y, type='n', xlab='X', ylab='Y')
points(g1_x, g1_y, col='red')
points(g2_x, g2_y, col='blue')
abline(a = -1.0*theta0/theta2, b = -1.0*theta1/theta2, col='dark green', lwd=3, lty=2)




