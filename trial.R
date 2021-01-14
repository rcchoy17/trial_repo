########################################################
# LSHTM Machine Learning Spring Course                 #
# Week #1, Student Activity #4 (Day 1)                 #
# NHANES and linear regression                         #
# January 2020                                         #
########################################################
load("nr_complete.Rdata")
data=nr_complete

#Set number of observations to use in training data set
N = 1000
y=data$BPSysAve[1:N]
x=data$Age[1:N]

#Normalise the variables

y = scale(y)
x = scale(x)

#Set initial values
b0_last = 0
b1_last = 1
alpha = 0.1
number_iterations = 100

#create vectors to hold values of parameters and cost
b0 = c(b0_last)
b1 = c(b1_last)
f <- function (b0, b1, x) b0 + b1*x
cost = c((1/(2*N))*sum((f(b0, b1, x) - y)^2))

#loop to perform gradient descent
for (i in 1:number_iterations) {
  #simultaneous updating of values
  b0_new = b0_last - alpha*(1/N)*sum(f(b0_last, b1_last, x) - y)
  b1_new = b1_last - alpha*(1/N)*sum((f(b0_last, b1_last, x) - y)*x)
 
  
  #update cost function and add new values to vectors
  cost_new = c((1/(2*N))*sum((f(b0_new, b1_new, x) - y)^2))
  
  b0 = c(b0,b0_new) #append new value
  b1 = c(b1,b1_new) #append new value
  cost = c(cost, cost_new) #append new value
  
  #update values of parameters for next iteration
  b0_last = b0_new
  b1_last = b1_new
  
  #Find fitted values of outcome given updated parameters
  h = f(b0_new, b1_new, x)
}

#Plot B0 vs Cost
plot(b0, cost, pch=16, col='dark gray')
temp_b0 <- b0[order(b0)]
temp_cost <- cost[order(b0)]
lines(temp_b0, temp_cost, col='dark gray')
lines(b0, cost, col="red")

#Plot B1 vs Cost
plot(b1, cost, pch=16, col='dark gray')
temp_b1 <- b1[order(b1)]
temp_cost <- cost[order(b1)]
lines(temp_b1, temp_cost, col='dark gray')
lines(b1, cost, col="red")

#Plot fit of model & parameters to the data
plot(x, y, col='dark gray', xlab="Age", ylab="Sys. BP")
lines(x, ...<fitted values>, col="blue")

###########################################
# Gradient descent with stopping criteria #
###########################################

#loop to perform gradient descent
#HINT: can use while loop


age = c(30,40,13,27,3,8,31,39,24,19,28,27,18,26,6,29,5,3,25,12,21,25,28,69,22,6)
median(age)
