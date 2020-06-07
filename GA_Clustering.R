setwd("~/GitHub/GA_Clustering/data")

##### PACKAGES #####
library(tidyverse)
library(MASS)
library(e1071)
library(ggplot2)
library(EnvStats)

install.packages("EnvStats")


##### FUNCTIONS #####

## Data Generation Functions ##
scenarios_generation <- function(){
  
  # Data Set 1
  mu1 <- c(2,2)
  mu2 <- c(-2,2)
  r <- 1
  
  n_data_points = 500
  
  x1_x = runif(n_data_points/2, min = mu1[1]-1, max = mu1[1]+1)
  boundsx1_y = c(quadratic(1,-2*mu1[2],mu1[2]*mu1[2]+(x1_x-mu1[1])*(x1_x-mu1[1])-r*r ))
  boundsx1_y <- rbind(boundsx1_y[1:(length(boundsx1_y)/2)],boundsx1_y[((length(boundsx1_y)/2)+1):length(boundsx1_y)])
  x1_y <- c(1:(n_data_points/2))
  for(i in 1:n_data_points/2){
    x1_y[i] <- runif(1, min = boundsx1_y[2,i], max = boundsx1_y[1,i])
  }

  x1 = cbind(x1_x,x1_y)
  
  x2_x = runif(n_data_points/2, min = mu2[1]-1, max = mu2[1]+1)
  boundsx2_y = c(quadratic(1,-2*mu2[2],mu2[2]*mu2[2]+(x2_x-mu2[1])*(x2_x-mu2[1])-r*r ))
  boundsx2_y <- rbind(boundsx2_y[1:(length(boundsx2_y)/2)],boundsx2_y[((length(boundsx2_y)/2)+1):length(boundsx2_y)])
  x2_y <- c(1:(n_data_points/2))
  for(i in 1:n_data_points/2){
    x2_y[i] <- runif(1, min = boundsx2_y[2,i], max = boundsx2_y[1,i])
  }
  
  x2 = cbind(x2_x,x2_y)
  
  
  y1 <- rep("1",n_data_points/2)
  y2 <- rep("2",n_data_points/2)
  
  y <- as.factor(c(y1, y2))
  X <- rbind(x1, x2) 
  
  data_scenario1 <- data.frame(y, X)
  colnames(data_scenario1) <- c("Y", "X1", "X2")
  
  #plots
  cols = 3-ifelse(data_scenario1[,1]== "2",1, -1)
  plot(data_scenario1[,-1], col = cols, main = "Scenario 1", ylim = c(-0.3,4.3), xlim = c(-4,4))
  legend('topleft', c('Yes', 'No'), pch = 1, col = c(2, 4))
  
  
  # Data Set 2
  mu1 = c(-3,3)
  mu2 = c(0,1)
  mu3 = c(3,3)
  
  n_data_points = 300
  x1_x = runif(n_data_points/3, min = mu1[1]-1, max = mu1[1]+1)
  boundsx1_y = c(quadratic(1,-2*mu1[2],mu1[2]*mu1[2]+(x1_x-mu1[1])*(x1_x-mu1[1])-r*r ))
  boundsx1_y <- rbind(boundsx1_y[1:(length(boundsx1_y)/2)],boundsx1_y[((length(boundsx1_y)/2)+1):length(boundsx1_y)])
  x1_y <- c(1:(n_data_points/3))
  for(i in 1:n_data_points/3){
    x1_y[i] <- runif(1, min = boundsx1_y[2,i], max = boundsx1_y[1,i])
  }
  
  x1 = cbind(x1_x,x1_y)
  
  x2_x = runif(n_data_points/3, min = mu2[1]-1, max = mu2[1]+1)
  boundsx2_y = c(quadratic(1,-2*mu2[2],mu2[2]*mu2[2]+(x2_x-mu2[1])*(x2_x-mu2[1])-r*r ))
  boundsx2_y <- rbind(boundsx2_y[1:(length(boundsx2_y)/2)],boundsx2_y[((length(boundsx2_y)/2)+1):length(boundsx2_y)])
  x2_y <- c(1:(n_data_points/3))
  for(i in 1:n_data_points/3){
    x2_y[i] <- runif(1, min = boundsx2_y[2,i], max = boundsx2_y[1,i])
  }
  
  x2 = cbind(x2_x,x2_y)
  
  x3_x = runif(n_data_points/3, min = mu3[1]-1, max = mu3[1]+1)
  boundsx3_y = c(quadratic(1,-2*mu3[2],mu3[2]*mu3[2]+(x3_x-mu3[1])*(x3_x-mu3[1])-r*r ))
  boundsx3_y <- rbind(boundsx3_y[1:(length(boundsx3_y)/2)],boundsx3_y[((length(boundsx3_y)/2)+1):length(boundsx3_y)])
  x3_y <- c(1:(n_data_points/3))
  for(i in 1:n_data_points/3){
    x3_y[i] <- runif(1, min = boundsx3_y[2,i], max = boundsx3_y[1,i])
  }
  
  x3 = cbind(x3_x,x3_y)
  
  y1 <- rep("1",n_data_points/3)
  y2 <- rep("2",n_data_points/3)
  y3 <- rep("3",n_data_points/3)
  
  y <- as.factor(c(y1, y2, y3))
  X <- rbind(x1, x2, x3)
  
  data_scenario2 <- data.frame(y, X)
  colnames(data_scenario2) <- c("Y", "X1", "X2")
  
  #plots
  cols = ifelse(data_scenario2[,1]== '1',2,ifelse(data_scenario2[,1]== '2',3,4))
  plot(data_scenario2[,-1], col = cols, main = "Scenario 2")
  legend('bottomleft', c('class 1', 'class 2', 'class 3'), pch = 1, col = c(2:4))
  
  
  # Data Set 3
  min1 = c(-3.3,0.7)
  max1 = c(-0.7,3.3)
  min2 = c(-1.3,0.7)
  max2 = c(1.3,3.3)
  min3 = c(0.7,0.7)
  max3 = c(3.3,3.3)
  min4 = c(-3.3,-1.3)
  max4 = c(-0.7,1.3)
  min5 = c(-1.3,-1.3)
  max5 = c(1.3,1.3)
  min6 = c(0.7,-1.3)
  max6 = c(1.3,3.3)
  min7 = c(-3.3,-3.3)
  max7 = c(-0.7,-0.7)
  min8 = c(-1.3,-3.3)
  max8 = c(1.3,-0.7)
  min9 = c(0.7,-3.3)
  max9 = c(3.3,-0.7)
  
  n_data_points = 900
  x1 = cbind(runif(n_data_points/9, min = min1[1], max = max1[1]),runif(n_data_points/9, min = min1[2], max = max1[2]))
  x2 = cbind(runif(n_data_points/9, min = min2[1], max = max2[1]),runif(n_data_points/9, min = min2[2], max = max2[2]))
  x3 = cbind(runif(n_data_points/9, min = min3[1], max = max3[1]),runif(n_data_points/9, min = min3[2], max = max3[2]))
  x4 = cbind(runif(n_data_points/9, min = min4[1], max = max4[1]),runif(n_data_points/9, min = min4[2], max = max4[2]))
  x5 = cbind(runif(n_data_points/9, min = min5[1], max = max5[1]),runif(n_data_points/9, min = min5[2], max = max5[2]))
  x6 = cbind(runif(n_data_points/9, min = min6[1], max = max6[1]),runif(n_data_points/9, min = min6[2], max = max6[2]))
  x7 = cbind(runif(n_data_points/9, min = min7[1], max = max7[1]),runif(n_data_points/9, min = min7[2], max = max7[2]))
  x8 = cbind(runif(n_data_points/9, min = min8[1], max = max8[1]),runif(n_data_points/9, min = min8[2], max = max8[2]))
  x9 = cbind(runif(n_data_points/9, min = min9[1], max = max9[1]),runif(n_data_points/9, min = min9[2], max = max9[2]))
  
  y1 <- rep("1",n_data_points/9)
  y2 <- rep("2",n_data_points/9)
  y3 <- rep("3",n_data_points/9)
  y4 <- rep("4",n_data_points/9)
  y5 <- rep("5",n_data_points/9)
  y6 <- rep("6",n_data_points/9)
  y7 <- rep("7",n_data_points/9)
  y8 <- rep("8",n_data_points/9)
  y9 <- rep("9",n_data_points/9)
  
  y <- as.factor(c(y1, y2, y3, y4, y5, y6, y7, y8, y9))
  X <- rbind(x1, x2, x3, x4, x5, x6, x7, x8, x9)
  
  data_scenario3 <- data.frame(y, X)
  colnames(data_scenario3) <- c("Y", "X1", "X2")
  
  plot(X1 ~ X2, data=data_scenario3, type='n', ylim=c(-4, 4))
  for(i in 1:9){
    min = (i-1)*(n_data_points/9)+1
    max = i*(n_data_points/9)
    text(data_scenario3$X1[min:max],data_scenario3$X2[min:max],label=i)
  }
  
  #plots
  cols = ifelse(data_scenario3[,1]== '1',2,ifelse(data_scenario3[,1]== '2',3,4))
  plot(data_scenario2[,-1], col = cols, main = "Scenario 3")
  legend('bottomleft', c('class 1', 'class 2', 'class 3', 'class 4', 'class 5', 'class 6', 'class 7', 'class 8', 'class 9'),
         pch = 1, col = c(2:4))
  
  
  ## Data Set 4
  n_data_points = 300
  
  x1 <- rtri(n_data_points/2, min = 0, max = 2, mode = 1)
  x2 <- rtri(n_data_points/2, min = 1, max = 3, mode = 2)
  for(i in 1:9){
    x1 <- cbind(x1,rtri(n_data_points/2, min = 0, max = 2, mode = 1))
    x2 <- cbind(x2,rtri(n_data_points/2, min = 0, max = 2, mode = 1))
  }
  
  y1 <- rep("1",n_data_points/2)
  y2 <- rep("2",n_data_points/2)
  
  y <- as.factor(c(y1, y2))
  X <- rbind(x1, x2)
  
  data_scenario4 <- data.frame(y, X)
  colnames(data_scenario4) <- c("Y", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10")
  
  
  return(list(data_scenario1, data_scenario2, data_scenario3, data_scenario4))
}

quadratic <- function(a,b,c){
  if(delta(a,b,c) > 0){ # first case D>0
    x_1 = (-b+sqrt(delta(a,b,c)))/(2*a)
    x_2 = (-b-sqrt(delta(a,b,c)))/(2*a)
    result = c(x_1,x_2)
  }
  else if(delta(a,b,c) == 0){ # second case D=0
    x = -b/(2*a)
  }
}

delta<-function(a,b,c){
  b^2-4*a*c
}

## Algorithm Functions ##
ga_clustering <- function(data, dim, n_clusters, pop_size, num_iterations, crossover_rate, mutation_rate, 
                          selection = "roulette", crossover = ){
  
  # Obtain lower and upper bounds to create the population
  maxValues <- max(data[1], na.rm = TRUE)
  minValues <- min(data[1], na.rm = TRUE)
  for(i in 2:dim){
    maxValues <- c(maxValues,max(data[i], na.rm = TRUE))
    minValues <- c(minValues,min(data[i], na.rm = TRUE))
  }
  
  
  # Create the initial population
  population <- runif(dim*n_clusters, min = minValues, max = maxValues)
  for(i in 1:(pop_size-1)){
    newindividual <- runif(dim, min = minValues, max = maxValues)
    population <- cbind(population,newindividual)
  }
  
  
  
  
}

createClusters <- function(chromosome, n_clusters, data){
  
  
}

fitness <- function(chromosome, dim, data){
  sum <- 0
  for(i in 1:length(data[1])){
    
  }
}

euclideanDist <- function(point1,point2){
  if(length(point1)!=length(point2)){
    print("lengths missmatched")
  }
  else{
    distance = 0
    for(i in 1:length(point1)){
      distance = distance + (point1[i]-point2[i])*(point1[i]-point2[i])
    }
  }
}


##### APPLICATION #####
set.seed(2020)


## Artifical Data Sets Analysis ##
list_scenarios <- scenarios_generation()



## Real Life Data Sets Analysis ##
iris_data <- read.csv("Iris.csv")
lungCancer_data <- read.csv("lung-cancer.data.csv")
transfusion_data <- read.csv("transfusion.data.csv")
redWineQuality_data <- read.csv("winequality-red.csv", sep = ";")                                       
whiteWineQuality_data <- read.csv("winequality-white.csv", sep = ";")

data <- read.csv("Iris.csv")
data <- data[2:length(data)]
ga_clustering(data[2,5])