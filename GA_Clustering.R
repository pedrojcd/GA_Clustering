##### PACKAGES #####
library(tidyverse)
library(MASS)
library(e1071)
library(ggplot2)


##### FUNCTIONS #####

ga_clustering <- function(data, dim, n_clusters, pop_size, num_iterations, crossover_rate, mutation_rate, 
                            selection = "roulette", crossover = ){
  maxValues <- max(data, na.rm = TRUE)
  minValues <- min(data, na.rm = TRUE)
  population <- runif(pop_size, min = minValues, max = maxValues)
  for(i in 1:pop_size-1){
    newindividual <- c(1:(dim*n_clusters))
    population <- cbind(population,newindividual)
  }
  
}

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
  
  n_data_points = 270
  x1 = runif(n_data_points/9, min = min1, max = max1)
  x2 = runif(n_data_points/9, min = min2, max = max2)
  x3 = runif(n_data_points/9, min = min3, max = max3)
  x4 = runif(n_data_points/9, min = min4, max = max4)
  x5 = runif(n_data_points/9, min = min5, max = max5)
  x6 = runif(n_data_points/9, min = min6, max = max6)
  x7 = runif(n_data_points/9, min = min7, max = max7)
  x8 = runif(n_data_points/9, min = min8, max = max8)
  x9 = runif(n_data_points/9, min = min9, max = max9)
  
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
  
  #plots
  cols = ifelse(data_scenario3[,1]== '1',2,ifelse(data_scenario3[,1]== '2',3,4))
  plot(data_scenario2[,-1], col = cols, main = "Scenario 3")
  legend('bottomleft', c('class 1', 'class 2', 'class 3', 'class 4', 'class 5', 'class 6', 'class 7', 'class 8', 'class 9'),
         pch = 1, col = c(2:4))
  
  
  ## Data Set 4
  min = c(-3,3)
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


# Usage
set.seed(2020)

list_scenarios <- scenarios_generation()

                                       