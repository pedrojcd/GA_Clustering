##### PACKAGES #####
library(tidyverse)
library(MASS)

##### FUNCTIONS #####

ga_clustering <- function(data, dim, n_clusters, pop_size, num_iterations, crossover_rate, mutation_rate, 
                            selection = "roulette", crossover = ){
  
}

scenarios_generation <- function(){
  
  # Scenario 1
  mu1 = c(2,2)
  mu2 = c(-2,2)
  
  n_data_points = 10
  x1 = mvrnorm(n_data_points/2, mu = mu1, Sigma = .5*diag(2))
  x2 = mvrnorm(n_data_points/2, mu = mu2, Sigma = .5*diag(2))
  y1 <- rep("Yes",n_data_points/2)
  y2 <- rep("No",n_data_points/2)
  
  y <- as.factor(c(y1, y2))
  X <- rbind(x1, x2)
  
  data_scenario1 <- data.frame(y, X)
  colnames(data_scenario1) <- c("Y", "X1", "X2")
  
  #plots
  cols = 3-ifelse(data_scenario1[,1]== "Yes",1, -1)
  plot(data_scenario1[,-1], col = cols, main = "Scenario 1", ylim = c(0,4), xlim = c(-5,5))
  legend('topleft', c('Yes', 'No'), pch = 1, col = c(2, 4))
  
  
  # Scenario 2
  mu1 = c(-3,3)
  mu2 = c(0,1)
  mu3 = c(3,3)
  
  n_data_points = 300
  x1 = runif(n_data_points/3, min = mu1-1, max = mu1+1)
  x2 = runif(n_data_points/3, min = mu2-1, max = mu2+1)
  x3 = runif(n_data_points/3, min = mu3-1, max = mu3+1)
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
  legend('topleft', c('1', '2', '3'), pch = 1, col = c(2:4))
  
  return(list(data_scenario1, data_scenario2))
}


# Usage
set.seed(2020)

list_scenarios <- scenarios_generation()

                                       