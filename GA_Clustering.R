##### PACKAGES #####
library(tidyverse)
library(MASS)

##### FUNCTIONS #####

ga_clustering <- function(data, dim, n_clusters, pop_size, num_iterations, crossover_rate, mutation_rate, 
                            selection = "roulette", crossover = ){
  
}

scenarios_generation <- function(mu1, mu2, n_data_points = 200) {
  # Scenario 1
  x1 = mvrnorm(n_data_points/2, mu = mu1, Sigma = .1*diag(2))
  x2 = mvrnorm(n_data_points/2, mu = mu2, Sigma = .1*diag(2))
  y1 <- rep("Yes",100)
  y2 <- rep("No",100)
  
  y <- as.factor(c(y1, y2))
  X <- rbind(x1, x2)
  
  data_scenario1 <- data.frame(y, X)
  colnames(data_scenario1) <- c("Y", "X1", "X2")
  
  #plots
  cols = 3-ifelse(data_scenario1[,1]== "Yes",1, -1)
  plot(data_scenario1[,-1], col = cols, main = "Scenario 1")
  legend('topleft', c('Yes', 'No'), pch = 1, col = c(2, 4))
  
  # Scenario 2
  mean_matrix <- mvrnorm(10, mu = mu1, Sigma = diag(2))
  mean_matrix_2 <- mvrnorm(10, mu = mu2, Sigma = diag(2))
  data_yes <- c()
  data_no <- c()
  for (kk in 1:10) {
    aux_yes <- mvrnorm(10, mu = mean_matrix[kk,], Sigma = .1*diag(2))
    data_yes <- rbind(data_yes, aux_yes)
    
    aux_no <- mvrnorm(10, mu = mean_matrix_2[kk,], Sigma = .1*diag(2))
    data_no <- rbind(data_no, aux_no)
  }
  
  y_yes <- rep("Yes", n_data_points/2)
  y_no <- rep("No", 100)
  
  y <- as.factor(c(y_yes, y_no))
  X <- rbind(data_yes, data_no)
  
  data_scenario2 <- data.frame(y, X)
  colnames(data_scenario2) <- c("Y", "X1", "X2")
  
  #plots
  cols = 3-ifelse(data_scenario2[,1]== "Yes",1, -1)
  plot(data_scenario2[,-1], col = cols, main = "Scenario 2")
  legend('topleft', c('Yes', 'No'), pch = 1, col = c(2, 4))
  
  return(list(data_scenario1, data_scenario2))
}


# Usage

mu1 = c(0,0)
mu2 = c(4,4)

list_scenarios <- scenarios_generation(mu1, mu2)

                                       