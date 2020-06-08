setwd("~/GitHub/GA_Clustering/data")

##### PACKAGES #####
library(tidyverse)
library(MASS)
library(e1071)
library(ggplot2)
library(EnvStats)
library(pracma)
library(plotrix)

install.packages("plotrix")


##### FUNCTIONS #####

## Data Generation Functions ##
scenarios_generation <- function(){
  
  # Data Set 1
  mu1 <- c(1.5,2)
  mu2 <- c(-1.5,2)
  r <- 1
  
  n_data_points = 10
  
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
  mu1 = c(-2,2)
  mu2 = c(0,1)
  mu3 = c(2,2)
  r = 1
  
  n_data_points = 300
  x1_x = runif(n_data_points/3, min = mu1[1]-1, max = mu1[1]+1)
  boundsx1_y = c(quadratic(1,-2*mu1[2],mu1[2]*mu1[2]+(x1_x-mu1[1])*(x1_x-mu1[1])-r*r ))
  boundsx1_y <- rbind(boundsx1_y[1:(length(boundsx1_y)/2)],boundsx1_y[((length(boundsx1_y)/2)+1):length(boundsx1_y)])
  x1_y <- c(1:(n_data_points/3))
  for(i in 1:(n_data_points/3)){
    x1_y[i] <- runif(1, min = boundsx1_y[2,i], max = boundsx1_y[1,i])
  }
  
  x1 = cbind(x1_x,x1_y)
  
  x2_x = runif(n_data_points/3, min = mu2[1]-1, max = mu2[1]+1)
  boundsx2_y = c(quadratic(1,-2*mu2[2],mu2[2]*mu2[2]+(x2_x-mu2[1])*(x2_x-mu2[1])-r*r ))
  boundsx2_y <- rbind(boundsx2_y[1:(length(boundsx2_y)/2)],boundsx2_y[((length(boundsx2_y)/2)+1):length(boundsx2_y)])
  x2_y <- c(1:(n_data_points/3))
  for(i in 1:(n_data_points/3)){
    x2_y[i] <- runif(1, min = boundsx2_y[2,i], max = boundsx2_y[1,i])
  }
  
  x2 = cbind(x2_x,x2_y)
  
  x3_x = runif(n_data_points/3, min = mu3[1]-1, max = mu3[1]+1)
  boundsx3_y = c(quadratic(1,-2*mu3[2],mu3[2]*mu3[2]+(x3_x-mu3[1])*(x3_x-mu3[1])-r*r ))
  boundsx3_y <- rbind(boundsx3_y[1:(length(boundsx3_y)/2)],boundsx3_y[((length(boundsx3_y)/2)+1):length(boundsx3_y)])
  x3_y <- c(1:(n_data_points/3))
  for(i in 1:(n_data_points/3)){
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
  draw.circle(mu1[1],mu1[2],radius=1.1,nv=100,border="red",col="transparent",lty=1,density=NULL,
              angle=45,lwd=1)
  draw.circle(mu2[1],mu2[2],radius=1.1,nv=100,border="green",col="transparent",lty=1,density=NULL,
              angle=45,lwd=1)
  draw.circle(mu3[1],mu3[2],radius=1.1,nv=100,border="blue",col="transparent",lty=1,density=NULL,
              angle=45,lwd=1)
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
  
  n_data_points = 450
  x1 = cbind(rtri(n_data_points/9, -3.3, -0.7, -2),rtri(n_data_points/9, 0.7, 3.3, 2))
  x2 = cbind(rtri(n_data_points/9, -1.3, 1.3, 0)  ,rtri(n_data_points/9, 0.7, 3.3, 2))
  x3 = cbind(rtri(n_data_points/9, 0.7, 3.3, 2)   ,rtri(n_data_points/9, 0.7, 3.3, 2))
  x4 = cbind(rtri(n_data_points/9, -3.3, -0.7, -2),rtri(n_data_points/9, -1.3, 1.3, 0))
  x5 = cbind(rtri(n_data_points/9, -1.3, 1.3, 0)  ,rtri(n_data_points/9, -1.3, 1.3, 0))
  x6 = cbind(rtri(n_data_points/9, 0.7, 3.3, 2)   ,rtri(n_data_points/9, -1.3, 1.3, 0))
  x7 = cbind(rtri(n_data_points/9, -3.3, -0.7, -2),rtri(n_data_points/9, -3.3, -0.7, -2))
  x8 = cbind(rtri(n_data_points/9, -1.3, 1.3, 0)  ,rtri(n_data_points/9, -3.3, -0.7, -2))
  x9 = cbind(rtri(n_data_points/9, 0.7, 3.3, 2)   ,rtri(n_data_points/9, -3.3, -0.7, -2))
  
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
  
  #Plots
  plot(X1 ~ X2, data=data_scenario3, type='n', ylim=c(-4, 4))
  for(i in 1:9){
    min = (i-1)*(n_data_points/9)+1
    max = i*(n_data_points/9)
    text(data_scenario3$X1[min:max],data_scenario3$X2[min:max],label=i)
  }
  
  
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
                          selection = "roulette", crossover = "single"){
  
  sampleSize <- nrow(data)
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
    population <- cbind(population,runif(dim*n_clusters, min = minValues, max = maxValues))
  }
  
  # Define the fitness of the initial population
  fitnessOfPop <- c(fitness(chromosome = population[1:(n_clusters*dim),], dim = dim, n_clusters = n_clusters, data = data, sampleSize))
  maxFitness = fitnessOfPop[1]
  best_hypothesis <- population[1:(n_clusters*dim),1]
  for(i in 2:pop_size){
    fitnessOfPop <- cbind(fitnessOfPop,fitness(chromosome = population[1:(n_clusters*dim),i], dim = dim, n_clusters = n_clusters, data = data, sampleSize))
    if(maxFitness < fitnessOfPop[i]){
      maxFitness = fitnessOfPop[i]
      best_hypothesis <- population[1:(n_clusters*dim),i]
    }
  }
  fitnessOfPop <- c(fitnessOfPop)
  # Define parameters
  numOfCrossoverPopulation <- as.integer(crossover_rate*pop_size)
  if(mod(numOfCrossoverPopulation,2)==1){
    numOfCrossoverPopulation = numOfCrossoverPopulation+1
  }
  numOfSelected <- as.integer(pop_size-numOfCrossoverPopulation)
  #Simulation
  for(i in 1:num_iterations){
    
    print(1/maxFitness)
    
    probability <- fitnessOfPop
    probability <- c(probability/sum(probability))
    
    #Selection
    newpopulationIndexes <- sample(1:length(probability),size = numOfSelected,prob=probability)
    newpopulation <- population[1:(n_clusters*dim),newpopulationIndexes]
      
    #Crossover
    for(j in 1:(numOfCrossoverPopulation/2)){
      parentsIndexes <- sample(1:pop_size,size=2,prob=probability)
      parents <- population[1:(n_clusters*dim),parentsIndexes]
      random <- sample(1:(n_clusters*dim), size =1)
        
      temp <- parents[random:(n_clusters*dim),1]
      parents[random:(n_clusters*dim),1] <- parents[random:(n_clusters*dim),2]
      parents[random:(n_clusters*dim),2] <- temp
      newpopulation <- cbind(newpopulation,parents[1:(n_clusters*dim),1],parents[1:(n_clusters*dim),2])
    }
    
    #Mutation
    for(j in 1:pop_size){
      random <- runif(1,min=0,max=1)
      
      if(random<mutation_rate){
        random <- runif(1,min=-1,max=1)
        index <- sample(1:(n_clusters*dim), size =1)
          
        if(newpopulation[index]==0){
          newpopulation[index] <- newpopulation[index]+2*random
        }
        else{
          newpopulation[index] <- newpopulation[index]+2*random*newpopulation[index]
        }
      }
      
    }
    
    #Replace population
    population <- newpopulation
    
    #Recompute fitness values
    for(i in 1:pop_size){
      fitnessOfPop[i] <- fitness(chromosome = population[1:(n_clusters*dim),i], dim = dim, n_clusters = n_clusters, data = data, sampleSize = sampleSize)
      if(maxFitness < fitnessOfPop[i]){
        maxFitness = fitnessOfPop[i]
        best_hypothesis <- population[1:(n_clusters*dim),i]
      }
    }
  }
  
  print(1/maxFitness)
  print(best_hypothesis)
  
  return(best_hypothesis)
  
}

fitness <- function(chromosome, dim, n_clusters, data, sampleSize){
  return(1/clustering_metric(chromosome, dim, n_clusters, data, sampleSize))
}

clustering_metric <- function(chromosome, dim, n_clusters, data, sampleSize){
  sum <- 0
  for(x in 1:sampleSize){
    min <- dist(rbind(chromosome[1:dim], data[x,1:dim]))
    for(j in 1:(n_clusters-1)){
      newdist <- dist(rbind(chromosome[1:dim], data[x,1:dim]))
      if(min>newdist){
        min <- newdist
      }
    }
    sum <- sum + min
  }
  
  return(as.numeric(sum))
}


##### APPLICATION #####
set.seed(2020)


### Artifical Data Sets Analysis ###
list_scenarios <- scenarios_generation()



### Real Life Data Sets Analysis ###
iris_data <- read.csv("data/Iris.csv")
lungCancer_data <- read.csv("data/lung-cancer.data.csv")
transfusion_data <- read.csv("data/transfusion.data.csv")
redWineQuality_data <- read.csv("data/winequality-red.csv", sep = ";")                                       
whiteWineQuality_data <- read.csv("data/winequality-white.csv", sep = ";")

## Iris Data Test
data <- iris_data[2:5]

# k-means test
for(test in 1:5){
  cluster_means <- kmeans(data,3, iter.max = 1000)
  print(cluster_means$centers)
  print(clustering_metric(c(cluster_means$centers[1,1:4],cluster_means$centers[2,1:4],cluster_means$centers[3,1:4]),
                4,3,data,150))
}

# GA clustering
for(test in 1:5){
  ga_means <- ga_clustering(data, 4, 3, 100, 100, 0.8, 0.001)
  print(clustering_metric(ga_means,4,3,data,150))
}

## Red Wine Data Test
data <- redWineQuality_data[1:11]
for(test in 1:5){
  cluster_means <- kmeans(data,3, iter.max = 1000)
  print(cluster_means$centers)
  metricVector <- c(cluster_means$centers[1:4])
}

ga_means <- ga_clustering(data, 11, 6, 10, 100, 0.8, 0.001)


# White Wine Data Test
