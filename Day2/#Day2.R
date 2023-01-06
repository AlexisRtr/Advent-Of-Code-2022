library(dplyr)
library(tidyr)


## Setup the rigth repo
setwd("Day2/")
getwd()

## PArt 1
score_shape <- function(shape){
  score <- 0
  if (shape == "X"){ # Rock
    score <- 1
  }
  if (shape == "Y"){
    score <- 2 # Paper
  }
  if (shape == "Z"){ # Scissors
    score <- 3
  }
  print(score)
  return(score)
}

score_outcome <- function(shape_opponent, shape_me){
  score <- 0
  
  # Win
  if ((shape_opponent == "A" && shape_me == "Y") | 
      (shape_opponent == "B" && shape_me == "Z") |
      (shape_opponent == "C" && shape_me == "X") ) {
    # I win 6 points
    score <-  6
  }
  
  # Draw
  if ((shape_opponent == "A" && shape_me == "X") | 
      (shape_opponent == "B" && shape_me == "Y") |
      (shape_opponent == "C" && shape_me == "Z") ) { 
    # Draw I win 3 points
    score <- 3
  }
  
  # Loose
  if ((shape_opponent == "A" && shape_me == "Z") | 
      (shape_opponent == "B" && shape_me == "X") |
      (shape_opponent == "C" && shape_me == "Y") ) { # Draw
    # Draw I win 3 points
    score <- 0
  }
  print(score)
  return(score)
}

score_outcome("A","Y")

calculator <- function(path_to_write){
  ## Import data
  
  data <- read.delim(path_to_write, header = FALSE)
  data <- as.data.frame(data)
  
  ## initialize
  global_score <- 0
  ## Read each score 
  for (i in seq(1,length(data$V1))){
    
    # assign results for each player
    played_opponent <- substr(data$V1[i],1,1)
    played_me <- substr(data$V1[i],3,3)
    
    global_score <- global_score + 
      score_shape(shape = played_me) + 
      score_outcome(shape_opponent = played_opponent, shape_me = played_me)
    # print(global_score)
  }
  
  return(print(global_score))
}


### Part 1 - results
## Demo
path <- "input.txt"
calculator(path)

## Puzzle
path_real <- "input_real_part1.txt"
calculator(path_real)


### Part 2 - results
new_shape <- function(shape_opponent, shape_me){
  
  # Need to lose
  if (shape_me == "X"){
    score <- 0
    if (shape_opponent == "A"){
      new.shape_me <<- "Z"
    }
    if (shape_opponent == "B") {
      new.shape_me <<- "X"
    }
    if (shape_opponent == "C") {
      new.shape_me <<- "Y"
    }
  }
  
  # Need to draw
  if (shape_me == "Y"){
    score <- 3
    if (shape_opponent == "A"){
      new.shape_me <<- "X"
    }
    if (shape_opponent == "B") {
      new.shape_me <<- "Y"
    }
    if (shape_opponent == "C") {
      new.shape_me <<- "Z"
    }
  }
  
  # Need to win
  if (shape_me == "Z"){
    score <- 6
    if (shape_opponent == "A"){
      new.shape_me <<- "Y"
    }
    if (shape_opponent == "B") {
      new.shape_me <<- "Z"
    }
    if (shape_opponent == "C") {
      new.shape_me <<- "X"
    }
  }
  
  print(new.shape_me)
  return(new.shape_me)
}


calculator_day2 <- function(path_to_write){
  ## Import data
  
  data <- read.delim(path_to_write, header = FALSE)
  data <- as.data.frame(data)
  
  ## initialize
  global_score <- 0
  ## Read each score 
  for (i in seq(1,length(data$V1))){
    
    # assign results for each player
    played_opponent <- substr(data$V1[i],1,1)
    played_me <- substr(data$V1[i],3,3)
    print(played_me)
    
    #Replacing the new shape I played to respect the result
    new_played_me <- new_shape(shape_opponent = played_opponent,shape_me = played_me) 
    print(new_played_me)
    
    global_score <- global_score + 
      score_shape(shape = new_played_me) + 
      score_outcome(shape_opponent = played_opponent, shape_me = new_played_me)
    print("-------------------")
  }
  
  return(print(global_score))
}

# Demo
path <- "input.txt"
calculator_day2(path)

# Puzzle
path <- "input_real_part2.txt"
calculator_day2(path)
