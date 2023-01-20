library(dplyr)
library(tidyr)
library(stringr)
library(readr)

## Set repo
setwd("C:/Users/alexi/OneDrive/Documents/Advent-Of-Code-2022/Day8")
getwd()

## Load data
path <- "input_real.txt"
data <- read_file("input_real.txt")
data <- as.data.frame(data)

# Split the string 
map <- str_split(string = data$data, pattern = '\r\n')
map <- as.data.frame(map)
names(map) <- c("V1")

# Separate the df from 1 column into 100 ones
col_names <- as.character(seq(1,100))
map <- map %>% separate( V1, col_names , sep = '') %>% 
  select(c(-'1')) # Delete first column empty

# Rename in str columns
names(map) <- rep(x = "a",1,99)
map %>% mutate_if(is.character,as.numeric)
map <-  lapply(map,as.numeric)
map <- as.data.frame(map)
sapply(map, class)
map <- map[1:99,] # remove last row, full of NA


#### FUNCTION: CHECK 4 DIRECTIONS POSSIBLE AND RETURN IF MAX ####
is_visible <- function(map,position,direction){
  
  i_row <- position[1]
  i_col <- position[2]
  length_line <- length(map[,1])
  length_col <- length(map[1,])

  # For each direction, check if the position is the max of the next line/column
    
  if (direction == "right"){
    i_col_next <- i_col+1
    max_right <- max(map[i_row,i_col_next:length_col])
    
    if (max_right < map[i_row,i_col]) {
      return("VISIBLE")
    }
    else{
      return("NO")
    }
  }
  
  if (direction == "left"){
    i_col_past <- i_col-1
    max_left <- max(map[i_row,1:i_col_past])
    
    if (max_left < map[i_row,i_col]) {
      return("VISIBLE")
    }
    else{
      return("NO")
    }
  }
  
  if (direction == "bottom"){
    i_row_next <- i_row+1
    max_bottom <- max(map[i_row_next:length_col,i_col])
    
    if (max_bottom < map[i_row,i_col]) {
      return("VISIBLE")
    }
    else{
      return("NO")
    }
  }
  
  if (direction == "top"){
    i_row_past <- i_row-1
    max_top <- max(map[1:i_row_past,i_col])
    
    if (max_top < map[i_row,i_col]) {
      return("VISIBLE")
    }
    else{
      return("NO")
    }
  }
  
}


#### FUNCTION: CHECK ALL POSITIONS AND COUNT THE NUMBER OF TREES VISIBLE FROM OUTSIDE ####
# MAP: MATRIX
# POSITION: (a: index line, b: index column)
# DIRECTION: "LEFT" "TOP" "DOWN" "RIGTH"

nb_visible <- function(map){
  
  c <- 0
  for (i in seq(2,98)){
    for (j in seq(2,98)){
      for (pos in position_list){
        res <- is_visible(map = map,position = c(i,j),direction = pos)
        if (res == "VISIBLE") {
          c <- c+1
          break # AVOID COUNTING SEVERAL TIME THE SAME TREE
        }
      }
    }
  }
  # ADD THE TREE ON THE EDGE
  edge <- length(map[,1])*2 + length(map[1,])*2 -4

  return(c+edge)
}

position_list <- c("right","left","top","bottom")
nb_visible(map)