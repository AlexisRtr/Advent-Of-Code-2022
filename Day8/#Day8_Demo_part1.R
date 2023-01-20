library(dplyr)
library(tidyr)
library(stringr)

## Set repo
setwd("C:/Users/alexi/OneDrive/Documents/Advent-Of-Code-2022/Day8")
getwd()

## Load data
path <- "input.txt"
data <- read.delim(path, header = FALSE)
data <- as.data.frame(data)
map <- data
map <- as.data.frame(lapply(map, as.numeric))
sapply(map, class)

map <- map %>% separate( V1, c('0','a', 'b','c','d', 'e') , sep = '') %>% 
  select(c(-'0'))

# MAP: MATRIX
# POSITION: (a,b)
# DIRECTION: LEFT TOP DOWN RIGTH

map

positon_test <- c("1","1")


is_visible <- function(map,position,direction){
  
  i_row <- position[1]
  i_col <- position[2]
  length_line <- length(map[1,])
  length_col <- length(map[,1])
  
  if (direction == "right"){
    i_col_next <- i_col+1
    max_right <- max(map[i_row,i_col_next:length_col])
    # print(max_right)
    # print(map[i_row,i_col_next:length_col])
    
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
    # print(max_left)
    # print(map[i_row,1:i_col_past])
    
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
    # print(max_bottom)
    # print(map[i_row_next:length_col,i_col])
    
    if (max_bottom < map[i_row,i_col]) {
      return("VISIBLE")
    }
    else{
      return("NO")
    }
  }
  if (direction == "top"){
    
    i_row_past <- i_row-1
    # print(map[1:i_row_past,i_col])
    
    max_top <- max(map[1:i_row_past,i_col])
    # print(max_top)
    # print(map[1:i_row_past,i_row])
    
    if (max_top < map[i_row,i_col]) {
      return("VISIBLE")
    }
    else{
      return("NO")
    }
  }
  
}

position_list <- c("right","left","top","bottom")

map_ <- map


nb_visible <- function(map){
  
  c <- 0
  for (i in seq(2,length(map_[,1])-1)){
    # print(i)
    for (j in seq(2,length(map_[,1])-1)){
      # print(j)
      for (pos in position_list){
        # print(pos)
        # print(c(i,j))
        res <- is_visible(map = map_,position = c(i,j),direction = pos)
        # print(res)
        if (res == "VISIBLE") {
          # print(c(i,j))
          # print(pos)
          c <- c+1
          break
        }
      }
    }
  }
  print(c)
  edge <- length(map[,1])*2 + length(map[1,])*2 -4

  print(edge)
  return(c+edge)
}
nb_visible(map)

# is_visible(map,position = c(3,2),direction = "right")
# is_visible(map,position = c(3,2),direction = "left")
# is_visible(map,position = c(2,3),direction = "bottom")
# is_visible(map,position = c(2,3),direction = "top")
