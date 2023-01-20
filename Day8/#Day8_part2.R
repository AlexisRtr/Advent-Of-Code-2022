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




#### PART 2 ####

#### FUNCTION: CHECK 4 DIRECTIONS POSSIBLE AND RETURN THE DISTANCE BETWEEN HIGH TREES ####
max_scenic_score <- function(map,position,direction){
  
  i_row <- position[1]
  i_col <- position[2]
  length_line <- length(map[,1])
  length_col <- length(map[1,])
  
  highest <- map[i_row,i_col]
  distance <- 1
  
  if (direction == "right"){
    i_col_next <- i_col+1
    
    # Check if there is a higher tree after
    while (map[i_row,i_col_next] < highest) {
      
      # If we are on the edge (beginning or end) we stop
      if (i_col_next == length_line) {
        return(distance)
      }
      
      # add one more distance and continu to search
      distance <- distance+1
      i_col_next <- i_col_next+1
    }
    return(distance)
  }
  if (direction == "left"){
    i_col_pre <- i_col-1
    
    while ((map[i_row,i_col_pre] < highest)) {
      if (i_col_pre == "1") {
        return(distance)
      }
      distance <- distance+1
      i_col_pre <- i_col_pre-1
    }
    return(distance)
    
  }
  if (direction == "bottom"){
    i_row_pre <- i_row+1
    while ((map[i_row_pre,i_col] < highest)) {
      if (i_row_pre == length_col) {
        return(distance)
      }
      distance <- distance+1
      i_row_pre <- i_row_pre+1
    }
    return(distance)
  }
  if (direction == "top"){
    i_row_next <- i_row-1
    while ((map[i_row_next,i_col] < highest)) {
      if (i_row_next == 1) {
        return(distance)
      }
      distance <- distance+1
      i_row_next <- i_row_next-1
    }
    return(distance)
  }
}

# Try to check if it works
# max_scenic_score(map = map, position = c(3,9),direction = "top")


#### FUNCTION: CHECK ALL POSITIONS AND RETURN THE TREE WITH THE HIGHEST DISTANCE ####
# MAP: MATRIX
# POSITION: (a: index line, b: index column)
# DIRECTION: "LEFT" "TOP" "DOWN" "RIGTH"

highest_scenic_score <- function(map){
  
  score_max <- 0
  c <- 0
  for (i in seq(2,98)){
    for (j in seq(2,98)){
      
      # Reset the score for each position
      score_pos <- 1
      
      for (pos in position_list){
        # * of score for each position
        score <- max_scenic_score(map = map,position = c(i,j),direction = pos)
        score_pos <- score_pos*score
      }
      
      # Check max
      if (score_max < score_pos){
        score_max <- score_pos
        print(score_max)
      }
    }
  }
  # print(c)
  
  # print(edge)
  return(score_max)
}

position_list <- c("right","left","top","bottom")
highest_scenic_score(map)

# Check if it works
# is_visible(map,position = c(3,2),direction = "right")
# is_visible(map,position = c(3,2),direction = "left")
# is_visible(map,position = c(2,3),direction = "bottom")
# is_visible(map,position = c(2,3),direction = "top")
