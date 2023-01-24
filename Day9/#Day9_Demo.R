library(dplyr)
library(tidyr)
library(stringr)

## Set repo
setwd("C:/Users/alexi/OneDrive/Documents/Advent-Of-Code-2022/Day9")
getwd()


####3 STRUCTURE ####

## FUNCTION: GET 1 DATASET 2 COLUMNS: DIRECTION, DISTANCE
preprocessing <- function(path){
  data <- read.delim(path, header = FALSE)
  data <- as.data.frame(data)
  
  data <- data %>%
    separate(V1, into = c('POS', 'DISTANCE'),
             sep = '\\s* \\s*')
  
  data <- as.data.frame(data)
  data$DISTANCE <- as.numeric(data$DISTANCE)
  sapply(data, class)
  return(data)
}


## FUNCTION TEMP POSITION 
  # PARAMTERS
    ## INITIAL POSITION, DIRECTION, DISTANCE TO GO
  # OUTPUT
    ## NEXTPOSITION
temp_motion <- function(position,direction){
  
  if (direction == "R"){
    next_position <- position[2]+1
    return(c(position[1],next_position))
  }
  if (direction == "L"){
    next_position <- position[2]-1
    return(c(position[1],next_position))
  }
  if (direction == "U"){
    next_position <- position[1]-1
    return(c(next_position,position[2]))
  }
  if (direction == "D"){
    next_position <- position[1]+1
    return(c(next_position,position[2]))
  }
}
position <- c(8,1)
temp_motion(position,direction = "D")
calculation_distance <- function(position1,position2){
  distance <- (position1[1]-position2[1])**2 +(position1[2]-position2[2])**2
  return(sqrt(distance))
}


## FUNCTION DO ONE MOVE: BRING HEAD + TAIL IN THE FINAL POSITION
  # PARAMETERS
    ## POSITION, FINAL DISTANCE
  # OUTPUT
    ## NEW POSITION FOR THE HEAD AND THE TAIL
add_pos <- function(list_pos,pos){
  pos <- as.numeric(paste0(pos[1],pos[2]))
  if (!(pos %in% list_pos )){
    # print("POSITION TAIL ADDDED")
    return(c(list_pos,pos))
  }
  else{
    return(c(list_pos))
  }
}


crossing_bridge <- function(map){
  n_col <- length(map[,1])
  # c <- 0
  
  list_pos_tail <- c()
  list_pos_head <- c()
  
  # starting position
  start_position <- c(300,300)
  distance_temp <- 0
  
  position_tail <- start_position
  position_head <- start_position
  
  for (i in seq(1,n_col)){
    # print(i)
    for (d in seq(map[i,2])){
      # cat("LINE ",i," MOVE ",map[i,1], " NUMBER " , d)
      list_pos_head <- c(list_pos_head,list(position_head))

      if (distance_temp > 2.1) {
        # c <- c+1
        # print(c)
        n_list <- length(list_pos_head)-1
        position_tail <- list_pos_head[n_list][[1]]
        list_pos_tail <- add_pos(list_pos_tail,position_tail)
      }

      dir <- map[i,1]

      position_head <- temp_motion(position = position_head,direction = dir)


      ## Moving head
      distance_temp <- calculation_distance(position1 = position_head,position2 = position_tail)
      if (distance_temp == 2){
        ## NEW POS FOR THE TAIL
        position_tail <- temp_motion(position = position_tail,direction = dir)
      }

      if (distance_temp == sqrt(2)){
        # DIAGONALE == TAIL DON'T MOVE
        # print("distance == 2")
        # First and last move: no need to move the tail
        if (d == 1 | d == map[i,2]){
          # print("position_tail NOT ADDED position_head")
          # position_tail <- position_head
        }
        else {
          # print("position_tail <- position_head")
          position_tail <- position_head
        }
      }

      # print("position_head")
      # print(position_head)
      # print("position_tail")
      # print(position_tail)
      # cat("LENGTH POSITION TAIL\t", length(list_pos_tail),"\n")
      # print((list_pos_tail))
      
      list_pos_tail <- add_pos(list_pos_tail,position_tail)
    }
    
    # cat("NB DE POSITION FOR ", dir," distance ", d," c ==")

  }
  return(list_pos_tail)
}

#### PART 1 DEMO: ####
path <- "C:/Users/alexi/OneDrive/Documents/Advent-Of-Code-2022/Day9/input.txt"
data_preprocessed <- preprocessing(path)
df <- data_preprocessed
a <- crossing_bridge(data_preprocessed)
length(a)
# 13


### TEST FUNCTIONS
# a <- c(299,287)
# b <- c(300,288)
# calculation_distance(a,b)
# 
# direction <- "R"
# distance <- 4
# df[8,1]
