library(dplyr)
library(tidyr)
library(stringr)

## Set repo
setwd("Day5")
getwd()


## Load data
path <- "input.txt"
# path <- "input_real.txt"
data <- read.delim(path, header = FALSE)
data_imported <- as.data.frame(data)
head(data)


# Separate in 2 dataframe

# Pyramide
pyr <- as.data.frame(data[1:3,])
names(pyr) <- c("col")
pyr <- pyr %>%
  separate(col, into = c('col1', 'col2','col3'),
           sep = '\\s* \\s*') 


names(pyr) <- c("1","2","3")
pyr

# Instruction dataframe
df <- as.data.frame(data[5:8,])
names(df) <- c("a")
df <-   as.data.frame(df)

df <- df %>%
  separate(a, into = c('a', 'move_nb','b','from', 'c','to'),
           sep = '\\s* \\s*') %>% 
  select(move_nb,from,to)
df



## Get the index from where to get the stack
index_stack_to_move <- function(dataset,col_nb){
  cat("\nFunction \tindex_stack_to_move\n")
  
  # If column empty, error
  if (tail(dataset[,col_nb], n=1) == "") {
      return(print("ERROR"))
  }
  
  print( seq(1,length(dataset[,col_nb])))
  
  # Check all rows
  for (i in seq(1,length(dataset[,col_nb]))) {
    cat("\nIndex row",i)
    if (nchar(dataset[i,col_nb]) > 0) {
      cat("Index: ",i," \nof the 1st letter to move: ", dataset[i,col_nb])
      return(i)
    }
  }
}

## Get index where to paste the stack
index_stack_to_paste <- function(dataset, col_nb) {
  cat("\nFunction \n index_stack_to_paste\n")
  # IF the column is empty, then return last elemnt of the column
  if (tail(dataset[,col_nb], n=1) == "") {
    print("Empty column")
      return(length(dataset[,col_nb]))
  }
    
  for (i in seq(1, length(dataset[,col_nb ]))) {
    print(i)
    
    if (nchar(dataset[i, col_nb]) > 0) {
      cat("index where to put the copied stack", i - 1)
      return(i - 1)
    }
  }
}

## Move an item from par.origin to par.destination
update_pyr <- function(data,par.origin, par.destination){
  print("Update pyr")
  
  # Select the letter to move
  print(data)
  print(origin)
  stack_to_be_moved <- index_stack_to_move(data, par.origin)
  print("stack_to_be_moved")
  print(stack_to_be_moved)
  
  # Save/copy to stack
  print(data)
  temp_stack <- data[stack_to_be_moved,par.origin]
  cat("Temp varirable to be move" , temp_stack)

  # Remove the stack in the column origin
  data[stack_to_be_moved,par.origin] <- ""

  # Paste the temp_stack element in the destination column
  new_row <- index_stack_to_paste(data,par.destination)
  print(data)
  print(new_row)
  if (is.null(new_row)) {
    new_row <- length(data[,par.destination])
    cat("NEW ROW ADDED",new_row)
    print(new_row)
  }
  
  if (new_row == "0"){
    # Create new row in data
    first_line <- rep("",length(names(data)))
    data <- rbind(first_line,data)
    new_row <- 1
  }
  data[new_row,par.destination] <- temp_stack

  print(data)
  return(data)
}

exe <- function(data_pyr){
  for (i in seq(1,length(df[,1]))){
    print(df[i,])
    
    ## Set the instruction computationally
    nb_move <- as.numeric(df[i,1])
    origin <- as.numeric(df[i,2])
    destination <- as.numeric(df[i,3])
    
    
    # Do action in pyr
    print("Number of change per move")
    for (l in seq(1,nb_move)){
      
      data_pyr <- update_pyr(data = data_pyr, par.origin = origin, par.destination = destination)
      print(data_pyr)
    }
  }
  return(data_pyr)
}

df
a <- exe(data_pyr = pyr)
a





## Small test to check
update_pyr(pyr,2,3)
update_pyr(pyr,1,3)
test <- update_pyr(pyr,3,1)




