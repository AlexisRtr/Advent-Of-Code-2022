library(dplyr)
library(tidyr)
library(stringr)

## Set repo
setwd("Day5")
getwd()


#### Load data ####
path <- "input.txt"
path <- "input_real.txt"
data <- read.delim(path, header = FALSE, ) # Modification done manually to add [.] on empty crates
data_imported <- as.data.frame(data)
head(data)


#### Separate in 2 dataframe ####

#### Pyramide ####
# Select only pyramide
pyr <- as.data.frame(data[1:9,])
names(pyr) <- c("col")

# Separate one column on X column
pyr <- pyr %>%
  separate(col, into = c('col1', 'col2','col3','col4','col5','col6','col7','col8','col9'),
           sep = '\\s* \\s*')
pyr[pyr == "[.]"] <- "" 
# rename properly
names(pyr) <- c("1","2","3","4","5","6","7","8","9")
# Keep the only interesting information
pyr <- pyr[1:8,]



#### Instruction dataframe ####
df <- as.data.frame(data[10:511,])
names(df) <- c("a")
df <-   as.data.frame(df)

# Separate in numerical values
df <- df %>%
  separate(a, into = c('a', 'move_nb','b','from', 'c','to'),
           sep = '\\s* \\s*') %>%
  select(move_nb,from,to)

#### Functions ####

#### Function to get the indexes of where to take and paste an object
## Get the index from where to get the stack
index_stack_to_move <- function(dataset,col_nb){
  # If column empty, error: NOT SURE how to tackle this case
  if (tail(dataset[,col_nb], n=1) == "") {
    print('ERROR')
    return(length(dataset[,col_nb]))
  }
  
  # Check all rows
  for (i in seq(1,length(dataset[,col_nb]))) {
    if (nchar(dataset[i,col_nb]) > 0) {
      return(i)
    }
  }
}

## Get index where to paste the stack
index_stack_to_paste <- function(dataset, col_nb) {
  # IF the column is empty, then return last elemnt of the column
  if (tail(dataset[,col_nb], n=1) == "") {
    print("Empty column")
    return(length(dataset[,col_nb]))
  }
  
  for (i in seq(1, length(dataset[,col_nb ]))) {
    if (nchar(dataset[i, col_nb]) > 0) {
      # cat("index where to put the copied stack", i - 1)
      return(i - 1)
    }
  }
}

#### Function update pyramide for each order ####
## Move an item from par.origin to par.destination

update_pyr <- function(data,par.origin, par.destination){
  
  # Select the letter to move
  stack_to_be_moved <- index_stack_to_move(data, par.origin)
  
  # Save/copy to stack
  temp_stack <- data[stack_to_be_moved,par.origin]
  
  # Remove the stack in the column origin
  data[stack_to_be_moved,par.origin] <- ""
  
  # Paste the temp_stack element in the destination column
  new_row <- index_stack_to_paste(data,par.destination)
  
  if (is.null(new_row)) {
    new_row <- length(data[,par.destination])
  }
  
  if (new_row == "0"){
    # Create new row in data
    first_line <- rep("",length(names(data)))
    data <- rbind(first_line,data)
    new_row <- 1
  }
  data[new_row,par.destination] <- temp_stack
  
  # print(data)
  return(data)
}


#### PART 1: Execute this update for the whole instruction ####

exe <- function(data_pyr){
  for (i in seq(1,length(df[,1]))){
    
    ## Set the instruction computationally
    nb_move <- as.numeric(df[i,1])
    origin <- as.numeric(df[i,2])
    destination <- as.numeric(df[i,3])
    
    # Check if nb_move > length total
    print(nb_move)
    if (nb_move > length(df[,1])) {
      print("NB_MPVE TOO LARGE")
      nb_move <- length(df[,1])
    }
    # Do action in pyr
    for (l in seq(1,nb_move)){
      data_pyr <- update_pyr(data = data_pyr, par.origin = origin, par.destination = destination)
    }
    # print(data_pyr)
  }
  return(data_pyr)
}

df

a <- exe(data_pyr = pyr)
a


#### Part 2 ####
update_pyr_all <- function(data,par.nb_move,par.origin, par.destination){
  
  # Select the letter to move
  first_index_to_move <- index_stack_to_move(data, par.origin)
  last_index_to_move <- first_index_to_move + par.nb_move -1 # gET THE OTHER LAST INDEX 
  
  # Save/copy to stack
  temp_stack <- data[first_index_to_move:last_index_to_move,par.origin] # SAVE EVERY CRATES TO SAVE AT ONCE
  
  # Remove the stack in the column origin
  data[first_index_to_move:last_index_to_move,par.origin] <- ""
  
  # Paste the temp_stack element in the destination column
  new_index_to_paste <- index_stack_to_paste(data,par.destination)
  new_index_to_paste_first <- new_index_to_paste - par.nb_move +1
  
  # IF THE SPACE TO PASTE THE TEMP CRATES IS NOT AVAILABLE --> ADD IT
  # ADD AS MANY LINES AS THERE ARE MISSING
  while (new_index_to_paste_first < 1) {
    first_line <- rep("",length(names(data)))
    # ADD LINES
    data <- rbind(first_line,data)
    
    # UPDATE INDEX
    new_index_to_paste <- index_stack_to_paste(data,par.destination)
    new_index_to_paste_first <- new_index_to_paste - par.nb_move +1
  }
  
  data[new_index_to_paste_first:new_index_to_paste,par.destination] <- temp_stack
  
  return(data)
}

update_pyr_all(data = pyr,par.nb_move = 8,par.origin = 7,par.destination = 4)

exe_part2 <- function(data_pyr){
  for (i in seq(1,length(df[,1]))){
    
    ## Set the instruction computationally
    nb_move <- as.numeric(df[i,1])
    origin <- as.numeric(df[i,2])
    destination <- as.numeric(df[i,3])
    
    # UPDATE PYR ONLY ONCE SINCE WE MOVE ALL CRATES AT ONCE
    data_pyr <- update_pyr_all(data = data_pyr,par.nb_move = nb_move, par.origin = origin, par.destination = destination)
  }
  return(data_pyr)
}


exe_part2(pyr)
