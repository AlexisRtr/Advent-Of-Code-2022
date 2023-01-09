library(dplyr)
library(tidyr)
library(stringr)

## Set repo
setwd("Day3")
getwd()

## Create an index table for letter + priority value
letters_list <- as.data.frame(c(letters,LETTERS))
names(letters_list) <- c("letters")

## Load data
path <- "input.txt"
path <- "input_real.txt"
data <- read.delim(path, header = FALSE)
data <- as.data.frame(data)

## Cut in half the runcksack
data <- data %>%
  rowwise() %>%
  mutate(rucksacks_1 = str_sub(V1, 1, nchar(V1)/2),
         rucksacks_2 = str_sub(V1,  nchar(V1)/2+1, nchar(V1)))

## Compare 2 rucksacks
check_rucksacks <- function(rucksack1_to_check,rucksack2_to_check){
  for (r in rucksack2_to_check[[1]]){
    
    # For each bag, check the letters in common
    common_letter <- r[r %in% rucksack1_to_check[[1]]]
    
    # Check if common_letter != chacter(0)
    if (length(common_letter)>0){
      priority <- which(letters_list$letters == common_letter )
      # print(priority)
      break # Add only one priority
    }
  }
  return(priority)
}

## Run part 1

priority_total <- 0
for (i in seq(1,length(data$V1))){

  # Vector of 1 string --> vector of characters
  rucksacks_1_letters <- strsplit(data$rucksacks_1[i],"")
  rucksacks_2_letters<- strsplit(data$rucksacks_2[i],"")
  
  # Check the 2 rucksacks
  priority <- check_rucksacks(rucksack1_to_check = rucksacks_1_letters,rucksack2_to_check = rucksacks_2_letters)
  
  # Sum total priority
  priority_total <- priority + priority_total 
}

print(priority_total)