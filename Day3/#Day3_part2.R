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
path <- "input_real_part2.txt"
data <- read.delim(path, header = FALSE)
data <- as.data.frame(data)
names(data) <- c("rucksacks")


## Compare 2 rucksacks
check_rucksacks_by_3 <- function(rucksack1_to_check,rucksack2_to_check,rucksack3_to_check){
  for (r2 in rucksack2_to_check[[1]]){

    # For each bag, check the letters in common
    common_letter_in_1_2 <- r2[r2 %in% rucksack1_to_check[[1]]]
    
    # Check if common_letter_in_1_2 != chacter(0)
    if (length(common_letter_in_1_2)>0){
      
      # print("common_letter_in_1_2")
      # print(common_letter_in_1_2)
      
      # Test if common_letter_in_1_2 is in 3
      check_common_letter_in_3 <- common_letter_in_1_2[common_letter_in_1_2 %in% rucksacks_3_letters[[1]]]
      
      # Check if common_letter_in_2_3 != chacter(0)
      if (length(check_common_letter_in_3) >0) {
        
        # print("check_common_letter_in_3")
        # print(check_common_letter_in_3)
        return(check_common_letter_in_3)
      } 
    }
  }
}


# Run part 2
priority_total <- 0
for (i in seq(1,length(data$rucksacks),3)){
  print("i")
  print(i)

  ## Extract 3 different Vectors of char
  rucksacks_1_letters <- strsplit(data$rucksacks[i],"")
  print(rucksacks_1_letters)
  rucksacks_2_letters<- strsplit(data$rucksacks[i+1],"")
  print(rucksacks_2_letters)
  rucksacks_3_letters<- strsplit(data$rucksacks[i+2],"")
  print(rucksacks_3_letters)
  
  # Check the 3 rucksacks
  common_letter <- check_rucksacks_by_3(rucksack1_to_check = rucksacks_1_letters,rucksack2_to_check = rucksacks_2_letters, rucksack3_to_check = rucksacks_3_letters)
  
  # Get priority number
  priority <- which(letters_list$letters == common_letter )
  # print(priority)
  
  priority_total <- priority + priority_total 
  # print(priority_total)
}
print(priority_total)







