# https://adventofcode.com/2022/day/1


####### PART 1 ######
## Installing and loading package

install.packages("xlsx")

library(xlsx)
library(dplyr)

## To add the Elf number for each calorie transported
add_number_elf <- function() {
  c <-  1
  for (i in seq(1, length(data$X1))) {
    # Assign the first elf to the first calories transported
    data$Elf_number[i] <- c
    
    # Update elf number for each blank
    if (data$X1[i] == "") {
      c <- c + 1 # Update the number
      data$Elf_number[i] <- "" # Blank space
      
    }
  }
  data$X1 <- as.numeric(data$X1)
  return(data)
}

## To calculate the max calories transported by each ELF
max_calorie <- function(){
  data <- add_number_elf()
  
  data$X1 <- as.numeric(data$X1)
  data_summarized <- data %>%
    group_by(Elf_number) %>%
    summarize(sum_ = sum(X1)) %>% 
    arrange(desc(sum_))
  
  print(data_summarized)
  return(data_summarized)
}

## Uploading data
# data <- read.xlsx2("Day1/input_test.xlsx", sheetName = "Feuil1", header = FALSE)
data <- read.xlsx2("Day1/input.xlsx", sheetName = "Feuil2", header = FALSE)

## Preprocessing
data['Elf_number'] <- "" # Add Elf number for each calories transported

## Return the max calories with the function
max_calorie()[1,]

####### PART 2 ######
data_elf <- as.data.frame(max_calorie())


print("The top 3 Elf is carrying a total calories of\n")
print(sum(data_elf$sum_[1:3]))
