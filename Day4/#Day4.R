library(dplyr)
library(tidyr)
library(stringr)

## Set repo
setwd("Day4")
getwd()


## Load data
# path <- "input.txt"
path <- "input_part1.txt"
data <- read.delim(path, header = FALSE)
data_imported <- as.data.frame(data)
head(data)



## Preprocessing 

# Get 2 different rows
data <- data_imported %>% separate(V1, c("Elf_1", "Elf_2"), sep = ",")
data <- data %>% 
  separate(Elf_1, c("elf_1_task_1", "elf_1_task_2"), sep = "-") %>% 
  separate(Elf_2, c("elf_2_task_1", "elf_2_task_2"), sep = "-")

data <-  lapply(data,as.numeric)
data <-  as.data.frame(data)
head(data)

sapply(data,class)


# data <- data %>%
#   mutate(elf_1_task_1 = str_sub(V1,1,1),
#          elf_1_task_2 = str_sub(V1,3,3),
#          elf_2_task_1 = str_sub(V1,5,5),
#          elf_2_task_2 = str_sub(V1,7,7),)
# 
# data


# Check if elf 1 has its assignment contain in elf 2 assignement





check_assignement <- function(){
  
  c <- 0
  
  # Check if the first assignement 1 of elf 1 is IN or OUT
  for (i in seq(1,length(data$elf_1_task_1))){
    
    
    # Check if assignement elf 1 in elf 2
    if ((data$elf_1_task_1[i] >= data$elf_2_task_1[i]) & (data$elf_1_task_2[i] <= data$elf_2_task_2[i])){
      print(i)
      print("ELF 1 assignement contained by the ones of ELF 2")
      c <- c + 1
      print(data[i,])
      print(data_imported$V1[i])
      data$Contained[i] <<- "1"
      
      if (data$elf_1_task_1[i] >= data$elf_2_task_1[i]) {
        data$Check_Task_1[i] <<- "1"
      }
      if (data$elf_1_task_2[i] <= data$elf_2_task_2[i]) {
        data$Check_Task_2[i] <<- "1"
      }
    }
    
    # Check if assignement elf 2 in elf 1
    if ((data$elf_1_task_1[i] <= data$elf_2_task_1[i]) & (data$elf_1_task_2[i] >= data$elf_2_task_2[i])){
      print(i)
      print("ELF 2 assignement contained by the ones of ELF 1")
      c <- c + 1
      print(data_imported$V1[i])
      data$Contained[i] <<- "1"
    }
  }
  return(print(c))
}

data['Check_Task_1'] <- ""
data['Check_Task_2'] <- ""
data['Contained'] <-  ""

check_assignement()

