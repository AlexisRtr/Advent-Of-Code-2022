library(dplyr)
library(tidyr)
library(stringr)

## Set repo
setwd("Day4")
getwd()


#### LOAD DATA ####
# path <- "input.txt"
path <- "input_part1.txt"
data <- read.delim(path, header = FALSE)
data_imported <- as.data.frame(data)
head(data)


# Preprocessing: df with 4 col (2 elf, 2 tasks)
preprocessing_data <- function(data.frame){
  data <- data.frame %>% separate(V1, c("Elf_1", "Elf_2"), sep = ",")
  data <- data %>% 
    separate(Elf_1, c("elf_1_task_1", "elf_1_task_2"), sep = "-") %>% 
    separate(Elf_2, c("elf_2_task_1", "elf_2_task_2"), sep = "-")
  
  data <-  lapply(data,as.numeric)
  data <-  as.data.frame(data)
  head(data)
  
  print(sapply(data,class))
  print(head(data))
  return(data)
}

# Reproduce the scheme to visualize the containment of tasks
add_line <- function(line_df,max_df){
  task <- ""
  
  for (i in seq(1,max_df)){
    if (line_df[1,1] <= i & i <= line_df[1,2]){
      task <- c(task,i)
    }
    else{
      if (i >= 10) {
        task <- c(task,"..")
      }
      else{
        task <- c(task,".")
      }
      
    }
  }
  return(paste(task, collapse = ''))
}

#### PART1: Check all the assignment contained ####
check_assignement <- function(data.frame){
  c <- 0
  data.frame <- as.data.frame(data.frame)
  # print(data.frame)
  
  # Check if the first assignement 1 of elf 1 is IN or OUT
  for (i in seq(1,length(data.frame$elf_1_task_1))){
    # print(i)
    # print(data.frame[i,])
    # print(add_line(data.frame[i,1:2],99))
    # print(add_line(data.frame[i,3:4],99))
    contained <- 0
    # Check if assignement elf 1 in elf 2
    if ((data.frame$elf_1_task_1[i] >= data.frame$elf_2_task_1[i]) & (data.frame$elf_1_task_2[i] <= data.frame$elf_2_task_2[i])){
      # print("ELF 1 assignement contained by the ones of ELF 2")
      contained <- 1 
      data.frame$Contained[i] <- "1"
      
    }
    
    # Check if assignement elf 2 in elf 1
    if ((data.frame$elf_1_task_1[i] <= data.frame$elf_2_task_1[i]) & (data.frame$elf_1_task_2[i] >= data.frame$elf_2_task_2[i])){
      # print("ELF 2 assignement contained by the ones of ELF 1")
      contained <- 1 
    }
    
    if (contained == 1) {
      # ADD THIS LINE (avoid double add when task elf1 == task elf2)
      c <- c + 1
    }
    }
  return(print(c))
}

df <- preprocessing_data(data.frame = data_imported)
check_assignement(data.frame = df)


#### PART2: Check all the assignment  without overlap ####
check_assignement_with_overlap <- function(data.frame){
  c <- 0
  data.frame <- as.data.frame(data.frame)
  # print(data.frame)
  
  # Check if the first assignement 1 of elf 1 is IN or OUT
  for (i in seq(1,length(data.frame$elf_1_task_1))){
    # print(i)
    # print(data.frame[i,])
    # print(add_line(data.frame[i,1:2],99))
    # print(add_line(data.frame[i,3:4],99))
    not_contained <- 0
    
    # ELF 1 overlap ELF 2 on the left
    if ((data.frame$elf_1_task_1[i] > data.frame$elf_2_task_2[i]) ){
      not_contained <- 1
    }
    # ELF 1 overlap ELF 2 on the right
    if ((data.frame$elf_1_task_2[i] < data.frame$elf_2_task_1[i])){
      not_contained <- 1 
    }
    
    if (not_contained == 1) {
      # ADD THIS LINE (avoid double add when task elf1 == task elf2)
      c <- c + 1
    }
  }
  return(1000-print(c))
}

df <- preprocessing_data(data.frame = data_imported)
a <- check_assignement_with_overlap(df)

