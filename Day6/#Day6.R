library(dplyr)
library(tidyr)
library(stringr)

## Set repo
setwd("../Day6")
getwd()




#### Load data ####
path0 <- "input0.txt"
path1 <- "input1.txt"
path2 <- "input2.txt"
path3 <- "input3.txt"
path4 <- "input4.txt"

test(path0)


test <- function(path){
  
  data <- read.delim(path, header = FALSE)
  # data_imported <- as.data.frame(data)
  head(data)
  list(data)
  
  code <- as.character(data$V1[1])
  code 
  code <- unlist(strsplit(code, ""))
  
  
  
  c <- ""
  already_appeared_char <- c("")
  print(data)
  for (index_letter in seq(1,length(code))){
    cat("\n\n")
    print(code[index_letter])
    # if (c == "OUT" & (length(already_appeared_char) == 5)){
    #   print(index_letter)
    #   break
    # }
    
    if (length(already_appeared_char) == 5) {
      # Check if element is already in
      print("LENGTH == 5")
      list_size <- "FULL"
      
      if (!(code[index_letter] %in% already_appeared_char)) {
        # Element NOT in the list 
        print("DONE")
        print(index_letter-1)
        break
      }
      # else{
      #   
      # }
    }
    
    if (!(code[index_letter] %in% already_appeared_char)) {
      
      # Case the element is not in the list 
      if (length(already_appeared_char) == 5) {
        print("LENGTH == 5 - RETURN THE INDEX")
        print(index_letter-1)
        break
      }
      print("Element NOT in the list")
      already_appeared_char <- c(already_appeared_char,code[index_letter])
      print(already_appeared_char)
      print("Element added to list already_appeared_char ")
    }
    
    # Case the element to be added is already in the list
    else{
      print("if element ALREADY in already_appeared_char CHECK THE LENGTH OF LIST")
      if (length(already_appeared_char) == 5) {
        print("LIST FULL - print the element")
        print(code[index_letter])
      }
    }
    
    
    
  }
  
}

check_if_repeated <- function(liste_letters) {
  already_appeared_char <- c()
  
  for (i in seq(1, length(liste_letters))) {
    
    if (is.null(liste_letters[i])) {
      print("Lsite nulle")
    }
    
    else{
      if (!(liste_letters[i] %in% already_appeared_char)) {
        already_appeared_char <- c(already_appeared_char, liste_letters[i])
      }
      else{
        print("doublon")
        print(liste_letters[i])
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

exe <- function(path,nb_char_to_check){
  data <- read.delim(path, header = FALSE)
  code <- as.character(data$V1[1])
  code <- unlist(strsplit(code, ""))
  
  for (i in seq(1,length(code))){
    print(i)
    n_min <- i
    n_max <- i+nb_char_to_check-1
    sub_list <- code[n_min:n_max]
    
    # Check doublon
    unique_list <- check_if_repeated(sub_list)
    
    if (unique_list == TRUE) {
      print(code[i])
      return(i+nb_char_to_check-1)
    }
  }
}

#### Small test  ####
path0 <- "input0.txt"
path1 <- "input1.txt"
path2 <- "input2.txt"
path3 <- "input3.txt"
path4 <- "input4.txt"

exe(path0,4)
exe(path1,4)
exe(path2,4)
exe(path3,4)
exe(path4,4)

#### Real input  ####
path_real <- "input_real.txt"

exe(path_real,4)


#### PART 2 ####
exe(path0,14)
exe(path1,14)
exe(path2,14)
exe(path3,14)
exe(path4,14)
exe(path_real,14)

