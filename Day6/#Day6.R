library(dplyr)
library(tidyr)
library(stringr)

## Set repo
setwd("../Day6")
getwd()

# Function to check if there is a repeated vector in a list
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

# Check the repeated in a list of nb_char_to_check
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
path0 <- "input/input0.txt"
path1 <- "input/input1.txt"
path2 <- "input/input2.txt"
path3 <- "input/input3.txt"
path4 <- "input/input4.txt"

exe(path0,4)
exe(path1,4)
exe(path2,4)
exe(path3,4)
exe(path4,4)

#### Real input  ####
path_real <- "input/input_real.txt"

exe(path_real,4)


#### PART 2 ####
exe(path0,14)
exe(path1,14)
exe(path2,14)
exe(path3,14)
exe(path4,14)
exe(path_real,14)

