####  Campus Supervisor Error Sorter ####
# Important comments are indicated by ' # NOTE:'

# install.packages("readr") # NOTE: The first time you run the program, remove the pound sign before the word install on this line in order to install the readr package. But you only have to do this on the first time you use this program. After the package finishes installing, add the pound sign back so that the package doesn't re-install the next time you run the program.

#----------------Read data into R:-----------------------------

library(readr)
setwd("C:/location") # NOTE: Replace the part in quotes with the path to the folder containing your file. You can also use the shortcut Ctrl + Shift + H to select the working directory manually.)

original <- read_delim("Campus.txt", delim = "\n", escape_double = FALSE, col_names = FALSE) # NOTE: Replace "Campus.txt" with name of source file
Campus_new <- original
# Data frame in original format is called original, Campus_new will be the new formatted version


start <- Sys.time()

#------------------Adding empty Date, Message, Details columns and rename column 1:----------------


Campus_new$Date <- rep(NA, nrow(Campus_new))
Campus_new$Message <- rep(NA, nrow(Campus_new))
Campus_new$Detail <- rep(NA, nrow(Campus_new))
colnames(Campus_new)[colnames(Campus_new)== "X1"] <- "Err"

#----------------- Define function that tests if a row in the data starts with WARNING, INFO, SEVERE, or Unable------------

is_Err <- function(a){
  if (startsWith(a, "WARNING")==T |
      startsWith(a, "INFO")==T |
      startsWith(a, "SEVERE")==T ) {
    out <- TRUE
  }
  else {
    out <- FALSE
  }
  out
}


is_other <- function(a){
  if (startsWith(a, "Unable")==T){
    out <- TRUE
  }
  else {
    out <- FALSE
  }
  out
}


#---------------- Begin loop to associate each Detail portion with the  Err (SEVERE, INFO, or WARNING) -------------------

row <- nrow(Campus_new)
for (i in row:2) {
  if (((is_Err(Campus_new[[i,1]])==T) | (is_other(Campus_new[[i,1]])==T))==F) {
    Campus_new[i-1,"Detail"] <- paste(Campus_new[i,"Err"], Campus_new[i,"Detail"], sep = " , ")
    Campus_new <- Campus_new[-c(i),]
  }
}


#---------------- Separate Err and Time --------------------

for (i in 1:nrow(Campus_new)) {
  if (is_Err(Campus_new[[i,"Err"]])) {
    first <- Campus_new[[i,"Err"]]
    sepd <- unlist(strsplit(first, "[\\[\\]]", perl=T))
    Campus_new[i, "Err"] <- sepd[1]
    Campus_new[i, "Date"] <- sepd[2]
    Campus_new[i, "Message"] <- toString(sepd[4:length(sepd)])
  }
}

Campus_new_unique <- Campus_new[!duplicated(Campus_new[,3:4]),] #duplicates removed

# Find computation time
end <- Sys.time()
end-start #run time


#------------------ Export newly formatted file to csv ------------

write_csv(Campus_new_unique, "C://location.csv") 
# NOTE: replace the line above with the file path for wherever you want to keep your file


