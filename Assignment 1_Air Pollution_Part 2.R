complete <- function(directory, id_Nos = 1:332) {
  
  #store the current working directorr in a vriable
  old.dir <- getwd()
  
  #go to the directory having data files
  setwd("~/Documents/DS/specdata")
  
  #store file names in a variable
  fl_names <- list.files(path = ".", pattern = ".csv")
  
  #creat a data frame for required files
  Com_Case <- data.frame(id = c(id_Nos), nobs=0)
  
  #difine some temporary variable
  x <- 0 
  y <- 0
  NOBS <- 0
  
  #store the required data in data frame as per the id
  for(i in 1:length(fl_names[id_Nos])) 
    {
    #load the csv file
      x <- read.csv(fl_names[id_Nos[i]])
    #remove the NA's
      y <- na.omit(x)
    #store the number of complete cases as the number of rows
      NOBS <- dim(y)[1]
    #store the value in the nobs column
      Com_Case$nobs[i] <- NOBS
    }
  #print the data frame
  print(Com_Case)
  
  #return to the original working directory
  setwd(old.dir)
  
}