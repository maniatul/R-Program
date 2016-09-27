corr <- function(directory, threshold = 0) {
  
  #store the current working directorr in a vriable
  old.dir <- getwd()
  
  #go to the directory having data files
  setwd("~/Documents/DS/specdata")
  
  #take the value of threshhold in a variable
  n <- threshold
  #store file names in a variable
  fl_names <- list.files(path = ".", pattern = ".csv")
  
  
  #creat a data frame for required files
  CC <- data.frame()
  
  #difine some temporary variable
  y <- 0
  yrow <- 0
  z <- 0
  crltn <- 0
  
  #store the required data in data frame 
  for(i in 1:length(fl_names))
  {
    #load the csv file
    CC <- read.csv(fl_names[i])
    #remove the NA's
    y <- na.omit(CC)
    
    #check the number of complete cases as the number of rows
    yrow <- dim(y)[1]
   
    #check the required condition, zero is checked if there are no
    #complete cases.
    if (!yrow==0 & yrow > n)
      {
    #store the number of complete cases as the number of rows
        z <- cor(y$sulfate, y$nitrate)
    
    #store the value in correlation vector    
        crltn[i] <- z
        
    }
    #else store NA in the vector
        else crltn[i] <- NA
  }
    
  #remove the NAs
  crltn <- na.omit(crltn)

  #return to the original working directory
  setwd(old.dir)
  #return the desired vector. It is important to return in end as the 
  #function must return this vector
  crltn  
}