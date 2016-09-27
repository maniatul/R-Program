pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  #store the current working directorr in a vriable
  old.dir <- getwd()
  
  #go to the directory having data files
  setwd("~/Documents/DS/specdata")
  
  #store file names in a variable
  fl_names <- list.files(path = ".", pattern = ".csv")
  
  #creat a data frame for required files
  dt_frm <- data.frame()
  
  #store the required data in data frame as per the id
  for(i in 1:length(fl_names[id])) 
    {
      dt_frm <- rbind(dt_frm, read.csv(fl_names[id[i]]))
    }
  
  # Print the output mean
  print(mean(dt_frm[, pollutant], na.rm = 1))
  
  #return to the original working directory
  setwd(old.dir)
  
}
