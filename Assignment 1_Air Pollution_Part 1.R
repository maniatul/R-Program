pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  old.dir <- getwd()
  setwd("~/Documents/DS/specdata")
  
  
  #store file names in a variable
  fl_names <- list.files(path = ".", pattern = ".csv")
  
  #creat data frame of given files
  dt_frm <- data.frame()
  for(i in 1:length(fl_names[id])) {
  dt_frm <- rbind(dt_frm, read.csv(fl_names[id[i]]))
  }
  head(dt_frm)
  print(mean(dt_frm[, pollutant], na.rm = 1))
  
  setwd(old.dir)
  

  

}



