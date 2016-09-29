rankall <- function(outcome, num = "best") {
  
  #store the current working directorr in a vriable
  old.dir <- getwd()
  
  #go to the directory having data files
  setwd("C:/Users/Atul/Documents/DS/Hospital care")
  
  #take avariable to change the required column names as original names are
  #too long. it is optional only if you want to take a look at the data.
  ot <- c("heart attack", "heart failure", "pneumonia")
  
  #give the num values according to the best num argument
  if (num=="best") num <- 1
  
  #read the csv file into a data frame
  dt_frm <- read.csv("outcome-of-care-measures.csv")
  #subset the data frame for desired variables only
  dt_frm <- dt_frm[, c(2, 7, 11, 17, 23)]
  
  #change the column names(optional)
  colnames(dt_frm)[1:5] <- c("Hname", "Sname", "heart attack", 
                             "heart failure", "pneumonia")
  
  #subset again according to the outcome
  OC <- match(outcome, colnames(dt_frm))
  dt_frm <- dt_frm[, c(1,2,OC)]
  
  #convert the not awailable in outcome variable to NA
  dt_frm[, 3] <- as.numeric(as.vector(dt_frm[, 3]))
  
  #Get the NA out of picture
  dt_frm <- na.omit(dt_frm)

  #state name vector
  stName <- unique(as.character(dt_frm$Sname))

  #sort according to the hospital name and outcome
  dt_frm <- dt_frm[order(as.numeric(as.vector(dt_frm[,3])), 
                        as.character(as.vector(dt_frm[,1]))), ]
  
  #define a new variable to select the desired rows of data frame
  Rnum <- 0
  
if (num=="worst")
  {
    for (i in 1:length(stName))
    {
      stateworst <- 0
      stateworst <- which(dt_frm[,2] %in% stName[i])
      Rnum[i] <- stateworst[length(stateworst)]
    }
  }

else
  {
    for (i in 1:length(stName)) 
    {
      statePositions <- 0
      statePositions <- which(dt_frm[,2] %in% stName[i])
  
      if (num > length(statePositions))
      {
        Rnum[i] <- statePositions[1]
        dt_frm[statePositions[1], 1] <- NA
      }
      else Rnum[i] <- statePositions[num]
  
    }
  }

  #return to the original working directory
  setwd(old.dir)
  
  #output data frame
    ouput <- dt_frm[Rnum, c(1,2)]
    ouput <- ouput[order(as.character(as.vector(ouput[,2]))),]
  
}