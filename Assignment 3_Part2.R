best <- function(state, outcome) {
  
#store the current working directorr in a vriable
  old.dir <- getwd()
  
#go to the directory having data files
  setwd("C:/Users/Atul/Documents/DS/Hospital care")
  
  
#take avariable to change the required column names as original names are
#too long. it is optional only if you want to take a look at the data.
  ot <- c("heart attack", "heart failure", "pneumonia")
  
#read the csv file into a data frame
  dt_frm <- read.csv("outcome-of-care-measures.csv")
#subset the data frame for desired variables only
  dt_frm <- dt_frm[, c(2, 7, 11, 17, 23)]

#subset again according to the state to be searched for best hospital 
  dt_frm <- subset(dt_frm, dt_frm[,2]==state)

#change the column names(optional)
  colnames(dt_frm)[1:5] <- c("name", "state", "heart attack", "heart failure", "pneumonia")
  
#print the error messages if wrong arguments are passed
  if (!is.element(state, dt_frm$state)) {stop("invalid state")}
  if (!is.element(outcome, ot)) {stop("invalid outcome")}

#calculate according to the outcome argument
  if (outcome=="heart attack")
  {
    #sort according to the hospital name and outcome
      sorted_df <- dt_frm[order(as.numeric(as.vector(dt_frm[,3])), 
                                as.character(as.vector(dt_frm[,1]))), ]

    #print the result
      print(as.character(sorted_df$name[1]))
  }
  
  if (outcome=="heart failure")
  {
    #sort according to the hospital name and outcome
    sorted_df <- dt_frm[order(as.numeric(as.vector(dt_frm[,4])), 
                              as.character(as.vector(dt_frm[,1]))), ]
    
    #print the result
      print(as.character(sorted_df$name[1]))
  }
  
  if (outcome=="pneumonia")
  {
    #sort according to the hospital name and outcome
    sorted_df <- dt_frm[order(as.numeric(as.vector(dt_frm[,5])), 
                              as.character(as.vector(dt_frm[,1]))), ]
    
    #print the result
      print(as.character(sorted_df$name[1]))
  }

  #return to the original working directory
  setwd(old.dir)
  
}