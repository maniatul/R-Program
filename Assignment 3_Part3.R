rankhospital <- function(state, outcome, num = "best") {
  
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
  
  #change the column names(optional)
  colnames(dt_frm)[1:5] <- c("Hname", "Sname", "heart attack", 
                             "heart failure", "pneumonia")
 
  #subset again according to the outcome
  OC <- match(outcome, colnames(dt_frm))
  
  dt_frm <- dt_frm[, c(1,2,OC)]

  #subset again according to the state to be searched for best hospital
  dt_frm <- subset(dt_frm, dt_frm[,2]==state)
  
  #convert the not awailable in outcome variable to NA
  dt_frm[, 3] <- as.numeric(as.vector(dt_frm[, 3]))
  
  #Get the NA out of picture
  dt_frm <- na.omit(dt_frm)
  
  #give the num values according to the best and worst argument
  if (num=="best") num <- 1
  if (num=="worst") num <- dim(dt_frm)[1]
  
  #print the error messages if wrong arguments are passed
  if (!is.element(state, dt_frm$Sname)) {stop("invalid state")}
  if (!is.element(outcome, ot)) {stop("invalid outcome")}
  if (num > dim(dt_frm)[1]) return(NA)
  
  #sort according to the hospital name and outcome
      sorted_df <- dt_frm[order(as.numeric(as.vector(dt_frm[,3])), 
                                as.character(as.vector(dt_frm[,1])) ), ]
  #print the result
      print(as.character(sorted_df$Hname[num]))
 
  #return to the original working directory
  setwd(old.dir)
  
}