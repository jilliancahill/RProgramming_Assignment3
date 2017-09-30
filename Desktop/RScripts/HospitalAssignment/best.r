# Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should be sorted in alphabetical order and the first hospital in that set should be chosen (i.e. if hospitals “b”, “c”,and “f” are tied for best, then hospital “b” should be returned).

best<-function(state,outcome){
  #read outcome data
  outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #put variables into consistent format for validation
  state<-toupper(state)
  outcome<-tolower(outcome)
  
  #check that state and outcome valid
  state.check<-state %in% unique(outcome.data$State)
  outcome.check<-outcome %in% c("heart attack","heart failure","pneumonia")
  
  if(state.check==FALSE){
    print("State is invalid. Enter valid 2-character abbreviated name of State.")
  }
  if(outcome.check==FALSE){
    print("Outcome is invalid. Enter valid outcome: Heart Attack, Heart Failure, or Pneumonia.")
  }
  #if state and outcome are valid return the top hospital with lowest mortality rate for chosen state and outcome, sort alphabetically when there's a tie
  if(state.check==TRUE&outcome.check==TRUE){
    outcome.data<-outcome.data[outcome.data$State==state,c(2,7,11,17,23)]
    colnames(outcome.data)[3:5]<-c("heart attack","heart failure","pneumonia")
    min<-min(as.numeric(outcome.data[outcome][!outcome.data[outcome]=="Not Available"]))
    return(sort(outcome.data$Hospital.Name[outcome.data[outcome]==min])[1])
  }
  
}