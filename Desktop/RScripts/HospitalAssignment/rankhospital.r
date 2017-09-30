
rankhospital<-function(state,outcome,num){
  #read outcome data
  outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #put variables into consistent format for validation
  state<-toupper(state)
  outcome<-tolower(outcome)
  if(class(num)=="character") num<-tolower(num)
  
  #check if input variables are valid
  state.check<-state %in% unique(outcome.data$State)
  outcome.check<-outcome %in% c("heart attack","heart failure","pneumonia")
  num.check<-num %in% c("best","worst")||num%%1==0
  
  if(state.check==FALSE){
    print("State is invalid. Enter valid 2-character abbreviated name of State.")
  }
  if(outcome.check==FALSE){
    print("Outcome is invalid. Enter valid outcome: Heart Attack, Heart Failure, or Pneumonia.")
  }
  if(num.check==FALSE){
    print("Rank is invalid. Enter valid rank: Best, Worst, or integer value.")
  }
  
  #if all inputs are valid, find the hospital corresponding to state, outcome, and rank provided
  if(state.check==TRUE & outcome.check==TRUE & num.check==TRUE){
    outcome.data<-outcome.data[outcome.data$State==state,c(2,7,11,17,23)]
    colnames(outcome.data)[3:5]<-c("heart attack","heart failure","pneumonia")
    outcome.data<-outcome.data[,c("Hospital.Name","State",outcome)]
    outcome.data<-outcome.data[outcome.data[,outcome]!="Not Available",]
    outcome.data[,outcome]<-as.numeric(outcome.data[,outcome])
    outcome.data<-outcome.data[order(outcome.data[,outcome],outcome.data$Hospital.Name),]
    outcome.data$rank<-row(outcome.data)[,1]
    
    if(num=="best") num<-1
    if(num=="worst") num<-max(outcome.data$rank)
    if(num<=max(outcome.data$rank)){
      return(outcome.data$Hospital.Name[outcome.data$rank==num])
    } else{
      return("NA")
    }
  }
}