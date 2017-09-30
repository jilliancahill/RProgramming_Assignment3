
rankall<-function(outcome,num="best"){
  outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  #put variables into consistent format for validation
  outcome<-tolower(outcome)
  if(class(num)=="character") num<-tolower(num)
  
  #check if input variables are valid
  outcome.check<-outcome %in% c("heart attack","heart failure","pneumonia")
  num.check<-num %in% c("best","worst")||num%%1==0
  
  if(outcome.check==FALSE){
    print("Outcome is invalid. Enter valid outcome: Heart Attack, Heart Failure, or Pneumonia.")
  }
  if(num.check==FALSE){
    print("Rank is invalid. Enter valid rank: Best, Worst, or integer value.")
  }
  
  #if both inputs are valid, find the hospital for each state that matches the ranking provided for outcome provided
  if(outcome.check==TRUE&num.check==TRUE){
    outcome.data<-outcome.data[,c(7,2,11,17,23)]
    colnames(outcome.data)[3:5]<-c("heart attack","heart failure","pneumonia")
    
    outcome.data<-outcome.data[,c("State","Hospital.Name",outcome)]
    outcome.data<-outcome.data[outcome.data[,3]!="Not Available",]
    
    outcome.data[,3]<-as.numeric(outcome.data[,3])
    outcome.data$State<-as.factor(outcome.data$State)
    
    outcome.data.list<-by(outcome.data,outcome.data$State,function(x){x[order(x[,3],x[[2]]),]})
    outcome.data.list<-lapply(outcome.data.list,function(x){cbind(x,Rank=row(x)[,1])})
    outcome.data<-Reduce(rbind,outcome.data.list)
    
    addrows.list<-by(outcome.data,outcome.data$State,
                     function(x){
                       max<-max(x$Rank)+1
                       r<-c(max:227)
                       newrows<-data.frame(x$State[1],NA,NA,Rank=r)
                       colnames(newrows)<-colnames(x)
                       test<-rbind(x,newrows)
                     }
    )
    outcome.data<-Reduce(rbind,addrows.list)
    
    
    
    if(num=="best") num<-1
    if(num=="worst"){
      outcome.data2<-subset(outcome.data,!is.na(Hospital.Name))
      worst.list<-by(outcome.data2,outcome.data2$State,function(x){x[x[,4]==max(x[,4]),]})
      worst.df<-Reduce(rbind,worst.list)     
      colnames(worst.df)[1:2]<-c("state","hospital")
      return(worst.df[,c(1:2)])
      
    }else if(num<=max(outcome.data$Rank)){
      df<-outcome.data[outcome.data$Rank==num,c(1,2)]
      colnames(df)<-c("state","hospital")
      return(df)
    } else{
      return("NA")
    }
  }
}


