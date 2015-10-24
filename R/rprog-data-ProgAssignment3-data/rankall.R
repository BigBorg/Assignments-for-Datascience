##Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital rank-
##ing (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
##containing the hospital in each state that has the ranking specified in num. For example the function call
##rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
##are the best in their respective states for 30-day heart attack death rates. The function should return a value
##for every state (some may be NA). The first column in the data frame is named hospital, which contains
##the hospital name, and the second column is named state, which contains the 2-character abbreviation for
##the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
##hospitals when deciding the rankings.
##Handling ties. The rankall function should handle ties in the 30-day mortality rates in the same way
##that the rankhospital function handles ties.

getrank<-function(x,num){
	if(num=="best"){
		x[1]
	}else if(num=="worst"){
		x[length(x)]
	}else if(num>length(x)){
		"<NA>"
	}else{
		x[num]
	}
}

rankall <- function(outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	## Check that outcome are valid
	try(if(outcome!="heart attack"&&outcome!="heart failure"&&outcome!="pneumonia"){
			stop("invalid outcome")
		})
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name
	if(outcome=="heart attack")	outcome="Heart.Attack"
	if(outcome=="heart failure") outcome="Heart.Failure"
	if(outcome=="pneumonia") outcome="Pneumonia"
	##get col name
	colId<-paste("Hospital.30.Day.Death..Mortality..Rates.from",outcome,sep=".")
	data[,colId]<-as.numeric(data[,colId])
	simplifieddata<-cbind(data[,c("Hospital.Name","State")],data[,colId])
	completedcases<-complete.cases(simplifieddata)
	simplifieddata<-simplifieddata[completedcases,]
	simplifieddata<-simplifieddata[order(simplifieddata[,2],simplifieddata[,3],simplifieddata[,1]),]
	hospital<-tapply(simplifieddata[,1],simplifieddata[,2],getrank,num)
	state<-as.factor(simplifieddata[,2])
	state<-levels(state)
	data.frame(cbind(hospital,state))
}