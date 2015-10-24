##a function called rankhospital takes three arguments: the 2-character abbreviated name of a
##state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
##The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
##of the hospital that has the ranking specified by the num argument. For example, the call
##	rankhospital("MD", "heart failure", 5)
##would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
##for heart failure. The num argument can take values ""best", "worst", or an integer indicating the ranking
##(smaller numbers are better). If the number given by num is larger than the number of hospitals in that
##state, then the function should return NA. Hospitals that do not have data on a particular outcome should
##be excluded from the set of hospitals when deciding the rankings.
rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	## Check that state and outcome are valid
	if(sum(data$State==state)<=0){
			stop("invalid state")
		}
	if(outcome!="heart attack"&&outcome!="heart failure"&&outcome!="pneumonia"){
		stop("invalid outcome")
	}
	
	## Return hospital name in that state with the given rank
	## 30-day death rate
	if(outcome=="heart attack")	outcome="Heart.Attack"
	if(outcome=="heart failure") outcome="Heart.Failure"
	if(outcome=="pneumonia") outcome="Pneumonia"
	
	## subset state data
	stateData<-subset(data,State==state)
	##get col name
	colId<-paste("Hospital.30.Day.Death..Mortality..Rates.from",outcome,sep=".")
	##convert col type form character to numeric
	stateData[,colId]<-as.numeric(stateData[,colId])
	##simplify frame
	stateData<-cbind(stateData[,c(1,2)],stateData[,colId])
	##get rid of NAs
	completedcases<-complete.cases(stateData)
	stateData<-stateData[completedcases,]
	##rank hospitals
	sorted<-stateData[order(stateData[,3],stateData[,2]),]
	##get result
	if(num=="best"){
		sorted[1,2]
	}else if(num=="worst"){
		sorted[nrow(sorted),2]
	}else if(num>nrow(sorted)){
		NA
	}else{
		sorted[num,2]
	}
}