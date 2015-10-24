best <- function(state, outcome) {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## Check that state and outcome are valid
	if(sum(data$State==state)<=0){
			stop("invalid state")
		}
	if(outcome!="heart attack"&&outcome!="heart failure"&&outcome!="pneumonia"){
		stop("invalid outcome")
	}

	## Return hospital name in that state with lowest 30-day death
	## rate
	if(outcome=="heart attack")	outcome="Heart.Attack"
	if(outcome=="heart failure") outcome="Heart.Failure"
	if(outcome=="pneumonia") outcome="Pneumonia"
	stateData<-subset(data,State==state)
	colId<-paste("Hospital.30.Day.Death..Mortality..Rates.from",outcome,sep=".")
	stateData[,colId]<-as.numeric(stateData[,colId])
	minMortalityRate<-min(stateData[,colId],na.rm=TRUE)
	bestHospital<-subset(stateData,stateData[,colId]==minMortalityRate)
	names<-sort(bestHospital[,"Hospital.Name"])
	names[1]
}