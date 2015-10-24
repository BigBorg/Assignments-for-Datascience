complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
		
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
		setwd(directory)
		nobs<-c()
		for(i in id){
			if(i>99){
				filename<-as.character(i)
			}else if(i>9){
				filename<-"0"
				filename<-paste(filename,as.character(i),sep="")
			}else{
				filename<-"00"
				filename<-paste(filename,as.character(i),sep="")
			}
			data_tmp<-read.csv(paste(filename,"csv",sep="."))
			good<-complete.cases(data_tmp)
			nobs<-c(nobs,sum(good))
		}
		result<-data.frame(id=id,nobs=nobs)
		setwd("..")
		result
}