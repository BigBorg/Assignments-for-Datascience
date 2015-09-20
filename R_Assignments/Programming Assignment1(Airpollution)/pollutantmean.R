pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    setwd(directory)
	data<-c()
	col_id<-if(pollutant=="sulfate"){
			2
		}else{
			3
		}
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
		data<-c(data,data_tmp[,col_id])
	}
	means<-mean(data,na.rm=TRUE)
	setwd("..")
	means
}