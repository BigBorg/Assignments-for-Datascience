corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
		com<-complete(directory)
		setwd(directory)
		com<-subset(com,nobs>threshold,c(1,2))
		cor<-c()
		for(i in com$id){
			if(i>99){
				filename<-as.character(i)
			}else if(i>9){
				filename<-"0"
				filename<-paste(filename,as.character(i),sep="")
			}else{
				filename<-"00"
				filename<-paste(filename,as.character(i),sep="")
			}
			data<-read.csv(paste(filename,"csv",sep="."))
			case<-complete.cases(data)
			data<-data[case,]
			tmp<-cor(data$sulfate,data$nitrate)
			cor<-c(cor,tmp)
		}
		setwd("..")
		cor
}