	#Download and unzip dataset
	if(!file.exists("course project1")){
		dir.create("course project1");
	}
	download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",destfile = "./course project1/dataset.zip");
	setwd("./course project1");
	unzip("dataset.zip");
	
	#Set language,load packages and process data
	mylocal<-Sys.getlocale("LC_ALL");
	Sys.setlocale("LC_ALL", "English");
	library(dplyr);
	library(lubridate);
	dataframe<-read.csv("household_power_consumption.txt",header=TRUE,nrow=69516,sep=";",stringsAsFactors=FALSE);
	tbldata<-tbl_df(dataframe);
	feb<-filter(tbldata,Date=="1/2/2007"|Date=="2/2/2007");
	feb<-mutate(feb,Date=as.Date(Date,"%d/%m/%Y"),Global_active_power=as.numeric(Global_active_power),Sub_metering_1=as.numeric(Sub_metering_1),Sub_metering_2=as.numeric(Sub_metering_2),Sub_metering_3=as.numeric(Sub_metering_3),Voltage=as.numeric(Voltage),Global_reactive_power=as.numeric(Global_reactive_power));
	xval<-ymd_hms(paste(feb$Date,feb$Time,sep=" "));
	
	#plot3
	png("plot3.png");
	plot(xval,feb$Sub_metering_1,type="l",xlab="",ylab="Energy sub metering");
	lines(xval,feb$Sub_metering_2,col="red");
	lines(xval,feb$Sub_metering_3,col="blue");
	legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=c(1,1,1),col=c("black","red","blue"));
	dev.off();
	
	#restore language
	Sys.setlocal("LC_ALL", mylocal);