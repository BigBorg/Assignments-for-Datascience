	#Course Project
	##Reading subjects
	subject_test<-read.csv("./test/subject_test.txt",header=FALSE);
	subject_train<-read.csv("./train/subject_train.txt",header = FALSE);
	
	##Reading measurements and rename cols(Step 4)
	X_test<-read.csv("./test/X_test.txt",header=FALSE,sep="");
	X_train<-read.csv("./train/X_train.txt",header = FALSE,sep="");
	colNames<-read.csv("features.txt",sep=" ",header = FALSE);
	names(X_test)<-colNames[,2];
	names(X_train)<-colNames[,2];
	
	##Reading activities
	Y_test<-read.csv("./test/y_test.txt",header = FALSE);
	Y_train<-read.csv("./train/y_train.txt",header = FALSE);
	
	##Creating test and train frames
	testframe<-cbind(subject_test,Y_test,X_test);
	names(testframe)[1:2]<-c("subject","Y");
	trainframe<-cbind(subject_train,Y_train,X_train);
	names(trainframe)[1:2]<-c("subject","Y");
	
	##Merge frames(Step 1)
	mergeddata<-rbind(testframe,trainframe);
	
	##Extract means and standard deviations(Step 2)
	library(dplyr);
	extract <- mergeddata [, grep("mean()", names(mergeddata),fixed = TRUE)];
	extract [,34:66] <- mergeddata [, grep("std()", names(mergeddata), fixed = TRUE)];
	extract<-cbind(subject=mergeddata$subject,Y=mergeddata$Y,extract);
	
	##Name the activities(Step 3)
	labels<-read.csv("activity_labels.txt",header=FALSE,sep=" ");
	fac<-factor(extract$Y);
	levels(fac)<-as.character(labels[,2]);
	extract$Y<-fac;
	
	##Creates a second, independent tidy data set with the average of each variable for each activity and each subject(Step 5)
	library(reshape2);
	melted<-melt(extract,id=c("subject","Y"),measure.vars=names(extract)[-(1:2)]);
	table<-dcast(melted,subject+Y~variable,mean);
	
	##save the output
	library(data.table);
	write.table(table,file="outputdata.txt",row.names = FALSE);