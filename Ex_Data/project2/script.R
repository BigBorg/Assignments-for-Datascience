if(!file.exists("projects")){
	dir.create("project2");
	setwd("./project2");
	download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",destfile="Data.zip");
	unzip("Data.zip");
}

NEI <- readRDS("summarySCC_PM25.rds");
SCC <- readRDS("Source_Classification_Code.rds");

library(dplyr);
library(ggplot2);

#question 1/plot 1 : Yes, the Emissions are decreasing according to the plot.
groupedNEI<-group_by(NEI,year);
sumEmissions<-summarize(groupedNEI,sum(Emissions));
png("plot1.png");
plot(sumEmissions$year,sumEmissions$`sum(Emissions)`,xlab = "year",ylab="Emissions",main="Emissions Trend(USA)",type="l");
dev.off();

#question 2/plot 2: Decreasing
baltimore<-filter(groupedNEI,fips=="24510");	##This line of code might take some time, please be patient...
sumEmissionsbal<-summarize(baltimore,sum(Emissions));
png("plot2.png");
plot(sumEmissionsbal$year,sumEmissionsbal$`sum(Emissions)`,xlab = "year",ylab="Emissions",main="Emission Trend(Baltimore)",type="l");
dev.off();

#question 3/plot 3: Emission of type "POINT" is increasing, other types decreasing.
png("plot3.png");
sumbaltype<-summarize(group_by(baltimore,year,type),Emissions=sum(Emissions));
p<-ggplot(sumbaltype,aes(year,Emissions));
p<-p+facet_grid(.~type) + labs(title="Emission Trend(type,Baltimore)") + geom_point() + geom_smooth(method="lm");
print(p);
dev.off();

#question 4/plot 4: Decreasing
png("plot4.png");
coal<-grep("^Fuel (.*) Coal",SCC$EI.Sector);
filt<-SCC[coal,1];
coalData<-filter(NEI,SCC %in% filt);
groupedcoalData<-group_by(coalData,year);
sumcoal<-summarize(groupedcoalData,Emissions=sum(Emissions));
q<-ggplot(data=sumcoal,aes(year,Emissions)) + geom_point() + geom_line() +geom_smooth(method="lm") + labs(x="year",y="Total Emissions") + labs(title="Emission Trend(coal,US)");
print(q);
dev.off();

#question 5/plot 5: Decreasing
png("plot5.png");
motorbal<-filter(baltimore,type=="ON-ROAD");
groupedmotorbal<-group_by(motorbal,year);
summotorbal<-summarize(groupedmotorbal,Emissions=sum(Emissions));
q<-ggplot(data = summotorbal,aes(year,Emissions)) + geom_point() + geom_line() + geom_smooth(method="lm") + labs(title="Emission Trend(motor vehicle,Baltimore)");
print(q);
dev.off();

#question 6/plot 6: 
png("plot6.png");
ballos<-filter(NEI,fips=="24510"|fips=="06037");
sumballos<-summarize(group_by(ballos,fips,year),Emissions=sum(Emissions));
q<-ggplot(data=sumballos,aes(year,Emissions)) + geom_point(aes(color=factor(fips,labels=c("Los Angeles","Baltimore")))) + geom_line( aes(color=factor(fips,labels = c("Los Angeles","Baltimore")))) + labs(color="City") +labs(title="Emission Trend(motor vehicle,Los Angeles & Baltimore)");
print(q);
dev.off();
