
library("dplyr")
library("lubridate")


download<-function(url, filename) {
        
        temp <- tempfile()
        download.file(url,temp, mode="wb")
        unzip(temp, filename)
        unlink(temp)
}


prepareDF<-function(filename) {
        ## read the table
        df <- read.table(filename, sep=";", na.strings ="?",
                         colClasses=c("character","character",rep("numeric",7)),header=T)
        
        
        ## combine the date and time into a class
        df <- mutate(df, DateTime=dmy_hms(paste(df$Date,df$Time)))
        
        ## subset to the desired dates - 2007-02-01 2007-02-02
        df<-filter(df,DateTime >= ymd_hms("2007-02-01 00:00:00"), DateTime < ymd_hms("2007-02-03 00:00:00"))
        
}


makePlot<-function(pngName) {
        
        
        ## plot 3
        with(electric.df,plot(x=c(DateTime,DateTime,DateTime), 
                              y=c(Sub_metering_1,Sub_metering_2,Sub_metering_3), 
                              xlab="", ylab="Energy sub metering", type="n"))
        
        with(electric.df,lines(x=DateTime, 
                               y=Sub_metering_1, col = "black"))
        
        with(electric.df,lines(x=DateTime, 
                               y=Sub_metering_2, col = "red" ))
        
        with(electric.df,lines(x=DateTime, 
                               y=Sub_metering_3, col = "blue"))
        
        legend("topright", 
               legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
               col=c("black","red","blue"), lty=c(1,1,1))
        
        dev.copy(png,pngName)
        dev.off()
}








##download("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","household_power_consumption.txt")

##electric.df<-prepareDF("household_power_consumption.txt")

makePlot("plot3.png")

