
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
        
        ## plot 4 - multi
        png(file=pngName,width=480,height=480)
        par(mfcol = c(2,2))
        
        
        ## first panel
        with(electric.df, {
                plot(x=DateTime, y=Global_active_power, xlab="", 
                     ylab="Global Active Power", type="n")
                lines(x=DateTime, y=Global_active_power)
        })
        
        
        
        
        
        ## second panel
        with(electric.df, {
                plot(x=c(DateTime,DateTime,DateTime), 
                     y=c(Sub_metering_1,Sub_metering_2,Sub_metering_3), 
                     xlab="", ylab="Energy sub metering", type="n")
                
                lines(x=DateTime, 
                      y=Sub_metering_1, col = "black")
                
                lines(x=DateTime, 
                      y=Sub_metering_2, col = "red" )
                
                lines(x=DateTime, 
                      y=Sub_metering_3, col = "blue")
                
                legend("topright", 
                       legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
                       col=c("black","red","blue"), lty=c(1,1,1), bty = "n")
        })
        
        
        ## third panel
        with(electric.df, {
                plot(x=DateTime, 
                     y=Voltage, 
                     xlab="datetime", ylab="Voltage", type="n")
                lines(x=DateTime, 
                      y=Voltage, col = "black")
                

        })
        
        
        ## fourth panel
        with(electric.df, {
                plot(x=DateTime, 
                     y=Global_reactive_power, 
                     xlab="datetime", type="n")
                lines(x=DateTime, 
                      y=Global_reactive_power, col = "black")
                
        })
        
        
        dev.off()
}








download("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","household_power_consumption.txt")

electric.df<-prepareDF("household_power_consumption.txt")

makePlot("plot4.png")

