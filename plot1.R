
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
        
        ## plot 1
        with(electric.df, hist(Global_active_power,xlab="Global Active Power (kilowatts)",
                               main="Global Active Power",col="red"))
        
        dev.copy(png,pngName)
        dev.off()
        
}








download("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","household_power_consumption.txt")

electric.df<-prepareDF("household_power_consumption.txt")

makePlot("plot1.png")

