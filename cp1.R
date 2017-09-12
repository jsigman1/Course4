knitr::opts_chunk$set(echo = TRUE, results = "asis")
 downloadURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
 downloadFile <- "./Data/household_power_consumption.zip"
 householdFile <- "./Data/household_power_consumption.txt"

t <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
 t$Date <- as.Date(t$Date, "%d/%m/%Y")
t <- subset(t,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
 t <- t[complete.cases(t),]
 dateTime <- paste(t$Date, t$Time)
 dateTime <- setNames(dateTime, "DateTime")
 t <- t[ ,!(names(t) %in% c("Date","Time"))]
 t <- cbind(dateTime, t)
 t$dateTime <- as.POSIXct(dateTime)

##Global Active Power Histogram
hist(t$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")

##Save file locally
dev.copy(png, "1plot.png", width=480, height=480)
dev.off()

##Plot 2
plot(t$Global_active_power~t$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

##Save file locally
dev.copy(png, "2plot.png", width=480, height=480)
dev.off()

##Plot 3
with(t, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

##Save file locally
dev.copy(png, "3plot.png", width=480, height=480)
dev.off()

##Plot 4, composite

 par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
 with(t, {
       plot(Global_active_power~dateTime, type="l", 
                      ylab="Global Active Power (kilowatts)", xlab="")
       plot(Voltage~dateTime, type="l", 
                       ylab="Voltage (volt)", xlab="")
       plot(Sub_metering_1~dateTime, type="l", 
                       ylab="Global Active Power (kilowatts)", xlab="")
       lines(Sub_metering_2~dateTime,col='Red')
       lines(Sub_metering_3~dateTime,col='Blue')
       legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
                           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
       plot(Global_reactive_power~dateTime, type="l", 
                      ylab="Global Rective Power (kilowatts)",xlab="")
   })

##Save file locally
dev.copy(png, "4plot.png", width=480, height=480)
dev.off()


