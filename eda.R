#data loading and preparation section

power<-read.table("power.txt", sep=";", header = TRUE, na.strings = "?" , colClasses = c("character","character",
                "numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

power$Date<-as.Date(power$Date,"%d/%m/%Y")

data<-subset(power, Date>=as.Date("2007-02-01") & Date<=as.Date("2007-02-02"))

Datetime<-paste(data$Date,data$Time)

data<-cbind(data,Datetime)
data$Datetime <- as.POSIXct(Datetime)


data<-data[,3:ncol(data)]
#-------------------- Data Visualisation--------------------------

#---------------Plot 1---------------------

par(mfrow=c(1,1), mar=c(2,2,2,2))
# Plot 1
hist(data$Global_active_power, col="red", border = "black", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")

dev.copy(png,"plot1.png", width=480, height=480)
dev.off()

#---------------Plot 2---------------------

plot(data$Global_active_power~data$Datetime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

dev.copy(png,"plot2.png", width=480, height=480)
dev.off()

#---------------Plot 3---------------------

with(data,{plot(Datetime,Sub_metering_1,xlab ="Datetime", type="l",col="black", ylab = "Energy sub meteric", xlab="")
lines(Datetime,Sub_metering_2,col="red")
lines(Datetime,Sub_metering_3,col="blue")
})

legend("topright", col = c("black","red","blue"), lwd=c(1,1,1), c("Sub_metering_1","Sub_metering_2","Sub_metering_2"))
dev.copy(png,"plot3.png", width=480, height=480)
dev.off()


#---------------Plot 4---------------------

par(mfrow=c(2,2), mar=c(1,1,1,1))
plot(data$Datetime,data$Global_active_power, col="black", type="l", xlab = "", ylab = "Global Active Power")
plot(data$Datetime, data$Voltage, col="black",type="l", xlab = "datetime", ylab = "Voltage")

with(data,{
  
  plot(Datetime,Sub_metering_1, col="black", type="l", xlab = "", ylab = "Energy sub metering")
  lines(Datetime, Sub_metering_2, col="red", type="l")
  lines(Datetime, Sub_metering_3, col="blue", type="l")
  
})
legend("topright", col=c("black", "red", "blue"),lty=1,bty="n", lwd=c(2,2,2),c ("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

plot(data$Datetime,data$Global_reactive_power, type="l", xlab = "datetime", ylab = "Global Reactive Power")

dev.copy(png,"plot4.png", width=480, height=480)
dev.off()