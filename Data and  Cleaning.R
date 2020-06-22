# Getting and Cleanning Data 


data <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", 
                   colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

data$Date <- as.Date(data$Date,"%d/%m/%Y")

data1 <- subset(data,Date>=as.Date("2007-02-01") & Date<=as.Date("2007-2-2"))
data1 <- data1[complete.cases(data1),]
dateTime <- paste(data1$Date,data1$Time)
dateTime <- setNames(dateTime,"DateTime")
data1 <- data1[ ,!(names(data1) %in% c("Date","Time"))]
data1 <- cbind(dateTime,data1)
data1$dateTime <- as.POSIXct(dateTime)
rownames(data1) <- NULL
