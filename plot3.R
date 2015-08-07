fillDat <- function(url=
                      "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip") {
  if (!require(data.table)) {
    library(data.table)
  }
  if (!exists("dat")) {
    datFile = "dat.txt"
    if (!file.exists(datFile)) {
      datZip <- "dat.zip"
      if (!file.exists(datZip)) {
        download.file(url, datZip, mode="wb")
      }
      datFile <- unzip(datZip, unzip(datZip, list=T)[1,1])
    }
    dat <<- fread(datFile, na.strings='?')
    dat <<- dat[dat$Date=="1/2/2007" | dat$Date=="2/2/2007"]
  }
  
  dat$Global_active_power <- as.numeric(dat$Global_active_power)
  #as.Date(strptime(paste(dat[1, 1, with=F], dat[1, 2, with=F]), format="%d/%m/%Y %S:%M:%H"))
  #dat <<- dat[, dateTime:=as.Date(strptime(paste(Date, Time), format="%d/%m/%Y %S:%M:%H"))]
  dat <<- dat[, dateTime:=as.POSIXct(strptime(paste(Date, Time), format="%d/%m/%Y %H:%M:%S"))]
}

plot3 <- function() {
  fillDat()
  with(dat, plot(dateTime, Sub_metering_1, ylab="Energy sub metering", xlab="", type="n"))
  with(dat, lines(dateTime, Sub_metering_1))
  with(dat, lines(dateTime, Sub_metering_2, col="red"))
  with(dat, lines(dateTime, Sub_metering_3, col="blue"))
  legend("topright", col = c("black","red", "blue"), 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1))
  
  dev.copy(png, file = "plot3.png")
  dev.off()
}