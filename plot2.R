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

plot2 <- function() {
  fillDat()
  with(dat, plot(dateTime, Global_active_power, ylab="Global Active Power (kilowatts)", xlab="",
                 type="l"))
  dev.copy(png, file = "plot2.png")
  dev.off()
}