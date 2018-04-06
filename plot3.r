plot3 <- function(my_working_dir="C:/Users/ealonvic/Documents/Rhmwk/exdata%2Fdata%2Fhousehold_power_consumption", DEBUGR=FALSE)
{
# Programiing assignment #3 of 4
# Requirements:
# 
# 1) A data file, "household_power_consumption.txt" must be downloaded and saved to a local file system,
# 2) The function call supports two input parameters:
#    a) Explict file path for the input text file delimited in double quotes.
#    b) TRUE|FALSE to enable debug print statements.
# Example of working fille directory is listed below:
# setwd("C:/Users/ealonvic/Documents/Rhmwk/exdata%2Fdata%2Fhousehold_power_consumption")

library(dplyr)

setwd(my_working_dir)

if ( DEBUGR == TRUE) {
	cat(getwd())
	cat ("\n")
    cat(dir())
	cat ("\n")
}
# 
#  READ the data set that was down loaded from UC Irvine Machine Learning Repository.
my_df <- read.table(file="household_power_consumption.txt", sep =';', blank.lines.skip = TRUE, col.names = c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), fill=TRUE, header=TRUE)
if ( DEBUGR == TRUE) {
cat (paste("Population size is ", nrow(my_df)))
cat ("\n")
}
# output shuld be  2075259 rows of data
# 

# You may find it useful to convert the Date and Time variables to Date/Time classes in R using the strptime()  and as.Date() functions.
# Convert String Vector / type for Date column into date type 
my_df$Date <- as.Date(my_df$Date, "%d/%m/%Y")
# sort the data based on the Date
my_df <- my_df[order(as.Date(my_df$Date, format="%d-%m-%Y")),] 
# We will only be using data from the dates 2007-02-01 and 2007-02-02. One alternative is to read the data from just those dates rather than reading in the 
# entire dataset and subsetting to those dates.
DATE1 <- as.Date("2007-02-01")
DATE2 <- as.Date("2007-02-02")
#
sample_df <- subset(my_df, Date >= DATE1)
sample_df <- subset(sample_df, Date <= DATE2)
# Data may contain "?"  the dplyr library is needed.
sample_df <- filter(sample_df, !is.nan(Sub_metering_1))
sample_df <- filter(sample_df, !is.nan(Sub_metering_2))
sample_df <- filter(sample_df, !is.nan(Sub_metering_3))
# convert to real numbers
#sample_df$Global_active_power <- as.numeric(as.character(sample_df$Global_active_power))
# sample_df$Global_reactive_power <- as.numeric(as.character(sample_df$Global_reactive_power))
# sample_df$Voltage <- as.numeric(as.character(sample_df$Voltage))
# sample_df$Global_intensity <- as.numeric(as.character(sample_df$Global_intensity))
sample_df$Sub_metering_1 <- as.numeric(as.character(sample_df$Sub_metering_1))
sample_df$Sub_metering_2 <- as.numeric(as.character(sample_df$Sub_metering_2))
sample_df$Sub_metering_3 <- as.numeric(as.character(sample_df$Sub_metering_3))
#
sample_df$Timestamp <-paste(as.character(sample_df$Date),as.character(sample_df$Time))
sample_df$Timestamp <-as.POSIXct(as.character(sample_df$Timestamp), format = "%Y-%m-%d %H:%M:%S")
#
# Generate the line chart and save as PNG / image.
png(filename = "plot3.png",width = 480, height = 480)
plot(sample_df$Timestamp,sample_df$Sub_metering_1, ylab="Energy sub meetering", xlab="", type="n")
points (sample_df$Timestamp, sample_df$Sub_metering_1, type="l")
points (sample_df$Timestamp, sample_df$Sub_metering_2, type="l", col="red")
points (sample_df$Timestamp, sample_df$Sub_metering_3, type="l", col="blue")
legend("topright",pch=15,col=c("black", "red", "blue"),legend=c("Sub_Metering_1","Sub_Metering_2", "Sub_Metering_3"))
dev.off()
#
if (DEBUGR == TRUE) {
cat (paste("Sample size is ", nrow(sample_df)))
cat ("\n")
}
#
if (DEBUGR == TRUE) {
return (sample_df)
}
else
return (0)
}







