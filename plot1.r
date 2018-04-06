
plot1 <- function(my_working_dir="C:/Users/ealonvic/Documents/Rhmwk/exdata%2Fdata%2Fhousehold_power_consumption", DEBUGR=FALSE)
{
# Programiing assignment #1
# Requirements:
# 1) This function requires the dplyr libary to be installed.
# 2) A data file, 'household_power_consumption.txt", must be downloaded and saved to a local file system,
# 3) the functin call supports two input parameters:
#    a) Explict file path for the input text file delimited in double quotes.
#    b) TRUE|FALSE to enable debug print statements.
# Example of working fille directory is listed below:
# setwd("C:/Users/ealonvic/Documents/Rhmwk/exdata%2Fdata%2Fhousehold_power_consumption")


setwd(my_working_dir)
if ( DEBUGR == TRUE) {
	cat(getwd())
	cat ("\n")
    cat(dir())
	cat ("\n")
}

require(dplyr)

my_df <- read.table(file="household_power_consumption.txt", sep =';', blank.lines.skip = TRUE, col.names = c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), fill=TRUE, header=TRUE)
if ( DEBUGR == TRUE) {
cat (paste("Population size is ", nrow(my_df)))
cat ("\n")
}
# output shuld be [1] 2075259       9
# 
# You may find it useful to convert the Date and Time variables to Date/Time classes in R using the strptime()  and as.Date() functions.
# Convert String Vector / type for Date column into date type 
my_df$Date <- as.Date(my_df$Date, "%d/%m/%Y")
# convert String Vector / type for Time column into POSIXlt type 
# my_df$Time <- strptime(my_df$Time, "%H:%M:%S") )
# my_df$Time <- as.POSIXct(strptime(my_df$Time, "%H:%M:%S"))
# sort the data based on the Date
my_df <- my_df[order(as.Date(my_df$Date, format="%d-%m-%Y")),] 
# We will only be using data from the dates 2007-02-01 and 2007-02-02. One alternative is to read the data from just those dates rather than reading in the 
# entire dataset and subsetting to those dates.
DATE1 <- as.Date("2007-02-01")
DATE2 <- as.Date("2007-02-02")
#
sample_df <- subset(my_df, Date >= DATE1)
sample_df <- subset(sample_df, Date <= DATE2)
# Note that in this dataset missing values are coded as "?".
#
sample_df <- filter(sample_df, !is.nan(Global_active_power))
sample_df <- filter(sample_df, !is.nan(Sub_metering_2))
# convert to real numbers
sample_df$Global_active_power <- as.numeric(as.character(sample_df$Global_active_power))
sample_df$Global_reactive_power <- as.numeric(as.character(sample_df$Global_reactive_power))
sample_df$Voltage <- as.numeric(as.character(sample_df$Voltage))
sample_df$Global_intensity <- as.numeric(as.character(sample_df$Global_intensity))

# Summarize the results 
results <- sample_df %>% group_by(Global_active_power) %>% tally(sort = TRUE)
# break down the results into Frequency buckets
#
glabel <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5)
gfreq <- c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
#
for (i in 1:nrow(results)) {
#
if (results$Global_active_power[i] < 0.051) { 
    gfreq[1] <-	gfreq[1] + results$n[i]
} else if (results$Global_active_power[i] < 1.001) { 
    gfreq[2] <-	gfreq[2] + results$n[i]
} else if (results$Global_active_power[i] < 1.501) { 
        gfreq[3] <- gfreq[3] + results$n[i]
} else if (results$Global_active_power[i] < 2.001) { 
        gfreq[4] <- gfreq[4] + results$n[i]
} else if (results$Global_active_power[i] < 2.501) { 
        gfreq[5] <- gfreq[5] + results$n[i]
} else if (results$Global_active_power[i] < 3.001) { 
        gfreq[6] <- gfreq[6] + results$n[i]
} else if (results$Global_active_power[i] < 3.501) { 
        gfreq[7] <- gfreq[7] + results$n[i]
} else if (results$Global_active_power[i] < 4.001) { 
        gfreq[8] <- gfreq[8] + results$n[i]
} else if (results$Global_active_power[i] < 4.501) { 
        gfreq[9] <- gfreq[9] + results$n[i]
} else if (results$Global_active_power[i] < 5.001) { 
        gfreq[10] <- gfreq[10] + results$n[i]
} else if (results$Global_active_power[i] < 5.501) { 
        gfreq[11] <- gfreq[11] + results$n[i]
		}
else {
gfreq[12] <- gfreq[12] + results$n[i]
}
#
# end of for loop   
}
#
gfreq[1] <- NA
gfreq <- gfreq[!is.na(gfreq)]
#
#
png(filename = "plot1.png",width = 480, height = 480)
# barplot(results$n, ylab="Frequency", xlab="Global Active Power (kilowatts)", names.arg=c(results$Global_active_power),ylim = c( 0 , 1200 ),xlim = c( 0 , 6 ), col="red")
barplot(gfreq, ylab="Frequency", xlab="Global Active Power (kilowatts)",ylim = c( 0 , 1200 ),xlim = c( 0 , 6 ), names.arg=glabel, space=0,width=0.5, col="red")
title(main = "Global Active Power", font.main = 2)
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
return (1)
}







