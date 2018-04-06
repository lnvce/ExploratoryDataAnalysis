

https://github.com/lnvce/ExploratoryDataAnalysis

## Synopsis

run_analysis example for UCI HAR data set

## Code Example

[Down load the Data set!](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)
Extract the data file into a folder that is accessible with R application.
Save the four R scripts (plot1.r, plot2.r, plot3.r, plot4.r) in a local file directory where the data file (text file) has been saved.
...
R> setwd("[The workspace file directory / file directory that contains 'household_power_consumption.txt' "])
R> source("[your_source_code_file_directory/]plot1.r", local = true)
R> retval <- plot1(["file directory that contains the data set"], FALSE) 
R> dir()
R> source("[your_source_code_file_directory/]plot2.r", local = true)
R> retval <- plot2(["file directory that contains the data set"], FALSE) 
R> dir()
R> source("[your_source_code_file_directory/]plot1.3", local = true)
R> retval <- plot3(["file directory that contains the data set"], FALSE) 
R> dir()
R> source("[your_source_code_file_directory/]plot1.4", local = true)
R> retval <- plot4(["file directory that contains the data set"], FALSE) 
R> dir()



...
## Motivation

# The logic performs the following steps:
1. Reads the text file named "household_power_consumption.txt" from input file directory (input #1 parameter delimited with double quotes).
2. Extracts only the data between two days ["2007-02-01" and "2007-02-02"]
3. As per the assigment spectifications, generates a plot, and saves the file as a PNG file type.
4. A PNP file is generated for each script (plot1.r --> plot1.png, ..., plot4.r --> plot4.png).

## Installation

1. Software requirements:
* R version 3.4.4 (2018-10-15)
* dplyr package,
* Down data set from "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
* Unzip "Electric power consumption"  and staged in a local file system.
2. R application's working file directory must be set.
##  API Reference

https://github.com/rdpeng/ExData_Plotting1

## Test

return_val <- plot1("C:/user/data", FALSE) 
dir()
return_val <- plot2("C:/user/data", FALSE) 
dir()
return_val <- plot3("C:/user/data", FALSE) 
dir()
return_val <- plot4("C:/user/data", FALSE) 
dir()

## Author

Eric Lonvick (lonvick@ameritech.net)
