# Utah Precipitation Totals Analysis
# Author: Jonah Meherg
# 6/5/19-

# The purpose of this project is to use precipitation totals by city for the state of Utah to try and 
#   predict precipitation across the state of Utah using time series and spatial correlation


# Data used for OCT 2006 - SEP 2007 from https://www.wrh.noaa.gov/slc/climate/prelim0607wateryear.htm,
#   saved as 'HTML only'
# Data used for OCT 2007 - SEP 2008 from https://www.wrh.noaa.gov/slc/climate/prelim0708wateryear.htm, 
#   saved as 'HTML only'
# Data used for OCT 2008 - SEP 2009 from https://www.wrh.noaa.gov/slc/climate/prelim0809wateryear.htm, 
#   saved as 'HTML only'  

#List of used libraries for this project
library(XML)

# This script does the following
# 1. Read in data
# 2. Clean data into more usable format
# 3. Combine all data into one data set
# 4. Add latitude and longitude for cities
# 5. Time Series Spatial Correlation Analysis

# 1. Read in data

utahPrecip0607.df <- as.data.frame(readHTMLTable("Data/prelim0607wateryear.htm"))
utahPrecip0708.df <- as.data.frame(readHTMLTable("Data/prelim0708wateryear.htm"))
utahPrecip0809.df <- as.data.frame(readHTMLTable("Data/prelim0809wateryear.htm"))

# 2. Clean data into more usable format

