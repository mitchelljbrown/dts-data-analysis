#===============================================================================
" 
Functions For Getting Thermal Conductivity From Formatted Data

1. read_snapshots
- reads in .rds file of preocessed raw data and outputs a data.table of thermal conductivity

2. read_snapshots_csv
- same as first funciton but outputs csv instead of a data.table

3. Process_and_bind
- takes many inputs and outputs thermal conductivity of all in a single structured data.table
"
#===============================================================================

knitr::opts_chunk$set(echo = TRUE)
library(dts)
library(arrow)
library(dplyr)
library(tibble)
library(duckdb)
library(data.table)
library(plotly)
library(viridis)
library(gam)
library(mgcv)
library(tidyverse)
library(plotly)
library(ggplot2)
library(grid)

#===============================================================================

# read in all the data and store in data.table "alldata"
process_and_bind <- function(inputs, TOC, stickup,
                             strt, end, 
                             resample=FALSE, ultima=NULL, 
                             temps=FALSE, set_back=0,
                             time_resolution=180) {
  
  alldata <- data.table()
  i <- 1
  for (input in inputs) {
    dts <- readRDS(input)
    
    if (input == "2018_08_MLS/channel_1/out180_1/dts_data/dts.rds") {
      power = 10 
    } else {
      power = 15
    }
    x <- read_snapshots(dts, 
                        TOC=TOC,
                        strt=strt, end=end, 
                        power=power,
                        resample=resample,
                        ultima=ultima,
                        temps=temps,
                        set_back=set_back,
                        time_resolution=time_resolution)
    
    x$test <- strsplit(input, '/')[[1]][1]
    
    alldata <- rbind(alldata, x)
  }
  return(alldata)
}

#===============================================================================

process_and_bind.multiple_times <- function(inputs, time_intervals, 
                                            TOC, 
                                            strt, end, 
                                            resample=FALSE, ultima=NULL) {
  # get folder names
  name_list <- c()
  for (input in inputs) {
    name <- strsplit(input, "[/]")[[1]][1]
    name_list <- append(name_list, name)
  }
  print(name_list)
  
  # read multiple files into a data.frame
  alldata <- data.table()
  df <- list()
  name <- list()
  i <- 1
  for (time in time_intervals) {
    j <- 1
    for (input in inputs) {
      dts <- readRDS(input)
      
      #account for one test with output of 10
      if (input == "2018_08_MLS/channel_1/out180_1/dts_data/dts.rds")
      {power = 10
      } else {power = 15}
      
      #read in snapshot and output csv
      df[[j]] <- read_snapshots(dts, 
                                TOC=29.5782, 
                                strt = time[1], end = time[2], 
                                power = power)
      df[[j]]$test <- name_list[[j]]
      df[[j]]$time <- toString(time)
      j <- j + 1
    }
    names(df) <- name_list
    i <- i + 1
    alldata <- rbind(alldata, rbindlist(df))
  }
  return(alldata)
}
#===============================================================================

# output data.table of thermal conductivity
read_snapshots <- function(dts, TOC, 
                           strt, end, power, 
                           resample=FALSE, ultima=NULL, temps=FALSE,
                           set_back=0, time_resolution=180) {
  
  if (resample==TRUE) {
    # interpolate to specified test
    dts <- resample_distance(dts, ultima)
    # remove na
    dts$trace_data <- na.omit(get_data_table(dts))
    dts$trace_distance <- na.omit(get_distance_table(dts))
  } 
  
  heat <- process_heating(dts, TOC, 
                          set_back=set_back, time_resolution=time_resolution)
  
  # create a data.table with distance as columns, temperature as rows
  heat_matrix <- data.table(t(to_matrix(heat)))
  
  # create vector of elapsed time for heating
  elapsed_time <- heat$trace_time[type=='heating']$elapsed_time
  # bind elapsed time with heat_matrix as data.table
  data <- data.table(elapsed_time,heat_matrix)
  # remove first row
  data <- data[-1]
  
  if (temps==TRUE) {
    return(data)
  }
  
  # find duration for slope loop input
  heating_duration <- max(heat$trace_time[type=='heating']$elapsed_time)
  
  # isolating time
  x1 <- data[elapsed_time >= strt & elapsed_time <= end,]
  input <- log(x1$elapsed_time)  
  output <- x1[,elapsed_time:=NULL]   
  
  # making my own function
  depth <- heat$trace_distance$distance
  mat <- as.matrix(output)
  slp <- lm(mat~input)
  out <- summary(slp)
  
  # get coefficients of regression
  slope <- c()
  ste <- c()
  rsq <- c()
  for (col in out) {
    slope <- append(slope, col$coefficients[2,1])
  }
  for (col in out) {
    ste <- append(ste, col$coefficients[2,2])
  }
  for (col in out) {
    rsq <- append(rsq, col$r.squared)
  }
  
  # format resultant data.table
  therm <- data.table(depth, slope, rsq, ste)
  therm[,Therm_con := ((1.0/slope) * power / (4.0*pi))]
  therm <- therm[,c('depth', 'Therm_con', 'rsq', 'ste')]
  
  return(therm)
}

#===============================================================================

"Examples"

# 2017_FLUTe - TOC1 = 117.616, BOC = 187.312
# 2018_MLS - TOC1 = 29.5782, BOC = 96.9976
# 2019_MLS - Same as above
# 2019_10_MLS - TOC1 = 29.4507, BOC = 97.0677

# read multiple files
inputs <- c("2018_08_MLS/channel_1/out180_1/dts_data/dts.rds",
            "2019_05_MLS/channel_1/out180/dts_data/dts.rds",
            "2019_10_MLS/channel_1/out180/dts_data/dts.rds",
            "2020_02_MLS/channel_2/out180/dts_data/dts.rds",
            "2021_02_MLS/channel_2/out180/dts_data/dts.rds",
            "2021_08_MLS/channel_1/out180/dts_data/dts.rds")
dts <- readRDS("2019_05_MLS/channel_1/out180/dts_data/dts.rds")

inputs <- "2018_08_MLS/channel_1/out180_1/dts_data/dts.rds"

time_intervals <- list(c(0, 600), 
                       c(0, 900), 
                       c(300, 900), 
                       c(600, 1200), 
                       c(600, 1800), 
                       c(600, 2400), 
                       c(600, 3000),
                       c(3600, 36000))


ultima <- readRDS("2018_08_MLS/channel_1/out180_1/dts_data/dts.rds")

# Example 1: read and bind data
alldata <- process_and_bind(inputs, TOC=29.5782, BOC=97.0677, strt=0, end=900)

write.csv(alldata, "early_time1.csv", row.names=FALSE)

# Example 2: read, interpolate, and bind data
alldata <- process_and_bind(inputs, TOC=29.5782, strt=3600, end=36000,
                            resample=TRUE, 
                            ultima=ultima)

write.csv(alldata, "thermal_conductivity.csv")

# Plotting
l <- ggplot(alldata, aes(depth, Therm_con, group = test, color = test)) 
m <- l + geom_line() +
  ylim(c(0,10))

ggplotly(m)


# Example 3: Multiple Time Intervals
alldata <- process_and_bind.multiple_times(inputs, time_intervals, TOC=29.5782, strt=3600, end=36000,
                                           resample=TRUE, 
                                           ultima=ultima)

# plotting
alldata <- alldata[alldata$test=="2018_08_MLS"]
l <- ggplot(alldata, aes(depth, Therm_con, group=time, color=time))
m <- l + geom_line() + 
  ylim(c(0,10))

ggplotly(m)
#===============================================================================

