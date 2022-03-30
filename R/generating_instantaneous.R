#===============================================================================
" 
This file uses multiple snapshots to generate instantaneous slopes and 
thermal conductivities over a large time frame at each depth

purpose: To assess how accurate the thermal conductivities generated using larger time periods.
Ideally, we should see instantaneous slopes that closely resemble the overall slope acroos the entire duration of the test

steps
1. Create data.table of all snapshots and their instantaneous slopes
  i. interpolate to higher resolution data (ultima in this case)
  ii. process the data using 'process_heating' function
  iii. use 'fit_convove' to get instantaneous measurements

2. Create data.table of slopes at each depth using a 1hr - 10hr interval

3. combine both datasets from steps 1 and 2 and plot

"
#===============================================================================

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
library(waterlevel)
library(earthtide)


# 1. combine all snapshots after fit_convolve===================================

get_instantanesous <- function(inputs, ultima) {
  
  alldata <- list()
  time <- list()
  i <- 1
  for (input in inputs) {
    
    x <- readRDS(input)
    
    # interpolate to specified test
    x <- resample_distance(x, ultima)
    # remove na
    x$trace_data <- na.omit(get_data_table(x))
    x$trace_distance <- na.omit(get_distance_table(x))
    
    # process the data
    x <- process_heating(x, TOC= 29.5782, set_back = 0)
    
    # generate difference in slopes with fit_convolve
    start_time <- Sys.time()
    df <- fit_convolve(x)
    end_time <- Sys.time()
    total <- end_time - start_time
    print(total)
    time[i] <- total
    df$instantaneous_slope <- df$delta_temperature/df$delta_time_log
    df$type <- strsplit(input, split='/')[[1]][1]
    
    alldata[[i]] <- df
    
    i <- i + 1
  }
  
  df <- rbindlist(alldata)
  df[,thermal_conductivity := ((1.0/instantaneous_slope) * 15 / (4.0*pi))]
  df$depth <- as.double(df$depth)
  
  return(df)
}

# remove 2018 - something wrong with this data
one <- data[data$type == "2018_08_MLS"]
one[,thermal_conductivity := ((1.0/instantaneous_slope) * 10 / (4.0*pi))]
two <- data[-c(which(data$type=="2018_08_MLS")),] 
data <- rbind(one, two)

# 2. find slopes over large time periods========================================

get_bulk_thermal <- function(df) {
  depths <- unique(df$depth)
  tests <- unique(df$type)
  snapshots <- c()
  slopes <- c()
  dist <- c()
  i <- 1
  for (j in 1:length(depths)){
    x <- df[with(df, depth == depths[j])]
    for (test in tests) {
      y <- x[with(x, type == test)]
      
      temp1 <- y[y$elapsed_time==3600]$cumulative_delta_temperature
      temp2 <- y[y$elapsed_time==36000]$cumulative_delta_temperature
      time1 <- y[y$elapsed_time==3600]$elapsed_time_log
      time2 <- y[y$elapsed_time==36000]$elapsed_time_log
      slope <- (temp2-temp1)/(time2-time1)
      
      snapshots[i] <- test
      slopes[i] <- slope
      dist[i] <- depths[j]
      
      i <- i + 1
    }
  }
  df1 <- data.table(slopes, snapshots, dist)
  colnames(df1) <- c("slopes", "snapshots", "depth")
  df1$depth <- as.factor(df1$depth)
  df1[,thermal_conductivity := ((1.0/slopes) * 15 / (4.0*pi))]
  
  return(df1)
}

# 3. use both datasets to plot==================================================

data <- readRDS("slope_therm_data.rds")
instant <- data[[1]]
overall <- data[[2]]
overall$depth <- as.double(levels(overall$depth))[overall$depth]

# isolate the distance on both datasets

port_inst <- instant[with(instant, depth > (ports$TopDepth[2] - 1) & depth < (ports$BottomDepth[2]+1))]
port_over <- overall[with(overall, depth > (ports$TopDepth[2] - 1) & depth < (ports$BottomDepth[2]+1))]


p <- ggplot(port_inst, aes(elapsed_time_log, thermal_conductivity, group=type, color=type))
g <- p + geom_line(aes(color=type)) + 
  ylim(c(0,10)) +
  xlim(c(log(3600),log(36000))) +
  facet_wrap (depth~., scales="fixed", ncol=5) +
  geom_hline(data=port_over, aes(yintercept=thermal_conductivity, color = snapshots))

ggplotly(g)

#===============================================================================
"Examples"

# read in port data
ports <- read.csv("C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6/ports.csv")
ports <- ports[1:9,]
ports$TopDepth <- ports[,1] + 0.47
ports$BottomDepth <- ports[,2] + 0.47

# provide inputs
inputs <- c("2018_08_MLS/channel_1/out180_1/dts_data/dts.rds",
            "2019_05_MLS/channel_1/out180/dts_data/dts.rds",
            "2019_10_MLS/channel_1/out180/dts_data/dts.rds",
            "2020_02_MLS/channel_2/out180/dts_data/dts.rds",
            "2021_02_MLS/channel_2/out180/dts_data/dts.rds",
            "2021_08_MLS/channel_1/out180/dts_data/dts.rds")

inputs <- "2019_05_MLS/channel_1/out180/dts_data/dts.rds"

input <- "2018_08_MLS/channel_1/out180_1/dts_data/dts.rds"
ultima <- readRDS(input)

# Example 1

# step 1: get instantaneous
data <- get_instantanesous(inputs, ultima)

copy <- data

# account for the difference in power
one <- data[data$type == "2018_08_MLS"]
one <- one[,thermal_conductivity := ((1.0/instantaneous_slope) * 10 / (4.0*pi))]
two <- data[-c(which(data$type=="2018_08_MLS")),] 
data <- rbind(one, two)
copy <- data
# save data
saveRDS(data, file = "instantaneous_thermal.rds")
data <- readRDS("instantaneous_thermal.rds")

# step 2: get overall
data1 <- get_bulk_thermal(df=data)

# filter out "inf"
data <- data %>% filter_if(~is.numeric(.), all_vars(!is.infinite(.)))

# combine data as list of data tables
alldata <- list()
alldata[[1]] <- data
alldata[[2]] <- data1

# save result
saveRDS(alldata, file = "instantaneous_03_16.rds")

instant <- alldata[[1]]
overall <- alldata[[2]]
overall$depth <- as.double(levels(overall$depth))[overall$depth]

# isolate the distance on both datasets

port_inst <- instant[with(instant, depth > (ports$TopDepth[3] - 0.5) & depth < (ports$BottomDepth[3]+0.5))]
port_over <- overall[with(overall, depth > (ports$TopDepth[3] - 0.5) & depth < (ports$BottomDepth[3]+0.5))]

# plot
p <- ggplot(port_inst, aes(elapsed_time_log, thermal_conductivity, group=type, color=type))
g <- p + geom_line(aes(color=type)) + 
  ylim(c(0,10)) +
  xlim(c(log(3600),log(36000))) +
  facet_wrap (depth~., scales="fixed", ncol=5) +
  geom_hline(data=port_over, aes(yintercept=thermal_conductivity, color = snapshots))
ggplotly(g)

