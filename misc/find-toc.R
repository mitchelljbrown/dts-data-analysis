# Find the GS automatically
library(dts)
library(dtsmls)
library(data.table)
library(dplyr)
library(quantmod)
library(pracma)

# TOC = 33.7523 + 0.60

TOC <- 33.7523 + 0.60

setwd("D:/Sentry Well DTS Data/SEN7")
input <- "2021_08/channel 1/out60/dts_data/dts.rds"
input1 <- "2019_10/channel 1/allfiles/dts_data/dts.rds"
dts <- readRDS(input)
dts1 <- readRDS(input1)

dts <- resample_distance(dts, dts1)

# convert to matrix for plotting
mat <- to_matrix(dts)

#count distance from bottom of hole
plot(mat[,500])

# find distance where bottom of borehole occurs
# create a funciton called find_ground_surface
b <- as.vector(mat[,500])

findpeaks(b, minpeakheight = 40, nups=2)

# make column for bottom
dts$trace_distance[, bottom := FALSE]

# Change index 394 to "TRUE"
dts$trace_distance[392, 8] <- TRUE

# Now find the distance
bottom_distance <- as.numeric(dts$trace_distance[bottom==TRUE][,1])

#new TOC 
newtoc <- bottom_distance - 65.7657


# distance between bottom and TOC 65.7657
full <- bottom_distance - TOC

# now we can use this on sussesive tests

# Tests that differ: 2020-03-05, 2020-10-29, 2021-03-10