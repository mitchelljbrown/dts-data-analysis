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

#-----------------------------------Read Files---------------------------------

input  <- 'C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6/2019_05_MLS/channel_1'



RDS_input <- 'C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6/2019_05_MLS/channel_1/outall/dts_data/dts.rds'
#RDS_input1 <- 'C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6/2019_05_MLS/channel_1/out180/dts_data/dts.rds'
RDS_output <- 'out'

system.time(dts_out <- read_dts_xml_3(input, 
                                      out_dir = file.path(input, RDS_output),
                                      n_cores = 1,
                                      return_stokes = FALSE,
                                      in_memory = TRUE,
                                      time_aggregate_interval = 10, output_rds = TRUE))

#-------------------------------------------------------------------------------


inputs <- c('C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6/2019_10_MLS/channel_1',
            'C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6/2020_02_MLS/channel_2',
            'C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6/2021_02_MLS/channel_2',
            'C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6/2021_08_MLS/channel_1')
  
for (input in inputs) {
  system.time(dts_out <- read_dts_xml_3(input, 
                                        out_dir = file.path(input, RDS_output='out'),
                                        n_cores = 1,
                                        return_stokes = FALSE,
                                        in_memory = TRUE,
                                        time_aggregate_interval = 10, output_rds = TRUE))
}


RDS_input <- 'C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6/2021_02_MLS/channel_2/out/dts_data/dts.rds'
dts <- readRDS(RDS_input)
