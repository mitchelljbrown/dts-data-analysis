#===============================================================================
"
Perfoms many rudimentry processing steps required for A-DTS data

This function can be used as a preliminary step for the following tasks:
1. get temp data
2. get instantaneous data
3. get thermal conductivity data
"
#===============================================================================
# setwd("C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6")
# library(dts)
# inp <-"2018_08_MLS/channel_1/allfiles/dts_data/dts.rds"
# inp <- "2021_02_MLS/channel_2/allfiles/dts_data/dts.rds"
# inp <- "2019_05_MLS/channel_1/allfiles/dts_data/dts.rds"
# x <- readRDS(inp)
# o <- process_heating(x, 20)
# x <- y



process_heating <- function(x, TOC, BOC = 70, 
                            set_back=0, heating='heating') {
  
  #get rid of zero values if present in x
  if (to_matrix(x)[1,1] == '0') {
    
    x$trace_distance <- get_distance_table(x)[with(x$trace_distance, distance > 0),]
    x$trace_data <- get_data_table(x)[with(x$trace_data, distance > 0),]
  }
  
  # step find water bath
  x <- find_water_bath(x, buffer = 0.05)
  
  # step shift to
  x <- bath_calibration(x, smooth = TRUE)
  
  # step find heating times
  x <- heating_time(x, heating_type = heating)
  
  # make heating start before actual to visualize initial temp rise
   
  # find index where heating starts
  num <- x$trace_time[type==heating, which=TRUE]

  # split time data
  heating_start <- x$trace_time[1:(num[1]-(set_back+1)),]
  heating_end <- x$trace_time[(num[1]-set_back):nrow(x$trace_time),]
  heating_end$type <- heating
  x$trace_time <- rbind(heating_end, heating_start)
  
  
  # find heating distances
  x <- heating_distance(x, heating_type = heating) 
  
  # subset to borehole
  #x <- subset_distance(x, TOC, BOC)
  
  #isolate only heating times
  x <- get_time_type(x, time_type = heating)
  
  #make vectors for elapsed time and log elapsed time
  x$trace_time[type =='heating', log_elapsed_time := log(elapsed_time)]
  x$trace_data$distance <- get_data_table(x)$distance - TOC
  x$trace_distance$distance <- get_distance_table(x)$distance - TOC
  x <- subset_distance(x, 0, BOC)
  
  return(x)
}

# c <- process_heating(dts, 20)
# 
# c$trace_data <- get_data_table(c)[elapsed_time >= 0 & elapsed_time <= 360]
# c$trace_time <- get_time_table(c)[elapsed_time >= 0 & elapsed_time <= 360]

# 
# TOC = TOC + Stickup
# BOC = BOC + Stickup
# input <- "C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6/2018_08_MLS/channel_1/out10/dts_data/dts.rds"
# dts <- readRDS(input)
# x <- dts
# #   x <- readRDS(input)
#   x <- find_water_bath(x, buffer = 0.05)
#   x <- bath_calibration(x, smooth = TRUE)
#   x <- heating_time(x, heating_type = 'heating')
#   x <- heating_distance(x, heating_type = 'heating') 
#   x <- subset_distance(x, 40, 60)
#   a <- fit_convolve(x, n_knots=20)
# 
# fit_convolve(x)

# 
# setwd("C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6")
# input <- "2018_08_MLS/channel_1/out/dts_data/dts.rds"
# TOC <- 29.5782
# heating <- "heating"
# set_back <- 0
# x <- readRDS(input)
# 
# dts  <- process_heating(x, TOC=TOC)
# 
# dts$trace_data <- get_data_table(dts)[elapsed_time <= 3600]
# dts$trace_time <- get_time_table(dts)[elapsed_time <= 3600]
# 
# 
# l <- get_data_table(dts)[distance=="43.0427"]
# l$temperature <- l$temperature - l$temperature[1]
# 
# 
# 
