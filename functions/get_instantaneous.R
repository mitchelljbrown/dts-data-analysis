

# use fit_convolve() to find instantaneous slops within the specified interval
get_instantaneous <- function(inputs, n_knots=NULL,
                               resample=FALSE, ultima=NULL,
                               TOC=29.5782, max_duration=3600) {
  
  alldata <- list()
  time <- list()
  i <- 1
  for (input in inputs) {
    
    x <- readRDS(input)
    
    if (resample==TRUE) {
      # interpolate to specified test
      x <- resample_distance(x, ultima)
      # remove na
      x$trace_data <- na.omit(get_data_table(x))
      x$trace_distance <- na.omit(get_distance_table(x))
    }
    
    # process the data
    x <- process_heating(x, TOC= TOC, set_back = 0)
    
    x$trace_data <- get_data_table(x)[elapsed_time <= max_duration]
    x$trace_time <- get_time_table(x)[elapsed_time <= max_duration]
    
    # generate difference in slopes with fit_convolve
    start_time <- Sys.time()
    df <- fit_convolve(x, n_knots=n_knots)
    end_time <- Sys.time()
    total <- end_time - start_time
    print(total)
    time[i] <- total
    df$instantaneous_slope <- df$delta_temperature/df$delta_time_log
    df[,cumulative_instansaneous_slope := cumsum(instantaneous_slope)]
    df$type <- strsplit(input, split='/')[[1]][1]
    
    alldata[[i]] <- df
    
    i <- i + 1
  }
  
  df <- rbindlist(alldata)
  df[,thermal_conductivity := ((1.0/instantaneous_slope) * 15 / (4.0*pi))]
  df$depth <- as.double(df$depth)
  
  return(df)
}

#===============================================================================

# find the thermal conductivity value determined from fitting the slope over a large interval

# FIXME: do we only want two points or all points?
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
# 
# setwd("C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6")
# 
# input <- "2018_08_MLS/channel_1/out/dts_data/dts.rds"
# TOC <- 29.5782
# 
# b <- to_matrix(dts)
# 
# p <- get_instantanesous(input)
# 
# q <- p[depth==43.0427]
# 
# l
# ss <- smooth.spline(l$log_time, l$temperature)
# sf <- splinefun(x=l$log_time, l$temperature, method="hyman")
# rm <- rollmean(l$temperature, 5)
# 
# plot(ss, type='l')
# lines(log(l$elapsed_time), l$temperature, col="red")
# lines(log(l$elapsed_time), rm, col="blue")
