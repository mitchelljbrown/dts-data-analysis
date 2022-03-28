#===============================================================================
"
Perfoms many rudimentry processing steps required for A-DTS data

This function can be used as a preliminary step for the following tasks:
1. get temp data
2. get instantaneous data
3. get thermal conductivity data
"
#===============================================================================

process_heating <- function(x, TOC, 
                            BH_depth=70, stickup=0.47, set_back=0, 
                            heating='heating', subset_by='heated') {
  
  #get rid of zero values if present in x
  if (to_matrix(x)[1,1] == '0') {
    
    x$trace_distance <- get_distance_table(x)[with(x$trace_distance, distance > 0),]
    x$trace_data <- get_data_table(x)[with(x$trace_data, distance > 0),]
  }
  
  time_resolution <- as.numeric(x$trace_time$start[2]) - as.numeric(x$trace_time$start[1])
  
  # step find water bath
  x <- find_water_bath(x, buffer = 0.05)
  
  # step shift to
  x <- bath_calibration(x, smooth = TRUE)
  
  # step find heating times
  x <- heating_time(x, heating_type = heating)
  
  # find index where heating starts
  num <- x$trace_time[type==heating, which=TRUE]
  
  # split time data
  heating_start <- x$trace_time[1:(num[1]-(set_back+1)),]
  heating_end <- x$trace_time[(num[1]-set_back):nrow(x$trace_time),]
  heating_end$type = heating
  
  # create new time sequence so 0 starts where set_back specified
  z <- seq(0, length.out=(nrow(heating_end)), by=time_resolution)
  heating_end$elapsed_time <- z
  x$trace_time <- rbind(heating_end, heating_start)
  
  # find heating distances
  x <- heating_distance(x, heating_type = heating) 
  
  # remove distances in unit
  x <- subset_distance(x, by = subset_by)  
  
  #isolate only heating data
  x <- get_time_type(x, time_type = heating)
  
  #make vectors for elapsed time and log elapsed time
  x$trace_time[type =='xing', log_elapsed_time := log(elapsed_time)]
  x$trace_data$distance <- get_data_table(x)$distance - (TOC + stickup)
  x$trace_distance$distance <- get_distance_table(x)$distance - (TOC + stickup)
  x <- subset_distance(x, 0, BH_depth)
  
  return(x)
}
