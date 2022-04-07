

inputs <- c("2018_08_MLS/channel_1/out/dts_data/dts.rds",
            "2019_05_MLS/channel_1/out/dts_data/dts.rds",
            "2019_10_MLS/channel_1/out/dts_data/dts.rds",
            "2020_02_MLS/channel_2/out/dts_data/dts.rds",
            "2021_02_MLS/channel_2/out/dts_data/dts.rds",
            "2021_08_MLS/channel_1/out/dts_data/dts.rds")

dts <- readRDS(inputs[1])


n <- read_snapshots(dts, TOC=29, 0, 3600, 10)






# output data.table of thermal conductivity
read_snapshots <- function(dts, TOC, 
                           strt, end, power, 
                           resample=FALSE, ultima=NULL, temps=FALSE,
                           set_back=0) {
  
  if (resample==TRUE) {
    # interpolate to specified test
    dts <- resample_distance(dts, ultima)
    # remove na
    dts$trace_data <- na.omit(get_data_table(dts))
    dts$trace_distance <- na.omit(get_distance_table(dts))
  } 
  
  heat <- process_heating(dts, TOC, 
                          set_back=set_back)
  
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

x <- dts
set_back <- 0
heating='heating'
subset_by='heated'

process_heating <- function(x, TOC, 
                            BH_depth=70, stickup=0.47, set_back=0, 
                            heating='heating', subset_by='heated') {
  
  #get rid of zero values if present in x
  if (to_matrix(x)[1,1] == '0') {
    
    x$trace_distance <- get_distance_table(x)[with(x$trace_distance, distance > 0),]
    x$trace_data <- get_data_table(x)[with(x$trace_data, distance > 0),]
  }
  
  # time_resolution <- as.numeric(x$trace_time$start[2]) - as.numeric(x$trace_time$start[1])
  
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
  
  
  z <- as.numeric(heating_end$trace_time$end) - as.numeric(heating_end$trace_time$end[1])
  
  # create new time sequence so 0 starts where set_back specified
  # z <- seq(0, length.out=(nrow(heating_end)), by=time_resolution)
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







