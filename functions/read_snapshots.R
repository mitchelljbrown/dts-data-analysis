
# heat <- process_heating(x, 20)
# l <- get_data_table(heat)[elapsed_time >= start & elapsed_time <= end]
# k <- read_snapshots(x, TOC=20, out="dtsobject")

# output data.table of thermal conductivity
read_snapshots <- function(dts, TOC,
                           power=15,
                           start=0.0, end=36000.0,
                           resample=FALSE, ultima=NULL,
                           set_back=0, smooth=FALSE,
                           out="TC") {
  
  if (resample==TRUE) {
    # interpolate to specified test
    dts <- resample_distance(dts, ultima)
    # remove na
    dts$trace_data <- na.omit(get_data_table(dts))
    dts$trace_distance <- na.omit(get_distance_table(dts))
  } 
  
  heat <- process_heating(dts, TOC, set_back=set_back)
  
  heat$trace_data <- get_data_table(heat)[elapsed_time >= 0 & elapsed_time <= 1800]
  heat$trace_time <- get_time_table(heat)[elapsed_time >= 0 & elapsed_time <= 1800]
  
  # return the dts object
  if (out=='dtsobject'){
    return(heat)
  }
  
  # create a data.table with distance as columns, temperature as rows
  heat_matrix <- data.table(t(to_matrix(heat)))
  
  # create vector of elapsed time for heating
  elapsed_time <- heat$trace_time[type=='heating']$elapsed_time
  # bind elapsed time with heat_matrix as data.table
  data <- data.table(elapsed_time,heat_matrix)
  # remove first row
  data <- data[-1]
  
  # find duration for slope loop input
  heating_duration <- max(heat$trace_time[type=='heating']$elapsed_time)
  
  # isolating time
  input <- log(data$elapsed_time)  
  output <- data[,elapsed_time:=NULL]   
  
  # smooth all columns in output 
  if (smooth==TRUE) {
    output <- data.table(apply(output, 2, function(col) smooth.spline(input, col)$y))
  }
  
  # return matrix of temperatures with elapsed time, else generate thermal conductivities
  if (out=='temp') {
    return(cbind(input, output))
  }
  
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


# copy <- output
# 
# c <- data.table(apply(output, 2, function(col) smooth.spline(input, col)$y))
# 
# r <- data.table(c)
# 
# plot(input, r$`63.4467`)
# lines(input, copy$`63.4467`)









