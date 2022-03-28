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
