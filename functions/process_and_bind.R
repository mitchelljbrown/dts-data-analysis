# read in all the data and store in data.table "alldata"
process_and_bind <- function(inputs, TOC, 
                             strt, end, 
                             resample=FALSE, ultima=NULL, 
                             temps=FALSE, set_back=0,
                             test_OR_agg = TRUE) {
  
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
                        set_back=set_back)
    
    # if (test_OR_agg==TRUE) {
    #   x$test <- strsplit(input, '/')[[1]][1]
    # } 
    if (test_OR_agg==FALSE) {
      x$aggregate <- as.numeric(gsub("out", "", strsplit(input, "[/]")[[1]][10]))
    }
    
    
    alldata <- rbind(alldata, x)
  }
  return(alldata)
}

#===============================================================================

process_and_bind.multiple_times <- function(inputs, time_intervals, 
                                            TOC, 
                                            resample=FALSE, ultima=NULL,
                                            input_name_location = 1,
                                            smooth=FALSE) {
  # get folder names
  name_list <- c()
  for (input in inputs) {
    name <- strsplit(input, "[/]")[[1]][input_name_location]
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
                                power = power,
                                smooth=smooth)
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