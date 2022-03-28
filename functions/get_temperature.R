#===============================================================================
"
IMPORTANT: This file needs to be updated!!!

Read in multiple snapshots and ouput temperature profiles for each snapshot
at each depth

"
#===============================================================================


setwd("C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6")

inputs <- c("2018_MLS/channel_1/out180/dts_data/dts.rds",
            "2019_05_MLS/channel_1/out180/dts_data/dts.rds",
            "2021_08_MLS/channel_1/out180/dts_data/dts.rds",
            "2019_10_MLS/channel_1/out180/dts_data/dts.rds",
            "2020_02_MLS/channel_2/out180/dts_data/dts.rds",
            "2021_02_MLS/channel_2/out180/dts_data/dts.rds")

ports <- read.csv("C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6/ports.csv")
ports <- ports[1:9,]
ports$TopDepth <- ports[,1] + 0.47
ports$BottomDepth <- ports[,2] + 0.47

# input <- "2021_02_MLS/channel_2/out180/dts_data/dts.rds"

# For loop takes list of inputs and generates temp-time plots for each distance specified
All_data <- data.table()
for (input in inputs) {
  
  # read dts data
  dts <- readRDS(input)
  
  heat <- get_heating_data_1(dts)
  
  if (dts$device$type=="xt") {
    data <- to_data_table(heat, xt=TRUE)
  } else {
    data <- to_data_table(heat, xt=FALSE)
  }
  
  # isolate time
  ten_hour <- data[data$elapsed_time > 0 & elapsed_time < 36000]
  
  # first subset to borehole
  list_of_distances <- colnames(ten_hour)
  list_of_distances <- list_of_distances[c(3:length(list_of_distances))]
  
  ten_nums <- as.double(list_of_distances) - 29.5782 - 0.47
  
  alright <- c("elapsed_time", "log_elapsed_time")
  columns_ <- append(alright, ten_nums)
  colnames(ten_hour) <- columns_
  
  to_subset <- ten_nums >= 5.0 & ten_nums <= 65.55
  index <- which(to_subset == TRUE)
  first <- index[1]
  last <- index[length(index)]
  #add two to account for elapsed time columns
  sub <- subset(ten_hour, select=(first+2):(last+2))
  
  # to subtract the fist row from every row
  sub <- sub %>% mutate_if(is.numeric, funs(.-first(.)))
  
  # format data.table for plotting
  df <- data.table(ten_hour$log_elapsed_time, sub)
  df1 <- melt(df,  measure.vars = colnames(df[,-1]))
  
  
  # split input string to assign a type
  input_split <- strsplit(input, split='/')
  grab_test <- input_split[[1]][1]
  
  df1$type <- grab_test
  All_data <- rbind(All_data, df1) 
}

# make column to filter
dat <- All_data$variable
filters <- as.numeric(levels(dat))[dat]
All_data$filter <- filters

write.csv(All_data, "all_temperature_data.csv")

# filter
port1 <- All_data %>% filter(All_data$filter >= ports$TopDepth[1], All_data$filter <= ports$BottomDepth[1])




final <- ggplot(port1, aes(x =V1, y=value, color = type, group = type)) +
  geom_line(aes(color=type)) +
  facet_wrap (variable~., scales="fixed", ncol=5)


#write.csv(All_data, "all_temperature_data.csv")






