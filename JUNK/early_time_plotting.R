time_intervals <- list(c(60, 120),
                       c(60, 180), 
                       c(60, 240), 
                       c(60, 300), 
                       c(60, 360), 
                       c(60, 420),
                       c(60, 480),
                       c(60, 900))

time_intervals <- list(
                       c(120, 180), 
                       c(120, 240), 
                       c(120, 300), 
                       c(120, 360), 
                       c(120, 420),
                       c(120, 480),
                       c(120, 900))

time_intervals <- list(
                      c(180, 300),
                      c(180, 360),
                      c(180, 420),
                      c(180, 480),
                      c(180, 700),
                      c(180, 900))

# time_intervals <- list( 
#                       c(240, 300), 
#                       c(240, 360), 
#                       c(240, 420),
#                       c(240, 480),
#                       c(240, 700),
#                       c(240, 900))

inputs <- c("C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6/2018_08_MLS/channel_1/out60/dts_data/dts.rds")

ultima <- readRDS("C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6/2018_08_MLS/channel_1/out60/dts_data/dts.rds")

alldata <- process_and_bind.multiple_times(inputs, time_intervals, 
                                           TOC=29.5782,
                                           resample=TRUE, 
                                           ultima=ultima,
                                           input_name_location=8)

g <- ggplot(alldata, aes(depth, Therm_con, group = time)) 
ggplotly(g + geom_line(aes(color = time)) + 
           scale_x_reverse() + 
           coord_flip() +
           ylim(c(0, 3)) + 
           ggtitle("Time Comparison") + 
           ylab("Thermal Conductivity (W/mK)") +
           xlab("Depth Below Ground Surface (m)"))


#120 - 300