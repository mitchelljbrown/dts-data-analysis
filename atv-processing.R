library(data.table)
library(atv)
library(raster)
library(rasterVis)
library(ggplot2)
library(splines)
# parameters --------------------------------------------------------------

top <- 34.5
bot <- 160

dif_tt  <- 10
dif_amp <- 2500

# read data
# get intervals from ports
ports <- data.table(read.csv("C:/Users/Mitchell/Desktop/Thesis Data/rawSenData/SEN6/ports.csv"))
ports <- ports[1:9,]
ports$TopDepth <- ports[,1] + 0.47
ports$BottomDepth <- ports[,2] + 0.47

# read amplitude data and process
amp <- atv::read_las('atv-amplitude.las')
amp <- data.table(amp)
amp <- melt(amp, id.vars='Depth')
setnames(amp, 'variable', 'angle')
setnames(amp, 'Depth', 'depth')
amp[, angle := as.numeric(gsub('Ang', '', angle))]

saveRDS(amp, file = "amplitude.rds")

# now set distance intervals
# to do this, extract distance from processed dts data
input <- 'R Files/Reports/port-responses/thermal_conductivity.csv'
x <- data.table(read.csv(input))[,X:=NULL]
x <- x[test=='2018_08_MLS']
intervals <- x$depth

# subset to ports
top <- ports$TopDepth[1]
bottom <- ports$BottomDepth[1]

port_interval <- intervals[intervals > top & intervals < bottom]

n_intervals <- length(port_interval) - 1
interval_subs <- data.table(id = 1:n_intervals,
                            top = port_interval[-n_intervals],
                            bot = port_interval[-1],
                            mid = (port_interval[-n_intervals] + port_interval[-1])/2)

#?
imgs <- interval_subs[amp, on = .(top <= depth, bot >= depth), nomatch = 0]

ggplot(imgs, aes(x = angle, y = top)) +
  geom_tile(aes(fill = value, color = value)) + 
  facet_wrap(mid~., scales = 'free_y') + 
  scale_fill_viridis_c() + 
  scale_color_viridis_c() + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


# read data ---------------------------------------------------------------

tt  <- atv::read_las('/home/jonathankennel/Storage/analyses/cambridge_pumping_responses/data/uw1_tt.las')
amp <- atv::read_las('/home/jonathankennel/Storage/analyses/cambridge_pumping_responses/data/uw1_amp.las')

tt  <- as.data.table(tt)
amp <- as.data.table(amp)


amp <- melt(amp, id.vars = 'Depth')
setnames(amp, 'variable', 'angle')
setnames(amp, 'Depth', 'depth')
amp[, angle := as.numeric(gsub('Ang', '', angle))]

interval_size <- 0.25
intervals <- seq(40, 45.25, interval_size)
n_intervals <- length(intervals) - 1


interval_subs <- data.table(id = 1:n_intervals, 
                            top = intervals[-n_intervals], 
                            bot = intervals[-1],
                            mid = (intervals[-n_intervals] + intervals[-1])/2)

imgs <- interval_subs[amp, on = .(top <= depth, bot >= depth), nomatch = 0]

ggplot(imgs, aes(x = angle, y = top)) +
  geom_tile(aes(fill = value, color = value)) + 
  facet_wrap(mid~., scales = 'free_y') + 
  scale_fill_viridis_c() + 
  scale_color_viridis_c() + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


#===============================================================================

amp <- atv::read_las('atv-amplitude.las')

amp <- as.data.table(amp)

amp <- melt(amp, id.vars = 'Depth')
setnames(amp, 'variable', 'angle')
setnames(amp, 'Depth', 'depth')
amp[, angle := as.numeric(gsub('Ang', '', angle))]
