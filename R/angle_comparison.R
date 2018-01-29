## script to visualize relationship of wave amplitude and distance
library(dplyr)
library(tidyr)

source("R/build_spider_fun.R")

# Result: movement perceived by female is 2.32x higher than if waves were kept 
# static
size_static <- 3 # average 3 degrees moved in female fov at 40mm
size_wave <- seq(3, 7, length.out = 41) # males increase amplitude 2.3 betw 10-40mm
distance <- c(10:50)
angle_static <- rad2deg(atan(size_static/distance))
angle_wave <- rad2deg(atan(size_wave/distance))

comparison <- data.frame(size_static,
                         size_wave,
                         distance,
                         angle_static,
                         angle_wave)

comparison <- gather(comparison, "type", "angle_moved", 4:5)

comp <- ggplot() +
  theme_bw() +
  geom_line(data = comparison, aes(x = distance, y = angle_moved, color = type)) +
  theme(legend.position = "bottom") +
  ylim(0, 20) +
  xlab("distance (mm)") +
  ylab("visual angle (degrees)")

comp
