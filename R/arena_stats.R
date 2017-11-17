# run statistics and plot long distance waving data. Needs "waves.csv" file produced by "build_spider.r"

library(dplyr)
library(ggplot2)
library(utils)

#------------------------READ & WRANGLE DATA------------------------------------------------------#
waves <- read.csv(
    "waves.csv",
    header = TRUE
  )

# add down/upstroke column
waves$stroke <- ifelse(waves$amplitude.male < 0, "down", "up")

# add absolute amplitude column
waves$amp <- ifelse(waves$amplitude.male < 0, waves$amplitude.male * -1, waves$amplitude.male)

# reorder by treatment so output is more intuitive
order <- c("white", "0x", "1x", "1.5x")
waves <- waves %>%
  mutate(treat =  factor(treat, levels = order)) %>%
  arrange(treat)

# make new table with means for each trial (each trial contains different number of waves)
by_trial <- waves %>% group_by(id, treat)
waves.by_trial <- by_trial %>% summarize(
  amplitude = mean(amp),
  velocity = mean(velocity.male),
  duration = mean(duration),
  n.waves = n()
)

#------------------------SUMMARY STATS------------------------------------------------------------#

by_treatment <- waves.by_trial %>% group_by(treat)
smry.av <- by_treatment %>% summarize(
  mean.amplitude = mean(amplitude),
  mean.velocity = mean(velocity),
  mean.n = mean(n.waves),
  sd.amplitude = sd(amplitude),
  sd.velocity = sd(velocity),
  sd.n = sd(n.waves)
)

test<- aov(n.waves ~ treat, data = waves.by_trial)
summary(test)

#------------------------PLOT ZONE----------------------------------------------------------------#

## Simple scatterplot of amplitude vs velocity
plot.av <- ggplot(waves.by_trial, aes(x = velocity, y = amplitude)) +
  geom_point() +
  xlab("velocity (degrees/s)") +
  ylab("amplitude (degrees)") +
  theme_classic()

## same as heatmap
# df <- data.frame(waves.by_trial$velocity, waves.by_trial$amplitude)
# colnames(df) <- c("x", "y")
# 
# the_heat <- ggplot(data = df, aes(x, y)) +
#   stat_density2d(
#     aes(fill = ..level.., alpha = ..level..),
#     geom = 'polygon',
#     colour = 'black',
#     size = .25) +
#   scale_fill_continuous(low = "white", high = "red") +
#   guides(alpha = "none") +
#   theme_classic() +
#   theme(legend.position = "none") +
#   xlim(0, 175) +
#   ylim(0, 60)
# 
#   rm(df)

## same plot but faceted by background
plot.av.background <- ggplot(waves.by_trial, aes(x = velocity, y = amplitude)) +
  geom_point() +
  xlab("velocity (degrees/s)") +
  ylab("amplitude (degrees)") +
  facet_wrap(~ treat) +
  theme_classic() +
  theme(strip.background  = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.line = element_blank())

##
plot.av.by_strokes <- ggplot(waves, aes(x = velocity.male, y = amplitude.male, color = stroke)) +
  geom_point()

histo.v <- ggplot(waves, aes(x = velocity.male)) +
  geom_histogram(binwidth = 5)

histo.a <- ggplot(waves, aes(x = amp)) +
  geom_histogram(binwidth = 5)

#------------------------END MATTER---------------------------------------------------------------#

save.image("arena.RData")
