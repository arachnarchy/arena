# run statistics and plot long distance waving data. Needs "waves.csv" file produced by "build_spider.r"

library(dplyr)
library(ggplot2)
library(utils)
library(lme4)
library(nlme)

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
waves_by_trial <- by_trial %>% summarize(
  amplitude_male = mean(amp),
  velocity_male = mean(velocity.male),
  visual_angle = mean(visual.angle),
  velocity_va = mean(visual.velocity),
  duration = mean(duration),
  n.waves = n()
)

#------------------------SUMMARY STATS------------------------------------------------------------#

by_treatment <- waves_by_trial %>% group_by(treat)
smry.av <- by_treatment %>% summarize(
  count <- n(),
  mean.amplitude_male = mean(amplitude_male),
  mean.velocity_male = mean(velocity_male),
  mean.visual_angle = mean(visual_angle),
  mean.velocity_va = mean(velocity_va),
  mean.n = mean(n.waves),
  sd.amplitude_male = sd(amplitude_male),
  sd.velocity_male = sd(velocity_male),
  sd.visual_angle = sd(visual_angle),
  sd.velocity_va = sd(velocity_va),
  sd.n = sd(n.waves)
)

test <-
  lme(visual_angle ~ treat, random = ~ 1 | id, data = waves_by_trial)
summary(test)
anova(test)

#------------------------PLOT ZONE----------------------------------------------------------------#

## 1: Simple scatterplot of male amplitude vs velocity
plot_ampVvel <- ggplot(waves_by_trial, aes(x = velocity_male, y = amplitude_male)) +
  geom_point() +
  xlab("velocity (degrees/s)") +
  ylab("amplitude (degrees)") +
  theme_classic()

## 2: Simple scatterplot of visual angle vs va velocity
plot_visVvel <- ggplot(waves_by_trial, aes(x = visual_angle, y = velocity_va)) +
  geom_point() +
  xlab("velocity (degrees/s)") +
  ylab("visual angle (degrees)") +
  theme_classic()

## 3: plot visual angle by background categories
plot_visual_angle <- ggplot(waves_by_trial, aes(x = treat, y = visual_angle)) +
  geom_point(size = 1) +
  geom_boxplot(alpha = 0) +
  xlab("background") +
  ylab("visual angle (degrees)") +
  theme_classic()

plot_visual_angle

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

## plot 1 faceted by background
plot_ampVvel_background <- ggplot(waves_by_trial, aes(x = velocity_male, y = amplitude_male)) +
  geom_point() +
  xlab("velocity (degrees/s)") +
  ylab("amplitude (degrees)") +
  facet_wrap(~ treat) +
  theme_classic() +
  theme(strip.background  = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.line = element_blank())

## plot 2 faceted by background
plot_visVvel_background <- ggplot(waves_by_trial, aes(x = velocity_va , y = visual_angle)) +
  geom_point() +
  xlab("velocity (degrees/s)") +
  ylab("visual angle (degrees)") +
  facet_wrap(~ treat) +
  theme_classic() +
  theme(strip.background  = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.line = element_blank())

plot_ampVvel_background
plot_visVvel_background

##
plot.av.by_strokes <- ggplot(waves, aes(x = velocity.male, y = amplitude.male, color = stroke)) +
  geom_point()

histo.v <- ggplot(waves, aes(x = velocity.male)) +
  geom_histogram(binwidth = 5)

histo.a <- ggplot(waves, aes(x = amp)) +
  geom_histogram(binwidth = 5)

#------------------------END MATTER---------------------------------------------------------------#

save.image("arena.RData")
