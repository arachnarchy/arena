## Run statistics and plot long distance waving data. 
## Needs "waves.csv" file produced by "build_spider.r"

library(dplyr)
library(ggplot2)
library(utils)
library(lme4)
library(nlme)
library(ggpubr)


#------------------------READ & WRANGLE DATA-----------------------------------#

make_plots <- 0 # make all the plots?

waves <- read.csv("waves.csv", header = TRUE)

# add down/upstroke column
waves$stroke <- ifelse(waves$amplitude.male < 0, "down", "up")

# add absolute amplitude column
waves$amp <- ifelse(waves$amplitude.male < 0, 
                    waves$amplitude.male * -1, 
                    waves$amplitude.male)

## reorder by treatment so output is more intuitive
order <- c("white", "0x", "1x", "1.5x")
waves <- waves %>%
  mutate(treat =  factor(treat, levels = order)) %>%
  arrange(treat)

## make new table with means by trial (each contains different number of waves)
by_trial <- waves %>% group_by(id, treat)
waves_by_trial <- by_trial %>% summarize(
  amplitude_male = mean(amp),
  velocity_male = mean(velocity.male),
  visual_angle = mean(visual.angle),
  velocity_va = mean(visual.velocity),
  duration = mean(duration),
  distance_1st = distance[which.min(start)],
  distance = mean(distance),
  n.waves = n()
)

## add crude numeric coding of treatments
waves_by_trial <- waves_by_trial %>%
  mutate(treat_n = ifelse(treat == "white", 0,
                          ifelse(treat == "0x", 0.1,
                                 ifelse(treat == "1x", 1, 1.1))))

#------------------------SUMMARY STATS-----------------------------------------#

by_treatment <- waves_by_trial %>% group_by(treat)
smry.av <- by_treatment %>% summarize(
  count <- n(),
  mean.amplitude_male = mean(amplitude_male),
  mean.velocity_male = mean(velocity_male),
  mean.visual_angle = mean(visual_angle),
  mean.velocity_va = mean(velocity_va),
  mean.distance = mean(distance),
  mean.n = mean(n.waves),
  sd.amplitude_male = sd(amplitude_male),
  sd.velocity_male = sd(velocity_male),
  sd.visual_angle = sd(visual_angle),
  sd.velocity_va = sd(velocity_va),
  sd.n = sd(n.waves)
)

#TO-DO: code treatments as ordinal
# test <-
#   lme(visual_angle ~ treat_n, random = ~ 1 | id, data = waves_by_trial)
test <-
  lme(distance_1st ~ treat_n, random = ~ 1 | id, data = waves_by_trial)
summary(test)
anova(test)

#------------------------PLOT ZONE----------------------------------------------------------------#

if(make_plots == 1){
## 1: Simple scatterplot of male amplitude vs velocity
plot_ampVvel <- ggplot(waves_by_trial, 
                       aes(x = velocity_male, 
                           y = amplitude_male)) +
  geom_point() +
  xlab("velocity (degrees/s)") +
  ylab("amplitude (degrees)") +
  theme_classic()

## 2: Simple scatterplot of visual angle vs va velocity
plot_visVvel <- ggplot(waves_by_trial, 
                       aes(x = visual_angle, 
                           y = velocity_va)) +
  geom_point() +
  xlab("velocity (degrees/s)") +
  ylab("visual angle (degrees)") +
  theme_classic()

## 3: plot wave amplitude by background categories
plot_amp_background <- ggplot(waves_by_trial, 
                              aes(x = treat, 
                                  y = amplitude_male)) +
  geom_point(size = 1) +
  geom_boxplot(alpha = 0) +
  xlab("background") +
  ylab("wave amplitude (degrees)") +
  theme_classic()

## 3: plot visual angle by background categories
plot_vis_background <- ggplot(waves_by_trial, 
                              aes(x = treat, 
                                  y = visual_angle)) +
  geom_point(size = 1) +
  geom_boxplot(alpha = 0) +
  xlab("background") +
  ylab("visual angle (degrees)") +
  theme_classic()

## 4: plot distance by background categories
plot_dist_background <- ggplot(waves_by_trial, 
                               aes(x = treat, 
                                   y = distance)) +
  geom_point(size = 1) +
  geom_boxplot(alpha = 0) +
  ylim(0, 50) +
  xlab("background") +
  ylab("distance (mm)") +
  theme_classic()

## 4b: plot distance at 1st wave by background categories
plot_dist1_background <- ggplot(waves_by_trial, 
                                aes(x = treat, 
                                    y = distance_1st)) +
  geom_point(size = 1) +
  geom_boxplot(alpha = 0) +
  ylim(0, 50) +
  xlab("background") +
  ylab("distance (mm)") +
  theme_classic()

## 5: Scatterplot of amplitude_male vs vs distance to female
plot_ampVdist <- ggplot(waves_by_trial, 
                        aes(x = distance, 
                            y = amplitude_male)) +
  geom_point() +
  xlab("distance (mm)") +
  ylab("wave amplitude (degrees)") +
  theme_classic()

## 6: Scatterplot of visual angle vs vs distance to female
plot_visVdist <- ggplot(waves_by_trial, 
                        aes(x = distance, 
                            y = visual_angle)) +
  geom_point() +
  xlab("distance (mm)") +
  ylab("visual angle (degrees)") +
  theme_classic()

# plots to gauge normality
ggdensity(waves_by_trial$visual_angle) # density plot
ggqqplot(waves_by_trial$visual_angle) # quantile-quantile plot

# plot_amp_background
# plot_vis_background
# plot_dist_background
# plot_ampVdist
# plot_visVdist

## plot 1 faceted by background
plot_ampVvel_background <- ggplot(waves_by_trial, 
                                  aes(x = velocity_male, 
                                      y = amplitude_male)) +
  geom_point() +
  xlab("velocity (degrees/s)") +
  ylab("amplitude (degrees)") +
  facet_wrap(~ treat) +
  theme_classic() +
  theme(strip.background  = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.line = element_blank())

## plot 2 faceted by background
plot_visVvel_background <- ggplot(waves_by_trial, 
                                  aes(x = velocity_va , 
                                      y = visual_angle)) +
  geom_point() +
  xlab("velocity (degrees/s)") +
  ylab("visual angle (degrees)") +
  facet_wrap(~ treat) +
  theme_classic() +
  theme(strip.background  = element_blank(),
        panel.border = element_rect(fill = NA),
        axis.line = element_blank())

# plot_ampVvel_background
# plot_visVvel_background

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


##
plot.av.by_strokes <- ggplot(waves, 
                             aes(x = velocity.male, 
                                 y = amplitude.male, 
                                 color = stroke)) +
  geom_point()

histo.v <- ggplot(waves, aes(x = velocity.male)) +
  geom_histogram(binwidth = 5)

histo.a <- ggplot(waves, aes(x = amp)) +
  geom_histogram(binwidth = 5)
}
#------------------------END MATTER---------------------------------------------------------------#

write.csv(waves_by_trial, file = "waves_by_trial.csv")
save.image("arena.RData")
