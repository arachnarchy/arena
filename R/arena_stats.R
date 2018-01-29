## Run statistics and plot long distance waving data. 
## Needs "waves.csv" file produced by "build_spider.r"

library(dplyr)
library(ggplot2)
library(utils)
library(lme4) # for mixed models
library(lmerTest) # for Satterthwaite approximation to get p-values in lmer
library(ggpubr) # for density and qq plots


#------------------------READ & WRANGLE DATA-----------------------------------#

waves <- read.csv("waves.csv", header = TRUE)

# add down/upstroke column
waves$stroke <- ifelse(waves$amplitude.male < 0, "down", "up")

# add absolute amplitude column
waves$amp <- ifelse(waves$amplitude.male < 0, 
                    waves$amplitude.male * -1, 
                    waves$amplitude.male)

## reorder by treatment so output is more parsable
order <- c("white", "0x", "1x", "1.5x")
waves <- waves %>%
  mutate(treat =  factor(treat, levels = order)) %>%
  arrange(treat)

## make new table with means by trial (each contains different number of waves)
waves_by_trial <- waves %>% group_by(id, treat) %>% summarize(
  amplitude_male = mean(amp),
  velocity_male  = mean(velocity.male),
  visual_angle   = mean(visual.angle),
  velocity_va    = mean(visual.velocity),
  duration       = mean(duration),
  distance_1st   = distance[which.min(start)],
  distance       = mean(distance),
  n.waves        = n()
)

## add numeric re-coding of treatments (tested with many spacings, no diff)
waves_by_trial <- waves_by_trial %>%
  mutate(treat_n = ifelse(treat == "white", 0,
                          ifelse(treat == "0x", 1,
                                 ifelse(treat == "1x", 2, 3))))

#------------------------SUMMARY STATS-----------------------------------------#

## Summary table
summary_by_background <- waves_by_trial %>% group_by(treat) %>% summarize(
  count <- n(),
  mean.amplitude_male = mean(amplitude_male),
  mean.velocity_male  = mean(velocity_male),
  mean.visual_angle   = mean(visual_angle),
  mean.velocity_va    = mean(velocity_va),
  mean.distance       = mean(distance),
  mean.n              = mean(n.waves),
  sd.amplitude_male   = sd(amplitude_male),
  sd.velocity_male    = sd(velocity_male),
  sd.visual_angle     = sd(visual_angle),
  sd.velocity_va      = sd(velocity_va),
  sd.n                = sd(n.waves)
)

## Treatment effect comparison ------------------------------------------------#

## Effects with treatment as categorical
lmer1_cat <- lmer(amplitude_male ~ treat + (1|id), data = waves_by_trial)
lmer2_cat <- lmer(visual_angle ~ treat + (1|id), data = waves_by_trial)
lmer3_cat <- lmer(distance ~ treat + (1|id), data = waves_by_trial)
lmer4_cat <- lmer(distance_1st ~ treat + (1|id), data = waves_by_trial)

smry_lmer1_cat <- summary(lmer1_cat)
smry_lmer2_cat <- summary(lmer2_cat)
smry_lmer3_cat <- summary(lmer3_cat)
smry_lmer4_cat <- summary(lmer4_cat)

## Effects with treatment as ordinal (continuous)
lmer1_ord <- lmer(amplitude_male ~ treat_n + (1|id), data = waves_by_trial)
lmer2_ord <- lmer(visual_angle ~ treat_n + (1|id), data = waves_by_trial)
lmer3_ord <- lmer(distance ~ treat_n + (1|id), data = waves_by_trial)
lmer4_ord <- lmer(distance_1st ~ treat_n + (1|id), data = waves_by_trial)

lmer5_ord <- lmer(velocity_male ~ treat_n + (1|id), data = waves_by_trial)
lmer6_ord <- lmer(velocity_va ~ treat_n + (1|id), data = waves_by_trial)

smry_lmer1_ord <- summary(lmer1_ord)
smry_lmer2_ord <- summary(lmer2_ord)
smry_lmer3_ord <- summary(lmer3_ord)
smry_lmer4_ord <- summary(lmer4_ord)

smry_lmer5_ord <- summary(lmer5_ord)
smry_lmer6_ord <- summary(lmer6_ord)

anova(lmer2_cat, lmer2_ord) # compare AIC for both models >>> ordinal better

#------------------------PLOT ZONE---------------------------------------------#
  
theme_set(theme_classic()) # set new global look for plots

## Simple scatterplot of male amplitude vs velocity
plot_ampVvel <- ggplot(waves_by_trial, 
                       aes(x = velocity_male, 
                           y = amplitude_male)) +
  geom_point() +
  xlab("velocity (degrees/s)") +
  ylab("amplitude (degrees)")

## Simple scatterplot of visual angle vs va velocity
plot_visVvel <- ggplot(waves_by_trial, 
                       aes(x = visual_angle, 
                           y = velocity_va)) +
  geom_point() +
  xlab("velocity (degrees/s)") +
  ylab("visual angle (degrees)")

## Boxplot wave amplitude by background categories
plot_amp_background <- ggplot(waves_by_trial, 
                              aes(x = treat, 
                                  y = amplitude_male)) +
  geom_point(size = 1) +
  geom_boxplot(alpha = 0) +
  xlab("background") +
  ylab("wave amplitude (degrees)")

## Boxplot visual angle by background categories
plot_vis_background <- ggplot(waves_by_trial, 
                              aes(x = treat, 
                                  y = visual_angle)) +
  geom_point(size = 1) +
  geom_boxplot(alpha = 0) +
  xlab("background") +
  ylab("visual angle (degrees)")

## Boxplot distance by background categories
plot_dist_background <- ggplot(waves_by_trial, 
                               aes(x = treat, 
                                   y = distance)) +
  geom_point(size = 1) +
  geom_boxplot(alpha = 0) +
  ylim(0, 50) +
  xlab("background") +
  ylab("distance (mm)")

## Boxplot distance at 1st wave by background categories
plot_dist1_background <- ggplot(waves_by_trial, 
                                aes(x = treat, 
                                    y = distance_1st)) +
  geom_point(size = 1) +
  geom_boxplot(alpha = 0) +
  ylim(0, 50) +
  xlab("background") +
  ylab("distance (mm)")

## Scatterplot of amplitude_male vs vs distance to female
plot_ampVdist <- ggplot(waves_by_trial, 
                        aes(x = distance, 
                            y = amplitude_male)) +
  geom_point() +
  xlim(0,50) +
  ylim(0,50) +
  xlab("distance (mm)") +
  ylab("wave amplitude (degrees)")

## Scatterplot of velocity_male vs vs distance to female
plot_velVdist <- ggplot(waves_by_trial, 
                        aes(x = distance, 
                            y = velocity_male)) +
  geom_point() +
  xlab("distance (mm)") +
  ylab("wave velocity (degrees/s)")

## Scatterplot of visual angle vs vs distance to female
plot_visVdist <- ggplot(waves_by_trial, 
                        aes(x = distance, 
                            y = visual_angle)) +
  geom_point() +
  xlim(0,50) +
  ylim(0,50) +
  xlab("distance (mm)") +
  ylab("visual angle (degrees)")

## Scatterplot of velocity_va vs distance to female
plot_vel_vaVdist <- ggplot(waves_by_trial, 
                        aes(x = distance, 
                            y = velocity_va)) +
  geom_point() +
  xlab("distance (mm)") +
  ylab("wave velocity (degrees/s)")

# plots to gauge normality
# ggdensity(waves_by_trial$visual_angle) # density plot
# ggqqplot(waves_by_trial$visual_angle) # quantile-quantile plot

## Save publication-ready plots -----------------------------------------------#

# ggsave wrapper to set default plot export settings
my.ggsave <-
  function(filename = default_name(plot),
           plot = plot,
           height = 3,
           width = 3,
           dpi = 300) {
    ggsave(
      filename = filename,
      plot = plot,
      height = height,
      width = width,
      dpi = dpi
    )
  }

my.ggsave("figures/amplitude vs distance.jpg", plot_ampVdist)
my.ggsave("figures/angle vs distance.jpg", plot_visVdist)
my.ggsave("figures/amplitude vs background.jpg", plot_amp_background)
my.ggsave("figures/angle vs background.jpg", plot_vis_background)
my.ggsave("figures/distance vs background.jpg", plot_dist_background)

#------------------------END MATTER--------------------------------------------#

write.csv(waves_by_trial, file = "waves_by_trial.csv")
save.image("arena.RData")
