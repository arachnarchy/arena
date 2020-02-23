## Run statistics and plot long distance waving data. 
## Needs "waves.csv" file produced by "build_spider.r"

##-----------------------LOAD PACKAGES------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(utils)
library(lme4) # for mixed models
library(lmerTest) # for Satterthwaite approximation to get p-values in lmer
library(ggpubr) # for density and qq plots
library(nlstools) # for CI on nonlinear fit

theme_set(theme_classic()) # set new global look for plots

##-----------------------READ & WRANGLE DATA------------------------------------

waves <- read.csv("waves.csv", header = TRUE)


# add trial ID
waves$trial_ID <- paste0(waves$id, " ", waves$treat)
waves_by_trial$trial_n <- seq(1:65)

# remove that one trial with only 2 waves
waves <- waves[!waves$trial_ID == "1856 0x",]

# add time column
trials24 <- c("1854 0x","1854 white","1856 0x","1863 0x","1863 white",
              "1864 white","1864 0x","1866 white","1895 0x","1895 white",
              "1927 white", "1929 0x","1929 white","1939 0x","1927 0x",
              "1939 white","1946 0x","1946 white","1947 0x","1967 0x",
              "1984 0x","1974 white","1974 0x","1927 1x","1988 white",
              "1988 0x")

trials30 <- c("1864 1.5x","1867 1x")

for(i in 1:nrow(waves)) {
  if (waves$trial_ID[i] %in% trials24) {
    waves$fps[i] <- 24
  } else if (waves$trial_ID[i] %in% trials30) {
    waves$fps[i] <- 30
  } else {
    waves$fps[i] <- 120
  }
  
  waves$time[i] <-
    1000 / waves$fps[i] * 
    (waves$start[i] - min(waves$start[waves$trial_ID == waves$trial_ID[i] &
                                               waves$leg == waves$leg[i]]))
}

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
  trial_duration = max(time),
  n.waves        = n(),
  wave_rate      = (n.waves / trial_duration) * 1000
)

waverate <-
  waves_by_trial[] %>% 
  group_by(id) %>% 
  summarise(waverate = mean(wave_rate)) %>% 
  summarize(wr = mean(waverate),sd = sd(waverate))

# simple up-downstroke summary stats without looking at backgrounds
updown_by_trial <- 
  waves %>% 
  group_by(id, stroke) %>% 
  summarize(amplitude_male = mean(amplitude.male),
            velocity_male = mean(velocity.male),
            trial_duration = max(time),
            n.waves        = n(),
            wave_rate      = (n.waves / trial_duration) * 1000)

updown_stats <- updown_by_trial %>%
  group_by(stroke) %>%
  summarise(
    waves.n = n(),
    amplitude = mean(amplitude_male),
    amplitude.sd = sd(amplitude_male),
    velocity = mean(velocity_male),
    velocity.sd = sd(velocity_male),
    rate = mean(wave_rate),
    rate.sd = sd(wave_rate)
  )

t.test(abs(wave_rate) ~ stroke, data = updown_by_trial, paired = TRUE)
t.test(abs(amplitude_male) ~ stroke, data = updown_by_trial, paired = TRUE)
t.test(abs(velocity_male) ~ stroke, data = updown_by_trial, paired = TRUE)

## add numeric re-coding of treatments (tested with many spacings, no diff)
waves_by_trial <- waves_by_trial %>%
  mutate(treat_n = ifelse(treat == "white", 0,
                          ifelse(treat == "0x", 1,
                                 ifelse(treat == "1x", 2, 3))))


##-----------------------STATS-------------------------------------------------

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


## Treatment effect comparison -------------------------------------------------

## Effects with treatment as categorical
lme_amp_treat <- lme(amplitude.male~treat, random=~1|id, data=waves)
lme_va_treat <- lme(visual.angle~treat, random=~1|id, data=waves)
lme_dist_treat <- lme(distance~treat, random=~1|id, data=waves)
lme_vm_treat <- lme(velocity.male~treat, random=~1|id, data=waves)

anova_lme_amp_treat <- anova(lme_amp_treat)
anova_lme_va_treat <- anova(lme_va_treat)
anova_lme_dist_treat <- anova(lme_dist_treat)
anova_lme_vm_treat <- anova(lme_vm_treat)

## Model fit to visual angle vs distance --------------------------------------#
stat.fun <- function(a, x){rad2deg(atan(a/x))} # static model
dyna.fun <- function(a, b, x){rad2deg(atan((a + (b * x))/x))} # dynamic model
dyna.fun.r <- function(a, x){rad2deg(atan((a + (0.02 * x))/x))} 

x <- waves_by_trial$distance
y <- waves_by_trial$visual_angle

# plot curve with guessed values until close
plot(x, y, xlim = c(0, 50), ylim = c(0, 20))
# curve(stat.fun(2, x), add = TRUE)
# curve(dyna.fun(0.5, 0.01, x), add = TRUE)
curve(
  rad2deg(atan((1.868 + .02 * x) / x)),
  xlim = c(0, 50),
  ylim = c(0, 20),
  add = TRUE
)
curve(
  rad2deg(atan((1.693381 + .02 * x) / x)),
  xlim = c(0, 50),
  ylim = c(0, 20),
  add = TRUE
)
curve(
  rad2deg(atan((2.043169 + .02 * x) / x)),
  xlim = c(0, 50),
  ylim = c(0, 20),
  add = TRUE
)

# run nonlinear regression fit
stat.fit <- nls(y ~ stat.fun(a, x), start = list(a = 5))
dyna.fit <- nls(y ~ dyna.fun(a, b, x), start = list(a = 1, b = .04))
dyna.fit.r <- nls(y ~ dyna.fun.r(a, x), start = list(a = 1))
coef.stat <- coef(stat.fit)
coef.dyna <-  coef(dyna.fit)
coef.dyna.r <-  coef(dyna.fit.r)

# confidence intervals of nls results
dyna.r.ci <- confint2(dyna.fit.r) # produces negative value for a

# save curves for ggplotting
dyna.r.curve <- as.data.frame(curve(from = 1, to = 50, dyna.fun.r(a = 1.858, x)))
dyna.r.ci.lo <- as.data.frame(curve(from = 1, to = 50, dyna.fun.r(a = 1.693381, x)))
dyna.r.ci.hi <- as.data.frame(curve(from = 1, to = 50, dyna.fun.r(a = 2.043169, x)))
stat.curve <- as.data.frame(curve(from = 1, to = 50, stat.fun(a = 1.858, x)))
  


#------------------------PLOT ZONE----------------------------------------------


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

## Violin wave amplitude by background categories
violin_amp_background <- ggplot(waves, 
                                aes(x = treat, 
                                    y = abs(amplitude.male))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab("background") +
  ylab("wave amplitude (degrees)")

effects_amp_background <- plot(Effect("treat",lme_amp_treat),
                               main="",
                               axes=list(
                                 x=list(treat=list(lab="background")),
                                 y=list(lab="wave amplitude (degrees)"))
)

## Violin visual angle by background categories
violin_vis_background <- ggplot(waves, 
                                aes(x = treat, 
                                    y = visual.angle)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  xlab("background") +
  ylab("perceived movement (degrees)")

effects_vis_background <- plot(Effect("treat",lme_va_treat),
                               main="",
                               axes=list(
                                 x=list(treat=list(lab="background")),
                                 y=list(lab="perceived movement (degrees)"))
)

## Violin distance by background categories
violin_dist_background <- ggplot(waves, 
                                 aes(x = treat, 
                                     y = distance)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  ylim(0, 50) +
  xlab("background") +
  ylab("distance (mm)")

effects_dist_background <- plot(Effect("treat",lme_dist_treat),
                                main="",
                                axes=list(
                                  x=list(treat=list(lab="background")),
                                  y=list(lab="distance (mm)"))
)

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
  ylab("perceived movement (degrees)")

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
  #geom_text(aes(label = trial_n, hjust= -0.25, vjust=0)) +
  geom_smooth(method='lm',color = "black", size = .5) +
  stat_poly_eq(formula = waves_by_trial$distance ~ waves_by_trial$amplitude_male,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0,50) +
  ylim(0,50) +
  xlab("distance (mm)") +
  ylab("wave amplitude (degrees)")

## Scatterplot of RAW amplitude_male vs vs distance to female (no avg by trial)
plot_RAWampVdist <- ggplot(waves, 
                        aes(x = distance, 
                            y = abs(amplitude.male),
                            color = trial_ID)) +
  #geom_point() +
  geom_smooth(method="lm", fill=NA) +
  xlim(0,60) +
  ylim(0,60) +
  xlab("distance (mm)") +
  ylab("wave amplitude (degrees)") +
  theme(legend.position="none")

## Lines of time vs distance to female (shows )
plot_distVtime <- ggplot(waves, 
                        aes(x = time, 
                            y = distance,
                            color = trial_ID)) +
  geom_line() +
  xlab("time") +
  ylab("distance") +
  theme(legend.position="none")

## Scatterplot of velocity_male vs vs distance to female
plot_velVdist <- ggplot(waves_by_trial, 
                        aes(x = distance, 
                            y = velocity_male)) +
  geom_point() +
  xlab("distance (mm)") +
  ylab("wave velocity (degrees/s)")

## Scatterplot of visual angle vs distance to female
plot_visVdist <- ggplot(waves_by_trial,
                        aes(x = distance,
                            y = visual_angle)) +
  geom_point() +
  geom_line(data = dyna.r.curve, aes(x = x, y = y, color = "dynamic wave")) +
  geom_line(data = dyna.r.ci.lo,
            aes(x = x, y = y, color = "dynamic wave"),
            size = .1) +
  geom_line(data = dyna.r.ci.hi,
            aes(x = x, y = y, color = "dynamic wave"),
            size = .1) +
  geom_line(data = stat.curve,
            aes(x = x, y = y, color = "static wave"),
            linetype = "dotted") +
  coord_cartesian(ylim = c(0, 15), xlim =c(0,50)) +
  theme(legend.position = c(.75, .9)) +
  theme(legend.title=element_blank()) +
  xlab("distance (mm)") +
  ylab("perceived movement (degrees)")

## Scatterplot of visual angle vs distance to female
plot_visVdist_nofit <- ggplot(waves_by_trial,
                        aes(x = distance,
                            y = visual_angle)) +
  geom_point() +
  coord_cartesian(ylim = c(0, 15), xlim =c(0,50)) +
  theme(legend.position = c(.75, .9)) +
  theme(legend.title=element_blank()) +
  xlab("distance (mm)") +
  ylab("perceived movement (degrees)")

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
           height = 4,
           width = 4,
           dpi = 300) {
    ggsave(
      filename = filename,
      plot = plot,
      height = height,
      width = width,
      dpi = dpi
    )
  }

my.ggsave("figures/amplitude vs distance.svg", plot_ampVdist)
my.ggsave("figures/angle vs distance.svg", plot_visVdist_nofit)
my.ggsave("figures/angle vs distance.svg", plot_visVdist)
my.ggsave("figures/amplitude vs background.svg", plot_amp_background)
my.ggsave("figures/distance vs background.svg", plot_dist_background)
my.ggsave("figures/angle vs background.svg", plot_vis_background)


my.ggsave("figures/angle vs distance.svg", plot_visVdist)

#------------------------END MATTER--------------------------------------------#

write.csv(waves_by_trial, file = "waves_by_trial.csv")
save.image("arena.RData")
