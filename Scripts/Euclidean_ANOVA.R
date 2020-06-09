
## Euclidean distances of treatment sites to reference and control centroids (NMDS coordinates)

Distances <- read.csv("Data/Distances_NMS_rares_1JUN20.csv")
Distances

library(vegan)
library(agricolae) #skewness, kurtosis, Tukeys
library(tidyverse)
library(car) ## ANOVA function
library(Hmisc) ## average and stdev to figures
library(gridExtra) ## arranging figure panels


## Look at data

glimpse(Distances)

Distances$YEAR <- as.factor(Distances$YEAR)
Distances$TRTYR <- as.factor(Distances$TRTYR)


# Histograms of distances

colnames(Distances)

par(mfrow = c(2,2))

hist(Distances$ConDis, 
     xlab = "Distance to Control centroid", main = " ",
     border = "black",
     col = "white")

hist(Distances$CattDis, 
     xlab = "Distance to Emergent Marsh centroid", main = " ",
     border = "black",
     col = "white")

hist(Distances$MeadDis,
     xlab = "Distance to Medow marsh centroid", main = " ",
     border = "black",
     col = "white")

#### distance to meadow ####

Meadow <-lm(MeadDis ~ YEAR, data = Distances)

Anova(Meadow, type = "3")

# Response: MeadDis
#               Sum Sq   Df  F value    Pr(>F)    
# (Intercept)  106.941    1 529.822  < 2.2e-16 ***
#  YEAR         10.795    2  26.741  2.487e-10 ***
#  Residuals    24.221  120  

hoc <- HSD.test(Meadow, "YEAR", group = TRUE, console = TRUE) # post hoc test

#          MeadDis    groups
# 2016    1.6150283      a
# 2018    1.4594066      a
# 2017    0.9233981      b



plot(residuals(Meadow)~fitted(Meadow))

#### Meadow Distance Jitter plot ####


meadow <- ggplot(Distances, aes(x = YEAR, y = MeadDis)) 

meadows <- meadow + geom_jitter(aes(shape = YEAR, color = YEAR), position = position_jitter(0.2)) +
  theme_classic() +
  stat_summary(
    aes(shape = YEAR),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange", size = 0.6,
    position = position_dodge(0.8)
  ) +
  labs(x = " ",
       y = expression(paste("Distance to Meadow Marsh Centroid"))) +
  theme(legend.position = "none")

MeadowRef <- meadows +  scale_color_manual(values = c(c("#5ab4ac","#5ab4ac", "#5ab4ac"))) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

MeadowRef

ggsave("Figures/Meadow_distances.JPEG")

#### distance to cattail ####

Cattail <-lm(CattDis ~ YEAR, data = Distances)

Anova(Cattail, type = "3")

#Response: CattDis
#              S  um Sq  Df  F value    Pr(>F)    
#(Intercept)   132.047   1 1454.773  < 2.2e-16 ***
#  YEAR          6.882   2   37.911  1.734e-13 ***
#  Residuals    10.892 120 


hoc <- HSD.test(Cattail, "YEAR", group = TRUE, console = TRUE)

# Emergent marsh groups
# 2016 1.794618      a
# 2018 1.378049      b
# 2017 1.237561      b

plot(residuals(Cattail)~fitted(Cattail))

### Emergent marsh jitter plot ####


cattail <- ggplot(Distances, aes(x = YEAR, y = CattDis)) 

cattails <- cattail + geom_jitter(aes(shape = YEAR, color = YEAR), position = position_jitter(0.2)) +
  theme_classic() +
  stat_summary(
    aes(shape = YEAR),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange", size = 0.6,
    position = position_dodge(0.8)
  ) +
  labs(x = " ",
       y = expression(paste("Distance to Emergent Marsh Centroid"))) +
  theme(legend.position = "none")

EmergentRef <- cattails +  scale_color_manual(values = c("#5ab4ac","#5ab4ac", "#5ab4ac")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) 

EmergentRef

ggsave("Figures/Emergent marsh_distances.JPEG")

# one way ANOVA distance to control condition 

Control <-lm(ConDis ~ YEAR, data = Distances)

Anova(Control, type = "3")

# Response: ConDis
#              Sum Sq  Df  F value    Pr(>F)    
# (Intercept)  1.776   1   75.593    2.148e-14 ***
#  YEAR        63.328  2  1348.033  < 2.2e-16 ***
#  Residuals    2.819 120

hoc <- HSD.test(Control, "YEAR", group = TRUE, console = TRUE)

#          ConDis groups
# 2018 1.7527246      a
# 2017 1.7067002      a
# 2016 0.2081043      b

plot(residuals(Control)~fitted(Control))
plot(Control)

### control jitter plot ####

control <- ggplot(Distances, aes(x = YEAR, y = ConDis)) 

controls <- control + geom_jitter(aes(shape = YEAR, colour = YEAR), position = position_jitter(0.2)) +
  theme_classic() + 
  stat_summary(
    aes(shape = YEAR),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange", size = 0.6,
    position = position_dodge(0.8)
  ) +
  labs(x = " ",
       y = expression(paste("Distance to Control Centroid"))) +
  theme(legend.position = "none")

ControlRef <- controls + scale_color_manual(values = c("#5ab4ac","#5ab4ac", "#5ab4ac")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

ControlRef

ggsave("Figures/Control_Distances.JPEG")

##### panel #####

grid.arrange(ControlRef, EmergentRef, MeadowRef, ncol = 3)

