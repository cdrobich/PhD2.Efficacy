

# packages ----------------------------------------------------------------

library(vegan)
library(agricolae) #skewness, kurtosis, Tukeys
library(tidyverse)
library(car)
library(ggpubr)
library(Hmisc)
library(patchwork)

# data --------------------------------------------------------------------

Efficacy <- read.csv("Data/Efficacy_univariate_2016_2018.csv")
Efficacy

LongPoint <- Efficacy %>% filter(Location == "Long Point",
                                 Year == "one")

live <- lm(LiveStem ~ Treatment, data = LongPoint)
summary(live)
Anova(live, type = 2)

#Anova Table (Type II tests)
#
#Response: LiveStem
#Sum Sq Df F value Pr(>F)
#Treatment   62.5  1  0.3066  0.583
#Residuals 7746.6 38 

depth <- lm(Depth ~ Treatment, data = LongPoint)
summary(depth)
Anova(depth, type = 2)

#Anova Table (Type II tests)
#
#Response: Depth
#Sum Sq Df F value Pr(>F)
#Treatment    4.9  1   0.089  0.767
#Residuals 2091.1 38


height <- lm(CanopyH ~ Treatment, data = LongPoint)
summary(height)
Anova(height, type = 2)

#Response: CanopyH
#           Sum Sq Df F value Pr(>F)
#Treatment   1513  1  0.6799 0.4148
#Residuals  84557 38

s <- ggplot(LongPoint, aes(x = Treatment, y = LiveStem)) +
  geom_violin(trim = FALSE) +
  labs(x = " ",
       y = "Live Stem density")


d <- ggplot(LongPoint, aes(x = Treatment, y = Depth)) +
  geom_violin(trim = FALSE) +
  labs(x = " ",
       y = "Water Depth (cm)")

c <- ggplot(LongPoint, aes(x = Treatment, y = CanopyH)) +
  geom_violin(trim = FALSE) + 
  labs(x = " ",
       y = "Canopy Height (cm)")

  

s + d + c + 
  plot_annotation(tag_levels = "A")
