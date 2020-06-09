Efficacy <- read.csv("Data/Efficacy_univariate_2016_2018.csv")
Efficacy


library(vegan)
library(agricolae) #skewness, kurtosis, Tukeys
library(tidyverse)
library(car)
library(Hmisc)


## Examine Data

colnames(Efficacy)
glimpse(Efficacy)

Efficacy$Year <- as.factor(Efficacy$Year)
Efficacy$Date <- as.factor(Efficacy$Date)
Efficacy$Treatment <- as.factor(Efficacy$Treatment)

Efficacy$logLight <- log10(Efficacy$Light)
Efficacy

unique(Efficacy$Treatment)


### ANCOVA; live Phragmites stem density as a function of water depth (covariate) and treatment (categorical)

# Subset the data so it is only the "one year after" measurements

YearTwo <- subset(Efficacy, Year == "two", select = SiteID:logLight)

# histogram of live stems

hist(YearTwo$LiveStem, 
     xlab = "Live stem (m-2))", main = " ",
     border = "black",
     col = "white")

YearTwo$logLiveStem <- log10(YearTwo$LiveStem + 1)

hist(YearTwo$logLiveStem, 
     xlab = "log Live Stem (m-2))", main = " ",
     border = "black",
     col = "white") # does nothing helpful, just too many zeroes

# ANCOVA with interaction term

ANCOVA <- lm(LiveStem ~ Treatment * Depth, data = YearTwo)

Anova(ANCOVA, type = "3")

# Response: LiveStem
# Sum Sq Df F value    Pr(>F)    
# (Intercept)      1765.9  1 13.4028 0.0004618 ***
# Treatment         563.2  1  4.2748 0.0420856 *  
# Depth             165.3  1  1.2544 0.2662431    
# Treatment:Depth    67.4  1  0.5114 0.4767202    
# Residuals       10013.7 76 


# Remove the interaction term, since it was not significant 

ANCOVA1 <- lm(LiveStem ~ Depth + Treatment, data = YearTwo)

Anova(ANCOVA1, type = "3")

# Response: LiveStem
# Sum Sq Df  F value    Pr(>F)    
# (Intercept)  3391.3  1  25.9027 2.473e-06 ***
# Depth          98.1  1   0.7493    0.3894    
# Treatment   24819.3  1 189.5712 < 2.2e-16 ***
# Residuals   10081.1 77 

plot(residuals(ANCOVA1)~fitted(ANCOVA1)) # kind of wonky with the treated sites since there is no growth

YearTwo %>% group_by(Treatment) %>% summarise(Water.avg = mean(Depth),
                                     LiveStem.avg = mean(LiveStem),
                                     LiveStem.sd = sd(LiveStem))

#Treatment Water.avg LiveStem.avg LiveStem.sd
#  1 Control        43.5        35.9      16.1  
# 2 Treatment      48.4         0.1       0.632


### ANCOVA figure ####

line <- ggplot(YearTwo, aes(x = Depth, y = LiveStem, shape = Treatment, color = Treatment)) +
  geom_point(size = 2) +
  stat_smooth(method = lm, level = 0.95, colour = "#999999") +
  theme_classic() +
  scale_color_manual(values = c("#d8b365","#5ab4ac")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  labs(x = "Water Depth (cm)",
       y = expression(paste("Live Stem Density per m"^-2))) 


line 

