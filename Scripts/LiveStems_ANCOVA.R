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

#Response: LiveStem
#                   Sum Sq Df F value    Pr(>F)    
#  (Intercept)     2538.9  1 21.8070 1.274e-05 ***
#  Treatment        988.5  1  8.4902  0.004687 ** 
#  Depth              8.8  1  0.0757  0.783910    
#  Treatment:Depth    5.0  1  0.0430  0.836327    
# Residuals       8848.4 76  

# Remove the interaction term, since it was not significant 

ANCOVA1 <- lm(LiveStem ~ Depth + Treatment, data = YearTwo)

Anova(ANCOVA1, type = "3")

#Anova Table (Type III tests)
#Response: LiveStem
#Sum Sq Df  F value    Pr(>F)    
#(Intercept)  4562.3  1  39.6791 1.706e-08 ***
#  Depth           4.0  1   0.0344    0.8534    
# Treatment   26047.0  1 226.5367 < 2.2e-16 ***
#  Residuals    8853.4 77

plot(residuals(ANCOVA1)~fitted(ANCOVA1)) # kind of wonky with the treated sites since there is no growth

YearTwo %>% group_by(Treatment) %>% summarise(Water.avg = mean(Depth),
                                     LiveStem.avg = mean(LiveStem),
                                     LiveStem.sd = sd(LiveStem))

#Treatment Water.avg LiveStem.avg LiveStem.sd
#  1 Control        43.9      36.8         15.3  
#  2 Treatment      47.8       0.0976       0.625


### ANCOVA figure ####

Figure <- ggplot(YearTwo, aes(x = Depth, y = LiveStem, shape = Treatment, color = Treatment)) 


Figures <- Figure + geom_point(size = 2) +
  stat_smooth(method = lm, level = 0.95, colour = "#999999") +
  theme_classic() +
  scale_color_manual(values = c("#d8b365","#5ab4ac")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18)) +
  labs(x = "Water Depth (cm)",
       y = expression(paste("Live Stem Density per m"^-2))) 


Figures

ggsave("Figures/Stems_ANCOVA.JPEG")
