
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


unique(Efficacy$Treatment) # check there is only Control and Treatment 

# Exploring the Data; response variables 

par(mfrow = c(2,2))

hist(Efficacy$LiveStem, 
     xlab = "Live stem (m-2))", main = " ",
     border = "black",
     col = "white")

hist(Efficacy$TotalStem, 
     xlab = "Total stem (m-2))", main = " ",
     border = "black",
     col = "white")


hist(Efficacy$CanopyH, 
     xlab = "Canopy height (cm))", main = " ",
     border = "black",
     col = "white")

hist(Efficacy$Light, 
     xlab = "PAR", main = " ",
     border = "black",
     col = "white")

## Perform log10 transformation on "Light"

Efficacy$logLight <- log10(Efficacy$Light)

# see how it looks now

hist(Efficacy$logLight, 
     xlab = "PAR", main = " ",
     border = "black",
     col = "white")


##### Two-way ANOVAs ####
# Assess if there is a difference in variables related to Phragmites 
# in treated plots after treatment occurs

#### Live Phrag stem density, two-way ANOVA ####

LiveANOVA<-lm(LiveStem~Year*Treatment, data = Efficacy)
summary(LiveANOVA)

Anova(LiveANOVA, type = "3") # apply Type III sums of square

#                Sum Sq  Df  F value  Pr(>F)    
# (Intercept)     47078   1 322.9347 < 2e-16 ***
#  Year              989   2   3.3931 0.03527 *  
#  Treatment         102   1   0.6981 0.40429    
#  Year:Treatment  16123   2  55.2985 < 2e-16 ***
#  Residuals       33967 233       

plot(residuals(LiveANOVA)~fitted(LiveANOVA)) # plot ANOVA residuals (look ok)


# Average and standard deviation among groups

Efficacy %>% group_by(Treatment, Year) %>% summarise(LiveStem.avg = mean(LiveStem),
                                               LiveStem.sd = sd(LiveStem)) 

# Treatment Year  LiveStem.avg LiveStem.sd
# Control   one          34.7       13.1  
# Control   three        29.3       12.6  
# Control   two          35.9       16.1  
# Treatment one          37         15.8  
# Treatment three       1.32       5.55 
# Treatment two         .1        0.632

### Live Stem ANOVA figure ####

## Create column that combines treatment and  year

Effic <- Efficacy %>% 
  unite("trtyr", Treatment:Year, remove = FALSE) 

## Make figures

Live <- ggplot(Effic, aes(x = Date, y = LiveStem))

Lives <- Live + geom_jitter(
  aes(shape = Treatment, color = Treatment), 
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size = 2) +
  theme_classic() +
  stat_summary(
    aes(shape = Treatment),
    fun.data = "mean_sdl", fun.args = list(mult = 1), # average and standard deviation
    geom = "pointrange", size = 0.6,
    position = position_dodge(0.8)
  ) +
  labs(x = " ",
       y = expression(paste("Live Stem Density per m"^-2))) 

LiveStems <- Lives + scale_color_manual(values = c("#d8b365","#5ab4ac")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

LiveStems


##### Total Phragmites stem density, two-way ANOVA ####

TotalANOVA<-lm(TotalStem ~ Year*Treatment, data = Efficacy)
TotalANOVA

Anova(TotalANOVA, type = "3") # Type III Sums of Squares

# Response: TotalStem
# Sum Sq  Df  F value    Pr(>F)    
# (Intercept)    408208   1 186.5056 < 2.2e-16 ***
#  Year             1283   2   0.2932    0.7462    
# Treatment         670   1   0.3061    0.5806    
# Year:Treatment  79669   2  18.1999 4.526e-08 ***
#  Residuals      509971 233  

par(mfrow = c(1,1))
plot(residuals(TotalANOVA)~fitted(TotalANOVA)) # plots residuals

# Average and standard deviation among groups

Efficacy %>% group_by(Treatment, Year) %>% summarise(TotalStem.avg = mean(TotalStem),
                                                     TotalStem.sd = sd(TotalStem)) 


# Treatment Year  TotalStem.avg TotalStem.sd
# 1 Control   one           102.          46.3
# 2 Control   three         104.          49.9
# 3 Control   two           110.          50.2
# 4 Treatment one           108.          47.5
# 5 Treatment three          23.8         30.7
# 6 Treatment two            50.8         52.8

#### Total Stem two-way ANOVA figure ####

Total <- ggplot(Effic, aes(x = Date, y = TotalStem))

Totals <- Total + geom_jitter(
  aes(shape = Treatment, color = Treatment), 
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size = 2) +
  theme_classic() +
  stat_summary(     
    aes(shape = Treatment),
    fun.data = "mean_sdl", fun.args = list(mult = 1), 
    geom = "pointrange", size = 0.6,
    position = position_dodge(0.8)
  ) +
  labs(x = " ",
       y = expression(paste("Total Stem Density per m"^-2)))


TotalStems <- Totals + scale_color_manual(values = c("#d8b365","#5ab4ac")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

TotalStems


##### Canopy Height, two-way ANOVA ####

CanopyHANOVA<-lm(CanopyH ~ Year*Treatment, data = Efficacy)

Anova(CanopyHANOVA, type = "3")

# Response: CanopyH
#Sum Sq  Df   F value Pr(>F)    
# (Intercept)    5098415   1 1181.0195 <2e-16 ***
# Year               516   2    0.0598 0.9420    
# Treatment          198   1    0.0459 0.8305    
# Year:Treatment 1143343   2  132.4245 <2e-16 ***
# Residuals      1005852 233

par(mfrow = c(1,1))
plot(residuals(CanopyHANOVA)~fitted(CanopyHANOVA))

Efficacy %>% group_by(Treatment, Year) %>% summarise(CanopyHeight.avg = mean(CanopyH),
                                                     CanopyHeight.sd = sd(CanopyH)) 

#Treatment Year  CanopyHeight.avg CanopyHeight.sd
# 1 Control   one              362.             61.1
# 2 Control   three            364.             50.0
# 3 Control   two              367.             61.1
# 4 Treatment one              358.             41.5
# 5 Treatment three            118.            101. 
# 6 Treatment two               38.0            64.0


##### Canopy height, two-way ANOVA figure ####

Height <- ggplot(Effic, aes(x = Date, y = CanopyH))

Heights <- Height + geom_jitter(
  aes(shape = Treatment, color = Treatment), 
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size = 2) +
  theme_classic() +
  stat_summary(
    aes(shape = Treatment),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange", size = 0.6,
    position = position_dodge(0.8)
  ) +
  labs(x = " ",
       y = expression(paste("Canopy Height (cm)")))

CanopyHeight <- Heights + scale_color_manual(values = c("#d8b365","#5ab4ac")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

CanopyHeight

#### Incident light, two-way ANOVA ####

LightsANOVA<-lm(logLight ~ Year*Treatment, data = Efficacy)

Anova(LightsANOVA, type = "3")

# Response: logLight
# Sum Sq  Df F value    Pr(>F)    
# (Intercept)     6.392   1 39.9988 1.285e-09 ***
# Year            0.747   2  2.3371   0.09887 .  
# Treatment       0.183   1  1.1451   0.28568    
# Year:Treatment 14.172   2 44.3400 < 2.2e-16 ***
# Residuals      37.235 233    

par(mfrow = c(1,1))
plot(residuals(LightsANOVA)~fitted(LightsANOVA))


Efficacy %>% group_by(Treatment, Year) %>% summarise(Light.avg = mean(Light),
                                                     Light.sd = sd(Light)) 

# Treatment Year  Light.avg Light.sd
# 1 Control   one        4.06     4.27
# 2 Control   three      7.01    10.4 
# 3 Control   two        7.89    13.9 
# 4 Treatment one        4.67     3.88
# 5 Treatment three     61.5     26.8 
# 6 Treatment two       55.7     27.4



##### Incident light, two-way ANOVA figure #####

Light <- ggplot(Effic, aes(x = Date, y = Light))

Lights <- Light + geom_jitter(
  aes(shape = Treatment, color = Treatment), 
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size = 2) +
  theme_classic() +
  stat_summary(
    aes(shape = Treatment),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange", size = 0.6,
    position = position_dodge(0.8)
  ) +
  labs(x = " ",
       y = expression(paste("Percent Incident Light"," ", " (", "umol  ",  s^-1, " ", m^-2, sep=")")))


logLight <- Lights + scale_color_manual(values = c("#d8b365","#5ab4ac")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

logLight 


#### ANOVA Figure Panel #####
library(gridExtra)

grid.arrange(LiveStems, TotalStems, CanopyHeight, logLight, nrow = 2)


anovas <- arrangeGrob(LiveStems, TotalStems, CanopyHeight, logLight, nrow = 2)
anovas

ggsave("Figures/ANOVA_panel.JPEG", anovas)

