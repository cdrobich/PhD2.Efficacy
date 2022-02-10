

library(vegan)
library(agricolae) #skewness, kurtosis, Tukeys
library(tidyverse)
library(car)
library(ggpubr)
library(Hmisc)


Efficacy <- read.csv("Data/Efficacy_univariate_2016_2018.csv")
Efficacy

## Examine Data

colnames(Efficacy)
glimpse(Efficacy)

Efficacy$Year <- as.factor(Efficacy$Year)
Efficacy$Date <- as.factor(Efficacy$Date)
Efficacy$Treatment <- as.factor(Efficacy$Treatment)

Effic <- Efficacy %>% 
  unite("trtyr", Treatment:Year, remove = FALSE) 

Efficacy$logLight <- log10(Efficacy$Light)

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


#Response: LiveStem
#                Sum Sq  Df  F value  Pr(>F)    
#  (Intercept)     47078   1 339.7960 < 2e-16 ***
#  Year             1002   2   3.6150 0.02844 *  
#  Treatment         102   1   0.7345 0.39231    
#  Year:Treatment  16795   2  60.6120 < 2e-16 ***
#  Residuals       32281 233   


plot(residuals(LiveANOVA)~fitted(LiveANOVA)) # plot ANOVA residuals (look ok)


# Average and standard deviation among groups

Efficacy %>% group_by(Treatment, Year) %>% summarise(LiveStem.avg = mean(LiveStem),
                                               LiveStem.sd = sd(LiveStem),
                                               Live.min = min(LiveStem),
                                               Live.max = max(LiveStem)) 

#Treatment   Year  LiveStem.avg LiveStem.sd Live.min Live.max
# 1 Control   one        34.7         13.1         13       68
#2 Control   three      29.8         12.3         10       66
#3 Control   two        36.8         15.3         14       82
#4 Treatment one        37           15.8         11       81
#5 Treatment three       1.51         5.61         0       28
#6 Treatment two         0.0976       0.625        0        4

# 99.7% reduction in year one, and 95.9% reduction in year two (compared to before)

### Live Stem ANOVA figure ####

## Create column that combines treatment and  year


## Make figures

Live <- ggplot(Effic, aes(x = Date, y = LiveStem))

Lives <- Live + geom_jitter(
  aes(shape = Treatment, color = Treatment), 
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size = 3) +
  theme_classic() +
  stat_summary(
    aes(shape = Treatment),
    fun.data = "mean_sdl", fun.args = list(mult = 1), # average and standard deviation
    geom = "pointrange", size = 1,
    position = position_dodge(0.8)
  ) +
  labs(x = " ",
       y = expression(paste("Live Stem Density m"^-2))) 

LiveStems <- Lives + scale_color_manual(values = c("#969696","#006d2c")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  ylim(-10, 100) 
LiveStems


# for National Phrag presentation -----------------------------------------
str(Effic)

lives.plot <- ggplot(Effic, aes(x = Date, y = LiveStem, fill = Treatment))+
  geom_boxplot(colour = "white", alpha = 0.8) +
  theme_classic() +
  labs(x = " ",
       y = expression(paste("Live Stem Density m"^-2))) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  ylim(-10, 100) +
  theme(axis.title.y = element_text(colour = "white"),
        legend.title = element_blank(),
        axis.text.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.line = element_line(colour = "white")) +
  theme(panel.background = element_rect(
    fill = 'black'),
    plot.background = element_rect(fill = 'black')) +
  scale_fill_manual(values = c("#FE7F2D", "#619B8A")) +
  scale_shape_manual(values = c(21,22))



##### Total Phragmites stem density, two-way ANOVA ####

TotalANOVA<-lm(TotalStem ~ Year*Treatment, data = Efficacy)
TotalANOVA

Anova(TotalANOVA, type = "3") # Type III Sums of Squares

#Response: TotalStem
#                 Sum Sq  Df  F value    Pr(>F)    
#  (Intercept)    408208   1 190.1483 < 2.2e-16 ***
#  Year             1924   2   0.4482    0.6394    
#  Treatment         670   1   0.3121    0.5769    
#  Year:Treatment  82836   2  19.2931 1.765e-08 ***
#  Residuals      500201 233   

par(mfrow = c(1,1))
plot(residuals(TotalANOVA)~fitted(TotalANOVA)) # plots residuals

# Average and standard deviation among groups

Efficacy %>% group_by(Treatment, Year) %>% summarise(TotalStem.avg = mean(TotalStem),
                                                     TotalStem.sd = sd(TotalStem),
                                                     min = min(TotalStem),
                                                     max = max(TotalStem)) 


#Treatment Year  TotalStem.avg TotalStem.sd   min   max
#<fct>     <fct>         <dbl>        <dbl> <int> <int>
#1 Control   one           102.          46.3    20   193
#2 Control   three         105.          49.5    37   200
#3 Control   two           112.          49.0    15   229
#4 Treatment one           108.          47.5    23   229
#5 Treatment three          24.3         30.5     0   120
#6 Treatment two            50.2         52.3     0   198

#### Total Stem two-way ANOVA figure ####

Total <- ggplot(Effic, aes(x = Date, y = TotalStem))

Totals <- Total + geom_jitter(
  aes(shape = Treatment, color = Treatment), 
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size = 3) +
  theme_classic() +
  stat_summary(     
    aes(shape = Treatment),
    fun.data = "mean_sdl", fun.args = list(mult = 1), 
    geom = "pointrange", size = 1,
    position = position_dodge(0.8)
  ) +
  labs(x = " ",
       y = expression(paste("Total Stem Density m"^-2)))


TotalStems <- Totals + scale_color_manual(values = c("#969696","#006d2c")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  ylim(-10, 250)

TotalStems


##### Canopy Height, two-way ANOVA ####

CanopyHANOVA<-lm(CanopyH ~ Year*Treatment, data = Efficacy)

Anova(CanopyHANOVA, type = "3")

#Response: CanopyH
#                Sum Sq  Df   F value Pr(>F)    
#  (Intercept)    5098415   1 1217.4747 <2e-16 ***
#  Year              2237   2    0.2671 0.7659    
#  Treatment          198   1    0.0473 0.8279    
#  Year:Treatment 1157902   2  138.2504 <2e-16 ***
#  Residuals       975733 233 

par(mfrow = c(1,1))
plot(residuals(CanopyHANOVA)~fitted(CanopyHANOVA))

Efficacy %>% group_by(Treatment, Year) %>% summarise(CanopyHeight.avg = mean(CanopyH),
                                                     CanopyHeight.sd = sd(CanopyH),
                                                     min = min(CanopyH),
                                                     max = max(CanopyH)) 

#Treatment Year  CanopyHeight.avg CanopyHeight.sd   min   max

#1 Control   one              362.             61.1   237   526
#2 Control   three            367.             47.4   260   460
#3 Control   two              372.             50.4   270   470
#4 Treatment one              358.             41.5   285   470
#5 Treatment three            121.            102.      0   275
#6 Treatment two               40.7            65.5     0   207


##### Canopy height, two-way ANOVA figure ####

Height <- ggplot(Effic, aes(x = Date, y = CanopyH))

Heights <- Height + geom_jitter(
  aes(shape = Treatment, color = Treatment), 
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size = 3) +
  theme_classic() +
  stat_summary(
    aes(shape = Treatment),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange", size = 1,
    position = position_dodge(0.8)
  ) +
  labs(x = " ",
       y = expression(paste("Canopy Height (cm)")))

CanopyHeight <- Heights + scale_color_manual(values = c("#969696","#006d2c")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  ylim(-100, 600)

CanopyHeight

#### Incident light, two-way ANOVA ####

LightsANOVA<-lm(logLight ~ Year*Treatment, data = Efficacy)

Anova(LightsANOVA, type = "3")

#Response: logLight
#Sum Sq  Df F value    Pr(>F)    
# (Intercept)     6.392   1 43.4191 2.915e-10 ***
# Year            0.500   2  1.6969    0.1855    
# Treatment       0.183   1  1.2431    0.2660    
# Year:Treatment 15.077   2 51.2076 < 2.2e-16 ***
#  Residuals      34.302 233                      


par(mfrow = c(1,1))
plot(residuals(LightsANOVA)~fitted(LightsANOVA))


Efficacy %>% group_by(Treatment, Year) %>% summarise(Light.avg = mean(Light),
                                                     Light.sd = sd(Light),
                                                     min = min(Light),
                                                     max = max(Light)) 

#Treatment Year  Light.avg Light.sd    min   max
#<fct>     <fct>     <dbl>    <dbl>  <dbl> <dbl>
#1 Control   one        4.06     4.27  0.36   17.0
#2 Control   three      6.05     8.58  0.653  50.6
#3 Control   two        6.04     7.47  0.62   33.8
#4 Treatment one        4.67     3.88  0.3    13.6
#5 Treatment three     61.1     26.6   9.03   99.4
#6 Treatment two       56.3     27.4  11.0    99.6

##### Incident light, two-way ANOVA figure #####

Light <- ggplot(Effic, aes(x = Date, y = Light))

Lights <- Light + geom_jitter(
  aes(shape = Treatment, color = Treatment), 
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size = 3) +
  theme_classic() +
  stat_summary(
    aes(shape = Treatment),
    fun.data = "mean_sdl", fun.args = list(mult = 1),
    geom = "pointrange", size = 1,
    position = position_dodge(0.8)
  ) +
  labs(x = " ",
       y = expression(paste("Percent Incident Light"," ", " (", "\u00B5mol  ",  s^-1, " ", m^-2, sep=")")))


logLight <- Lights + scale_color_manual(values = c("#969696","#006d2c")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

logLight 


#### ANOVA Figure Panel #####
library(ggpubr)


anovas <- ggarrange(LiveStems, TotalStems,
          CanopyHeight, logLight,
          common.legend = TRUE,
          legend = "bottom",
          labels = "AUTO",
          hjust = -7,
          vjust = 2)

anovas

ggsave("Figures/ANOVA_panel.TIFF", anovas,
       dpi = 300)


##### Water depth and plots

water <- ggplot(Efficacy, aes(x = Date, y = Depth)) +
  geom_jitter(aes(shape = Treatment, color = Treatment), 
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size = 3) +
  theme_classic() +
  stat_summary(
    aes(shape = Treatment),
    fun.data = "mean_sdl", fun.args = list(mult = 1), # average and standard deviation
    geom = "pointrange", size = 1,
    position = position_dodge(0.8)
  ) +
  labs(x = " ",
       y = expression(paste("Water Depth (cm)"))) +
  scale_color_manual(values = c("#969696","#006d2c")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18)) +
  ylim(-10, 100)

water

ggsave("Figures/Water depth_trtyr.TIFF", water,
       dpi = 300)


##### Compare Total Stems among parks #####
After <- subset(Effic, 
                Treatment == "Treatment",
                select = SiteID:logLight)

After2 <- subset(After,
                 Year %in% c("two","three"))
                 


# compare

total.park <- lm(TotalStem ~ Date * Location, data = After2)
Anova(total.park, type = 3)

#Response: TotalStem
#               Sum Sq Df F value    Pr(>F)    
# (Intercept)     3892  1  3.9384   0.05071 .  
# Date            1243  1  1.2580   0.26546    
# Location       51297  1 51.9083 3.204e-10 ***
# Date:Location   4235  1  4.2855   0.04175 *  
# Residuals      77082 78


After2 %>% group_by(Location, Year) %>% summarise(total.avg = mean(TotalStem),
                                        total.sd = sd(TotalStem),
                                        max.total = max(TotalStem),
                                        min.total = min(TotalStem))

#Location      Year  total.avg  total.sd  max.total  min.total
#1 Long Point three       2.8     7.16        31         0
#2 Long Point two        14.0    21.5         88         0
#3 Rondeau    three      44.8    30.1        120         1
#4 Rondeau    two        84.7    49.6        198        11

location <- ggplot(After2, aes(x = Date, y = TotalStem )) +
  geom_jitter(aes(shape = Location, color = Location), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              size = 2) +
  theme_classic() +
  stat_summary(
    aes(shape = Location),
    fun.data = "mean_se", fun.args = list(mult = 1), # average and standard deviation
    geom = "pointrange", size = 0.6,
    position = position_dodge(0.8)
  ) +
  labs(x = " ",
       y = expression(paste("Total Stems (per m^2)"))) +
  scale_color_manual(values = c("#d8b365","#5ab4ac")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) 

location




live.park <- lm(LiveStem ~ Date * Location, data = After2)
Anova(live.park, type = 3)

#Response: LiveStem
#                Sum Sq Df F value Pr(>F)
#(Intercept)      0.00  1  0.0000 1.0000
#Date            14.40  1  0.8832 0.3502
#Location         0.37  1  0.0228 0.8804
#Date:Location    0.90  1  0.0552 0.8149
#Residuals     1271.68 78

After2 %>% group_by(Location, Year) %>% summarise(avg = mean(LiveStem),
                                                  sd = sd(LiveStem),
                                                  max = max(LiveStem),
                                                  min = min(LiveStem))


# A tibble: 4 x 6
# Groups:   Location [2]
#Location   Year    avg    sd   max   min
#1 Long Point three 1.2   4.92     22     0
#2 Long Point two   0     0         0     0
#3 Rondeau    three 1.81  6.31     28     0
#4 Rondeau    two   0.190 0.873     4     0



l.location <- ggplot(After2, aes(x = Date, y = LiveStem )) +
  geom_jitter(aes(shape = Location, color = Location), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              size = 2) +
  theme_classic() +
  stat_summary(
    aes(shape = Location),
    fun.data = "mean_se", fun.args = list(mult = 1), # average and standard deviation
    geom = "pointrange", size = 0.6,
    position = position_dodge(0.8)
  ) +
  labs(x = " ",
       y = expression(paste("Live Stems per m2"))) +
  scale_color_manual(values = c("#d8b365","#5ab4ac")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) 

l.location






### light and location ####

light.park <- lm(logLight ~ Year * Location, data = After2)
Anova(light.park, type = 3)

#Response: logLight
#               Sum Sq Df  F value    Pr(>F)    
# (Intercept)   64.209  1 968.1355 < 2.2e-16 ***
#  Date           0.164  1   2.4755  0.119680    
#  Location       0.470  1   7.0853  0.009434 ** 
#  Date:Location  0.561  1   8.4604  0.004726 ** 
#  Residuals      5.173 78 


After2 %>% group_by(Location, Year) %>% summarise(total.avg = mean(Light),
                                                  total.sd = sd(Light),
                                                  max.total = max(Light),
                                                  min.total = min(Light))

#Location   Year    total.avg  total.sd  max.total  min.total
#1 Long Point three      56.2     28.1      90.4      9.03
#2 Long Point two        68.8     26.2      99.6     19.8 
#3 Rondeau    three      65.7     24.9      99.4     24.9 
#4 Rondeau    two        44.3     23.2      85.2     11.0 
 
lightp <- ggplot(After2, aes(x = Date, y = Light )) +
  geom_jitter(aes(shape = Location, color = Location), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              size = 2) +
  theme_classic() +
  stat_summary(
    aes(shape = Location),
    fun.data = "mean_se", fun.args = list(mult = 1), # average and standard deviation
    geom = "pointrange", size = 0.6,
    position = position_dodge(0.8)
  ) +
  labs(x = " ",
       y = expression(paste("Incident Light (%)"))) +
  scale_color_manual(values = c("#d8b365","#5ab4ac")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) 

lightp

(reviewer.panel <- ggarrange(location, lightp,
          common.legend = TRUE,
          legend= "bottom", 
          labels = "AUTO",
          hjust = -7,
          vjust = 2))

ggsave("Figures/reviewer_comment_panel.jpeg",
       dpi = 300)

############## species richness ##########


rich <-lm(Rich ~ Year * Treatment, data = Efficacy)

Anova(rich, type = "3")

# Response: Rich
# Sum Sq  Df  F value  Pr(>F)    
# (Intercept)    600.23   1 149.8023 < 2e-16 ***
#  Year            24.08   2   3.0047 0.05148 .  
# Treatment        1.48   1   0.3692 0.54404    
# Year:Treatment  22.99   2   2.8686 0.05879 .  
# Residuals      933.59 233                     
# ---

par(mfrow = c(1,1))
plot(residuals(rich)~fitted(rich))
rich.st <- rstandard(rich)
qqnorm(rich.st)
qqline(rich.st)

Efficacy %>% group_by(Treatment, Year) %>% summarise(S.avg = mean(Rich),
                                                     S.sd = sd(Rich)) 

#Treatment Year  S.avg  S.sd
#<fct>     <fct> <dbl> <dbl>
#  1 Control   one    3.92  2.60
#  2 Control   three  2.82  1.67
# 3 Control   two    3.51  2.28
# 4 Treatment one    4.20  2.33
# 5 Treatment three  2.32  1.33
# 6 Treatment two    2.27  1.48



richness <- ggplot(Efficacy, aes(x = Date, y = Rich)) + 
  geom_jitter(
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
       y = expression(paste("Species Richness"," ", " (", "per", " ", m^-2, sep=")"))) +
  scale_color_manual(values = c("#d8b365","#5ab4ac")) +
  theme(panel.border = element_rect(fill = NA)) +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

richness 

ggsave("Figures/species_richness_ANOVA.jpeg", richness)
