
library(vegan)
library(tidyverse)

species <- read.csv("Data/BACI_reference.csv")


spp <- species[1:290, 10:50]
glimpse(spp)

sp.env <- species[1:290, 1:6]
sp.env$YEAR <- as.factor(sp.env$YEAR)
sp.env$TRTYR <- as.factor(sp.env$TRTYR)
sp.env$Treat <- as.factor(sp.env$Treat)

unique(sp.env$Treat)

sp.env <- sp.env %>% #rename the factors
  mutate(Treat = fct_recode(Treat,
                            "Control" = "3",
                            "Treatment" = "4",
                            "Meadow" = "5",
                            "Emergent" = "6")) 

# Shannon Weiner

H <- diversity(spp)

sp.env$H <- H

# Pielou's J 
J <- H/ log(specnumber(spp)) 

sp.env$J <- J


# Species richness

S1 <- specnumber(spp) # species richness with vegan
S2 <- rowSums(spp >0) # species richness like excel

sp.env$S1 <- S1
sp.env$S2 <- S2


sp.env %>% group_by(Treat, YEAR) %>% 
  summarise(SW.avg = mean(H),
            SW.sd = sd(H),
            J.avg = mean(J),
            J.sd = sd(J),
            S.avg = mean(S1),
            S.sd = sd(S1),
            s2.avg = mean(S2))
  
# Treat      YEAR SW.avg SW.sd   J.avg    J.sd S.avg  S.sd s2.avg
#1 Control    2016 0.145  0.283 NaN     NaN     1.59  1.71   1.59 
#2 Control    2017 0.205  0.389 NaN     NaN     1.41  1.39   1.41 
#3 Control    2018 0.135  0.319 NaN     NaN     1.13  1.21   1.13 

#4 Treatment  2016 0.280  0.495 NaN     NaN     1.76  1.51   1.76 
#5 Treatment  2017 0.0967 0.195 NaN     NaN     1.46  0.977  1.46 
#6 Treatment  2018 0.0306 0.103 NaN     NaN     1.12  0.600  1.12 

#7 Meadow     2017 0.958  0.434 0.584   0.251   5.27  2.02   5.27 
#8 Meadow     2018 1.15   0.593 NaN     NaN     5.47  2.45   5.47 

#9 Emergent   2017 0.0755 0.179 NaN     NaN     0.733 0.799  0.733
#10 Emergent   2018 0.0684 0.167 NaN     NaN     0.667 0.816  0.667


