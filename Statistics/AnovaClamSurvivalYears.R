library(dplyr)

#Anova for Weesuck & Tiana Clams grouped by Year
#Simplify Density data
SancSimp <- clamsanct.density %>%
  ungroup() %>%
  select(Year,Bay,N.Alive.m2)

#Separate by Bay
AnWee <- SancSimp %>%
  filter(Bay == "Weesuck Creek") %>%
  mutate(Year = as.factor(Year))

AnTia <- SancSimp %>%
  filter(Bay == "Tiana Bay") %>%
  mutate(Year = as.factor(Year))

#Perform ANOVA for each bay on year
lm.wee<-lm(N.Alive.m2~Year, data=AnWee)
summary(lm.wee)
anova.wee<-anova(lm.wee)
anova.wee$`Pr(>F)`

lm.tia<-lm(N.Alive.m2~Year, data=AnTia)
summary(lm.tia)
anova(lm.tia)

#Effect sizes
library(lsr)

etaSquared(lm.wee)

etaSquared(lm.tia)

## T-Test for survival rate
Site.ID <- c("TIANA5", "TIANA9A", "TIANA18", "WEE1A", "WEE6", "WEE11")
iDens <- c(28.15279, 28.20013, 32.89180, 25.50081, 31.76047, 26.83411)
PlantDens <- data.frame(Site.ID, iDens)

SitesInitial <- clamsanct.density.sites %>%
  group_by(Site.ID) %>%
  filter(Year == min(Year)) %>%
  ungroup() %>%
  select(Site.ID, Year.i = Year, Mean.i = Mean.m2, -Median.m2, -num, -SD.m2, -SEM)

Sites2021 <- clamsanct.density.sites %>%
  filter(Year==2021) %>%
  select(-Median.m2, -num, -SD.m2, -SEM)

Data2021 <- left_join(Sites2021, SitesInitial)

diff <- Data2021$Mean.m2 - Data2021$Mean.i
diff

shapiro.test(diff)

t.test(Data2021$Mean.m2, Data2021$Mean.i, paired = T, alternative = "two.sided")

mean((Data2021$Mean.m2-Data2021$Mean.i)/Data2021$Mean.i)*100
