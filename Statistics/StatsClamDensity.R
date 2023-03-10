library(dplyr)
library(pscl)
library(car)
library(emmeans)

## Zero inflated benthic survey model
setwd("D:/Projects/ShiRP/ShiRP_Git/BenthicSurvey") #Change to 2022
benthic.site <- read.csv("Sites_2012-2021.csv")
benthic.site <- read.csv("Sites_2012-2021.csv")
benthic.clam <- read.csv("Clams_2012-2021.csv")


# Calculate hard clam stats (total recovered and density) ==========================================================

#   Reclassify clams in a new dataframe (to be used in "TOTAL PRE-MARKET SIZE CLAMS 
#     RECOVERED (BY YEAR AND BAY SIDE)" sub-header in this script)
age.total <- benthic.clam %>%
  mutate(age = case_when(
    SW <= 25 ~ "pre-market",
    SW > 25 ~ "market"))

# Hard clam density per m2 for each quadrat
clam.density <- benthic.clam %>%
  group_by(Year,bay.side, Site.ID, Quadrat, Quadrat.size) %>%
  summarize(nClams = sum(Count)) %>%
  mutate(Clams.m2 = nClams/Quadrat.size) %>%
  mutate(QuadratName = paste(Year, Site.ID, Quadrat))

# Make a quadrat name for "site details" dataset too
benthic.site <- benthic.site %>%
  mutate(QuadratName = paste(Year, Site.ID, Quadrat))

# Check that all levels of Site.ID in the clam dataset are also in the site details dataset
clam.density$`Site.ID` <- as.factor(clam.density$`Site.ID`)
benthic.site$Site.ID <- as.factor(benthic.site$Site.ID)

all(levels(clam.density$`Site.ID`) %in% levels(benthic.site$Site.ID))

clam.density.all <- full_join(benthic.site, clam.density)
clam.density.all <- select(clam.density.all, Year, Site.ID, bay.side, Quadrat, QuadratName, Clams.m2)
clam.density.all$Clams.m2[is.na(clam.density.all$Clams.m2)] <- 0


clams <- clam.density.all %>%
  ungroup() %>%
  transmute(Year = as.factor(Year), bay.side, Site.ID, clams=Clams.m2)

clam.fit <- glm(clams~Year+bay.side,family = poisson, data = clams)

clam.zinf <- zeroinfl(clams~Year+bay.side, data = clams)

summary(clam.zinf)

cnull <- update(clam.zinf, . ~ 1)

pchisq(2 * (logLik(clam.zinf) - logLik(cnull)), df = 2, lower.tail = FALSE)

vuong(clam.fit, clam.zinf)

Anova(clam.zinf)

clampair = emmeans(clam.zinf, specs = pairwise ~ Year:bay.side)
clampair$contrasts

## Separate in to Juveniles and Adults

clam.size <- clam.density.size.all
Juv <- clam.size %>%
  filter(SizeClass == "Pre_market") %>%
  ungroup() %>%
  transmute(Year = as.factor(Year), bay.side, Site.ID, clams=Clams.m2)
Adult <- clam.size %>%
  filter(SizeClass == "Market")%>%
  ungroup() %>%
  transmute(Year = as.factor(Year), bay.side, Site.ID, clams=Clams.m2)

## Juvenile zero inf model
juv.fit <- glm(clams~Year+bay.side,family = poisson, data = Juv)

juv.zinf <- zeroinfl(clams~Year+bay.side, data = Juv)

summary(juv.zinf)

jnull <- update(juv.zinf, . ~ 1)

pchisq(2 * (logLik(juv.zinf) - logLik(jnull)), df = 2, lower.tail = FALSE)

vuong(juv.fit, juv.zinf)

Anova(juv.zinf)

juvpair = emmeans(juv.zinf, specs = pairwise ~ Year:bay.side)
juvpair$contrasts

write.csv(juvpair$contrasts, file = "JuvenileContrasts", row.names = F)
## Adult Zero inf model
ad.fit <- glm(clams~Year+bay.side,family = poisson, data = Adult)

ad.zinf <- zeroinfl(clams~Year+bay.side, data = Adult)
ad.zinf.bay <- zeroinfl(clams~bay.side, data = Adult)
ad.zinf.year <- zeroinfl(clams~Year, data = Adult)

summary(ad.zinf)

anull <- update(ad.zinf, . ~ 1)

pchisq(2 * (logLik(ad.zinf) - logLik(anull)), df = 2, lower.tail = FALSE)
pchisq(2 * (logLik(ad.zinf) - logLik(ad.zinf.bay)), df = 2, lower.tail = FALSE)

vuong(ad.zinf, anull)

Anova(ad.zinf)

adpair = emmeans(ad.zinf, specs = pairwise ~ Year:bay.side)
adpair$contrasts
