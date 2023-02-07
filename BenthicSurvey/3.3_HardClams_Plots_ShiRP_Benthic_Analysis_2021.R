# Analysis of ShiRP Benthic Survey Data for 2021 ShiRP Report
#   This script is for creating plots of hard clam data. There is some data frame manipulation, but it is done
#     in the specific plot's sub-header.

#   Preceding script: "3.2_HardClams_Densities_ShiRP_Benthic_Analysis_2021.R"


# Authors: Diana Chin, Dylan Cottrell, Flynn DeLany (based on code from Rebecca Kulp)

# Last Modified: 12/16/21


# NOTE: For the Here package to work appropriately, all called datasets must have a copy saved in the "Analysis" side
#   of the project. E.g. "Benthic_Survey_2012-2020.xlsx" should be saved in "Data -> "2021" and "Analysis" -> "2021"


# Load Data ========================================================================================================
clam.density.size.all <- read.csv("clam.density.size.all.csv")
age.total <- read.csv("age.total.csv")
clam.density.size <- read.csv("clam.density.size.csv")
benthic.site.smp <- read.csv("benthic.site.smp.csv")


# TOTAL PRE-MARKET SIZE CLAMS RECOVERED (BY YEAR) =====================================================================
#   Filter density df by age (pre-market vs. market) and calculate summary stats. When calculating mean densities, 
#     average together 2 replicate quadrats from each site.
mean.pre_market <- clam.density.size.all %>%
  filter(SizeClass == "Pre_market") %>%
  group_by(Year, Site.ID) %>%
  summarize(meanquads = mean(Clams.m2)) %>%
  group_by(Year) %>%
  summarize(Mean.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))
mean.pre_market$Year <- as.factor(mean.pre_market$Year)

mean.market <- clam.density.size.all %>%
  filter(SizeClass == "Market") %>%
  group_by(Year, Site.ID) %>%
  summarize(meanquads = mean(Clams.m2)) %>%
  group_by(Year) %>%
  summarize(Mean.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))
mean.market$Year <- as.factor(mean.market$Year)

# Plot
Y.lab <- expression("Juvenile Clam Abundance m"^-2)
X.lab <- c("Year")
colors <-c("dodgerblue3")

par(mar = c(5.1, 6.1, 4.1, 2.1), mgp = c(3.5, 0.75, 0)) 

ggplot(mean.pre_market, 
       aes(x = Year, y = Mean.m2)) + 
  geom_col(width = 0.65, fill = colors, color = "black") + 
  geom_errorbar(aes(ymin = Mean.m2 - SEM, ymax = Mean.m2 + SEM), width = 0.1) +
  labs(x = X.lab, y = Y.lab) +
  scale_x_discrete(breaks = levels(mean.pre_market$Year), labels = levels(mean.pre_market$Year)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 6),
                     breaks = seq(0, 6, 0.2)) +
  geom_vline(aes(xintercept = 5.5)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
  theme(axis.title.y = element_text(size = 16.5,
                                    margin = margin(1, 5, 0.5, 0.5),
                                    colour = "black"),
        axis.title.x = element_text(size = 18,
                                    margin = margin(8, 1, 2, 1),
                                    colour="black"),
        axis.text.y = element_text(size = 14,
                                   colour = c("black", NA)),
        axis.text.x = element_text(size = 14,
                                   colour = "black"),
        strip.text.x = element_text(size = 16, colour = "black"))
# use 6" height, 8" width for pdf


# TOTAL PRE-MARKET SIZE CLAMS RECOVERED (BY YEAR AND BAY SIDE) ========================================================
#   Calculate total juveniles recovered by year and save as variable (used for the overlying line plot)
pre_market.total <- age.total %>%
  filter(age == "pre-market") %>%
  group_by(Year) %>%
  summarise(count = sum(Count))
pre_market.total$Year <- as.factor(pre_market.total$Year)

#   Calculate total juveniles recovered by year AND BAY SIDE (used for bar plot)
pre_market.total.Bside <- age.total %>%
  filter(age == "pre-market") %>%
  group_by(Year, bay.side) %>%
  summarize(count = sum(Count))
pre_market.total.Bside$Year <- as.factor(pre_market.total.Bside$Year)

#     Need to add years with NA values in order to represent this nicely in the figure.
#       There's certainly a more elegant way to do this, but for not I will just make
#       a new data frame with the "NA" years, and then join this with the existing dataframe
Year <- c("2012", "2014")
bay.side <- c("E", "E")

pre_market.NAs <- data.frame(Year, bay.side)
pre_market.NAs$bay.side <- as.character(pre_market.NAs$bay.side)

pre_market.total.Bside <- bind_rows(pre_market.total.Bside, pre_market.NAs)

# Ok, finally time to make the figure
Y.lab <- expression("Number of Juvenile Clams")
X.lab <- c("Year")
line <- "black"

ggplot() + 
  geom_line(pre_market.total, mapping = aes(x = Year, y = count, group = 1)) +
  geom_point(pre_market.total, mapping = aes(x = Year, y = count, group = 1), color = "red") +
  geom_col(pre_market.total.Bside, mapping = aes(x = Year, y = count, fill = pre_market.total.Bside$bay.side),
           width = 0.65, position=position_dodge(), color = "black") +
  scale_fill_manual(values=c("dodgerblue3", "cyan")) +
  scale_x_discrete(name="Year") +
  scale_y_continuous(name = "Number of Juvenile Clams",
                     expand = c(0, 0),
                     limits = c(0, 600),
                     breaks = seq(0, 600, 20)) +
  geom_vline(aes(xintercept = 5.5)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
  theme(axis.title.y = element_text(size = 18,
                                    margin = margin(1, 5, 0.5, 0.5),
                                    colour = "black"),
        axis.title.x = element_text(size = 18,
                                    margin = margin(8, 1, 2, 1),
                                    colour="black"),
        axis.text.y = element_text(size = 14,
                                   colour = c("black",NA)),
        axis.text.x = element_text(size = 14,
                                   colour = "black"),
        strip.text.x = element_text(size = 16, colour = "black")) +
  labs(fill = "Region of Bay") +
  scale_color_manual(breaks=c("E", "W"),labels=c("Eastern", "Western"))

# TOTAL JUVENILE DENSITY (BY YEAR AND BAY SIDE) =======================================================================
#   Calculate total juvenile density by year AND BAY SIDE (used for bar plot)
pre_market.mean.Bside <- clam.density.size.all %>%
  filter(SizeClass == "Pre_market") %>%
  group_by(Year, Site.ID, Quadrat, bay.side) %>%
  summarize(meanquads = mean(Clams.m2)) %>%
  group_by(Year, bay.side) %>%
  summarize(Mean.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))
pre_market.mean.Bside$Year <- as.factor(pre_market.mean.Bside$Year)

#     Need to add years with NA values in order to represent this nicely in the figure.
#       There's certainly a more elegant way to do this, but for not I will just make
#       a new data frame with the "NA" years, and then join this with the existing dataframe
Year <- c("2012", "2014")
bay.side <- c("E", "E")

pre_market.dens.NAs <- data.frame(Year, bay.side)
pre_market.dens.NAs$bay.side <- as.character(pre_market.NAs$bay.side)

pre_market.mean.Bside <- bind_rows(pre_market.mean.Bside, pre_market.dens.NAs)

# Ok, finally time to make the figure
Y.lab <- expression("Juvenile Clam Density (individuals/m^2)")
X.lab <- c("Year")
line <- "black"

ggplot(pre_market.mean.Bside, aes(x = Year, y = Mean.m2, fill=bay.side)) +
  geom_col(width = 0.65, position=position_dodge(), color = 'black') +
  scale_fill_manual(values=c("dodgerblue3", "cyan")) +
  scale_x_discrete(name=X.lab) +
  scale_y_continuous(name = Y.lab,
                     expand = c(0, 0),
                     limits = c(0, 8),
                     breaks = seq(0, 8, 0.25)) +
  geom_errorbar(aes(ymin = Mean.m2 - SEM, ymax = Mean.m2 + SEM), width = 0.1,
                position=position_dodge(0.65)) +
  geom_vline(aes(xintercept = 5.5)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
  theme(axis.title.y = element_text(size = 12,
                                    margin = margin(1, 5, 0.5, 0.5),
                                    colour = "black"),
        axis.title.x = element_text(size = 18,
                                    margin = margin(8, 1, 2, 1),
                                    colour="black"),
        axis.text.y = element_text(size = 14,
                                   colour = c("black",NA)),
        axis.text.x = element_text(size = 14,
                                   colour = "black"),
        strip.text.x = element_text(size = 16, colour = "black")) +
  labs(fill = "Region of Bay") +
  scale_color_manual(breaks=c("E", "W"),labels=c("Eastern", "Western"))

# PRE-MARKET SIZE DENSITY AT ANNUAL SITES (BY YEAR) ====
#     This is a subset of only those sites that have been monitored annually since 2012 for 
#     W. Bay and since 2015 (for E. Bay)

#   First, filter appropriate sites
pre_market.annual <- clam.density.size.all %>%
  filter(Site.ID == "SGV 47" |
           Site.ID == "SGV 48" |
           Site.ID == "SGV 49" |
           Site.ID == "SGV 62" |
           Site.ID == "SGV 69" |
           Site.ID == "SGV 70" |
           Site.ID == "SGV 71" |
           Site.ID == "SGV 77" |
           Site.ID == "SGV 79" |
           Site.ID == "SGV 80" |
           Site.ID == "SGV 85" |
           Site.ID == "SGV 86" |
           Site.ID == "SGV 90" |
           Site.ID == "SGV 91" |
           Site.ID == "SGV 94" |
           Site.ID == "SGV 95" |
           Site.ID == "SGV CG1" |
           Site.ID == "SGV CG2" |
           Site.ID == "SGV CG3" |
           Site.ID == "SGV CG4" |
           Site.ID == "SGV COR" |
           Site.ID == "SGV FP" |
           Site.ID == "SGV LS1" |
           Site.ID == "SGV LS2" |
           Site.ID == "SGV LS3" |
           Site.ID == "SGV LS4" |
           Site.ID == "SGV SAND" |
           Site.ID == "SGV WI")

annual.pre_market.mean <- pre_market.annual %>%
  filter(SizeClass == "Pre_market") %>%
  group_by(Year, Site.ID) %>%
  summarize(meanquads = mean(Clams.m2)) %>%
  group_by(Year) %>%
  summarize(Mean.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))
annual.pre_market.mean$Year <- as.factor(annual.pre_market.mean$Year)

#     Need to add years with NA values in order to represent this nicely in the figure.
#       There's certainly a more elegant way to do this, but for not I will just make
#       a new data frame with the "NA" years, and then join this with the existing dataframe
Year <- c("2012", "2014")

annual.pre_market.dens.NAs <- data.frame(Year)

annual.pre_market.mean <- bind_rows(annual.pre_market.mean, annual.pre_market.dens.NAs)

# Ok, finally we can make the figure
Y.lab <- expression("Pre-market Size Clam Abundance m"^-2)
X.lab <- c("Year")

par(mar = c(5.1, 6.1, 4.1, 2.1), mgp = c(3.5, 0.75, 0)) 

ggplot(annual.pre_market.mean, 
       aes(x = Year, y = Mean.m2)) + 
  geom_col(width = 0.65, position=position_dodge()) +
  labs(x = X.lab, y = Y.lab) +
  scale_x_discrete("Year") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.9),
                     breaks = seq(0, 2, 0.1)) +
  geom_errorbar(aes(ymin = Mean.m2 - SEM, ymax = Mean.m2 + SEM), width = 0.1,
                position=position_dodge(0.65)) +
  geom_vline(aes(xintercept = 2.5)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
  theme(axis.title.y = element_text(size = 18,
                                    margin = margin(1, 5, 0.5, 0.5),
                                    colour = "black"),
        axis.title.x = element_text(size = 18,
                                    margin = margin(8, 1, 2, 1),
                                    colour="black"),
        axis.text.y = element_text(size = 14,
                                   colour = "black"),
        axis.text.x = element_text(size = 14,
                                   colour = "black"),
        strip.text.x = element_text(size = 16, colour = "black")) #+
  #labs(fill = "Region of Bay") +
  #scale_color_manual(breaks=c("E", "W"),labels=c("Eastern", "Western"))

# PRE-MARKET SIZE DENSITY AT ANNUAL SITES (BY YEAR AND BAY SIDE) ====
#     This is a subset of only those sites that have been monitored annually since 2012 for 
#     W. Bay and since 2015 (for E. Bay)

#   First, filter appropriate sites
pre_market.annual <- clam.density.size.all %>%
  filter(Site.ID == "SGV 47" |
           Site.ID == "SGV 48" |
           Site.ID == "SGV 49" |
           Site.ID == "SGV 62" |
           Site.ID == "SGV 69" |
           Site.ID == "SGV 70" |
           Site.ID == "SGV 71" |
           Site.ID == "SGV 77" |
           Site.ID == "SGV 79" |
           Site.ID == "SGV 80" |
           Site.ID == "SGV 85" |
           Site.ID == "SGV 86" |
           Site.ID == "SGV 90" |
           Site.ID == "SGV 91" |
           Site.ID == "SGV 94" |
           Site.ID == "SGV 95" |
           Site.ID == "SGV CG1" |
           Site.ID == "SGV CG2" |
           Site.ID == "SGV CG3" |
           Site.ID == "SGV CG4" |
           Site.ID == "SGV COR" |
           Site.ID == "SGV FP" |
           Site.ID == "SGV LS1" |
           Site.ID == "SGV LS2" |
           Site.ID == "SGV LS3" |
           Site.ID == "SGV LS4" |
           Site.ID == "SGV SAND" |
           Site.ID == "SGV WI")

annual.pre_market.mean.Bside <- pre_market.annual %>%
  filter(SizeClass == "Pre_market") %>%
  group_by(Year, Site.ID, bay.side) %>%
  summarize(meanquads = mean(Clams.m2)) %>%
  group_by(Year, bay.side) %>%
  summarize(Mean.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))
annual.pre_market.mean.Bside$Year <- as.factor(annual.pre_market.mean.Bside$Year)

#     Need to add years with NA values in order to represent this nicely in the figure.
#       There's certainly a more elegant way to do this, but for not I will just make
#       a new data frame with the "NA" years, and then join this with the existing dataframe
Year <- c("2012", "2014")
bay.side <- c("E", "E")

annual.pre_market.dens.NAs <- data.frame(Year, bay.side)
annual.pre_market.dens.NAs$bay.side <- as.character(pre_market.NAs$bay.side)

annual.pre_market.mean.Bside <- bind_rows(annual.pre_market.mean.Bside, annual.pre_market.dens.NAs)

# Ok, finally we can make the figure
Y.lab <- expression("Pre-market Size Clam Abundance m"^-2)
X.lab <- c("Year")

par(mar = c(5.1, 6.1, 4.1, 2.1), mgp = c(3.5, 0.75, 0)) 

ggplot(annual.pre_market.mean.Bside, 
       aes(x = Year, y = Mean.m2, fill = bay.side)) + 
  geom_col(width = 0.65, position=position_dodge()) +
  labs(x = X.lab, y = Y.lab) +
  scale_x_discrete("Year") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 2, 0.2)) +
  geom_errorbar(aes(ymin = Mean.m2 - SEM, ymax = Mean.m2 + SEM), width = 0.1,
                position=position_dodge(0.65)) +
  geom_vline(aes(xintercept = 2.5)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
  theme(axis.title.y = element_text(size = 18,
                                    margin = margin(1, 5, 0.5, 0.5),
                                    colour = "black"),
        axis.title.x = element_text(size = 18,
                                    margin = margin(8, 1, 2, 1),
                                    colour="black"),
        axis.text.y = element_text(size = 14,
                                   colour = "black"),
        axis.text.x = element_text(size = 14,
                                   colour = "black"),
        strip.text.x = element_text(size = 16, colour = "black")) +
  labs(fill = "Region of Bay") +
  scale_color_manual(breaks=c("E", "W"),labels=c("Eastern", "Western"))


# TOTAL ADULT DENSITY BY YEAR ========================================================================================
mean.market2 <- clam.density.size.all %>%
  filter(SizeClass == "Market") %>%
  group_by(Year, Site.ID) %>%
  filter(QuadratName != "2020 SGV 94 1" && QuadratName != "2020 SGV 94 2") %>%
  summarize(meanquads = mean(Clams.m2)) %>%
  group_by(Year) %>%
  summarize(Mean.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))
mean.market2$Year <- as.factor(mean.market$Year)
  

Y.lab <- expression("Adult Clam Abundance m"^-2)
X.lab <- c("Year")
colors <-c("dodgerblue3")

par(mar = c(5.1, 6.1, 4.1, 2.1), mgp = c(3.5, 0.75, 0)) 

ggplot(mean.market2, 
       aes(x = Year, y = Mean.m2)) + 
  geom_col(width = 0.65, fill = colors, color = "black") + 
  geom_errorbar(aes(ymin = Mean.m2 - SEM, ymax = Mean.m2 + SEM), width = 0.1) +
  labs(x = X.lab, y = Y.lab) +
  scale_x_discrete(breaks = levels(mean.market$Year), labels = levels(mean.market$Year)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 2.5),
                     breaks = seq(0, 2.5, 0.5)) +
  geom_vline(aes(xintercept = 5.5)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
  theme(axis.title.y = element_text(size = 18,
                                    margin = margin(1, 5, 0.5, 0.5),
                                    colour = "black"),
        axis.title.x = element_text(size = 18,
                                    margin = margin(8, 1, 2, 1),
                                    colour="black"),
        axis.text.y = element_text(size = 14,
                                   colour = "black"),
        axis.text.x = element_text(size = 14,
                                   colour = "black"),
        strip.text.x = element_text(size = 16, colour = "black"))
# use 6" height, 8" width for pdf

# TOTAL ADULT DENSITY (BY YEAR AND BAY SIDE) ==========================================================================
#   Calculate total market size clam density by year AND BAY SIDE
market.mean.Bside <- clam.density.size.all %>%
  filter(SizeClass == "Market") %>%
  group_by(Year, Site.ID, bay.side) %>%
  filter(QuadratName != "2020 SGV 94 1" && QuadratName != "2020 SGV 94 2") %>%
  summarize(meanquads = mean(Clams.m2)) %>%
  group_by(Year, bay.side) %>%
  summarize(Mean.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))
market.mean.Bside$Year <- as.factor(market.mean.Bside$Year)

#     Need to add years with NA values in order to represent this nicely in the figure.
#       There's certainly a more elegant way to do this, but for not I will just make
#       a new data frame with the "NA" years, and then join this with the existing dataframe
Year <- c("2012", "2014")
bay.side <- c("E", "E")

NAs <- data.frame(Year, bay.side)
NAs$bay.side <- as.character(NAs$bay.side)

market.mean.Bside <- bind_rows(market.mean.Bside, NAs)

# Ok, finally time to make the figure
Y.lab <- expression("Adult Clam Abundance m"^-2)
X.lab <- c("Year")
line <- "black"

par(mar = c(5.1, 6.1, 4.1, 2.1), mgp = c(3.5, 0.75, 0)) 

ggplot(market.mean.Bside, 
       aes(x = Year, y = Mean.m2, fill = bay.side)) + 
  geom_col(width = 0.65, position=position_dodge(), color = 'black') +
  scale_fill_manual(values=c("dodgerblue3", "cyan")) +
  scale_x_discrete(name=X.lab) +
  scale_y_continuous(name = Y.lab,
                     expand = c(0, 0),
                     limits = c(0, 2.2),
                     breaks = seq(0, 2.2, 0.2)) +
  geom_errorbar(aes(ymin = Mean.m2 - SEM, ymax = Mean.m2 + SEM), width = 0.1,
                position=position_dodge(0.65)) +
  geom_vline(aes(xintercept = 5.5)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
  theme(axis.title.y = element_text(size = 18,
                                    margin = margin(1, 5, 0.5, 0.5),
                                    colour = "black"),
        axis.title.x = element_text(size = 18,
                                    margin = margin(8, 1, 2, 1),
                                    colour="black"),
        axis.text.y = element_text(size = 14,
                                   colour = "black"),
        axis.text.x = element_text(size = 14,
                                   colour = "black"),
        strip.text.x = element_text(size = 16, colour = "black")) +
  labs(fill = "Region of Bay") +
  scale_color_manual(breaks=c("E", "W"),labels=c("Eastern", "Western"))

# use 6" height, 8" width for pdf


# MIKE DOALL WANTS ALL HARD CLAM DATA PRESENTED AS A STACKED BAR GRAPH  (ABUNDANCE VS YEAR, BY SIZE BIN). =============

clam.density.spread <- clam.density.size %>%
  spread(SizeClass, Clams.m2, fill = 0)

#   Join the spread dataset with a simplified site details dataset to add all the zero
#       quadats (no clams) and put in a 0 for all size classes for these.
clam.density.spread.all <- full_join(benthic.site.smp, clam.density.spread)

#   Remove quadrat size column. Not needed anymore.
clam.density.spread.all <- clam.density.spread.all[, -6]

# Put in a 0 for all size classes for quadrats with no clams.
clam.density.spread.all$less1[is.na(clam.density.spread.all$less1)] <- 0
clam.density.spread.all$less2[is.na(clam.density.spread.all$less2)] <- 0
clam.density.spread.all$less3[is.na(clam.density.spread.all$less3)] <- 0
clam.density.spread.all$little[is.na(clam.density.spread.all$little)] <- 0
clam.density.spread.all$topneck[is.na(clam.density.spread.all$topneck)] <- 0
clam.density.spread.all$cherry[is.na(clam.density.spread.all$cherry)] <- 0
clam.density.spread.all$chowder[is.na(clam.density.spread.all$chowder)] <- 0

# Gather back together again to make a dataset of clam densities by size class in all 
#   quadrats in all years, including the zero quadrats (no clams).
clam.density.all <- gather(clam.density.spread.all,
                           key = SizeClass,
                           value = Clams.m2,
                           -Year, -Site.ID, -bay.side, 
                           -Quadrat, -QuadratName)

# Average together 2 replicate quadrats from each site (still keeping size class resolution)
mean.dens <- clam.density.all %>%
  group_by(Year, Site.ID, bay.side, SizeClass) %>%
  summarize(meanquads = mean(Clams.m2)) %>%
  group_by(Year, bay.side, SizeClass) %>%
  summarize(Mean.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads)) %>%
  mutate(Bay.Year = paste(bay.side, Year, sep = " "))
mean.dens$Year <- as.factor(mean.dens$Year)

SizeClassGroups <- c(
  `chowder` = ">41 mm",
  `cherry` = "39-41 mm",
  `topneck` = "36-38 mm",
  `little` ="26-35 mm",
  `less3`="17-25 mm",
  `less2` = "9-16 mm",
  `less1` = "<9 mm"
)
mean.dens$SizeClass <- plyr::revalue(mean.dens$SizeClass, SizeClassGroups)

#     Reoder factors appropriately
mean.dens$Bay.Year <- factor(mean.dens$Bay.Year, 
                             levels = c("W 2012", "W 2014", 
                                        "W 2015", "W 2016", 
                                        "W 2017", "W 2018", 
                                        "W 2019", "W 2020",
                                        "W 2021",
                                        "E 2015", "E 2016",
                                        "E 2017", "E 2018",
                                        "E 2019", "E 2020",
                                        "E 2021"))

mean.dens$SizeClass <- factor(mean.dens$SizeClass,
                              levels = c("<9 mm", "9-16 mm", "17-25 mm", "26-35 mm", "36-38 mm", "39-41 mm", ">41 mm"))

#     Set axis titles, lables
Y.lab <- expression("Clam Abundance m"^-2)
X.lab <- c("Year")
labels <- c("2012", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021",
            "2015", "2016", "2017", "2018", "2019", "2020", "2021")

#     Set graphing paramenters
par(mar = c(5.1, 6.1, 4.1, 2.1), mgp = c(3.5, 0.75, 0)) 

windowsFonts(A = windowsFont("Arial"))

#     Ready to plot!
ggplot(mean.dens, 
       aes(x = Bay.Year, y = Mean.m2 , fill = SizeClass)) + 
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Paired", direction = -1) +
  labs(x = X.lab, y = Y.lab) +
  scale_x_discrete(breaks=c("W 2012", "W 2014", "W 2015", "W 2016", "W 2017", "W 2018", "W 2019", "W 2020", 
                            "W 2021", "E 2015", "E 2016", "E 2017", "E 2018", "E 2019", "E 2020", "E 2021"),
                   labels=labels) +
  scale_y_continuous(name = Y.lab,
                     expand = c(0, 0),
                     limits = c(0, 8),
                     breaks = seq(0, 8, 0.2)) +
  geom_vline(xintercept = 9.5, size = 1.5) +
  theme_bw() + 
  theme(text = element_text(family = "A"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black")) + 
  theme(axis.title.y = element_text(size = 18,
                                    margin = margin(1, 5, 0.5, 0.5),
                                    colour = "black"),
        axis.title.x = element_text(size = 18,
                                    margin = margin(8, 1, 2, 1),
                                    colour="black"),
        axis.text.y = element_text(size = 10,
                                   colour = c("black",NA,NA,NA,NA)),
        axis.text.x = element_text(size = 10,
                                   colour = "black"),
        strip.text.x = element_text(size = 16, colour = "black")) +
  guides(fill = guide_legend(title = "Size Class")) +
  theme(legend.text.align = 0,
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))
# Save PDF (5" x 10")


# Save data frames for later =======================================================================================
pre_market.clams <- clam.density.size.all %>%
  filter(SizeClass == "Pre_market") %>%
  group_by(Year, Site.ID) %>%
  summarize(Mean.m2 = mean(Clams.m2))

write.csv(pre_market.clams,"ClamDensityBySite_PreMarket_2012-2021.csv", row.names = F)
write.csv(pre_market.total, "TotalPreMarketClamsRecovered_2012-2021.csv", row.names = F)
write.csv(pre_market.total.Bside, "TotalPreMarketClamsRecovered_ByBaySide_2012-2021.csv", row.names = F)
write.csv(mean.pre_market,"PreMarketClam_MeanDensity_2012-2021.csv", row.names = F)
write.csv(pre_market.mean.Bside, "PreMarketClam_MeanDensity_ByBaySide_2012-2021.csv", row.names = F)
write.csv(annual.pre_market.mean, "PreMarketClam_MeanDensity_AnnuallySurveyed_2012-2021.csv")
write.csv(annual.pre_market.mean.Bside, "PreMarketClam_MeanDensity_ByBaySide_AnnuallySurveyed_2012-2021.csv", 
          row.names = F)

market.clams <- clam.density.size.all %>%
  filter(SizeClass == "Market") %>%
  group_by(Year, Site.ID) %>%
  summarize(Mean.m2 = mean(Clams.m2))

market.total <- age.total %>%
  filter(age == "market") %>%
  group_by(Year) %>%
  summarise(count = sum(Count))

market.total.Bside <- age.total %>%
  filter(age == "market") %>%
  group_by(Year, bay.side) %>%
  summarize(count = sum(Count))

write.csv(market.clams,"ClamDensityBySite_Market_2012-2021.csv", row.names = F)
write.csv(market.total, "TotalMarketClamsRecovered_2012-2021.csv", row.names = F)
write.csv(market.total.Bside, "TotalMarketClamsRecovered_byBaySide_2012-2021.csv", row.names = F)
write.csv(mean.market,"MarketClam_MeanDensity_2012-2021.csv", row.names = F)
write.csv(market.mean.Bside, "MarketClam_MeanDensity_ByBaySide_2012-2021.csv", row.names = F)
write.csv(mean.dens, "ClamDensity_ByAgeClassAndYear_2012-2021.csv", row.names = F)

# END OF SCRIPT =======================================================================================================
#   Proceding Script: "3.4_HardClams_Baymen_ShiRP_Benthic_Analysis_2021.R"