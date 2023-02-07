# Analysis of ShiRP Benthic Survey Data for 2021 ShiRP Report
#   This script is for looking at the data a bit differently, i.e. only looking at sites that baymen traditionally
#     would clam at. It basically excludes all sites along the edge of the bay which would be mostly seagrass 
#     habitat which baymen wouldn't work in. In 2018, ShiRP people are interested in why mean adult clam densities 
#     in benthic survey aren't higher despite greater focus on eastern bay and reports of higher commercial landings
#     in eastern bay.

#   Preceding script: "3.3_HardClams_Plots_ShiRP_Benthic_Analysis_2021.R"


# Authors: Diana Chin, Dylan Cottrell, Flynn DeLany (based on code from Rebecca Kulp)

# Last Modified: 12/20/21


# NOTE: For the Here package to work appropriately, all called datasets must have a copy saved in the "Analysis" side
#   of the project. E.g. "Benthic_Survey_2012-2021.xlsx" should be saved in "Data -> "2021" and "Analysis" -> "2021"


# Import data =======================================================================================================
# Limit the site and clam datasets to "clamming sites" according to custom list (created by Diana Chin and 
#   Alex Duperty)
baymen <- read_excel("Potential_Clamming_Sites_worksheet.xlsx", sheet = "list for R", na = "NA")
benthic.site <- read.csv("benthic.site.csv")
benthic.clam <- read.csv("benthic.clam.csv")

# Organize Data =====================================================================================================
# Redo the adult data to focus on "clamming sites" in WSB and ESB.
# Clamming sites in ESB exclude most perimeter sites - these are usually seagrass and/or very sandy 
#   and/or Crepidula and the baymen have the whole homogeneous center to work with.
# Clamming sites in WSB exclude any site that ever had seagrass plus any with large mussel or 
#   Crepidula shell hash or that are probably way too shallow, but the sites still include some 
#   where baymen are not that likely to try, like south of the channel closer to sandy/seagrass areas.

# Join "baymen" data to "benthic.site" and "benthic.clam" data
benthic.site.baymen <- inner_join(benthic.site, baymen, by = "Site.ID")
benthic.clam.baymen <- inner_join(benthic.clam, baymen, by = "Site.ID")

# Limit the clam dataset to adults only, i.e. SW > 25.
benthic.clam.baymen.adult <- filter(benthic.clam.baymen, SW > 25)

# Set up to calculate clam densities (per 1 m2) for WSB each year, and ESB each year since 2018.
baymen.density <- benthic.clam.baymen.adult %>%
  group_by(Year, Site.ID, Quadrat, Quadrat.size, Bay.Side) %>%
  summarize(nClams = sum(Count)) %>%
  mutate(Clams.m2 = nClams/Quadrat.size) %>%
  mutate(QuadratName = paste(Year, Site.ID, Quadrat))

# Get rid of the nClams and quadrat size columns for baymen clam dataset. 
baymen.density <- baymen.density[, -c(4,6)]

# Also simplify "clamming" sites dataset.
benthic.site.baymen.smp <- benthic.site.baymen %>%
  mutate(QuadratName = paste(Year, Site.ID, Quadrat)) %>%
  select(Year, Site.ID, Quadrat, QuadratName, Bay.Side)

# Join the "clamming" sites and baymen clam datasets.
baymen.density.all <- full_join(benthic.site.baymen.smp, baymen.density)

# Put in a 0 for all quadrats with no clams.
baymen.density.all$Clams.m2[is.na(baymen.density.all$Clams.m2)] <- 0

# Save for later.
write.csv(baymen.density.all,"ClamDensity_Baymen_AllSites_2021.csv", row.names = F)

# Factorize some variables and re-order bay side factor for plotting.
baymen.density.all$Year <- as.factor(baymen.density.all$Year)
baymen.density.all$Site.ID <- as.factor(baymen.density.all$Site.ID)
baymen.density.all$Bay.Side <- as.factor(baymen.density.all$Bay.Side)
baymen.density.all$Bay.Side <- factor(baymen.density.all$Bay.Side, levels = c("WSB", "ESB"))

# Calculate mean adult densities at "clamming" sites by year and bay.
mean.baymen.market <- baymen.density.all %>%
  group_by(Year, Site.ID, Bay.Side) %>%
  summarize(meanquads = mean(Clams.m2)) %>%
  group_by(Year, Bay.Side) %>%
  summarize(Mean.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))

# Make a column for Year+Bay.Side, make it a factor, and reorder.
mean.baymen.market <- mutate(mean.baymen.market, BayName = paste(Year, Bay.Side))
mean.baymen.market$BayName <- as.factor(mean.baymen.market$BayName)
mean.baymen.market$BayName <- factor(mean.baymen.market$BayName, levels = c("2012 WSB", "2014 WSB", "2015 WSB", 
                                                                          "2016 WSB", "2017 WSB", "2018 WSB", 
                                                                          "2019 WSB", "2020 WSB", "2021 WSB", "2018 ESB",
                                                                          "2019 ESB", "2020 ESB", "2021 ESB"))

# FIGURE - probably not going to use...
# Adult barplot, set of "clamming" sites.
Y.lab <- expression("Adult Clam Abundance m"^-2)
X.lab <- c("Year")
labels <- c("2012", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2018", "2019", "2020", "2021")

par(mar = c(5.1, 6.1, 4.1, 2.1), mgp = c(3.5, 0.75, 0)) 

ggplot(mean.baymen.market, 
       aes(x = BayName, y = Mean.m2, fill = Bay.Side)) + 
  geom_col(width = 0.65, color = "black") + 
  geom_errorbar(aes(ymin = Mean.m2 - SEM, ymax = Mean.m2 + SEM), width = 0.1) +
  labs(x = X.lab, y = Y.lab) +
  scale_x_discrete(breaks = levels(mean.baymen.market$BayName), labels = levels(mean.baymen.market$BayName)) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 6.5),
                     breaks = seq(0, 6, 1)) +
  scale_fill_manual(values = c("dodgerblue3","cyan")) +
  geom_vline(aes(xintercept = 9.5)) +
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
        axis.text.x = element_text(size = 9,
                                   colour = "black"),
        strip.text.x = element_text(size = 16, colour = "black"))
# use 6" height, 8" width for pdf

# Well...they are not going to like that, because it still shows ESB in 2018 is around the same 
#   sorts of densities as in WSB in 2018 and previous years, if not lower.

# Save for later.
write.csv(mean.baymen.market,"MarketClam_MeanDensity_Baymen.csv", row.names = F)


# Try with a more stringent set of clamming sites======================================================================
#   Can also try limiting to a very stringent subset of sites in WSB that we are pretty sure could
#     be clammer sites. These include a few around Weesuck that might be close to clam sanctuaries 
#     but don't show high densities, and a few outside Tiana Bay that should be south and outside 
#     of the general sanctuary area.

#Read in data
baymen2 <- read_excel("Potential_Clamming_Sites_worksheet.xlsx", sheet = "list for R 2", na = "NA")

#Join "baymen2" with "benthic.site" and "benthic.clam" data
benthic.site.baymen2 <- inner_join(benthic.site, baymen2, by = "Site.ID")
benthic.clam.baymen2 <- inner_join(benthic.clam, baymen2, by = "Site.ID")

# Limit the clam dataset to adults only, i.e. SW > 25.
benthic.clam.baymen2.market <- filter(benthic.clam.baymen2, SW > 25)

# Set up to calculate clam densities (per 1 m2) for WSB each year, and ESB each year since 2018.
baymen2.density <- benthic.clam.baymen2.market %>%
  group_by(Year, Site.ID, Quadrat, Quadrat.size, Bay.Side) %>%
  summarize(nClams = sum(Count)) %>%
  mutate(Clams.m2 = nClams/Quadrat.size) %>%
  mutate(QuadratName = paste(Year, Site.ID, Quadrat))

# Get rid of the nClams and quadrat size column for baymen clam dataset
baymen2.density <- baymen2.density[, -c(4,6)]

# Also simplify "clamming" sites dataset
benthic.site.baymen2.smp <- benthic.site.baymen2 %>%
  mutate(QuadratName = paste(Year, Site.ID, Quadrat)) %>%
  select(Year, Site.ID, Quadrat, QuadratName, Bay.Side)

# Join the "clamming" sites and baymen clam datasets
baymen2.density.all <- full_join(benthic.site.baymen2.smp, baymen2.density)

# Put in a 0 for all quadrats with no clams.
baymen2.density.all$Clams.m2[is.na(baymen2.density.all$Clams.m2)] <- 0

# Save for later.
write.csv(baymen2.density.all,"ClamDensity_Baymen2_AllSites_2021.csv", row.names = F)

# Factorize some variables and recode/re-order bay.side factor for plotting.
baymen2.density.all$Year <- as.factor(baymen2.density.all$Year)
baymen2.density.all$Site.ID <- as.factor(baymen2.density.all$Site.ID)
baymen2.density.all$Bay.Side <- recode(baymen2.density.all$Bay.Side, "ESB" = "E", "WSB" = "W")
baymen2.density.all$Bay.Side <- factor(baymen2.density.all$Bay.Side, levels = c("W", "E"))

# Calculate mean market size clam densities at "clamming" sites by year and bay.
mean.baymen2.market <- baymen2.density.all %>%
  group_by(Year, Site.ID, Bay.Side) %>%
  summarize(meanquads = mean(Clams.m2)) %>%
  group_by(Year, Bay.Side) %>%
  summarize(Mean.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))

# Make a column for Year + Bay Side and make a factor and reorder.
mean.baymen2.market <- mutate(mean.baymen2.market, BayName = paste(Year, Bay.Side))
mean.baymen2.market$BayName <- as.factor(mean.baymen2.market$BayName)
mean.baymen2.market$BayName <- factor(mean.baymen2.market$BayName, levels = c("2012 WSB", "2014 WSB", "2015 WSB",
                                                                              "2016 WSB", "2017 WSB", "2018 WSB", 
                                                                              "2019 WSB", "2020 WSB", "2021WSB",
                                                                              "2018 ESB", "2019 ESB", "2020 ESB",
                                                                              "2021 ESB"))

# For plotting, get rid of 2018 WSB because we sampled only 1 site in WSB in 2018 in this stringent set of 
#   "clamming" sites.
# Need to add years with NA values in order to represent these missing values nicely in the figure.
#       There's certainly a more elegant way to do this, but for now I will just make
#       a new data frame with the "NA" years, and then join this with the existing dataframe
Year <- c("2012", "2013", "2014", "2015", "2016", "2017", "2013")
Bay.Side <- c("E", "E", "E", "E", "E", "E", "W")

NAs <- data.frame(Year, Bay.Side)

mean.baymen2.market <- bind_rows(mean.baymen2.market, NAs)

# Save for later
write.csv(mean.baymen2.market,"ClamDensity_Baymen2_Market.csv", row.names = F)

# Re-order factor as necessary
mean.baymen2.market$Bay.Side <- factor(mean.baymen2.market$Bay.Side,
                                       levels = c("E", "W"))

# Adult barplot, set of very stringent "clamming" sites.
Y.lab <- expression("Clam Abundance m"^-2)
X.lab <- c("Year")
line <- "black"
#labels <- c("2012","2014","2015", "2016","2017", "2019", "2018", "2019")

par(mar = c(5.1, 6.1, 4.1, 2.1), mgp = c(3.5, 0.75, 0)) 

ggplot(mean.baymen2.market, 
       aes(x = Year, y = Mean.m2, fill = Bay.Side)) + 
  geom_col(width = 0.65, position=position_dodge(), color = "black") +
  scale_fill_manual(values=c("dodgerblue3", "cyan")) +
  geom_errorbar(aes(ymin = Mean.m2 - SEM, ymax = Mean.m2 + SEM), width = 0.1,
                position=position_dodge(0.65)) +
  scale_x_discrete(name=X.lab) +
  scale_y_continuous(name=Y.lab,
                     expand = c(0, 0),
                     limits = c(0, 3.5),
                     breaks = seq(0, 3.5, 0.5)) +
  geom_vline(aes(xintercept = 6.5)) +
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
        axis.text.x = element_text(size = 9,
                                   colour = "black"),
        strip.text.x = element_text(size = 16, colour = "black")) +
  labs(fill = "Region of Bay") +
  scale_color_manual(breaks=c("E", "W"),labels=c("Eastern", "Western"))
# use 6" height, 8" width for pdf


# END OF SCRIPT =======================================================================================================
#   End of scripts dealing with hard clam data
#   Procede to "4.1_Predators_ShiRP_Benthic_Analysis_2021.R"