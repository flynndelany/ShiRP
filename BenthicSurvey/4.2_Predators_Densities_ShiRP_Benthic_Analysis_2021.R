# Analysis of ShiRP Benthic Survey Data for 2021 ShiRP Report
#   This script is for calculating predator densities (total and by taxonomic group).

#   Preceding script: "4.1_Predators_ShiRP_Benthic_Analysis_2021.R"


# Authors: Diana Chin, Dylan Cottrell, Flynn DeLany (based on code from Rebecca Kulp)

# Converted to GitHub 2/8/2023 - reference repository for future edit history

# Calculate Predator Densities ========================================================================================
#   For GIS, calculate densities for any predator (not distinguishing by predator group e.g. crustaceans) by 
#     site for all years before 2022 and for 2022, first averaging together the 2 replicate quadrats for each site 
#     in each year.
#   Only doing this for predators >20 mm due to requirements of ShiRP report, but could easily copy for predators 
#     >10 mm ("all") like rest of script.

density.pred20.any.pre2022 <- benthic.pred20.all %>%
  filter(Year != 2022) %>%
  group_by(Year, Site.ID, Quadrat) %>%
  summarize(count = sum(N.m2)) %>%
  group_by(Year, Site.ID) %>%
  summarize(meanquads = mean(count)) %>%
  group_by(Site.ID) %>%
  summarize(n.m2 = mean(meanquads))

density.pred20.any.2022 <- benthic.pred20.all %>%
  filter(Year == 2022) %>%
  group_by(Site.ID, Quadrat) %>%
  summarize(count = sum(N.m2)) %>%
  group_by(Site.ID) %>%
  summarize(n.m2 = mean(count))

# Calculate means of predator groups/subgroups =======================================================================
# First averaging together the 2 replicate quadrats for each site in each year.

mean.pred20.crust <- benthic.pred20.all %>%
  filter(SpeciesGroup == "Crustaceans") %>%
  group_by(Year, Site.ID, Quadrat, Subgroup) %>%
  summarize(count = sum(N.m2)) %>%
  group_by(Year, Site.ID, Subgroup) %>%
  summarize(meanquads = mean(count)) %>%
  group_by(Year, Subgroup) %>%
  summarize(n.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))
mean.pred20.crust$Year <- as.factor(mean.pred20.crust$Year)

mean.pred20.gastro <- benthic.pred20.all %>%
  filter(SpeciesGroup == "Gastropods") %>%
  group_by(Year, Site.ID, Quadrat, Subgroup) %>%
  summarize(count = sum(N.m2)) %>%
  group_by(Year, Site.ID, Subgroup) %>%
  summarize(meanquads = mean(count)) %>%
  group_by(Year, Subgroup) %>%
  summarize(n.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))
mean.pred20.gastro$Year <- as.factor(mean.pred20.gastro$Year)

mean.pred20.fish <- benthic.pred20.all %>%
  filter(SpeciesGroup == "Fish") %>%
  group_by(Year, Site.ID, Quadrat, Subgroup) %>%
  summarize(count = sum(N.m2)) %>%
  group_by(Year, Site.ID, Subgroup) %>%
  summarize(meanquads = mean(count)) %>%
  group_by(Year, Subgroup) %>%
  summarize(n.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))
mean.pred20.fish$Year <- as.factor(mean.pred20.fish$Year)

mean.predall.crust <- benthic.predall.all %>%
  filter(SpeciesGroup == "Crustaceans") %>%
  group_by(Year, Site.ID, Quadrat, Subgroup) %>%
  summarize(count = sum(N.m2)) %>%
  group_by(Year, Site.ID, Subgroup) %>%
  summarize(meanquads = mean(count)) %>%
  group_by(Year, Subgroup) %>%
  summarize(n.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))
mean.predall.crust$Year <- as.factor(mean.predall.crust$Year)

mean.predall.gastro <- benthic.predall.all %>%
  filter(SpeciesGroup == "Gastropods") %>%
  group_by(Year, Site.ID, Quadrat, Subgroup) %>%
  summarize(count = sum(N.m2)) %>%
  group_by(Year, Site.ID, Subgroup) %>%
  summarize(meanquads = mean(count)) %>%
  group_by(Year, Subgroup) %>%
  summarize(n.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))
mean.predall.gastro$Year <- as.factor(mean.predall.gastro$Year)

mean.predall.fish <- benthic.predall.all %>%
  filter(SpeciesGroup == "Fish") %>%
  group_by(Year, Site.ID, Quadrat, Subgroup) %>%
  summarize(count = sum(N.m2)) %>%
  group_by(Year, Site.ID, Subgroup) %>%
  summarize(meanquads = mean(count)) %>%
  group_by(Year, Subgroup) %>%
  summarize(n.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))
mean.predall.fish$Year <- as.factor(mean.predall.fish$Year)

# Calculate total predator density per year
mean.density.allpred <- benthic.predall.all %>%
  group_by(Year, Site.ID, Quadrat) %>%
  mutate(total.pred = sum(N.m2)) %>%
  group_by(Year, Site.ID) %>%
  mutate(meanquads = mean(total.pred))

mean.density.allpred <- mean.density.allpred[!duplicated(mean.density.allpred[, c("Year", "Site.ID", "meanquads")]),]

mean.density.allpred <- mean.density.allpred %>%
  group_by(Year) %>%
  summarize(Mean.m2 = mean(meanquads), SD = sd(meanquads), SEM = se(meanquads))

# END OF SCRIPT =======================================================================================================
    # Procede to: "4.3_Predators_Plots_ShiRP_Benthic_Analysis_2021.R"

