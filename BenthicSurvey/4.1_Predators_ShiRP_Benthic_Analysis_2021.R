# Analysis of ShiRP Benthic Survey Data for 2021 ShiRP Report
#   This script is for importing and organizing the predator data.

#   Preceding script: "1_LoadAllPackages_ShiRP_Benthic_Analysis_2021.R"


# Authors: Diana Chin, Dylan Cottrell, Flynn DeLany (based on code from Rebecca Kulp)

# # Converted to GitHub 2/8/2023 - reference repository for future edit history


# Import data =========================================================================================================
benthic.fauna.raw <- read_excel("Data/Benthic_Survey_2012-2022.xlsx", sheet = "Fauna.Measurements", na = "NA")
benthic.fauna.counts.raw <- read_excel("Data/Benthic_Survey_2012-2022.xlsx", sheet = "FaunaCounts", na = "NA")
benthic.sitesanct.raw <- read_excel("Data/Benthic_Survey_2012-2022.xlsx", sheet = "Quadrat.Details", na = "NA")
# This will throw warnings about the "1/4" in the Quadrat field but fine because we are going to get rid of them 
#   anyway.

# Remove several items from these datasets:
#   NOTE: SITES REMOVED HERE SHOULD BE SAME AS "3.1_HardClams_ShiRP_Benthic_Analysis_2020.R"

#     From site details dataset, remove cases that are "1/4" in Quadrat field (these are records of when Rebecca did 
#       an extra 1/4 m2 quadrat for Diana in 2016 looking for Solemya only). Gets rid of 2015 SGV 49 quadrat "3" too 
#       but that's ok here because only a bivalve in the fauna dataset for that quadrat.
#     From site details dataset, remove 2016 SGV 92 Q1 and SGV93 Q2 (error in sample processing and don't know which 
#       data correspond to which quadrat).
#     From site details dataset and fauna dataset, remove 2017 SGV 86 8/10/17 and keep 8/15/2017. Reason given is 
#       they made a return trip to SGV 86, otherwise not sure why the first one is bad or why it was removed from the
#       site details but not clam dataset in previous code.
#     Keep the clam sanctuaries sampled in 2016 and 2017.
#     From both site details and benthic fauna data sets, remove Sedge 1-7 (these were an attempt to validate reports
#       of new-set clams and oysters near Sedge Island, though none were found. These were not surveyed as part of 
#       benthic sampling protocol)

exclude <- benthic.sitesanct.raw %>%
  filter((Quadrat == 4373) |
           (Year == 2016 & Site.ID == "SGV 92" & Quadrat == 1) | 
           (Year == 2016 & Site.ID == "SGV 93" & Quadrat == 2) | 
           (Year == 2017 & Site.ID == "SGV 86" & Date == "2017-08-10") |
           (Quadrat %!in% c(1,2)) |
           (Year == 2019 & Site.ID == "Sedge 1") |
           (Year == 2019 & Site.ID == "Sedge 2") |
           (Year == 2019 & Site.ID == "Sedge 3") |
           (Year == 2019 & Site.ID == "Sedge 4") |
           (Year == 2019 & Site.ID == "Sedge 5") |
           (Year == 2019 & Site.ID == "Sedge 6") |
           (Year == 2019 & Site.ID == "Sedge 7"))
benthic.sitesanct <- anti_join(benthic.sitesanct.raw, exclude)

exclude2 <- benthic.fauna.raw %>%
  filter((Year == 2017 & Site.ID == "SGV 86" & Date == "2017-08-10") |
           (Year == 2019 & Site.ID == "Sedge 1") |
           (Year == 2019 & Site.ID == "Sedge 2") |
           (Year == 2019 & Site.ID == "Sedge 3") |
           (Year == 2019 & Site.ID == "Sedge 4") |
           (Year == 2019 & Site.ID == "Sedge 5") |
           (Year == 2019 & Site.ID == "Sedge 6") |
           (Year == 2019 & Site.ID == "Sedge 7"))

benthic.fauna <- anti_join(benthic.fauna.raw, exclude2)

exclude3 <- benthic.fauna.counts.raw %>%
  filter((Year == 2017 & Site.ID == "SGV 86" & Date == "2017-08-10") |
           (Year == 2019 & Site.ID == "Sedge 1") |
           (Year == 2019 & Site.ID == "Sedge 2") |
           (Year == 2019 & Site.ID == "Sedge 3") |
           (Year == 2019 & Site.ID == "Sedge 4") |
           (Year == 2019 & Site.ID == "Sedge 5") |
           (Year == 2019 & Site.ID == "Sedge 6") |
           (Year == 2019 & Site.ID == "Sedge 7"))

benthic.fauna.counts <- anti_join(benthic.fauna.counts.raw, exclude3)

# Create list of what was considered predators during 2021 field sampling 
preds <- c("American Eel", "Blue Crab", "Channeled Whelk", "Common Mud Crab", "Cunner", "Cusk Eel", 
           "Flatclaw Hermit", "Green Crab", "Grubby sculpin", "Hermit Crab", "Horseshoe Crab", 
           "Knobbed Whelk", "Lady Crab", "Long Claw Hermit", "Longnose Spider Crab", "Mantis Shrimp", 
           "Moon Snail", "Northern Puffer", "Oyster Drill", "Oyster Pea Crab", "Oyster Toadfish", "Pea Crab", 
           "Portly Spider Crab", "Rock Crab", "Sand Shrimp", "Sayi Crab", "Sculpin", "Sea Robin", 
           "Shame-faced crab", "Spider Crab", "Starfish", "Summer Flounder", "Whelk", "Winter Flounder")

Bivalves <- c("Bay Scallop", "Blood Ark", "Blue Mussel", "Dwarf Surf Clam", "Egg Cockle", "False Angelwing",
              "File Yoldia", "Jingle Shell", "Razor Clam", "Soft Shell Clam", "Solemya Clam", "Surf Clam",
              "Tagelus Clam", "Transverse Ark")
# Limit the fauna dataset to two sets - predators >20 mm (whatever metric) because this is what was done previously, 
#     and predators >10 mm per sampling methodology.
# >20mm will take out predators on the smaller juveniles - small oyster drills (up to 20 mm according 
#     to Mackenzie 1977), most mud crabs, hermit crabs, sand shrimp, etc.
benthic.pred20 <- benthic.fauna %>%
  filter(Size.mm >= 20 & CommonName %in% preds) %>%
  select(Year, Site.ID, Quadrat, Quadrat.size, CommonName, Count) %>%
  group_by(Year, Site.ID, Quadrat, Quadrat.size, CommonName) %>%
  summarize(Count = sum(Count)) %>%
  mutate(N.m2 = Count / Quadrat.size) %>%
  mutate(QuadratName = paste(Year, Site.ID, Quadrat))

# Calculate densities.
# Assign predator species to species groups e.g. fish.
# Make a quadrat name identifier.
benthic.predall <- benthic.fauna %>%
  filter(CommonName %in% preds) %>%
  select(Year, Site.ID, Quadrat, Quadrat.size, CommonName, Count) %>%
  group_by(Year, Site.ID, Quadrat, Quadrat.size, CommonName) %>%
  summarize(Count = sum(Count)) %>%
  mutate(N.m2 = Count / Quadrat.size) %>%
  mutate(QuadratName = paste(Year, Site.ID, Quadrat))

# Simplify site details dataset: get rid of the year 2012 since no fauna were measured then anyway; 
#   make a quadrat name; get rid of most fields.
benthic.sitesanct.smp <- benthic.sitesanct %>%
  filter(Year != 2012) %>%
  mutate(QuadratName = paste(Year, Site.ID, Quadrat)) %>%
  select(Year, Site.ID, Quadrat, QuadratName)

# Spread predator names, fill 0s for predators not found in a given quadrat.
# Get rid of the Count column first because otherwise R thinks separate species in the same quadrat are unique cases.
benthic.pred20 <- benthic.pred20[, -6]
benthic.predall <- benthic.predall[, -6]

benthic.pred20.spread <- benthic.pred20 %>%
  spread(CommonName, N.m2, fill = 0)

benthic.predall.spread <- benthic.predall %>%
  spread(CommonName, N.m2, fill = 0)

# Join the spread predator datasets with the simplified site details dataset to add all the zero quadrats (no fauna).
# Get rid of quadrat size column.
benthic.pred20.spread.all <- full_join(benthic.sitesanct.smp, benthic.pred20.spread)
benthic.pred20.spread.all <- benthic.pred20.spread.all[, -5]

benthic.predall.spread.all <- full_join(benthic.sitesanct.smp, benthic.predall.spread)
benthic.predall.spread.all <- benthic.predall.spread.all[, -5]

# Gather back together again to make a dataset of predators in all quadrats in all years, 
#   including the zero quadrats (no fauna).
benthic.pred20.all <- gather(benthic.pred20.spread.all,
                             key = Species,
                             value = N.m2,
                             -Year, -Site.ID, 
                             -Quadrat, -QuadratName)

benthic.predall.all <- gather(benthic.predall.spread.all,
                              key = Species,
                              value = N.m2,
                              -Year, -Site.ID, 
                              -Quadrat, -QuadratName)

# Put in 0s for all the NAs.
benthic.pred20.all$N.m2[is.na(benthic.pred20.all$N.m2)] <- 0
benthic.predall.all$N.m2[is.na(benthic.predall.all$N.m2)] <- 0

# Assign species to crustaceans, gastropods, or fish and consolidate species into subgroups.
#   There have been 3 starfish in sampling history, not concerned that these are left out.
crustaceans <- c("Blue Crab", "Common Mud Crab", "Flatclaw Hermit", "Green Crab", "Hermit Crab", 
                 "Horseshoe Crab", "Lady Crab", "Long Claw Hermit", "Longnose Spider Crab", "Mantis Shrimp", 
                 "Oyster Pea Crab", "Pea Crab", "Portly Spider Crab", "Rock Crab", "Sand Shrimp", "Sayi Crab", 
                 "Shame-faced crab", "Spider Crab")

gastropods <- c("Channeled Whelk", "Knobbed Whelk", "Moon Snail", "Oyster Drill", "Whelk")

fish <- c("American Eel", "Cunner", "Cusk Eel", "Grubby sculpin", "Northern Puffer", "Oyster Toadfish", 
          "Sculpin", "Sea Robin", "Summer Flounder", "Winter Flounder")

blue <- c("Blue Crab")
mud <- c("Common Mud Crab", "Sayi Crab")
green <- c("Green Crab")
horseshoe <- c("Horseshoe Crab")
lady <- c("Lady Crab")
spider <- c("Longnose Spider Crab", "Portly Spider Crab", "Spider Crab")
rock <- c("Rock Crab")
smallclawed <- c("Flatclaw Hermit", "Hermit Crab", "Long Claw Hermit", "Oyster Pea Crab", "Pea Crab", 
                 "Shame-faced crab")
shrimp <- c("Mantis Shrimp", "Sand Shrimp")
moonsnail <- c("Moon Snail")
oysterdrill <- c("Oyster Drill")
whelk <- c("Channeled Whelk", "Knobbed Whelk", "Whelk")
cunner <- c("Cunner")
eel <- c("American Eel", "Cusk Eel")
flounder <- c("Summer Flounder", "Winter Flounder")
puffer <- c("Northern Puffer")
oystertoadfish <- c("Oyster Toadfish")
searobin <- c("Sea Robin")
sculpin <- c("Grubby sculpin", "Sculpin")

benthic.pred20.all <- benthic.pred20.all %>%
  mutate(SpeciesGroup = case_when(
    Species %in% crustaceans ~ "Crustaceans",
    Species %in% gastropods ~ "Gastropods",
    Species %in% fish ~ "Fish"
  )) %>%
  mutate(Subgroup = case_when(
    Species %in% blue ~ "Blue",
    Species %in% mud ~ "Mud",
    Species %in% green ~ "Green",
    Species %in% horseshoe ~ "Horseshoe",
    Species %in% lady ~ "Lady",
    Species %in% spider ~ "Spider",
    Species %in% rock ~ "Rock",
    Species %in% smallclawed ~ "Small-Clawed",
    Species %in% shrimp ~ "Shrimp",
    Species %in% moonsnail ~ "Moon Snail",
    Species %in% oysterdrill ~ "Oyster Drill",
    Species %in% whelk ~ "Whelk",
    Species %in% cunner ~ "Cunner",
    Species %in% eel ~ "Eel",
    Species %in% flounder ~ "Flounder",
    Species %in% puffer ~ "Norther Puffer",
    Species %in% oystertoadfish ~ "Oyster Toadfish",
    Species %in% searobin ~ "Sea Robin",
    Species %in% sculpin ~ "Sculpin"
  ))

benthic.predall.all <- benthic.predall.all %>%
  mutate(SpeciesGroup = case_when(
    Species %in% crustaceans ~ "Crustaceans",
    Species %in% gastropods ~ "Gastropods",
    Species %in% fish ~ "Fish"
  )) %>%
  mutate(Subgroup = case_when(
    Species %in% blue ~ "Blue",
    Species %in% mud ~ "Mud",
    Species %in% green ~ "Green",
    Species %in% horseshoe ~ "Horseshoe",
    Species %in% lady ~ "Lady",
    Species %in% spider ~ "Spider",
    Species %in% rock ~ "Rock",
    Species %in% smallclawed ~ "Small-Clawed",
    Species %in% shrimp ~ "Shrimp",
    Species %in% moonsnail ~ "Moon Snail",
    Species %in% oysterdrill ~ "Oyster Drill",
    Species %in% whelk ~ "Whelk",
    Species %in% cunner ~ "Cunner",
    Species %in% eel ~ "Eel",
    Species %in% flounder ~ "Flounder",
    Species %in% puffer ~ "Norther Puffer",
    Species %in% oystertoadfish ~ "Oyster Toadfish",
    Species %in% searobin ~ "Sea Robin",
    Species %in% sculpin ~ "Sculpin"
  ))


# END OF SCRIPT =======================================================================================================
#   Procede to: "4.2_Predators_Densities_ShiRP_Benthic_Analysis_2021.R"