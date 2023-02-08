# Analysis of ShiRP Benthic Survey Data for 2021 ShiRP Report
#   This script is for calculating clam densities for all benthic sampling stations 

#   Preceding script: "3.1_HardClams_ShiRP_Benthic_Analysis_2021.R"


# Authors: Diana Chin, Dylan Cottrell, Flynn DeLany (based on code from Rebecca Kulp)

# Converted to GitHub 2/8/2023 - reference repository for future edit history

# Check Environment for Data ====================
exists('benthic.site')
exists('benthic.clam')

#IF ANY FALSE RUN PRECEDING SCRIPTS

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
# Looks good (Readout = TRUE)

# Deal with "zeroes" (i.e. quadrats which had no clams in them)
#   Just for reference and checking, find all the quadrats that are in the site details dataset 
#     but not in the clam density dataset because no clams were found there, and streamline the 
#     resulting zeros dataset a bit.

#   Rename the Site.ID field for the clam dataset or it will cause problems.
names(clam.density)[3] <- "Site.ID"

#   Check that the number of sites that are zeros for clams and the number of sites that have clams 
#     add up to the total number of sites under consideration.
clam.zero <- anti_join(benthic.site, clam.density)

clam.zero <- clam.zero %>%
  select(Year, Site.ID, bay.side, QuadratName) %>%
  mutate(Clams.m2 = 0)

length(which(clam.density$QuadratName %in% benthic.site$QuadratName))
length(which(clam.zero$QuadratName %in% benthic.site$QuadratName))
# Looks ok - Compare sum of above lengths to # of observations in "benthic.site" data frame
#   The sum has been one less than the total for a few years - haven't been able to figure it out but 
#   it's a minor issue so should be fine.

# Make a dataset of clam densities in all quadrats in all years, including the zero quadrats (no clams).
#   This new dataset should have same number of observations as the "benthic.site" data frame
clam.density.all <- full_join(benthic.site, clam.density)
clam.density.all <- select(clam.density.all, Year, Site.ID, bay.side, Quadrat, QuadratName, Clams.m2)
clam.density.all$Clams.m2[is.na(clam.density.all$Clams.m2)] <- 0

# Save for later
write.csv(clam.density.all,"ClamDensity_PerSite_2012-2021.csv", row.names = F)

# Now we want the same calculation of clam densities per 1 m2 for all quadrats but with the clams 
#   divided into size classes by shell width
#     Use the same categories as clam sanctuaries (as decided by Mike Doall). These categories were first used for
#       the 2019 ShiRP presentation. The 2019 report used the previous categories.
#       New Categories:
#         (less than 1yr = <9mm, 1-2 yr = 9-16mm, 2-3yr = 17-25mm, little = 26-36mm, cherry = 37-41mm, 
#         chowder = >41mm)

# Rename the Site.ID field for the clam size class dataset or it will cause problems later.
names(benthic.clam)[6] <- "Site.ID"

# Define size classes and then organize appropriate
clam.density.size <- benthic.clam %>%
  mutate(SizeClass = case_when(
    SW <= 9 ~ "less1",
    SW > 9 & SW <= 16 ~ "less2",
    SW > 16 & SW <= 25 ~ "less3",
    SW > 25 & SW <= 35 ~ "little",
    SW > 35 & SW <= 38 ~ "topneck",
    SW > 38 & SW <= 41 ~ "cherry",
    SW > 41 ~ "chowder"
  )
  ) %>%
  group_by(Year, Site.ID, bay.side, Quadrat, Quadrat.size, SizeClass) %>%
  summarize(nClams = sum(Count)) %>%
  mutate(Clams.m2 = nClams/Quadrat.size) %>%
  mutate(QuadratName = paste(Year, Site.ID, Quadrat)) 

#   Ok - this looks good, but must remove any clams that for whatever reason were not measured (included up to
#         this point to include them in the totals, but cannot be used for size-class plots)
exclude3 <- clam.density.size %>% 
  filter((Year == 2019 & Site.ID == "SGV AD7" & Quadrat == 2))
clam.density.size <- anti_join(clam.density.size, exclude3)

# Spread size classes, fill 0s for unoccupied size classes in a given quadrat, calculate 
#   densities for pre-market/market, and total from all size classes.
#     Get rid of the nClams column first because otherwise R thinks separate size classes in 
#     the same quadrat are unique cases.
clam.density.size <- clam.density.size[, -7]

#     Spread size classes and fill 0s
clam.density.size.spread <- clam.density.size %>%
  spread(SizeClass, Clams.m2, fill = 0) %>%
  mutate(Pre_market = sum(less1, less2, less3), Market = sum(little, topneck, cherry, chowder), 
         Total = sum(less1, less2, less3, little, topneck, cherry, chowder))

# Join the spread dataset with a simplified site details dataset to add all the zero 
#   quadrats (no clams) and put in a 0 for all size classes for these.
benthic.site.smp <- benthic.site %>%
  select(Year, Site.ID, bay.side, Quadrat, QuadratName)
clam.density.size.spread.all <- full_join(benthic.site.smp, clam.density.size.spread)

# Get rid of quadrat size column, don't need it anymore
clam.density.size.spread.all <- clam.density.size.spread.all[, -6]

# Put in a 0 for all size classes for quadrats with no clams.
clam.density.size.spread.all$less1[is.na(clam.density.size.spread.all$less1)] <- 0
clam.density.size.spread.all$less2[is.na(clam.density.size.spread.all$less2)] <- 0
clam.density.size.spread.all$less3[is.na(clam.density.size.spread.all$less3)] <- 0
clam.density.size.spread.all$little[is.na(clam.density.size.spread.all$little)] <- 0
clam.density.size.spread.all$topneck[is.na(clam.density.size.spread.all$topneck)] <- 0
clam.density.size.spread.all$cherry[is.na(clam.density.size.spread.all$cherry)] <- 0
clam.density.size.spread.all$chowder[is.na(clam.density.size.spread.all$chowder)] <- 0
clam.density.size.spread.all$Pre_market[is.na(clam.density.size.spread.all$Pre_market)] <- 0
clam.density.size.spread.all$Market[is.na(clam.density.size.spread.all$Market)] <- 0
clam.density.size.spread.all$Total[is.na(clam.density.size.spread.all$Total)] <- 0

# Gather back together again to make a dataset of clam densities by size class in all 
#   quadrats in all years, including the zero quadrats (no clams)
clam.density.size.all <- gather(clam.density.size.spread.all,
                                key = SizeClass,
                                value = Clams.m2,
                                -Year, -Site.ID, -bay.side, 
                                -Quadrat, -QuadratName)

# Final Resulting Dataframes: age.total, clam.density.size, benthic.site.smp, clam.density.size.all

# END OF SCRIPT =======================================================================================================
#   Proceding script: "3.3_HardClams_Plots_ShiRP_Benthic_Analysis_2021.R"