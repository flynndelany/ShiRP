# Analysis of ShiRP Benthic Survey Data for 2021 ShiRP Report
#   This script is for plotting predator densities (total and by taxonomic group).

# Authors: Flynn DeLany

# Last Modified: 12/21/21

# NOTE: For the Here package to work appropriately, all called datasets must have a copy saved in the "Analysis" side
#   of the project. E.g. "Benthic_Survey_2012-2021.xlsx" should be saved in "Data -> "2021" and "Analysis" -> "2021"

# ArcPro can average overlaying points for numerical data. Character data requires ==================================
# a manual check that is performed here. This script will also prep dataframes for
# easier data manipulation within ArcPro.

library(dplyr)
library(readxl)

setwd("C:/Users/flynn/OneDrive/Desktop/ShiRP/Benthic_Survey/Analysis/2021")
dir()

# Read in columns of interest from Benthic_Survey excel file in the Quadrat details sheet. ==========================

Details2021 <- read_xlsx("Benthic_Survey_2012-2021.xlsx","Quadrat.Details") %>%
  filter(Year==2021) %>%
  select(Year, Site.ID, Quadrat, bay.side, Habitat.Type, Seagrass = 'Seagrass.in.Area?') %>%
  group_by(Site.ID)

# Check if Habitat Type and Seagrass in the Area matches between each quadrat per site. =============================

Details2021$Seagrass[Details2021$Quadrat==1]==Details2021$Seagrass[Details2021$Quadrat==2]

Details2021$Habitat.Type[Details2021$Quadrat==1]==Details2021$Habitat.Type[Details2021$Quadrat==2]

# Discrepancy found between the habitat types for the quadrats of site SGV AD10. ====================================
# Value for Q1 was set to match Q2 as MA based on total average coverage between the Quadrats.

# If all values are true, continue.

# Condense Sites into one row per site. =============================================================================

x <- Details2021 %>%
  filter(Quadrat == 1) %>%
  arrange(Site.ID)

# Did Seagrass in area change from last year ========================================================================

# Get 2020 data 

Details2020 <- read_xlsx("Benthic_Survey_2012-2021.xlsx","Quadrat.Details") %>%
  filter(Year==2020) %>%
  select(Year, Site.ID, Quadrat, bay.side, Seagrass = 'Seagrass.in.Area?') %>%
  group_by(Site.ID)

# Check if quadrat values match

Details2020$Seagrass[Details2020$Quadrat==1]==Details2020$Seagrass[Details2020$Quadrat==2]

# Select columns of interest and join to 2021 data

y <- Details2020 %>%
  filter(Quadrat == 1) %>%
  arrange(Site.ID) %>%
  select(Site.ID, Seagrass)

z <- left_join(x, y, by="Site.ID")

# Conditional statement to whether seagrass presence changed.

z <- z %>%
  mutate(HabDiff20 = if(is.na(Seagrass.y)){SeagrassDiff = NA}else if(Seagrass.x=="N"&Seagrass.y=="Y"){SeagrassDiff = "Loss"}
         else if (Seagrass.x=="Y"&Seagrass.y=="N"){SeagrassDiff = "Gain"}else{SeagrassDiff = "NoDiff"}) %>%
  select(- Seagrass.y)

write.csv(DiffDetails, "BenthicSurveyForGIS.csv", row.names = F)

#Again for 2019

Details2019 <- read_xlsx("Benthic_Survey_2012-2021.xlsx","Quadrat.Details") %>%
  filter(Year==2019) %>%
  select(Year, Site.ID, Quadrat, bay.side, Seagrass = 'Seagrass.in.Area?') %>%
  filter(!grepl('Sedge', Details2019$Site.ID)) %>%
  group_by(Site.ID)

# Check if quadrat values match

Details2019$Seagrass[Details2019$Quadrat==1]==Details2019$Seagrass[Details2019$Quadrat==2]

# Select columns of interest and join to 2019 data

a <- Details2019 %>%
  filter(Quadrat == 1) %>%
  arrange(Site.ID) %>%
  select(Site.ID, Seagrass)

b <- left_join(x, a, by = 'Site.ID')

b <- b %>%
  mutate(HabDiff19 = if(is.na(Seagrass.y)){SeagrassDiff = NA}else if(Seagrass.x=="N"&Seagrass.y=="Y"){SeagrassDiff = "Loss"}
         else if (Seagrass.x=="Y"&Seagrass.y=="N"){SeagrassDiff = "Gain"}else{SeagrassDiff = "NoDiff"}) %>%
  select(Site.ID, HabDiff19)

DiffDetails <- left_join(z, b, by = 'Site.ID')

write.csv(DiffDetails, "BenthicSurveyForGIS.csv", row.names = F)
