# Analysis of ShiRP Benthic Survey Data for 2021 ShiRP Report
#   This script is for plotting predator densities (total and by taxonomic group).

# Authors: Flynn DeLany

# Converted to GitHub 2/8/2023 - reference repository for future edit history

# ArcGIS Pro can average overlaying points for numerical data. Character data requires
# a manual check that is performed here. This script will also prep dataframes for
# easier data manipulation within ArcGIS Pro.

library(dplyr)
library(readxl)

# Read in columns of interest from Benthic_Survey excel file in the Quadrat details sheet. ==========================

Details2022 <- read_xlsx("Data/Benthic_Survey_2012-2022.xlsx","Quadrat.Details") %>%
  filter(Year==2022 & is.na(Habitat.Type)!=T) %>%
  select(Year, Site.ID, Quadrat, bay.side, Habitat.Type, Seagrass = 'Seagrass.in.Area?') %>%
  group_by(Site.ID)

# Check if Habitat Type and Seagrass in the Area matches between each quadrat per site. =============================

Details2022$Seagrass[Details2022$Quadrat==1]==Details2022$Seagrass[Details2022$Quadrat==2]

Details2022$Habitat.Type[Details2022$Quadrat==1]==Details2022$Habitat.Type[Details2022$Quadrat==2]

# Discrepancy found between the habitat types for the quadrats of site SGV AD10. ====================================
# Value for Q1 was set to match Q2 as MA based on total average coverage between the Quadrats.

# If all values are true, continue.

# Condense Sites into one row per site. =============================================================================

x <- Details2022 %>%
  filter(Quadrat == 1) %>%
  arrange(Site.ID)

# Did Seagrass in area change from last year ========================================================================

# Get 2020 data 

Details2021 <- read_xlsx("Data/Benthic_Survey_2012-2022.xlsx","Quadrat.Details") %>%
  filter(Year==2021 & is.na(Habitat.Type)!=T) %>%
  select(Year, Site.ID, Quadrat, bay.side, Seagrass = 'Seagrass.in.Area?') %>%
  group_by(Site.ID)

# Check if quadrat values match

Details2021$Seagrass[Details2021$Quadrat==1]==Details2021$Seagrass[Details2021$Quadrat==2]

# Select columns of interest and join to 2021 data

y <- Details2021 %>%
  filter(Quadrat == 1 ) %>%
  arrange(Site.ID) %>%
  select(Site.ID, Seagrass)

z <- left_join(x, y, by="Site.ID")

# Conditional statement to whether seagrass presence changed.

z <- z %>%
  mutate(HabDiff21 = if(is.na(Seagrass.y)){SeagrassDiff = NA}else if(Seagrass.x=="N"&Seagrass.y=="Y"){SeagrassDiff = "Loss"}
         else if (Seagrass.x=="Y"&Seagrass.y=="N"){SeagrassDiff = "Gain"}else{SeagrassDiff = "NoDiff"}) %>%
  select(- Seagrass.y)

#Again for 2019

Details2020 <- read_xlsx("Data/Benthic_Survey_2012-2022.xlsx","Quadrat.Details") %>%
  filter(Year==2020) %>%
  select(Year, Site.ID, Quadrat, bay.side, Seagrass = 'Seagrass.in.Area?') 
Details2020 <- Details2020 %>%
  filter(!grepl('Sedge', Details2020$Site.ID)) %>%
  group_by(Site.ID)

# Check if quadrat values match

Details2020$Seagrass[Details2020$Quadrat==1]==Details2020$Seagrass[Details2020$Quadrat==2]

# Select columns of interest and join to 2019 data

a <- Details2020 %>%
  filter(Quadrat == 1) %>%
  arrange(Site.ID) %>%
  select(Site.ID, Seagrass)

b <- left_join(x, a, by = 'Site.ID')

b <- b %>%
  mutate(HabDiff20 = if(is.na(Seagrass.y)){SeagrassDiff = NA}else if(Seagrass.x=="N"&Seagrass.y=="Y"){SeagrassDiff = "Loss"}
         else if (Seagrass.x=="Y"&Seagrass.y=="N"){SeagrassDiff = "Gain"}else{SeagrassDiff = "NoDiff"}) %>%
  select(Site.ID, HabDiff20)

DiffDetails <- left_join(z, b, by = 'Site.ID')

write.csv(DiffDetails, "Data/BenthicSurveyForGIS.csv", row.names = F)
