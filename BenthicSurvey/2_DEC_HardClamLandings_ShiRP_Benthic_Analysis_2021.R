# Analysis of ShiRP Benthic Survey Data for 2021 ShiRP Report
#   This script is for reported hard clam landings (from DEC) since 2008

#   Preceding script: "1_LoadAllPackages_ShiRP_Benthic_Analysis_2020.R"


# Authors: Diana Chin, Dylan Cottrell, Flynn DeLany (based on code from Rebecca Kulp)

# Last Modified: 12/16/21


# NOTE: For the Here package to work appropriately, all called datasets must have a copy saved in the "Analysis" side
#   of the project. E.g. "Benthic_Survey_2012-2021.xlsx" should be saved in "Data -> "2021" and "Analysis" -> "2021"

#Read in hard clam landings dataset ====================================================================================
landings<- read_excel("ShB_HardClamLandings_ByArea_2008-2018.xlsx", sheet = "Sheet1")

# Convert "Year" to factor
landings$Year<- as.factor(landings$Year)

# HARD CLAM LANDINGS BY BAY HALF =======================================================================================

ggplot (data=landings, aes(x=Year, y=Landings, group = Bay )) +
  geom_line(aes(color=Bay)) +
  geom_point() +
  labs(y = "Hard Clam Landings (Bushels)") +
  theme_classic() +
  theme(legend.position = "none")

#Save as PDf (5.53" x 5.57")

# END OF SCRIPT ========================================================================================================