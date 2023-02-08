# ShiRP Bio-optical Model: Temperature data
#  This script is for calculating cumulative hours above a given temperature and manipulating
#     all data based on specified groups. For the ShiRP report we normally only consider hours over 25C, but I have
#     included code for other temperature thresholds should the need arise later

#   Preceeding script: "2_TrimToDates_ShiRP_BioOptical_Model_2021"

# Author: Kaitlyn O'Toole; Modified/streamlined by Dylan Cottrell, Flynn DeLany
# Last Modified: 12/20/2021

# NOTE: For the Here package to work appropriately, all called datasets must have a copy saved in the "Analysis" side
#   of the project. E.g. "ShinnecockBay_2020" should be saved in "Data -> "2020" -> "Hobos" and in "Analysis" -> "2020"


# Load Libraries =======================================================================================================
library(here)
library(readxl)
library(writexl)
library(dplyr)



# Import data ==========================================================================================================
temps_combined <- read_excel("temps_SubsetDates_u2021.xlsx")

# Count up how many hours a site, edge, hobo_num experienced a given temperature. This only counts hours 
#   above or below a given temp. If hours AT a given temp are desired, give range of interest 
#   (e.g. summarize (Hoursat24=sum((temp>24 & temp<25)/4))). The "/4" term is due to the fact that temps were 
#   recorded every 15 minutes (so 4 observations at a given temp is really only the equivalent of a single hour
#   at that temperature).

# Hours over 25C ======================================================================================================
Hoursover25 <- temps_combined %>%
  group_by(Site) %>%
  summarize(HoursOver25 = (sum(Temp>25)/4))
# Because the hobo at SH1 had a short after being deployed just 2 weeks, it never exceded 25C (hence only 13 totals
#   calculated here). Additionally, since this hobo recorded for such a short period of time, using it in the overall 
#   bio-optical model is not that useful so de facto exclusion is acceptable.

# Save as .xlsx file
write_xlsx(Hoursover25, path="Cumulative_Temps_u2021.xlsx",  
           col_names = TRUE, format_headers = FALSE)


# Hours under 23C ======================================================================================================
#HoursUnder23 <- temps_combined %>%
#  group_by(site, hobo) %>%
#  summarize(HoursUnder23 = sum((temp<23)*4))

# Hours over 23C ======================================================================================================
#HoursOver23 <- temps_combined %>%
#  group_by(site, hobo) %>%
#  summarize(HoursOver23 = sum((temp>23)*4))

# Hours over 24C ======================================================================================================
#HoursOver24 <- temps_combined %>%
#  group_by(site, hobo) %>%
#  summarize(HoursOver24 = sum((temp>24)*4))

# Hours over 26C ======================================================================================================
#Hoursover26 <- temps_combined %>%
#  group_by(site, hobo) %>%
#  summarize(HoursOver26 = (sum(temp>26)*4))

# Hours over 27C ======================================================================================================
#Hoursover27 <- temps_combined %>%
#  group_by(site, hobo) %>%
#  summarize(HoursOver27 = (sum(temp>27)*4))

# Hours over 28C ======================================================================================================
#Hoursover28 <- temps_combined %>%
#  group_by(site, hobo) %>%
#  summarize(HoursOver28 = (sum(temp>28)*4))

# Hours over 29C ======================================================================================================
#Hoursover29 <- temps_combined %>%
#  group_by(site, hobo) %>%
#  summarize(HoursOver29 = (sum(temp>29)*4))

# Hours over 30C ======================================================================================================
#Hoursover30<-temps_combined%>%
#  group_by(site, hobo) %>%
#  summarize(HoursOver30=(sum(temp>30)*4))

# Merge all the above dataframes into a single one =====================================================================
#Temp23 <- merge(HoursUnder23, HoursOver23, by = c("bay", "site", "edge", "hobo_num"))
#Temp23_24<-merge(Temp23, HoursOver24, by = c("bay", "site", "edge", "hobo_num"))
#Temp24_25<-merge(Temp23_24, Hoursover25, by = c("bay", "site", "edge", "hobo_num"))
#Temp25_26<-merge(Temp24_25, Hoursover26, by = c("bay", "site", "edge", "hobo_num"))
#Temp25_27<-merge(Temp25_26, Hoursover27, by = c("bay", "site", "edge", "hobo_num"))
#Temp25_28<-merge(Temp25_27, Hoursover28, by = c("bay", "site", "edge", "hobo_num"))
#Temp25_29<-merge(Temp25_28, Hoursover29, by = c("bay", "site", "edge", "hobo_num"))
#Cumulative_Temps<-merge(Temp25_29, Hoursover30, by = c("bay", "site", "edge", "hobo_num"))


# END OF SCRIPT=================================================================================================