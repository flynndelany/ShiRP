# ShiRP Bio-optical Model: Temperature data
#  This specific script is for trimming all hobo data to the dates of interest (i.e. ensuring no air temps are recorded)

#   Preceding script: "1_MergeAllHoboFiles_ShiRP_BioOptical_Model_2021.R"

# Author: Dylan Cottrell, Flynn DeLany
# Last Modified: 12/20/2021 


# NOTE: For the Here package to work appropriately, all called datasets must have a copy saved in the "Analysis" side
#   of the project. E.g. "ShinnecockBay_2021" should be saved in "Data -> "2021" -> "Hobos" and in "Analysis" -> "2021"

# Load Packages =======================================================================================================
library(here)
library(readxl)

# Import Data =========================================================================================================
temps_combined <- read_excel("temps_combined_u2021.xlsx")


# Clip temp data only to periods of interest ==========================================================================

#   Convert to character (dplyr doesn't like working with POSIXt)
temps_combined$DateTime <- as.POSIXct(temps_combined$DateTime, format = "%m/%d/%y %I:%M:%S %p")

#   Create variables for dates of interest
start<-as.Date("05/26/22", "%m/%d/%y")
end <- as.Date("10/10/22", "%m/%d/%y")

#   Now select these dates in the appropriate dataframe
ShB_dates <- subset(temps_combined, DateTime >= start & DateTime <= end)

# Save as .xlsx file (if saved as .csv, the function in previous script will fail when run again (b/c/ it searches for
#   all csv files which would include the file created now)
write_xlsx(ShB_dates, path ="temps_SubsetDates_u2021.xlsx",  
           col_names = TRUE, format_headers = FALSE)


# END OF SCRIPT =========================================================================================================
#   Procede to: "3_CumulativeHours_ShiRP_BioOptical_Model_2021.R"