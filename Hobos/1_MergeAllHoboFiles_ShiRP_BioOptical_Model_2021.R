# ShiRP Bio-optical Model: Temperature data
#  This specific script is for importing all hobo files (.csv) at once, converting to single df, and exporting as .xlsx 

#     NOTE: MAKE SURE ALL DATES ARE IN MM/DD/YY OR THIS CODE WILL FILL CELLS WITH NA VALUES!!!!!!!!!!!!!!!!!


# Author: Flynn DeLany (based on code by Dylan Cottrell)
# Last Modified: 12/20/2021 


#Set Data Path ========================================================================================
Hobofiles <- "D:/Projects/ShiRP/Hobos"

setwd(Hobofiles)
#NOTE: If file names are saved differently, the following code will need to be adjusted. Probably easier just to rename
#   the appropriate files to the same format ("Bay_Site_HoboNumber_Year.csv")


# Load necessary packages =============================================================================================
library(dplyr) 
library(writexl) # Save new dataframe
library(lubridate) # Select only dates of interest


# Ensure all hobo .csv files are in the correct location ==============================================================
# Return vector of paths to all hobo files (using working directory path package)
#   Check file paths and make sure all hobos are included. This call is for any ".csv" file, so ensure the only ones
#     included here are the hobo files.
# Code dependent on file names being the site locations.

allfiles <- dir(path = Hobofiles, pattern = ".csv")

# Read all .csv files and select rows of interest =====================================================================
# Before running loop be certain working directory is set correctly and files are named by site.
# Headers differ by Hobo serial number so select(contains()) is used to pull only Date/Time and Temp data.
# Site name is pulled as substring from csv file name (i.e. "COR 2021.csv" -> "COR 2021").

temps_combined <- data.frame()
x <- data.frame()

for (i in allfiles) {
  sitename <- gsub("\\..*","",i)
  x <- read.csv(i, skip = 1)
  x <- x %>%
    select(DateTime = contains("Date"), Temp = contains("Temp")) %>%
    mutate(Site = sitename)
  temps_combined <- rbind(temps_combined,x)
}

# Several hobos have NA values from HoboWare program filling rows for coupler information. Remove these NA rows.
temps_combined <- na.omit(temps_combined)

# Save as .xlsx file (if saved as .csv, the above function fails; it searches for all csv files which
#   would include the file created now)
write_xlsx(temps_combined, path="temps_combined_u2021.xlsx",  
           col_names = TRUE, format_headers = FALSE)


# END OF SCRIPT==================================================================================================
    # Proceed to: "2_TrimToDates_ShiRP_BioOptical_Model_2021.R"