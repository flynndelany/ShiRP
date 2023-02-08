# Analysis of ShiRP Hard Clam sanctuary for 2020 ShiRP Report
#   This script is for reading in and organizing the data, and for calculating hard clam densiteis in the surveyed
#       sanctuaries.
#   NOTE: CI in 2021 was monitored by Miek Doall on a weekly (or bi-weekly?) basis. Mike collected 10 clams from each
#       sanctuary we monitor each sampling date for the entire summer (Mid/late May through September). Therefore, 
#       densities were artificially reduced during this year.
#   NOTE: Sanctuaries were only surveyed once (mid-September) in 2021 due to COVID-19 restrictions. We traditionally
#       survey them in the spring and fall. Survival/densities are averaged between these two surveys though, so it
#       is not a problem here.


#   Preceding script: "1_LoadAllPackages_ShiRP_HardClamSanctuaries_2021.R"

# Authors: Diana Chin, Dylan Cottrell, Flynn DeLany (based on code from Rebecca Kulp)

# Converted 2/8/2023 - future version History on GitHub

# Read in and organize data ===========================================================================================
clamsanct.raw.nonzero <- read_excel("Data/SHiRP_ClamSanctuarySurvey_2013-2022.xlsx", sheet = "Sizes", na = "NA")
clamsanct.raw.zero <- read_excel("Data/SHiRP_ClamSanctuarySurvey_2013-2022.xlsx", sheet = "Field.Count.ZeroAlive", na = "NA")
# ^ Lots of warnings - looks like maybe some items in the "InputOrder" and "OrderInputList" fields were unexpected and 
#     misread. Really don't know what these fields are for anyway, disregard.
# ^ Do not remove NAs (they are only where measurements were not taken but clams were counted, and we care about all 
#     counts at the moment).

# Add the zero (empty) quadrat data to the rest of the measurement data, using all of the overlapping fields.
clamsanct.raw <- full_join(clamsanct.raw.nonzero, clamsanct.raw.zero, by = c("Bay","Site.ID","Date", "Year", 
                                                                             "Replicate", "Diver", "Processor", 
                                                                             "Quadrat", "Half", "Dead", "Alive"))
# ^ All rows appear to be accounted for.
# ^ Rebecca notes that for 9/17/2013 Wee1A Quadrat 13 is missing.


# Calculate hard clam survival through time ========================================================================== 

# Summarize numbers of dead, half-shell, and alive clams collected in each quadrat (all years, all sites, 
#   all quadrat sizes), then standardize clam number per quadrat to ALIVE clam number per 1 m2.
# Remove sites not monitored after 2013 (WEE1, WEE8A) and any that were outside the sanctuaries (i.e. any with 
#   "out" in the name).
# Add a field to aggregate bay and year, i.e. Weesuck Creek 2013.
clamsanct.density <- clamsanct.raw %>%
  group_by(.dots = c("Year", "Date", "Bay", "Site.ID", "Replicate", "Quadrat")) %>%
  summarize(N.Dead = sum(Dead), N.Half = sum(Half), N.Alive = sum(Alive)) %>%
  mutate(N.Alive.m2 = N.Alive/Quadrat) %>%
  filter(Site.ID %!in% c("WEE1", "WEE8A", "WEEOUT1", "TIANAOUT1", "TIANAOUT2", "TIANAOUT3")) %>%
  mutate(Bay.Year = paste(Bay, Year, sep = " "))

# Reorder the Bay.Year factors so the boxplots will be in the correct order.
clamsanct.density$Bay.Year <- factor(clamsanct.density$Bay.Year, levels = c("Weesuck Creek 2013", 
        "Weesuck Creek 2014", "Weesuck Creek 2015", "Weesuck Creek 2016", "Weesuck Creek 2017", "Weesuck Creek 2018",
        "Weesuck Creek 2019", "Weesuck Creek 2020", "Weesuck Creek 2021", "Weesuck Creek 2022",
        "Tiana Bay 2015", "Tiana Bay 2016", "Tiana Bay 2017", "Tiana Bay 2018", "Tiana Bay 2019",
        "Tiana Bay 2020", "Tiana Bay 2021", "Tiana Bay 2022"))

# Calculate the mean live clam number per 1 m2 using all Weesuck or all Tiana samples in a given year.
#   This is mostly to check against previous years' calculations, since the figure in the report is boxplots.
clamsanct.density.siteyr <- clamsanct.density %>%
  group_by(Bay.Year) %>%
  summarize(Mean.m2 = mean(N.Alive.m2), Median.m2 = median(N.Alive.m2), num=n(), SD.m2 = sd(N.Alive.m2), 
            SEM = se(N.Alive.m2))
max(clamsanct.density$N.Alive.m2)

clamsanct.density.sites <- clamsanct.density %>%
  group_by(Site.ID, Year) %>%
  summarize(Mean.m2 = mean(N.Alive.m2), Median.m2 = median(N.Alive.m2), num=n(), SD.m2 = sd(N.Alive.m2), 
            SEM = se(N.Alive.m2))

max(clamsanct.density$N.Alive.m2)


# END OF SCRIPT =====================================================================================================
#   Procede to "3_ClamDensityAndSurvival_Plots_ShiRP_HardClamSanctuaries_2021.R"