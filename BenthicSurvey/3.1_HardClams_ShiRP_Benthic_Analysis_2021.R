# Analysis of ShiRP Benthic Survey Data for 2020 ShiRP Report
#   This script is for organizing the hard clam data from benthic sampling (2012-2021)
#     Specifically, removing all unwanted datapoints.

#   Preceeding script: "1_LoadAllPackages_ShiRP_Benthic_Analysis_2021.R"


# Authors: Diana Chin, Dylan Cottrell, Flynn DeLany (based on code from Rebecca Kulp)

# Last Modified: 12/16/21


# NOTE: For the Here package to work appropriately, all called datasets must have a copy saved in the "Analysis" side
#   of the project. E.g. "Benthic_Survey_2012-2021.xlsx" should be saved in "Data -> "2021" and "Analysis" -> "2021"


# Read in benthic survey data =========================================================================================
benthic.clam.raw <- read_excel("Data/Benthic_Survey_2012-2022.xlsx", sheet = "Clam.Measurements", na = "NA")
benthic.site.raw <- read_excel("Data/Benthic_Survey_2012-2022.xlsx", sheet = "Quadrat.Details", na = "NA")
# This will throw warnings about the "1/4" in the Quadrat field but fine because we are going to get rid of 
#   them anyway (below).

# Remove several items from these datasets:
#     From site details dataset, remove cases that are "1/4" in Quadrat field (these are records 
#       of when Rebecca did an extra 1/4 m2 quadrat for Diana in 2016 looking for Solemya only). 
#       Gets rid of 2015 SGV 49 quadrat "3" too but ok here because no clams in clam dataset for 
#       that quadrat.
#     From site details dataset, remove 2016 SGV 92 Q1 and SGV93 Q2 (error in sample processing 
#       and don't know which data correspond to which quadrat).
#     From clam dataset, remove 2016 SGV "92.93" (same as above).
#     From site details dataset and clam dataset, remove 2017 SGV 86 8/10/17 and keep 8/15/2017. 
#       Reason given is they made a return trip to SGV 86, otherwise not sure why the first one is 
#       bad or why it was removed from the site details but not clam dataset in previous code.
#     From site details dataset and clam dataset, remove the clam sanctuaries sampled in 2016 
#       and 2017...these automatically have many clams.
#     From site details dataset and clam dataset, remove 2018 SGV 69 - fairly certain, by the 
#       number of clams, that we accidentally hit the WEE 19 clam sanctuary site.
#     From site details datset and clam dataset, remove Sedge 1-7. These were surveyed to verify 
#       reports of new-set clams and oysters on a sand bar north of Dockers (These sites were not 
#       surveyed as part of benthic sampling per se). No new-set clams found anyway...
exclude <- benthic.site.raw %>%
  filter((Year == 2016 & Site.ID == "SGV 92" & Quadrat == 1) | 
           (Year == 2016 & Site.ID == "SGV 93" & Quadrat == 2) | 
           (Year == 2017 & Site.ID == "SGV 86" & Date == "2017-08-10") |
           (Quadrat %!in% c(1,2)) | 
           (Site.ID %in% c("TIANA 18", "TIANA 5", "TIANA 9", "WEE 11", "WEE 1A", "WEE 6")) | 
           (Year == 2018 & Site.ID == "SGV 69") |
           (Year == 2019 & Site.ID == "Sedge 1") |
           (Year == 2019 & Site.ID == "Sedge 2") |
           (Year == 2019 & Site.ID == "Sedge 3") |
           (Year == 2019 & Site.ID == "Sedge 4") |
           (Year == 2019 & Site.ID == "Sedge 5") |
           (Year == 2019 & Site.ID == "Sedge 6") |
           (Year == 2019 & Site.ID == "Sedge 7") |
           (Year == 2020 & Site.ID == "SGV 94")  |
           (Year == 2021 & Site.ID == "SGV 94"))
benthic.site <- anti_join(benthic.site.raw, exclude)

exclude2 <- benthic.clam.raw %>%
  filter((Year == 2016 & `Site.ID` == "SGV 92.93") |
           (Year == 2017 & `Site.ID` == "SGV 86" & Date == "2017-08-10") |
           (`Site.ID` %in% c("TIANA 18", "TIANA 5", "TIANA 9", "WEE 11", "WEE 1A", "WEE 6")) | 
           (Year == 2018 & `Site.ID` == "SGV 69") |
           (Year == 2019 & Site.ID == "Sedge 1") |
           (Year == 2019 & Site.ID == "Sedge 2") |
           (Year == 2019 & Site.ID == "Sedge 3") |
           (Year == 2019 & Site.ID == "Sedge 4") |
           (Year == 2019 & Site.ID == "Sedge 5") |
           (Year == 2019 & Site.ID == "Sedge 6") |
           (Year == 2019 & Site.ID == "Sedge 7") |
           (Year == 2020 & Site.ID == "SGV 94")  |
           (Year == 2021 & Site.ID == "SGV 94"))
benthic.clam <- anti_join(benthic.clam.raw, exclude2)

# Final Resulting Dataframes: benthic.clam, benthic.site

# END OF SCRIPT ========================================================================================================
#   Proceding script: "3.2_HardClams_Densities_ShiRP_Benthic_Analysis_2021.R"