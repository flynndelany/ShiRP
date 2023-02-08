# Analysis of ShiRP Hard Clam sanctuary for 2020 ShiRP Report
#   This script contains all the packages, formulas, etc. to run all R scripts for the hard clam sanctuary analysis. Run
#     this script first and then open whatever specific script you want to work on

# Authors: Diana Chin, Dylan Cottrell,Flynn DeLany  (based on code from Rebecca Kulp)

# Last Modified: 01/31/22


# Set path for "Here" package ========================================================================================
setwd("D:/Projects/ShiRP/ShiRP_Git/Sanctuaries")


# Load Packages ======================================================================================================
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyverse)

# Create custom formulas/plotting parameters: ========================================================================

# Function for standard error.
se <- function(x) sd(x)/(sqrt(length(x)))

# Make a negation of %in%, i.e. exclude things are %in% a list.
`%!in%` = Negate(`%in%`)

# Default plotting parameters for R.
mar.default <- c(5.1, 4.1, 4.1, 2.1)
mgp.default <- c(3, 1, 0)


# END OF SCRIPT ======================================================================================================
#   Procede to "2_ClamDensityAndSurvivial_ShiRP_HardClamSanctuaries_2021.R"