# Analysis of ShiRP Benthic Survey Data for 2021 ShiRP Report
#   This script contains all the packages, formulas, etc. to run all R scripts for the benthic sampling analysis. Run
#     this script first and then open whatever specific script you want to work on

# Authors: Diana Chin, Dylan Cottrell, Flynn Delany (based on code from Rebecca Kulp)

# Last Modified: 12/16/21


#Set path for "Here" package ========================================================================================
#library(here)
#set_here()

#Set working Directory ==============================================================================================
setwd("D:/Projects/ShiRP/ShiRP_Git/BenthicSurvey")
# NOTE: For the Here package to work appropriately, all called datasets must have a copy saved in the "Analysis" side
#   of the project. E.g. "Benthic_Survey_2012-2021.xlsx" should be saved in "Data -> "2021" and "Analysis" -> "2021"


# Load necessary packages ============================================================================================
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)


# Create custom formulas/plotting parameters: ========================================================================

# Function for standard error.
se <- function(x) sd(x)/sqrt(length(x))

# Make a negation of %in%, i.e. exclude things are %in% a list.
`%!in%` = Negate(`%in%`)

# Default plotting parameters for R.
mar.default <- c(5.1, 4.1, 4.1, 2.1)
mgp.default <- c(3, 1, 0)


# END OF SCRIPT ======================================================================================================
#   Proceed to whichever analysis you choose