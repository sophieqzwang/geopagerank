
################################################################################
# Master file to produce geopagerank
# August 2025
################################################################################

# insert path to clean_pagerank folder
wd <- "G:/.shortcut-targets-by-id/1boJDCakyAAS94F5KjSRVfXd4ICmDvdRP/Measuring and Pricing Neighborhood Characteristics/clean_pagerank"

# Set to python location
python_path <- "C:/Users/joshboyd/AppData/Local/Programs/Python/Python313/python.exe"


################################################################################
# Setup
################################################################################

library(reticulate)
py$wd <- wd

################################################################################
################################################################################
#
# Start analysis
#
################################################################################
################################################################################

################################################################################
# Process raw data
################################################################################

# Take raw ACS data and make subsets 
source(paste0(wd, "/code/subordinate/Clean_ACS.R"))

# Take raw IRS data and clean
library(reticulate)

py_run_string(paste0("wd = r'''", wd, "'''"))
source_python(paste0(wd, "/code/subordinate/irs_data_clean.py"))

# Data Axel not local


################################################################################
# Generate rankings
################################################################################

# Generate ACS rankings
source(paste0(wd, "/code/subordinate/Pagerank_on_ACS.R"))

# Generate IRS county rankings
library(reticulate)

py_run_string(paste0("wd = r'''", wd, "'''"))
source_python(paste0(wd, "/code/subordinate/IRS_metro_pageranks.py"))

# Generate IRS metro rankings
library(reticulate)

py_run_string(paste0("wd = r'''", wd, "'''"))
source_python(paste0(wd, "/code/subordinate/geo_pagerank.py"))


################################################################################
# Make clean outputs
################################################################################

# ACS data already clean

# IRS data
source(paste0(wd, "/code/subordinate/make_one_IRS_file.R"))


################################################################################
# For Overleaf 12/05/2025
################################################################################

# RUN IRS metro ranking on ACS subset
py_run_string(paste0("wd = r'''", wd, "'''"))
source_python(paste0(wd, "/code/subordinate/replicate_ACS_IRS_comparison_12_4_2025.py"))

# compare ACS and IRS results on the same set of metros; also generate many plots
source(paste0(wd, "/code/subordinate/IRS_metro_rankings_12_4_2025.R"))







