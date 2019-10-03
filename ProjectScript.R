
# Clear Workspace
rm(list=ls())

# Set Working Directory on your own device
# Install all necessary packages
  # readr
  # dplyr
  # ggplot2
  # scales
  # ..... 

# Load libraries  
library(readr)

# Read CSV Files
df <- read_csv("PBType.csv")
accident <- read_csv("accident.csv")

# Specily Column Types
    # ST_CASE: Integer?
    # PBAGE: Integer?
    
#my_cols

# Remove Pedestrian Data Columns

# Only include Bicycle Fatalities

# Merge with Accident Data
