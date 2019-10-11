
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
new <- left_join(df,accident, by = "ST_CASE",copy=TRUE)
write_csv(new, "bikeaccidents.csv") 


# Remove Pedestrian Data Columns
new <- select(new, c(-VEH_NO,-PEDCTYPE, -PEDLOC, -PEDPOS, -PEDDIR, -MOTDIR, -MOTMAN, -PEDLEG, -PEDSNR, -PEDCGP, -STATE.y, -VE_TOTAL, -VE_FORMS, -PVH_INVL, -PEDS, -PERNOTMVIT, -PERMVIT, -PERSONS, -COUNTY, -CITY, -NHS, -RD_OWNER, -ROUTE, -MILEPT, -SP_JUR, -RELJCT1, -RELJCT2, -RAIL, -NOT_HOUR, -NOT_MIN, -ARR_HOUR, -ARR_MIN, -HOSP_HR, -HOSP_MN))      # No longer needed
# Only include Bicycle Fatalities
new <- subset(new, BIKECTYPE != 0)

#Merge Columns to create date

# Specily Column Types
#   ST_CASE: Integer?
#   PBAGE: Factor
#   STATE: Factor
# 
#my_cols

