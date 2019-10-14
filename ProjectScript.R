
# Clear Workspace
rm(list=ls())

# Set Working Directory on your own device
# Install all necessary packages
  # readr
  # dplyr
  # ggplot2
  # scales
  # ..... 
#install.packages("dplyr")
#install.packages("readr")
# Load libraries  
library(readr)
library(dplyr)
library(ggplot2)



# Read CSV Files
bikes <- read_csv("PBType.csv")
accident <- read_csv("accident.csv")
df <- left_join(bikes,accident, by = "ST_CASE",copy=TRUE)
write_csv(df, "bikeaccidents.csv") 

# Remove Pedestrian Data Columns
df <- select(df, c(-VEH_NO,-PEDCTYPE, -PEDLOC, -PEDPOS, -PEDDIR, -MOTDIR, -MOTMAN, -PEDLEG, -PEDSNR, -PEDCGP, -STATE.y, -VE_TOTAL, -VE_FORMS, -PVH_INVL, -PEDS, -PERNOTMVIT, -PERMVIT, -PERSONS, -COUNTY, -CITY, -NHS, -RD_OWNER, -ROUTE, -MILEPT, -SP_JUR, -RELJCT1, -RELJCT2, -RAIL, -NOT_HOUR, -NOT_MIN, -ARR_HOUR, -ARR_MIN, -HOSP_HR, -HOSP_MN))      # No longer needed
# Only include Bicycle Fatalities
df <- subset(df, BIKECTYPE != 0)

# Specily Column Types
df$STATE.x <- factor(df$STATE.x)
df$PBPTYPE <- factor(df$PBPTYPE)
df$PBSEX <- factor(df$PBSEX)
df$PBCWALK <- factor(df$PBCWALK)
df$PBSWALK <- factor(df$PBSWALK)
df$PBSZONE <- factor(df$PBSZONE)
df$BIKECTYPE <- factor(df$BIKECTYPE)
df$BIKELOC <- factor(df$BIKELOC)
df$BIKEPOS <- factor(df$BIKEPOS)
df$BIKEDIR <- factor(df$BIKEDIR)
df$BIKECGP <- factor(df$BIKECGP)
df$DAY_WEEK <- factor(df$DAY_WEEK)
df$RUR_URB <- factor(df$RUR_URB)
df$FUNC_SYS <- factor(df$FUNC_SYS)
df$HARM_EV <- factor(df$HARM_EV)
df$MAN_COLL <- factor(df$MAN_COLL)
df$TYP_INT <- factor(df$TYP_INT)
df$WRK_ZONE <- factor(df$WRK_ZONE)
df$REL_ROAD <- factor(df$REL_ROAD)
df$LGT_COND <- factor(df$LGT_COND)
df$WEATHER1 <- factor(df$WEATHER1)
df$WEATHER2 <- factor(df$WEATHER2)
df$WEATHER <- factor(df$WEATHER)
df$SCH_BUS <- factor(df$SCH_BUS)
df$CF1 <- factor(df$CF1)
df$CF2 <- factor(df$CF2)
df$CF3 <- factor(df$CF3)
df$DRUNK_DR <- as.integer(df$DRUNK_DR)
df$FATALS <- as.integer(df$FATALS)
df$PER_NO <- as.integer(df$PER_NO)
df$ST_CASE <- as.integer(df$ST_CASE)
df$TWAY_ID <- as.character(df$TWAY_ID)
df$TWAY_ID2 <- as.character(df$TWAY_ID2)
df$PBAGE <- as.numeric(df$PBAGE)
df$LATITUDE <- as.numeric(df$LATITUDE)
df$LONGITUD <- as.numeric(df$LONGITUD)
df$date <- as.Date(with(df, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")

#Changing Levels of Factor Data
levels(df$STATE.x) <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","Tennessee","Texas","Utah","Vermont","Washington","West Virginia","Wisconsin")
levels(df$PBSEX) <- c("Male","Female","Not Reported","Unknown")
levels(df$PBPTYPE) <- c("Bicyclist","Other")
levels(df$PBCWALK) <- c("None","Yes","Unknown")
levels(df$PBSWALK) <- c("None","Yes","Unknown")
levels(df$PBSZONE) <- c("None", "Yes", "Unknown")
levels(df$BIKECTYPE) <- c("Motorist Turning Error - Left Turn","Motorist Turning Error - Right Turn","Bicyclist Turning Error - Left Turn","Bicyclist Turning Error - Right Turn","Bicyclist Lost Control - Oversteering, Improper Braking, Speed","Bicyclist Lost Control - Alcohol/Drug Impairment","Bicyclist Lost Control - Surface Conditions","Bicyclist Lost Control - Other/Unknown","Motorist Lost Control - Mechanical Problems","Motorist Lost Control - Oversteering, Improper Braking, Speed","Motorist Lost Control - Alcohol/Drug Impairment","Motorist Lost Control - Other/Unknown","Motorist Drive-Out - Sign-Controlled Intersection","Bicyclist Ride-Out - Sign-Controlled Intersection","Motorist Drive-Through - Sign-Controlled Intersection","Bicyclist Ride-Through - Sign-Controlled Intersection","Sign-Controlled Intersection - Other/Unknown","Motorist Drive-Out - Right Turn on Red","Bicyclist - Ride-Out - Signalized Intersection","Motorist Drive-Through - Signalized Intersection","Bicyclist Ride-Through - Signalized Intersection","Bicyclist Failed to Clear - Trapped","Signalized Intersection - Other/Unknown","Bicyclist Failed to Clear - Unknown","Crossing Paths - Uncontrolled Intersection","Crossing Paths - Intersection - Other/Unknown","Motorist Left Turn - Same Direction","Motorist Left Turn - Opposite Direction","Motorist Right Turn - Same Direction","Motorist Right Turn - Opposite Direction","Motorist Right Turn on Red - Same Direction","Motorist Turn/Merge - Other/Unknown","Bicyclist Left Turn - Same Direction","Bicyclist Left Turn - Opposite Direction","Bicyclist Right Turn - Same Direction","Bicyclist Right Turn - Opposite Direction","Bicyclist Ride-out - Parallel Path","Motorist Overtaking - Undetected Bicyclist","Motorist Overtaking - Misjudged Space","Motorist Overtaking - Bicyclist Swerved","Motorist Overtaking - Other/Unknown","Bicyclist Overtaking - Passing on Right","Bicyclist Overtaking - Passing on Left","Bicyclist Overtaking - Other/Unknown","Wrong-Way/Wrong-Side - Bicyclist","Wrong-Way/Wrong-Side - Motorist","Wrong-Way/Wrong-Side - Unknown","Parallel Paths - Other/Unknown","Bicyclist Ride-Out - Residential Driveway","Bicyclist Ride-Out - Commercial Driveway","Bicyclist Ride-Out - Driveway, Unknown Type","Bicyclist Ride-Out - Other Midblock","Bicyclist Ride-Out - Unknown","Motorist Drive-Out - Commercial Driveway","Motorist Drive-Out - Driveway, Unknown Type","Crossing Paths - Midblock - Other/Unknown","Backing Vehicle","Play Vehicle-Related","Unusual Circumstances","Non-Trafficway","Unknown Approach Paths","Unknown Location")
levels(df$BIKELOC) <- c("At Intersection", "Intersection-Related","Not at Intersection", "Non-Trafficway Location","Unknown")
levels(df$BIKEPOS) <- c("Travel Lane", "Bicycle Lane", "Sidewalk", "Non-Trafficway-Parking Lot","Other", "Unknown")
levels(df$BIKEDIR) <- c("With Traffic", "Facing Traffic", "Not Applicable","Unknown")
levels(df$BIKECGP) <- c("Loss of Control/Turning Error","Motorist Failed to Yield - Sign-Controlled Intersection","Bicyclist Failed to Yield - Sign-Controlled Intersection","Motorist Failed to Yield - Signalized Intersection","Bicyclist Failed to Yield - Signalized Intersection","Crossing Paths - Other Circumstances","Motorist Left Turn/Merge","Motorist Right Turn/Merge","Bicyclist Left Turn/Merge","Bicyclist Right Turn/Merge","Motorist Overtaking Bicyclist","Bicyclist Overtaking Motorist","Wrong-Way/Wrong-Side","Parallel Paths - Other Circumstances","Bicyclist Failed to Yield - Midblock","Motorist Failed to Yield - Midblock","Backing Vehicle","Other/Unusual Circumstances","Non-Trafficway","Other/Unknown - Insufficient Details")
levels(df$DAY_WEEK) <- c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
levels(df$RUR_URB) <- c("Rural","Urban","Trafficway not in State Inventory", "Not Reported", "Unknown")
levels(df$FUNC_SYS) <- c("Interstate", "Other Freeways and Expressways", "Other", "Minor Arterial","Major Collector", "Minor Collector", "Local", "Trafficway not in State Inventory", "Not Reported", "Unknown")
levels(df$HARM_EV) <- c("Overturn", "Pedalcycle", "Animal", "Other Type Non-Motorist", "Guard Rail", "Impact Attenuator", "Passing Over Bridge or Overpass","Mail Box")
levels(df$MAN_COLL) <- c("Not a Collision with Motor Vehicle in Transport", "Sideswipe - Opposite Direction", "Sideswipe - Same Direction")
levels(df$TYP_INT) <- c("Not an Intersection","Four-Way Intersection", "T-Intersection", "Y-Intersection","Five-Point, or More", "L-Intersection", "Unknown")
levels(df$WRK_ZONE) <- c("None", "Construction", "Work Zone")
levels(df$REL_ROAD) <- c("On Roadway", "On Shoulder", "On Median", "On Roadside", "Off Roadway","Gore", "Unknown")
levels(df$LGT_COND) <- c("Daylight", "Dark - Not Lighted", "Dark - Lighted", "Dawn", "Dusk", "Dark - Unknown Lighting", "Not Reported", "Unknown")
levels(df$WEATHER) <- c("Normal", "Rain", "Snow", "Fog", "Cloudy", "Not Reported", "Unknown")
levels(df$WEATHER1) <- c("Normal", "Rain", "Snow", "Fog", "Cloudy", "Not Reported", "Unknown")
levels(df$WEATHER2) <- c("Normal", "Rain", " Cloudy")
levels(df$SCH_BUS) <- c("No", "Yes")
levels(df$CF1) <- c("None", "Reflected Glare","Motor Vehicle Struck by Falling Cargo","Non-Occupant Struck by Falling Cargo","Non-Occupant Struck Vehicle", "Police Pursuit Involved", " Speed Limit","Backup Due to Congestion","Unknown")
levels(df$CF2) <- c("None", "Non-Occupant Struck Vehicle", "Unknown")
levels(df$CF3) <- c("None", "Unknown")

print(levels(df$BIKECTYPE))

levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Turning Error - Left Turn"] <- "Motorist Turning Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Turning Error - Right Turn"] <- "Motorist Turning Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Turning Error - Left Turn"] <- "Bicyclist Turning Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Turning Error - Right Turn"] <- "Bicyclist Turning Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Lost Control - Oversteering, Improper Braking, Speed"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Lost Control - Alcohol/Drug Impairment"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Lost Control - Alcohol/Drug Impairment"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Lost Control - Surface Conditions"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Lost Control - Other/Unknown"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Lost Control - Mechanical Problems"] <- "Motorist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Lost Control - Oversteering, Improper Braking, Speed"] <- "Motorist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Lost Control - Alcohol/Drug Impairment"] <- "Motorist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Lost Control - Other/Unknown"] <- "Motorist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Drive-Out - Sign-Controlled Intersection"] <- "Motorist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Ride-Out - Sign-Controlled Intersection"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Drive-Through - Sign-Controlled Intersection"] <- "Motorist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Ride-Through - Sign-Controlled Intersection"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Sign-Controlled Intersection - Other/Unknown"] <- "Other"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Drive-Out - Right Turn on Red"] <- "Motorist Turning Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist - Ride-Out - Signalized Intersection"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Drive-Through - Signalized Intersection"] <- "Motorist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Ride-Through - Signalized Intersection"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Failed to Clear - Trapped"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Signalized Intersection - Other/Unknown"] <- "Other"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Failed to Clear - Unknown"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Crossing Paths - Uncontrolled Intersection"] <- "Other"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Crossing Paths - Intersection - Other/Unknown"] <- "Other"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Left Turn - Same Direction"] <- "Motorist Turning Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Left Turn - Opposite Direction"] <- "Motorist Turning Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Right Turn - Same Direction"] <- "Motorist Turning Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Right Turn - Opposite Direction"] <- "Motorist Turning Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Right Turn on Red - Same Direction"] <- "Motorist Turning Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Turn/Merge - Other/Unknown"] <- "Motorist Turning Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Left Turn - Same Direction"] <- "Bicyclist Turning Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Left Turn - Opposite Direction"] <- "Bicyclist Turning Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Right Turn - Same Direction"] <- "Bicyclist Turning Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Right Turn - Opposite Direction"] <- "Bicyclist Turning Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Ride-out - Parallel Path"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Overtaking - Undetected Bicyclist"] <- "Motorist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Overtaking - Misjudged Space"] <- "Motorist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Overtaking - Bicyclist Swerved"] <- "Motorist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Overtaking - Other/Unknown"] <- "Motorist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Overtaking - Passing on Right"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Overtaking - Passing on Left"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Overtaking - Other/Unknown"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Wrong-Way/Wrong-Side - Bicyclist"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Wrong-Way/Wrong-Side - Motorist"] <- "Motorist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Wrong-Way/Wrong-Side - Unknown"] <- "Other"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Parallel Paths - Other/Unknown"] <- "Other"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Ride-Out - Residential Driveway"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Ride-Out - Commercial Driveway"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Ride-Out - Driveway, Unknown Type"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Ride-Out - Other Midblock"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Ride-Out - Unknown"] <- "Bicyclist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Drive-Out - Commercial Driveway"] <- "Motorist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Drive-Out - Driveway, Unknown Type"] <- "Motorist Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Crossing Paths - Midblock - Other/Unknown"] <- "Other"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Backing Vehicle"] <- "Other"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Play Vehicle-Related"] <- "Other"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Unusual Circumstances"] <- "Other"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Non-Trafficway"] <- "Other"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Unknown Approach Paths"] <- "Other"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Unknown Location"] <- "Other"

print(levels(df$BIKECTYPE))

df <- subset(df, HOUR != 99)
df <- subset(df, BIKECTYPE != 0)
df <- subset(df, LONGITUD < 200)
df <- subset(df, LATITUDE < 60)


qplot(HOUR, BIKECTYPE, data = df, geom = "point", facets = . ~ BIKELOC)
qplot(HOUR, BIKECTYPE, data = df, geom = "point", color = BIKELOC)
qplot(BIKECTYPE, data = df, geom = "bar")
qplot(HOUR, data = df, geom = "histogram", binwidth = 4)
qplot(BIKECTYPE, BIKEDIR, data = df, geom = "point", size = HOUR)
qplot(LATITUDE, LONGITUD, data = df, color = BIKEDIR, size = HOUR)
qplot(BIKEDIR, data = df, geom = "bar")
qplot(HOUR, data = df, geom = "density", facets = . ~ BIKEDIR)
qplot(BIKECGP, data = df, geom = "bar")
qplot(LONGITUD, LATITUDE, data = df, xlim = c(-125, -66),
      ylim = c(25, 50), na.rm = TRUE)
qplot(BIKECGP, HOUR, data = df, geom = "jitter")

