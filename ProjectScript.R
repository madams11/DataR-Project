#Group 4
# Data Programming in R : Fall 2019
# US Bicycle Fatalities


# Clear Workspace
rm(list=ls())

# Set Working Directory on your own device
# Install all necessary packages

#install.packages("dplyr")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("choroplethr")
#install.packages("choroplethrMaps")

# Load libraries  
library(readr)
library(dplyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)

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

# Changing Levels of Factor Data
levels(df$STATE.x) <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","Tennessee","Texas","Utah","Vermont","Washington","West Virginia","Wisconsin")
levels(df$PBSEX) <- c("Male","Female","Not Reported","Unknown")
levels(df$PBPTYPE) <- c("Bicyclist","Other")
levels(df$PBCWALK) <- c("None","Yes","Unknown")
levels(df$PBSWALK) <- c("None","Yes","Unknown")
levels(df$PBSZONE) <- c("None", "Yes", "Unknown")
levels(df$BIKECTYPE) <- c("Motorist Turning Error - Left Turn","Motorist Turning Error - Right Turn","Bicyclist Turning Error - Left Turn","Bicyclist Turning Error - Right Turn","Bicyclist Lost Control - Oversteering, Improper Braking, Speed","Bicyclist Lost Control - Alcohol/Drug Impairment","Bicyclist Lost Control - Surface Conditions","Bicyclist Lost Control - Other/Unknown","Motorist Lost Control - Mechanical Problems","Motorist Lost Control - Oversteering, Improper Braking, Speed","Motorist Lost Control - Alcohol/Drug Impairment","Motorist Lost Control - Other/Unknown","Motorist Drive-Out - Sign-Controlled Intersection","Bicyclist Ride-Out - Sign-Controlled Intersection","Motorist Drive-Through - Sign-Controlled Intersection","Bicyclist Ride-Through - Sign-Controlled Intersection","Sign-Controlled Intersection - Other/Unknown","Motorist Drive-Out - Right Turn on Red","Bicyclist - Ride-Out - Signalized Intersection","Motorist Drive-Through - Signalized Intersection","Bicyclist Ride-Through - Signalized Intersection","Bicyclist Failed to Clear - Trapped","Signalized Intersection - Other/Unknown","Bicyclist Failed to Clear - Unknown","Crossing Paths - Uncontrolled Intersection","Crossing Paths - Intersection - Other/Unknown","Motorist Left Turn - Same Direction","Motorist Left Turn - Opposite Direction","Motorist Right Turn - Same Direction","Motorist Right Turn - Opposite Direction","Motorist Right Turn on Red - Same Direction","Motorist Turn/Merge - Other/Unknown","Bicyclist Left Turn - Same Direction","Bicyclist Left Turn - Opposite Direction","Bicyclist Right Turn - Same Direction","Bicyclist Right Turn - Opposite Direction","Bicyclist Ride-out - Parallel Path","Motorist Overtaking - Undetected Bicyclist","Motorist Overtaking - Misjudged Space","Motorist Overtaking - Bicyclist Swerved","Motorist Overtaking - Other/Unknown","Bicyclist Overtaking - Passing on Right","Bicyclist Overtaking - Passing on Left","Bicyclist Overtaking - Other/Unknown","Wrong-Way/Wrong-Side - Bicyclist","Wrong-Way/Wrong-Side - Motorist","Wrong-Way/Wrong-Side - Unknown","Parallel Paths - Other/Unknown","Bicyclist Ride-Out - Residential Driveway","Bicyclist Ride-Out - Commercial Driveway","Bicyclist Ride-Out - Driveway, Unknown Type","Bicyclist Ride-Out - Other Midblock","Bicyclist Ride-Out - Unknown","Motorist Drive-Out - Commercial Driveway","Motorist Drive-Out - Driveway, Unknown Type","Crossing Paths - Midblock - Other/Unknown","Backing Vehicle","Play Vehicle-Related","Unusual Circumstances","Non-Trafficway","Unknown Approach Paths","Unknown Location")
levels(df$BIKELOC) <- c("At Intersection", "Intersection-Related","Not at Intersection", "Non-Trafficway Location","Unknown")
levels(df$BIKEPOS) <- c("Travel Lane", "Bicycle Lane", "Sidewalk", "Non-Trafficway-Parking Lot","Other", "Unknown")
levels(df$BIKEDIR) <- c("With Traffic", "Facing Traffic", "","")
levels(df$BIKECGP) <- c("Loss of Control/Turning Error","Motorist Failed to Yield - Sign-Controlled Intersection","Bicyclist Failed to Yield - Sign-Controlled Intersection","Motorist Failed to Yield - Signalized Intersection","Bicyclist Failed to Yield - Signalized Intersection","Crossing Paths - Other Circumstances","Motorist Left Turn/Merge","Motorist Right Turn/Merge","Bicyclist Left Turn/Merge","Bicyclist Right Turn/Merge","Motorist Overtaking Bicyclist","Bicyclist Overtaking Motorist","Wrong-Way/Wrong-Side","Parallel Paths - Other Circumstances","Bicyclist Failed to Yield - Midblock","Motorist Failed to Yield - Midblock","Backing Vehicle","Other/Unusual Circumstances","Non-Trafficway","Other/Unknown - Insufficient Details")
levels(df$DAY_WEEK) <- c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
levels(df$RUR_URB) <- c("Rural","Urban","", "", "")
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

# Collapse Levels of Bicycle Crash Types

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
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Overtaking - Undetected Bicyclist"] <- "Motorist Passing Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Overtaking - Misjudged Space"] <- "Motorist Passing Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Overtaking - Bicyclist Swerved"] <- "Motorist Passing Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Motorist Overtaking - Other/Unknown"] <- "Motorist Passing Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Overtaking - Passing on Right"] <- "Bicyclist Passing Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Overtaking - Passing on Left"] <- "Bicyclist Passing Error"
levels(df$BIKECTYPE)[levels(df$BIKECTYPE) == "Bicyclist Overtaking - Other/Unknown"] <- "Bicyclist Passing Error"
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


# Age of Victim - Norean

# Sex of victim - Brian

# Add new column for time of day
df$TIME <- df$HOUR

# Define buckets of time based on the hour
mat0 <- df$HOUR >= 4 & df$HOUR < 7   # Will be set to Early Morning (4am to 6am)
mat1 <- df$HOUR >= 7 & df$HOUR < 11  # Will be set to Morning (7am to 10am)
mat2 <- df$HOUR >= 11 & df$HOUR < 15  # Will be set to Midday (11am to 2pm)
mat3 <- df$HOUR >= 15 & df$HOUR < 19  # Will be set to Afternoon (3pm to 6pm)
mat4 <- df$HOUR >= 19 & df$HOUR < 22  # Will be set to Evening (7pm to 9pm)
mat5 <- df$HOUR >= 22 | df$HOUR < 4  # Will be set to Overnight (10pm to 3am)

# Translate new column to the bucketed times of day
df$TIME[mat0] <- "Early Morning"
df$TIME[mat1] <- "Morning"
df$TIME[mat2] <- "Midday"
df$TIME[mat3] <- "Afternoon"
df$TIME[mat4] <- "Evening"
df$TIME[mat5] <- "Overnight"

# Set new column to factor, and set order
df$TIME <- factor(df$TIME, levels = c("Early Morning", "Morning", "Midday", 
                                      "Afternoon", "Evening", "Overnight"))



# Choropleth Map
mapdf <- group_by(df, STATE.x)
summ <- summarize(mapdf, value=n())
summ$region <- tolower(summ$STATE.x)
data("state.regions")
st <- state.regions
st$region <- factor(st$region)
summ <- summ[order(tolower(summ$region)), ]

new <- semi_join(st, summ)
new$value <- summ$value

p <- state_choropleth(new,new$value)
ggsave(filename = "United_States_Map.png", plot = p, width = 12, height = 8,
       units = "in")

# Top 5 States Table
top5state <- group_by(df, STATE.x)
top5state <- summarize(top5state, Number_of_Fatalities=n())
top5state <- top5state[order(top5state$Number_of_Fatalities, decreasing = TRUE), ]
top5state <- head(top5state,n=5)
top5state$STATE.x <- as.character(top5state$STATE.x)
top5state$STATE.x <- factor(top5state$STATE.x)
top5state <- arrange(desc(top5state$Number_of_Fatalities))

# Top 5 States Graph

top5state$STATE.x <- factor(top5state$STATE.x, levels=top5state[[1]])
ggplot(top5state,mapping=aes_(y=top5state$Number_of_Fatalities, x=top5state$STATE.x))+ geom_bar(stat="identity")

# Frequency by Time of Day - Jodie 
## Exclude records for which time of day was not available (8 fatalities)
df_time <- subset(df, HOUR != 99)  
acc_time <- qplot(TIME, data = df_time, geom = "bar", 
                  fill = I("darkblue"), 
                  color = I("greenyellow"), 
                  alpha = I(0.8))
acc_time <- acc_time + scale_x_discrete(name = "Time of Day")
acc_time <- acc_time + ylab("Number of Fatalities")
acc_time <- acc_time + theme(panel.grid.minor.y = element_blank())
acc_time <- acc_time + theme(panel.grid.major.x = element_blank())
acc_time <- acc_time + theme(panel.grid.minor.x = element_blank())
acc_time <- acc_time + theme(axis.ticks.x = element_blank())
acc_time

ggsave(filename = "Fatalities_by_Time.pdf", plot = acc_time, width = 6, height = 4,
       units = "in")


# Day of week graph - Jodie
day <- qplot(DAY_WEEK, data = df, geom = "bar", 
                  fill = I("darkblue"), 
                  color = I("greenyellow"), 
                  alpha = I(0.8))
day <- day + scale_x_discrete(name = "Day of the Week")
day <- day + ylab("Number of Fatalities")
day <- day + theme(panel.grid.minor.y = element_blank())
day <- day + theme(panel.grid.major.x = element_blank())
day <- day + theme(panel.grid.minor.x = element_blank())
day <- day + theme(axis.ticks.x = element_blank())
day

ggsave(filename = "Fatalities_by_Day.pdf", plot = day, width = 6, height = 4,
       units = "in")

# Time of Day of Crashes, Bike Direction, and Rural or Urban location - Jodie
time_dir <- qplot(TIME, data = df2, geom = "bar", facets = . ~ BIKEDIR, fill=RUR_URB)
time_dir <- time_dir + scale_fill_manual(name = "Location", values = c("darkblue", "greenyellow"))
time_dir <- time_dir + scale_x_discrete(name = "Time of Day")
time_dir <- time_dir + ylab("Number of Fatalities")
time_dir <- time_dir + theme(panel.grid.minor = element_blank())
time_dir <- time_dir + theme(panel.grid.major.x = element_blank())
time_dir <- time_dir + theme(axis.text.x = element_text(angle = 45))
time_dir

ggsave(filename = "Fatalities_by_Time_Dir_Urb.pdf", plot = time_dir, width = 6, height = 4,
       units = "in")

# Frequency Chart by Crash Type - Brian
df3 <- df
df3 <- subset(df3, BIKECTYPE != "Other")
df3$BIKECTYPE <- factor(df3$BIKECTYPE, levels=c("Motorist Passing Error","Bicyclist Error","Bicyclist Turning Error","Motorist Turning Error","Motorist Error","Bicyclist Passing Error"))
crashtype <- qplot(BIKECTYPE, data = df3, geom = "bar",fill = I("darkblue"), alpha = I(0.8))
crashtype <- crashtype + ylab("Number of Fatalities")
crashtype <- crashtype + scale_x_discrete(name = "Crash Type")
crashtype <- crashtype + theme(axis.text.x = element_text(angle = 45))
crashtype

# Frequency Chart by Sex of Rider - Brian
df5 <- df
df5 <- subset(df5, PBSEX != "Unknown")
df5 <- subset(df5, PBSEX != "Not Reported")
df5 <- subset(df5, BIKEDIR != "")

pbsex_dir <- qplot(TIME, data = df5, geom = "bar", facets = . ~ BIKEDIR, fill=PBSEX)
pbsex_dir <- pbsex_dir + scale_fill_manual(name = "Sex", values = c("darkblue", "greenyellow"))
pbsex_dir <- pbsex_dir + scale_x_discrete(name = "Time of Day")
pbsex_dir <- pbsex_dir + ylab("Number of Fatalities")
pbsex_dir <- pbsex_dir + theme(axis.text.x = element_text(angle = 45))
pbsex_dir


#Weather - Sam
df4 <- subset(df,df$WEATHER != "Not Reported")
df4 <- subset(df4, df4$WEATHER !="Unknown" )
df4$WEATHER <- factor(df4$WEATHER, levels=c("Normal","Cloudy","Rain","Snow","Fog"))
qplot(WEATHER, data = df4, geom = "bar",fill = I("darkblue"), color = I("greenyellow"), alpha = I(0.8))

#Drunk Driving - Sam

df1 <- df
df1$DRUNK_DR <- as.factor(df1$DRUNK_DR)
levels(df1$DRUNK_DR)[levels(df1$DRUNK_DR) == "0"] <- "Sober Driver"
levels(df1$DRUNK_DR)[levels(df1$DRUNK_DR) != "Sober Driver"] <- "Drunk Driver"

p <- qplot(DRUNK_DR, data = df1, geom = "bar", fill = BIKECTYPE)
p <- p + theme(axis.text.x = element_text(angle = 45))
p


# Other plots evaluated, but not included in final report

# Direction of traffic and urban vs rural
#   Pick a color pallette 
p <- qplot(BIKEDIR, data = df, geom = "bar", fill=RUR_URB)
p <- p + theme(axis.text.x = element_text(angle = 45))
p

# KEEP THIS ONE
# Time of Day, Direction of Travel, and Rural vs Urban
# null the Traffic Way not in State Inventory and Not Reported
qplot(HOUR, data = df, geom = "bar", binwidth=4, facets = . ~ BIKEDIR, fill=RUR_URB)



##Use this to look at a certain bike crash type. the bike crash group is even more detail 
p <- qplot(BIKECTYPE, data = df, geom = "bar")
p <- p + theme(axis.text.x = element_text(angle = 45))
p


# Type of Error, and Where the Crash Occurred
p <- qplot(BIKECTYPE, data = df, geom = "bar",fill=BIKELOC)
p  <- p + theme(legend.position = "bottom")
p <- p + theme(axis.text.x = element_text(angle = 45))
p

# Time of Crash, Type of Error, and Where the Crash Occurred. different view
qplot(HOUR, BIKECTYPE, data = df, geom = "point", color = BIKELOC)

qplot(LONGITUD, LATITUDE, data = df, xlim = c(-125, -66),
      ylim = c(25, 50), na.rm = TRUE)

# Bike Direction and Rural or Urban location
df2 <- subset(df, RUR_URB != "")
df2 <- subset(df2, BIKEDIR != "")

dir_loc <- qplot(BIKEDIR, data = df2, geom = "bar", fill=RUR_URB)
dir_loc <- dir_loc + scale_fill_manual(name = "Location", values = c("darkblue", "greenyellow"))
dir_loc <- dir_loc + scale_x_discrete(name = "Bike Direction")
dir_loc <- dir_loc + ylab("Number of Fatalities")
dir_loc <- dir_loc + theme(panel.grid.minor = element_blank())
dir_loc <- dir_loc + theme(panel.grid.major.x = element_blank())
dir_loc

ggsave(filename = "Fatalities_by_Dir_Urb.pdf", plot = time_dir, width = 6, height = 4,
       units = "in")


# Function - returns count of fatalities for input state
     # Reference state abbreviations for error handling.

st.codes <- data.frame(state = as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI",
                                           "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN",
                                           "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH",
                                           "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT",
                                           "WA", "WI", "WV", "WY")),
                       full = as.factor(c("Alaska","Alabama" ,  "Arkansas", "Arizona","California" , "Colorado" ,
                                          "Connecticut", "District of Columbia","Delaware" ,  "Florida" , "Georgia" ,
                                          "Hawaii","Iowa" ,"Idaho" , "Illinois" , "Indiana" ,  "Kansas" ,
                                          "Kentucky" , "Louisiana" , "Massachusetts", "Maryland" ,"Maine" ,
                                          "Michigan" , "Minnesota" , "Missouri" ,"Mississippi" ,  "Montana" ,
                                          "North Carolina","North Dakota", "Nebraska" , "New Hampshire" , "New Jersey" ,  "New Mexico" ,
                                          "Nevada" ,"New York" , "Ohio" , "Oklahoma" ,
                                          "Oregon" , "Pennsylvania" , "Puerto Rico", "Rhode Island" , "South Carolina", "South Dakota" ,
                                          "Tennessee" , "Texas" , "Utah" ,  "Virginia","Vermont" ,
                                          "Washington" , "Wisconsin", "West Virginia" , "Wyoming")))
my_error_handling <- function(st){
  if(! st %in% st.codes$state){
    error <- "Please enter a valid State or State Appreviation"
    stop(error)
  }
  rows <- grep(pattern = tolower(st), x = tolower(st.codes$state), fixed = TRUE)
  state <- st.codes[[rows,"full"]]
  state <- as.character(state)
  state
}

state_cnt <- function(state){
  if((tolower(state) %in% tolower(df$STATE.x))=="FALSE"){
    state <- my_error_handling(state)
  }
  
  state <- tolower(state)
  df2 <- subset(df, state == tolower(df$STATE.x))
  df2 <-count(df2, BIKECTYPE)
  df2[order(-df2$n), ]
}

# Test function
state_cnt("Iowa")
state_cnt("MN")
state_cnt("Bob")
state_cnt("California")
state_cnt("Arizona")
state_cnt("TX")
