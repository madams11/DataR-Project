library(tidyverse)
library(lubridate)
library(ggcal)

df <- read_csv("bikeaccidents.csv") %>% filter(BIKECTYPE != 0)
names(df)
# [1] "STATE.x"    "ST_CASE"    "VEH_NO"     "PER_NO"     "PBPTYPE"    "PBAGE"     
# [7] "PBSEX"      "PBCWALK"    "PBSWALK"    "PBSZONE"    "PEDCTYPE"   "BIKECTYPE" 
# [13] "PEDLOC"     "BIKELOC"    "PEDPOS"     "BIKEPOS"    "PEDDIR"     "BIKEDIR"   
# [19] "MOTDIR"     "MOTMAN"     "PEDLEG"     "PEDSNR"     "PEDCGP"     "BIKECGP"   
# [25] "STATE.y"    "VE_TOTAL"   "VE_FORMS"   "PVH_INVL"   "PEDS"       "PERNOTMVIT"
# [31] "PERMVIT"    "PERSONS"    "COUNTY"     "CITY"       "DAY"        "MONTH"     
# [37] "YEAR"       "DAY_WEEK"   "HOUR"       "MINUTE"     "NHS"        "RUR_URB"   
# [43] "FUNC_SYS"   "RD_OWNER"   "ROUTE"      "TWAY_ID"    "TWAY_ID2"   "MILEPT"    
# [49] "LATITUDE"   "LONGITUD"   "SP_JUR"     "HARM_EV"    "MAN_COLL"   "RELJCT1"   
# [55] "RELJCT2"    "TYP_INT"    "WRK_ZONE"   "REL_ROAD"   "LGT_COND"   "WEATHER1"  
# [61] "WEATHER2"   "WEATHER"    "SCH_BUS"    "RAIL"       "NOT_HOUR"   "NOT_MIN"   
# [67] "ARR_HOUR"   "ARR_MIN"    "HOSP_HR"    "HOSP_MN"    "CF1"        "CF2"       
# [73] "CF3"        "FATALS"     "DRUNK_DR"


# Set up additional columns -----------------------------------------------
df$MONTH_NAME <- factor(month.name[df$MONTH], levels = rev(month.name))
df$DATE <- ymd(paste0(df$YEAR,"-",df$MONTH,"-",df$DAY))
df$DAY_NAME <- wday(df$DATE,label = T,abbr = F)

# This didn't really yield much
ggplot(df, 
       aes(x = MONTH_NAME, 
           fill = DAY_NAME, 
           group = DAY_NAME)) + 
  geom_bar() + coord_flip() + facet_wrap(~DAY_NAME)

# Drop Hour 99 data
ggplot(df %>% filter(HOUR < 24), 
       aes(x = DAY_NAME, 
           fill = HOUR, 
           group = HOUR)) + 
  geom_bar() + coord_flip() + 
  facet_wrap(~MONTH_NAME) + 
  scale_fill_viridis_c()

# Hour + Day Name ---------------------------------------------------------

ggplot(df %>% 
         filter(HOUR < 24) %>%
         group_by(DAY_NAME, 
                  HOUR) %>%
         summarise(n = n()), 
       aes(x = DAY_NAME, 
           y = HOUR, 
           fill = n)) + 
  geom_tile() + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Number of Observations by Day of Week and Hour of Day") +
  scale_fill_viridis_c(na.value = "black")
ggsave("./plots/day_hour.png", width = 8, units = "in")

# Seperate by Month -------------------------------------------------------

# Showcases how little data there is
ggplot(df %>% 
         filter(HOUR < 24) %>%
         group_by(DAY_NAME, 
                  HOUR, 
                  MONTH_NAME) %>%
         summarise(n = n()), 
       aes(x = DAY_NAME, 
           y = HOUR, 
           fill = n)) + 
  geom_tile() + 
  facet_wrap(~MONTH_NAME) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Day of Week") +
  ylab("Hour of Day") +
  ggtitle("Number of Observations by Day of Week and Hour of Day") +
  scale_fill_viridis_c(na.value = "black")
ggsave("./plots/day_hour_month.png", width = 8, units = "in")


# Calendar View -----------------------------------------------------------

ggcal(df %>% 
        filter(HOUR < 24) %>%
        group_by(DAY_NAME, 
                 HOUR, 
                 MONTH_NAME) %>%
        summarise(n = n()), myfills2)
