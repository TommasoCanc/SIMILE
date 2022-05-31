###############################
# Author: Tommaso Cancellario #
# Reviewer:                   #
# Creation: 2022 - 05 - 30    #
# Last update: 2022 - 05 - 30 #
###############################

# Libraries
library(ggplot2)
library(lubridate)
library(suncalc)

# Set working directory
setwd("~/Desktop/")

# Load data chl bouy
df <- read.csv("SIMILE/Data/LM1buoy_pigments data_ hourly_2020_2021.csv")
df$dateHour <- paste(df$date, df$time)
df$dateHour <- ymd_hms(df$dateHour)
head(df)
str(df)

df$daily <- floor_date(df$dateHou, "day") # We can substitute with day 
df$hour <- floor_date(df$dateHou, "hour") # We can substitute with day 
head(df)

# filter sunset sunrise
day.night <- data.frame()

for(i in 1:length(unique(df$daily))){
  
  sun <- getSunlightTimes(
    date = seq.Date(as.Date(as.character(unique(df$daily)[i])), as.Date(as.character(unique(df$daily)[i])), 
                    by = 1),
    keep = c("sunrise", "sunset"),
    lat = 45.9283,
    lon = 8.5554,
    tz = "UTC") # UTC: Coordinated Universal Time
  
  # Night
  night <- floor_date(ymd_hms(sun$sunset), "hour")
  # Day
  day <- floor_date(ymd_hms(sun$sunrise), "hour")
  
  df.sub <- df[df$daily %in% unique(df$daily)[i], ]
  df.sub$dayNight <- NA
  df.sub[df.sub$hour < day,]$dayNight <-"Night"
  df.sub[df.sub$hour > night,]$dayNight <-"Night"
  df.sub$dayNight <- ifelse(is.na(df.sub$dayNight), "Day", df.sub$dayNight)
  
  day.night <- rbind(day.night, df.sub)
  
}

# Extract only night period
night <- day.night[day.night$dayNight == "Night", ]

chl.means <- aggregate(Chl.S_microg.L ~ daily, night, mean)
colnames(chl.means)[2] <- "Chl_tot"
chl.means$technique <- "buoy"
head(chl.means)
str(chl.means)


# Load data UV-VIS
df.uv_vis <- read.csv("./SIMILE/Data/UV-VIS.csv")
df.uv_vis$dateHour <- mdy(df.uv_vis$data)
df.uv_vis <- df.uv_vis[df.uv_vis$prof == "s", ] # Filter only superficial
df.uv_vis <- df.uv_vis[ ,c("dateHour", "Chl_tot")]
colnames(df.uv_vis)[1] <- "daily"
df.uv_vis$technique <- "uv_vis"
head(df.uv_vis)

# Join techniques
df_tot <- rbind(chl.means, df.uv_vis)


# Plot
ggplot(df_tot) + 
  geom_area(data = df_tot[df_tot$technique == "buoy", ], aes(x = daily, y = Chl_tot), 
            color = "#00AFBB", 
            fill = "#00AFBB", alpha = .5) +
    geom_point(data = df_tot[df_tot$technique == "uv_vis", ], aes(x = daily, y = Chl_tot), shape = 21, 
               color = "#BB0C00",
               fill = "#BB0C00") +
  theme_bw()
