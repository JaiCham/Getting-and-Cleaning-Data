library(tidyverse)
library(dplyr)
library(ggplot2)

#1 read csv
getwd()
storm_data <- read_csv("StormEvents_details-ftp_v1.0_d1996_c20220425.csv")
colnames(x=storm_data)

#2 Limit the data frame
columnlimit <- c("BEGIN_YEARMONTH", "BEGIN_DAY", "BEGIN_TIME", "END_YEARMONTH", "END_DAY", "END_TIME", "BEGIN_DATE_TIME", "END_DATE_TIME",
                  "EPISODE_ID", "EVENT_ID", "STATE", "STATE_FIPS", "CZ_NAME", "CZ_TYPE", "CZ_FIPS", "EVENT_TYPE", "SOURCE", "BEGIN_LAT",
                  "BEGIN_LON", "END_LAT", "END_LON")


storm_data_limit <- storm_data[columnlimit]
head(storm_data_limit,5)

#3 Arrange the data by beginning year and month (BEGIN_YEARMONTH) 
storm_data_limit <- arrange(storm_data_limit, BEGIN_YEARMONTH)

#4 Change state and county names to title case 
storm_data_limit$STATE <- str_to_title(storm_data_limit$STATE)
storm_data_limit$CZ_NAME <- str_to_title(storm_data_limit$CZ_NAME)

#5	Limit to the events listed by county FIPS (CZ_TYPE of “C”) and remove the CZ_TYPE column 
storm_data_limit <- filter(storm_data_limit, CZ_TYPE == "C")
storm_data_limit <- select(storm_data_limit, -CZ_TYPE)

#6	Pad the state and county FIPS with a “0” at the beginning 
# and then unite the two columns to make one fips column with the 5 or 6-digit county FIPS code 
storm_data_limit$STATE_FIPS <- str_pad(storm_data_limit$STATE_FIPS, width = 3, side = "left", pad = "0")
storm_data_limit$CZ_FIPS <- str_pad(storm_data_limit$CZ_FIPS, width = 3, side = "left", pad = "0")

storm_data_limit <- unite(storm_data_limit, "fips", c("STATE_FIPS", "CZ_FIPS"), sep = ' ')
storm_data_limit$fips

#7	Change all the column names to lower case 
storm_data_limit <- rename_all(storm_data_limit, tolower)

#8	There is data that comes with base R on U.S. states. Use that to create a dataframe with these three columns: state name, area, and region 
data("state")
us_state_info <- data.frame(state = state.name, 
                            area = state.area,
                            region = state.region)

#9 Create a dataframe with the number of events per state 
#Merge in the state information dataframe in step 8. 
#Remove any states that are not in the state information dataframe
freq_storm_data_limit <- data.frame(table(storm_data_limit$state))
freq_storm_data_limit <- rename(freq_storm_data_limit, c("state" = "Var1"))
new_merged <- merge(x = newset1, y = us_state_info, by.x = "state", by.y = "state")
head(new_merged)

#10	Create plot 
storm_plot <- ggplot(new_merged, aes(x = area, y = Freq)) + 
  geom_point(aes(color = region)) + 
  labs (x = "Land Area (Square Miles)", y = "# of storm events in 1996")
storm_plot

