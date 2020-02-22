# Load Packages
list.of.packages <- c("dplyr","RSQLite")
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos="http://cran.rstudio.com/")

library(RSQLite)
library(dplyr)

# Set Working Directory
# setwd("~/Dropbox/GS/Research/Sports/Baseball/Relief-Fatigue")

# author : Connect to Database they now opens the sqlite database created in
# 00_scrape_data.R to utilize the scraped data

my_db <- src_sqlite("Data/pitchRx.sqlite3", create = FALSE)

# In the next two chunks of code, they create the initial two tables that now
# reside in the environment and not in the sqlite database. Thus, now they are
# able to work with the data in R. They create two databases, one with pitcher
# statistics and the other with the complimentary batting stats.


# At-Bat Data
at_bat <- my_db %>%
  tbl("atbat") %>%
  dplyr::select(gameday_link, batter, pitcher, num, event,
                 inning_side, inning, batter_name, pitcher_name,
                 date,o, p_throws)

# Pitch Data
pitch <- my_db %>%
  tbl("pitch") %>%
  dplyr::select(des, type, x, y, start_speed, end_speed, sz_top,
               sz_bot, pfx_x, pfx_z, px, pz, x0, y0, z0, vx0, vy0, vz0, ax, ay, az, break_y,
               break_angle, break_length, pitch_type, spin_dir, spin_rate,
               inning_side, inning, num, count, zone, gameday_link, sv_id)


# From here, they now integrate the two datasets created. Now the hitting data
# is synced with the pitching data. Say if on a particular pitch by Pitcher X,
# batter Y hit a double. Now all of this data is in one row together.

#author : Merge the two datasets

pitch <- inner_join(at_bat, pitch, by = c("num", "gameday_link", "inning",
                                          "inning_side"))


# Author saves data as .Rdata so as not to need to query the sqlite database anymore. 

# author : Collect the Data as an R object  
pitch <- collect(pitch) 
save(pitch, file = "Data/pitch_raw.Rdata")
