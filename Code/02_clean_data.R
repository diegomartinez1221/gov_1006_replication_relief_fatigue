# Load Packages
list.of.packages <- c("dplyr")
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos="http://cran.rstudio.com/")

library(dplyr)


# this is the dataset collected and saved in 01_collect_data.R 

# author : Load Raw Data
load("Data/pitch_raw.Rdata")

# Dates are  very important to this project being about how long it takes for a
# pitcher to recover etc. Here they convert it into a data object which will be
# easier than a string to work with throughout the project 

#author: Change date to R Date Object
pitch$date <- as.Date(gsub("_", "-", pitch$date))

# They create an MLB Schedule dataframe here. This is helpful for them to
# distinguish between preseason, regular season, and postseason games. In their
# analysis, they only consider regular season games. It appears pitches in this
# dataset were not classified by when they occured as they had to manually set
# the opening day and end of season dates for each year. In the next chunk of
# code, they manully assign games to season and postseason columns.

years <- 2008:2016
opening <- as.Date(c("2008-03-31", "2009-04-05", "2010-04-04", "2011-03-31", "2012-03-28", "2013-03-31", "2014-03-31",
                     "2015-04-05", "2016-04-03"))
end.reg.season <- as.Date(c("2008-09-30", "2009-10-06","2010-10-05","2011-09-29", "2012-10-04", "2013-09-30",
                            "2014-09-29", "2015-10-05", "2016-10-03"))
mlb.schedule <- data.frame(opening, end.reg.season)
colnames(mlb.schedule) <- c("start", "end")
rownames(mlb.schedule) <- years

# author: Define which pitches are in regular season and postseason games
y <- format(pitch$date, "%Y")
pitch$reg_season <- as.numeric(pitch$date >= mlb.schedule[y,"start"]) & 
  (pitch$date <= mlb.schedule[y,"end"])
pitch$post_season <- as.numeric(pitch$date > mlb.schedule[y,"end"])
pitch$year <- y

# Beginning to filter the data. Now only keeping regular season and postseason
# games and limiting to the more recent years of 2014-2016.


# author: Exclude Spring Training games and Years not in Study
studied.years <- c(2014:2016)     
pitch <- pitch %>%
  filter(reg_season + post_season != 0) %>%
  filter(as.numeric(year) %in% studied.years)

# making distinct columns for balls and strikes. the counts were initially as
# 3-1 representing a count. Now they are distinct columns without the -.

#Create Balls and Strikes Variables

pitch$b <- substr(pitch$count, 1, 1)
pitch$s <- substr(pitch$count, 3, 3)

# author: Find fastest pitch for each pitcher in a given year
fastest.pitch.dat <- pitch %>%
  filter(!is.na(start_speed)) %>%
  group_by(pitcher, year, pitch_type, pitcher_name) %>%
  
  # keeping only pitchers that threw at least 50 pitches of each pitch type in a
  # given year. This is a measure to help keep relievers who do pitch on a more
  # regular basis (project centered around regular relievers)
  
  filter(n() > 50) %>%
  
  # finding the average speed of each pitch a pitcher throws. The group by's and
  # their differences are important in this chunk of code. 
  
  summarise(avg_fast_speed = mean(start_speed)) %>%
  arrange(desc(avg_fast_speed)) %>%
  ungroup() %>%
  
  # notice now the pitch_type group by is now removed. Thus meaning in the
  # filter, we are keeping the fastest pitch each pitcher has. This method is
  # employed because we cannot just set a 4-seam fastball to be the pitch
  # analyzed for velocity. Not everybody throws that type of pitch nor may it be
  # the fastest a given pitcher throws. 
  
  group_by(pitcher, year, pitcher_name) %>%
  filter(row_number() == 1) %>%
  dplyr::select(-pitch_type)

# Inner joining fastest pitch to the complete pitch dataset. Now it will only
# include all the data on the fastest pitchs per player. 

pitch <- inner_join(pitch, fastest.pitch.dat, by = 
                      c("pitcher", "year", "pitcher_name")) %>%
  mutate(diff_speed = start_speed - avg_fast_speed) %>%
  mutate(pitch_type = ifelse(pitch_type=="KC","CU",pitch_type))#Combine KC and CU

# author: Create Swing and Whiff Indicators
swinging <- c("In play, no out", "Foul", "In play, run(s)", "Swinging Strike", 'Foul Tip',
              "Foul (Runner Going)", "Swinging Strike (Blocked)")
whiff <- c("Swinging Strike", "Swinging Strike (Blocked)")

# create more columns from the indicators above. The as.number makes this a 0 or
# 1. 1 if it the result is in the designations above.

pitch$swinging <- as.numeric(pitch$des %in% swinging)
pitch$whiff <- as.numeric(pitch$des %in% whiff)

### author: Dataset for Starting Pitchers, not sure why at the moment since
### project is no relievers.

starters.dat <- pitch %>%
  filter(inning <= 1) 

# getting the numbers of pitches per outings, numbers of pitches thrown is
# related to the amount of a toxin and is an input variable throughout the
# project

starter_games <- starters.dat %>%
  group_by(gameday_link, pitcher, pitcher_name) %>%
  summarise(n = n()) %>%
  dplyr::select(-n)

# Now inner joining to get a dataset of only starting pitchers, but with all the
# rows and columns of pitch.

starters_pitch <- inner_join(pitch, starter_games)

# code to order the pitches and given them a number within each appearance as
# well as what the previous pitch was. This will allow them to tell how many
# pitchers were thrown across however many innings

starters_pitch <- starters_pitch %>%
  group_by(gameday_link, pitcher) %>%
  filter(sum(is.na(sv_id)) == 0 & sum(sv_id == "") == 0) %>%
  arrange(sv_id) %>%
  mutate(prev_pitches = row_number() - 1) %>%
  ungroup() %>%
  
  # only keeping fastball type pitches
  
  filter(!(pitch_type %in% c("SC","KN","FO", "EP")))

# saving to a .Rdata to work with later.

save(starters_pitch, file = "Data/starters_pitch.Rdata")

# author:
### Dataset for Relief Pitchers
# Remove years for pitchers who threw fewer than 300 ptiches
kPitchCutoff = 300

# relief pitchers are defined as everyone that was not a starter in the filter.
# They are making sure to only include relievers used regularly with the pitch
# cut off as this paper is about repeated us of pitchers.

relief_pitch <- pitch %>%
  filter(!(pitcher %in% starters.dat$pitcher)) %>%
  group_by(pitcher, year) %>%
  filter(n() >= kPitchCutoff) %>%
  ungroup() %>%
  mutate(pitcher = factor(pitcher))


# author: Calculate number of pitches thrown in each day
kSecondsInDay = 24*3600
kLookbackDays = 7


# Dataset for the number of pitches pitched by a pitcher each game in a given year. 

player.game = relief_pitch %>%
  group_by(date, gameday_link,pitcher, pitcher_name) %>%
  summarise(num.pitches = n())%>%
  mutate(num.date = as.numeric(strptime(date, format = "%Y-%m-%d"))/kSecondsInDay,
         year = substr(date,1,4)) %>%
  arrange(pitcher, num.date)


#author: Dataset for num.pitches thrown in last 7 days

# central question is what happens in a certain period of times, 7 days, for a
# reliever. this for loop runs through the data to group games by how many days
# a  reliever also pitched before.

npitch = matrix(0, nrow = nrow(player.game), ncol = kLookbackDays)
colnames(npitch) = paste0("npitch",1:kLookbackDays)
for(i in 1:nrow(player.game)){
  for(j in 1:kLookbackDays){
    index = ifelse((i-j)>0, i-j, NA) 
    if(!is.na(index) && #is lookback valid
       (player.game$pitcher[i] == player.game$pitcher[(i-j)]) && #same pitcher?
       (player.game$num.date[i] - player.game$num.date[(i-j)] <= kLookbackDays)){ #within a week?
      d = (player.game$num.date[i] - player.game$num.date[i-j]) #which day in late 7
      npitch[i,d] = player.game$num.pitches[i-j]
    }
  }
}

relief_games <- as.data.frame(cbind(as.matrix(player.game),npitch)) %>%
  dplyr::select(-num.date) %>%
  ungroup() %>%
  mutate(pitcher = factor(pitcher), date = as.Date(date))

# combining the amounts pitchers per game with the pitch by pitch data 
relief_pitch <- left_join(relief_pitch, relief_games) %>%
  ungroup() %>%
  arrange(pitcher, gameday_link) %>%
  mutate(date = as.Date(date), pitcher = factor(pitcher)) %>%
  filter(!(pitch_type %in% c("SC","KN","FO", "EP")))

# savind the datasets for future use.

save(relief_games, file = "Data/relief_games.Rdata")
save(relief_pitch, file = "Data/relief_pitch.Rdata")

### Dataset for Pitch Quality
# only want to analze fastballs and the most common pitches. 

# Remove uncommon pitcher/pitch type combos
pitch_quality <- pitch %>%
  filter(!(pitch_type %in% c("SC","KN","FO", "EP")))

# Remove In-Play Bunts
inplay <- c("In play, no out", "In play, run(s)")
bunt.events <- c("Bunt Lineout", "Bunt Popout", "Bunt Groundout", "Sac Bunt",
                 "Sacrifice Bunt DP")

pitch.swing <- pitch %>%
  filter(swinging == 1) %>%
  filter(!(des %in% inplay & event %in% bunt.events))

# Dataset with only full swings- used to model whiff rates
write.csv(pitch.swing,file = "Data/pitch_swing.csv")



