# author comments: Set working directory
# setwd("~/Dropbox/GS/Research/Sports/Baseball/Relief-Fatigue")

# Loading the necessary packages
list.of.packages <- c("pitchRx","RSQLite", "dplyr")
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos="http://cran.rstudio.com/")

# pitchRX is a package used to scrape MLB Statacast data. 

library(pitchRx)

# The authors method of saving the data in an SQL database to query. 

library(RSQLite)
library(dplyr)

# Building the database in the directory to store the Statcast data 

my_db <- src_sqlite("Data/pitchRx.sqlite3", create = TRUE)

# query to collect his data from the pitchRX package. They specifies that they wants
# every pitch from 2008-2017. We will see however, that for the project, they only
# use 2014-2016 data. Not really sure why went so far back in his query. Also if
# you notice, they send all the data that was scraped to the database initially
# created.

scrape(start = "2008-01-01", end = "2017-01-01", connect = my_db$con)





