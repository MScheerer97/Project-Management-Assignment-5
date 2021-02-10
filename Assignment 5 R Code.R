###################### Data Science Project Management - Assignment 5

rm(list = ls())


## Packages 


if (!require("httr")) install.packages("httr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("rlist")) install.packages("rlist")


library(httr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(rlist)

####################### API Key
setwd("C:/Users/marti/Desktop/Data Science Project Management/Assignments/Assignment 5")

# used .XXX to prevent the key value to appear in the global environment
source("API-Key.R")

setwd("C:/Users/marti/Desktop/Project-Management-Assignment-5")

####################################### Task 3 - Basic Interaction with API
# in order to replicate the following code, please adjust the code by using your own 
# key for the get requests

### 5 requests per second
root <-  "https://app.ticketmaster.com/discovery/v2/venues"
get_it <- GET(url = root, query = list(apikey = .key, 
                                        countryCode = "DE", 
                                       locale = "*"))
content <-  fromJSON(content(get_it, as = "text"))

venue_data <- content[["_embedded"]][["venues"]]
name <- venue_data$name
city <- venue_data$city$name
postalCode <- venue_data$postalCode
address <- venue_data$address$line1
url <- venue_data$url
longitude <- as.numeric(venue_data$location$longitude)
latitude <- as.numeric(venue_data$location$latitude)


venue_data <- data.frame(name, city, postalCode, address, url, longitude, latitude)

glimpse(venue_data)

#################################### Task 4

n <-  as.numeric(content$page[["totalElements"]])
pages <- as.numeric(content$page[["totalPages"]])
entries <- as.numeric(content$page[["size"]])

full_pages <- floor(n/entries)

venues <- data.frame(
  name = character(n),
  city = character(n),
  postalCode = character(n),
  address = character(n),
  url = character(n),
  longitude = character(n), 
  latitude = character(n)
)
