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
# set base url and make request again to be sure
root <-  "https://app.ticketmaster.com/discovery/v2/venues"
get_it <- GET(url = root, query = list(apikey = .key, 
                                       countryCode = "DE", 
                                       locale = "*", 
                                       page = 21))

content <-  fromJSON(content(get_it, as = "text"))

n <-  as.numeric(content$page[["totalElements"]])

# adjust pages for for-loop later
pages <- as.numeric(content$page[["totalPages"]])-1
entries <- as.numeric(content$page[["size"]])

entries_last_page <- n-entries*pages

# adjust full pages -1 for the for-loop

venue_data <- data.frame(
  name = character(n),
  city = character(n),
  postalCode = character(n),
  address = character(n),
  url = character(n),
  longitude = character(n), 
  latitude = character(n)
)

# robust code for the loop: last page is not full; considered by applying 
# if statement

for(j in 0:pages){
  if(j < pages){
    get_it <- GET(url = root, query = list(apikey = .key, 
                                           countryCode = "DE", 
                                           locale = "*", 
                                           page = j))
    content <-  fromJSON(content(get_it, as = "text"))
    
    if(!(is.null(content[["_embedded"]][["venues"]]$name))){
      venue_data[(j*entries+1):(j*entries+entries), "name"] <- content[["_embedded"]][["venues"]]$name
    }
    else{
      venue_data[(j*entries+1):(j*entries+entries), "name"] <- NA
    }
    
    if(!(is.null(content[["_embedded"]][["venues"]]$city$name))){
      venue_data[(j*entries+1):(j*entries+entries), "city"] <- content[["_embedded"]][["venues"]]$city$name
    }
    else{
      venue_data[(j*entries+1):(j*entries+entries), "city"] <- NA
    }
    
    if(!(is.null(content[["_embedded"]][["venues"]]$postalCode))){
      venue_data[(j*entries+1):(j*entries+entries), "postalCode"] <- content[["_embedded"]][["venues"]]$postalCode
    }
    else{
      venue_data[(j*entries+1):(j*entries+entries), "postalCode"] <- NA
    }
    
    if(!(is.null(content[["_embedded"]][["venues"]]$url))){
      venue_data[(j*entries+1):(j*entries+entries), "url"] <- content[["_embedded"]][["venues"]]$url
    }
    else{
      venue_data[(j*entries+1):(j*entries+entries), "url"] <- NA
    }
    
    if(!(is.null(content[["_embedded"]][["venues"]]$address$line1))){
      venue_data[(j*entries+1):(j*entries+entries), "address"] <- content[["_embedded"]][["venues"]]$address$line1
    }
    else{
      venue_data[(j*entries+1):(j*entries+entries), "address"] <- NA
    }

    if(!(is.null(content[["_embedded"]][["venues"]]$location))){
      venue_data[(j*entries+1):(j*entries+entries), "longitude"] <- content[["_embedded"]][["venues"]]$location$longitude
      venue_data[(j*entries+1):(j*entries+entries), "latitude"] <- content[["_embedded"]][["venues"]]$location$latitude
    }
    else{
      venue_data[(j*entries+1):(j*entries+entries), "longitude"] <- NA
      venue_data[(j*entries+1):(j*entries+entries), "latitude"] <- NA
    }
    
    
    ######################## Now code for the last page: only 18 entries at 
    ######################## the time of writing this code
    
  } else if(j == pages){
    get_it <- GET(url = root, query = list(apikey = .key, 
                                           countryCode = "DE", 
                                           locale = "*", 
                                           page = j))
    content <-  fromJSON(content(get_it, as = "text"))
    
    if(!(is.null(content[["_embedded"]][["venues"]]$name))){
      venue_data[(j*entries+1):(j*entries+entries_last_page), "name"] <- content[["_embedded"]][["venues"]]$name
    }
    else{
      venue_data[(j*entries+1):(j*entries+entries_last_page), "name"] <- NA
    }
    
    if(!(is.null(content[["_embedded"]][["venues"]]$city$name))){
      venue_data[(j*entries+1):(j*entries+entries_last_page), "city"] <- content[["_embedded"]][["venues"]]$city$name
    }
    else{
      venue_data[(j*entries+1):(j*entries+entries_last_page), "city"] <- NA
    }
    
    if(!(is.null(content[["_embedded"]][["venues"]]$postalCode))){
      venue_data[(j*entries+1):(j*entries+entries_last_page), "postalCode"] <- content[["_embedded"]][["venues"]]$postalCode
    }
    else{
      venue_data[(j*entries+1):(j*entries+entries_last_page), "postalCode"] <- NA
    }
    
    if(!(is.null(content[["_embedded"]][["venues"]]$url))){
      venue_data[(j*entries+1):(j*entries+entries_last_page), "url"] <- content[["_embedded"]][["venues"]]$url
    }
    else{
      venue_data[(j*entries+1):(j*entries+entries_last_page), "url"] <- NA
    }
    
    if(!(is.null(content[["_embedded"]][["venues"]]$address$line1))){
      venue_data[(j*entries+1):(j*entries+entries_last_page), "address"] <- content[["_embedded"]][["venues"]]$address$line1
    }
    else{
      venue_data[(j*entries+1):(j*entries+entries_last_page), "address"] <- NA
    }
    
    if(!(is.null(content[["_embedded"]][["venues"]]$location))){
      venue_data[(j*entries+1):(j*entries+entries_last_page), "longitude"] <- content[["_embedded"]][["venues"]]$location$longitude
      venue_data[(j*entries+1):(j*entries+entries_last_page), "latitude"] <- content[["_embedded"]][["venues"]]$location$latitude
    }
    else{
      venue_data[(j*entries+1):(j*entries+entries_last_page), "longitude"] <- NA
      venue_data[(j*entries+1):(j*entries+entries_last_page), "latitude"] <- NA
    }
  
    
  }
  Sys.sleep(0.2)
}

venue_data$latitude <- as.numeric(venue_data$latitude)
venue_data$longitude <- as.numeric(venue_data$longitude)

glimpse(venue_data)
sum(is.na(venue_data$longitude))


############################ Event locations in Germany 
# create NA
long_range <- c(5.866944, 15.043611)
lat_range <- c(47.271679, 55.0846)

venue_data$longitude <- ifelse((venue_data$longitude > long_range[2]) | 
                        (venue_data$longitude < long_range[1]), NA, venue_data$longitude)

venue_data$latitude <- ifelse((venue_data$latitude > lat_range[2]) | 
                                 (venue_data$latitude < lat_range[1]), NA, venue_data$latitude)


ggplot() +
  geom_polygon(
    aes(x = long, y = lat, group = group), data = map_data("world", region = "Germany"),
    fill = "grey90",color = "black") +
  geom_point(data=mapdata, aes(x=longitude, y=latitude), color="red") +
  theme_void() + coord_quickmap() +
  labs(title = "Event locations across Germany", caption = "Source: ticketmaster.com") +
  theme(title = element_text(size=8, face='bold'),
        plot.caption = element_text(face = "italic"))


























