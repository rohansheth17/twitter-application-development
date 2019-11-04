install.packages("twitteR")
install.packages("data.table", dependencies=TRUE)
require(data.table)
library(maps)
library(ggplot2)
citation("ggmap")
library("maptools")
gpclibPermit()
library("maps")
library("ggmap")
register_google(key = "AIzaSyDuPOH5A6aona1B0ISgsmTiuKHgb_PEMyw") 
install.packages("revgeo", dependencies = TRUE)
library('revgeo')
library(twitteR)
consumer_key <- '5ln7NEPebQZ76rzYWMoTFh4sz'
consumer_secret <- 'wgMPx0axKj0sieNtCZOvxOXbWbI8UQpRqhPaB0ZIO7ATndqyQD'
access_token <- '1098101509756211201-AZYJqdOKNQDmq8ATsEA6oUWXrjCiEd'
access_secret <- 'zvrtWw6EHOByV9INUWFnzcw1XNGHQShijzqyZXApoYROo'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
devtools::install_github("wmurphyrd/fiftystater")
install.packages("mapproj")
library(mapproj)
library(fiftystater)
library(plyr)
library(dplyr)
View(fifty_states)
fifty_states$group <- ifelse(fifty_states$group == "Alabama.1", 1, fifty_states$group)



# write.csv(tweetsnew1.df, "C:\\Users\\rohan\\OneDrive\\Documents\\tweetsJan2018flu.csv")
# tweetsnew2.df <- tweetsnew1.df[!is.na(tweetsnew1.df$longitude) & !is.na(tweetsnew1.df$latitude) ,]
# write.csv(tweetsnew2.df, "C:\\Users\\rohan\\OneDrive\\Documents\\tweetsJan2018latflu.csv")
#twtnews.df <- as.data.frame(fread(file="C:\\Users\\rohan\\OneDrive\\Documents\\LatLong.csv", header=TRUE))




flu.tweets = searchTwitter("flu",n=1500)
flushot.tweets = searchTwitter("#flushot",n=680)
hashflu.tweets = searchTwitter("#flu",n=1500)
influenza.tweets = searchTwitter("influenza",n=1500)
highfever.tweets = searchTwitter("high fever",n=1500)
flufever.tweets = searchTwitter("flu fever",n=1500)
fluvaccine.tweets = searchTwitter("fluvaccine",n=550)

allTweets.df <- twListToDF(c(flu.tweets,flushot.tweets,hashflu.tweets,influenza.tweets,highfever.tweets,flufever.tweets,fluvaccine.tweets))
View(allTweets.df)

influenzaflufever.df <- twListToDF(c(influenza.tweets,flufever.tweets))
View(influenzaflufever.df)

flu.df <-twListToDF(influenza.tweets)
flu.df <- flu.df[(flu.df$isRetweet) != 'TRUE',]
flu.df <- unique(flu.df)
View(flu.df)
allTweets.df <- allTweets.df[(allTweets.df$isRetweet) != 'TRUE',]
allTweets.df <- unique(allTweets.df)
influenzaflufever.df <- influenzaflufever.df[(influenzaflufever.df$isRetweet) != 'TRUE',]
influenzaflufever.df <- unique(influenzaflufever.df)
# write.csv(flu.df, "C:\\Users\\rohan\\OneDrive\\Documents\\flu1000.csv")
# write.csv(flu.df, "C:\\Users\\rohan\\OneDrive\\Documents\\flushot780.csv")
# write.csv(flu.df, "C:\\Users\\rohan\\OneDrive\\Documents\\hashflu1000.csv")
# write.csv(flu.df, "C:\\Users\\rohan\\OneDrive\\Documents\\influenza3000.csv")
# write.csv(flu.df, "C:\\Users\\rohan\\OneDrive\\Documents\\highfever1000.csv")
# write.csv(flu.df, "C:\\Users\\rohan\\OneDrive\\Documents\\flufever1000.csv")
# write.csv(flu.df, "C:\\Users\\rohan\\OneDrive\\Documents\\fluvaccine550.csv")
write.csv(allTweets.df, "C:\\Users\\rohan\\OneDrive\\Documents\\allTweetsFINAL.csv")
# write.csv(influenzaflufever.df, "C:\\Users\\rohan\\OneDrive\\Documents\\influenzaflufever.csv")
userInfo <- lookupUsers(flu.df$screenName)
userInfo <- lookupUsers(allTweets.df$screenName)
userInfo <- lookupUsers(influenzaflufever.df$screenName)
userFrame <- twListToDF(userInfo)
locatedUsers <- !is.na(userFrame$location)
locations <- geocode(userFrame$location[locatedUsers])
colnames(locations) <- c("long", "lat")
locs <- locations
locations <- locations[!is.na(locations$long) & !is.na(locations$lat) ,]
View(locations)
state_names <- revgeo(longitude= locations$long, latitude= locations$lat, output="frame")
statnams <- state_names
View(statnams)
View(state_names)
state_names <- state_names[(state_names$country) == "United States of America",]
write.csv(state_names, "C:\\Users\\rohan\\OneDrive\\Documents\\state_namesallTweetsFINAL.csv")
cont <- as.data.frame(table(state_names$state))
# cont <- count(state_names$state)
# cont <- as.data.frame(cont)
View(cont)
colnames(cont) <- c("id", "count")
cont$id <- tolower(cont$id)


choro <- left_join(fifty_states, cont)
View(choro)


ggplot(choro, aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=count))+geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()+theme(legend.position = "bottom",panel.background = element_blank())











# mapp.df <- merge(fifty_states, cont, by = "id", all.x = TRUE)
# View(mapp.df)
# mapp.df$count[is.na(mapp.df$count)] <- 0
# mapp.df <- as.data.frame(mapp.df)
# states <- map_data("state")
# View(states)
#states <- data.frame(state.name, state.x77)
#map.df <- merge(fifty_states,locations,by = c("long","lat"), all.x=T)



# p <- ggplot(mapp.df, aes(x=long,y=lat,map_id = id)) +
#   # map points to the fifty_states shape data
#   geom_map(aes(fill = count), map = fifty_states) +
#   expand_limits(x = mapp.df$long, y = mapp.df$lat) +
#   scale_fill_gradientn(colours=rev(heat.colors(10)))+
#   coord_map() +
#   # scale_x_continuous(breaks = NULL) +
#   # scale_y_continuous(breaks = NULL) +
#   # labs(x = "", y = "") +
#   theme(legend.position = "bottom",
#         panel.background = element_blank())


