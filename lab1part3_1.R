#install.packages("twitteR")
#install.packages("data.table", dependencies=TRUE)
#install.packages("revgeo", dependencies = TRUE)
#devtools::install_github("wmurphyrd/fiftystater")
#install.packages("mapproj")


require(data.table)
library(maps)
library(ggplot2)
citation("ggmap")
library("maptools")
gpclibPermit()
library("maps")
library("ggmap")
library(mapproj)
library(fiftystater)
library(plyr)
library(dplyr)
library('revgeo')
library(twitteR)

register_google(key = "AIzaSyCO0iWcOjTVVeshAjIcIQUw6DrgkdtK9Vo")

consumer_key <- 'IDvMLdSYEaj68ZTTGlUI7E5b1'
consumer_secret <- 'MctkDL4cq9SXF928L8tZBMpj8TeDmCoMIIvIDtJwxwnlL2DqE3'
access_token <- '1062843121371897856-YH5hrzMNCMcaUcT5MkrUO1gH9gudMk'
access_secret <- 'TBex7S6iJDmgPlkiadoQkMwnJNIXmNPO8UIZI4Qf123zS'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#View(fifty_states)
fifty_states$group <- ifelse(fifty_states$group == "Alabama.1", 1, fifty_states$group)


flu.tweets = searchTwitter("flu",n=1500)
flushot.tweets = searchTwitter("#flushot",n=1500)
hashflu.tweets = searchTwitter("#flu",n=1500)
influenza.tweets = searchTwitter("influenza",n=1500)
highfever.tweets = searchTwitter("high fever",n=1500)
flufever.tweets = searchTwitter("flu fever",n=1500)
fluvaccine.tweets = searchTwitter("fluvaccine",n=1500)

#allTweets.df <- twListToDF(c(flu.tweets,flushot.tweets,hashflu.tweets,influenza.tweets,highfever.tweets,flufever.tweets,fluvaccine.tweets))
#View(allTweets.df)

#influenzaflu.df <- twListToDF(c(influenza.tweets))
#View(influenza.df)

flu.df <-twListToDF(flufever.tweets)
flu.df <- flu.df[(flu.df$isRetweet) != 'TRUE',]
flu.df <- unique(flu.df)

#View(flu.df)

#allTweets.df <- allTweets.df[(allTweets.df$isRetweet) != 'TRUE',]
#allTweets.df <- unique(allTweets.df)

#influenza.df <- influenzaflu.df[(influenza.df$isRetweet) != 'TRUE',]
#influenza.df <- unique(influenza.df)


write.csv(flu.df, "C:\\Users\\aksha\\Documents\\Lab1EDA\\lab1Part2\\lab1Part3\\flufever3000.csv")
# write.csv(flu.df, "C:\\Users\\rohan\\OneDrive\\Documents\\flushot780.csv")
# write.csv(flu.df, "C:\\Users\\rohan\\OneDrive\\Documents\\hashflu1000.csv")
# write.csv(flu.df, "C:\\Users\\rohan\\OneDrive\\Documents\\influenza1000.csv")
# write.csv(flu.df, "C:\\Users\\rohan\\OneDrive\\Documents\\highfever1000.csv")
# write.csv(flu.df, "C:\\Users\\rohan\\OneDrive\\Documents\\flufever1000.csv")
# write.csv(flu.df, "C:\\Users\\rohan\\OneDrive\\Documents\\fluvaccine550.csv")
# write.csv(allTweets.df, "C:\\Users\\rohan\\OneDrive\\Documents\\allTweets.csv")
# write.csv(influenzaflufever.df, "C:\\Users\\rohan\\OneDrive\\Documents\\influenzaflufever.csv")


userInfo <- lookupUsers(flu.df$screenName)
#userInfo <- lookupUsers(allTweets.df$screenName)
#userInfo <- lookupUsers(influenzaflufever.df$screenName)

userFrame <- twListToDF(userInfo)
locatedUsers <- !is.na(userFrame$location)
locations <- geocode(userFrame$location[locatedUsers])

colnames(locations) <- c("long", "lat")
locs <- locations
locations <- locations[!is.na(locations$long) & !is.na(locations$lat) ,]
#View(locations)

state_names <- revgeo(longitude= locations$long, latitude= locations$lat, output="frame")
statnams <- state_names

#View(statnams)
#View(state_names)

state_names <- state_names[(state_names$country) == "United States of America",]

write.csv(state_names, "C:\\Users\\aksha\\Documents\\Lab1EDA\\lab1Part2\\lab1Part3\\StateNames_flufever3000.csv")
cont <- as.data.frame(table(state_names$state))

View(cont)
colnames(cont) <- c("id", "count")
cont$id <- tolower(cont$id)


choro <- left_join(fifty_states, cont)
#View(choro)


ggplot(choro, aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=count))+geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()+theme(legend.position = "bottom",panel.background = element_blank())


