install.packages("twitteR")


library(twitteR)

consumer_key <- '5ln7NEPebQZ76rzYWMoTFh4sz'
consumer_secret <- 'wgMPx0axKj0sieNtCZOvxOXbWbI8UQpRqhPaB0ZIO7ATndqyQD'
access_token <- '1098101509756211201-AZYJqdOKNQDmq8ATsEA6oUWXrjCiEd'
access_secret <- 'zvrtWw6EHOByV9INUWFnzcw1XNGHQShijzqyZXApoYROo'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


tweets1 <- searchTwitter("flu", n = 50000, since = "2018-01-01")

tweetsnew1.df <-twListToDF(tweets1)


write.csv(tweetsnew1.df, "C:\\Users\\rohan\\OneDrive\\Documents\\tweetsJan2018flu.csv")

tweetsnew2.df <- tweetsnew1.df[!is.na(tweetsnew1.df$longitude) & !is.na(tweetsnew1.df$latitude) ,]

write.csv(tweetsnew2.df, "C:\\Users\\rohan\\OneDrive\\Documents\\tweetsJan2018latflu.csv")

#searchTwitter('from:', resultType="recent", n=10)
install.packages("data.table", dependencies=TRUE)
require(data.table)
twtnews.df <- as.data.frame(fread(file="C:\\Users\\rohan\\OneDrive\\Documents\\LatLong.csv", header=TRUE))
View(twtnews.df)
library(maps)
library(ggplot2)
citation("ggmap")
library("maptools")
gpclibPermit()
library("maps")
library("ggmap")
register_google(key = "AIzaSyDuPOH5A6aona1B0ISgsmTiuKHgb_PEMyw") 

#chicagoMVT$Longitude <- round(as.numeric(chicagoMVT$Longitude), 2)
#chicagoMVT$Latitude <- round(as.numeric(chicagoMVT$Latitude), 2)
install.packages("revgeo", dependencies = TRUE)
library('revgeo')
install.packages("openssl")
library(openssl)

flu.tweets = searchTwitter("flu OR flushots",n=1000)
flu.df <-twListToDF(flu.tweets)
View(flu.df)
flu.df <- flu.df[(flu.df$isRetweet) != 'TRUE',]
View(flu.df)
flu.df <- unique(flu.df)
write.csv(flu.df, "C:\\Users\\rohan\\OneDrive\\Documents\\flu1000.csv")
View(flu.df)
userInfo <- lookupUsers(flu.df$screenName)
userFrame <- twListToDF(userInfo)
locatedUsers <- !is.na(userFrame$location)
#View(locatedUsers)
locations <- geocode(userFrame$location[locatedUsers])
colnames(locations) <- c("long", "lat")
locs <- locations
locations <- locations[!is.na(locations$long) & !is.na(locations$lat) ,]
View(locations)
state_names <- revgeo(longitude= locations$long, latitude= locations$lat, output="frame")
statnams <- state_names
View(statnams)
View(state_names)
state_names <- statnams[(statnams$country) == "United States of America",]
#state_names <- state_names[(state_names$state) != "District of Columbia",]
cont <- count(state_names$state)
cont <- as.data.frame(cont)
#count <- table(state_names$state)
#count <- as.data.frame(count)
#count <- count[(count$Freq != "0"),]
View(cont)
colnames(cont) <- c("id", "count")
cont$id <- tolower(cont$id)
# with(locations, plot(long, lat))
# worldMap <- map_data("world")
# zp1 <- ggplot(worldMap)
# zp1 <- zp1 + geom_path(aes(x = long, y = lat, group = group), colour = gray(2/3), lwd = 1/3)
# zp1 <- zp1 + geom_point(data = locations,  aes(x = long, y = lat), colour = "RED", alpha = 1/2, size = 1)
# zp1 <- zp1 + coord_equal()
# zp1 <- zp1 + theme_minimal()
# print(zp1)

devtools::install_github("wmurphyrd/fiftystater")
library(fiftystater)
library(plyr)

View(fifty_states)

pachaas_states <- fifty_states

View(pachaas_states)

fifty_states$group <- ifelse(fifty_states$group == "Alabama.1", 1, fifty_states$group)


library(ggplot2)
library(maps)
mapp.df <- merge(fifty_states, cont, by = "id", all.x = TRUE)
View(mapp.df)
mapp.df$count[is.na(mapp.df$count)] <- 0
mapp.df <- as.data.frame(mapp.df)

states <- map_data("state")
View(states)


#colnames(cont) <- c("region", "count")
mappp.df <- merge(states, cont, by = "region", all = TRUE)
mappp.df$count[is.na(mappp.df$count)] <- 0
mappp.df <- mappp.df[(mappp.df$region != 'district of columbia'),]
#states <- data.frame(state.name, state.x77)
#map.df <- merge(fifty_states,locations,by = c("long","lat"), all.x=T)
#choro.df <- left_join(fifty_states, locations)
#View(choro.df)
#map.df <- merge(states,locations, all.x=T)
#View(map.df)
#map.df <- map.df[order(map.df$order),]
#View(map.df)

install.packages("mapproj")
library(mapproj)

ggplot(mapp.df, aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=count))+geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)))+
  coord_map()

#scale_fill_gradientn(colours=rev(heat.colors(12)))+

#library(dplyr)
#choro <- left_join(map_data("state"), locations %>% add_rownames("region"))

#View(choro)

#ggplot(choro, aes(long, lat)) + geom_polygon(aes(group = group)) + coord_quickmap()


p <- ggplot(mapp.df, aes(map_id = id)) +
  # map points to the fifty_states shape data
  geom_map(aes(fill = count), map = fifty_states) +
  expand_limits(x = mapp.df$long, y = mapp.df$lat) +
  scale_fill_gradientn(colours=rev(heat.colors(10)))+
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom",
        panel.background = element_blank())


q <- ggplot(mapp.df, aes(x=long,y=lat,map_id = id)) +
  # map points to the fifty_states shape data
  geom_map(aes(fill = count), map = fifty_states) +
  expand_limits(x = mapp.df$long, y = mapp.df$lat) +
  scale_fill_gradientn(colours=rev(heat.colors(10)))+
  coord_map() +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom",
        panel.background = element_blank())




