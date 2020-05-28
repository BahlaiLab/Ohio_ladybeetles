#data are downloaded from Government sources or via Social Explorer.

#compute county centroids
#using Ohio Department of Transportation data

library(sf)
library(ggplot2)

oh<- st_read("ODOT_shapes/ODOT_County_Boundaries.shp") 

plot(oh)
sf_cent <- st_centroid(oh)

plot(sf_cent)


counties_df<-fortify(sf_cent)
write.csv(counties_df, file="oh_co_centroids.csv")

#now we need to bring in the census data and lightly pre-process it to pull out the useful information

#because questions change with each census, I'll do my best to allign it into relvant information 
#that connects up okay. Also the headers are all codes (described in the data dictionary within
# the Ohio_Census folder so I'm going to have to pull things and manually rename them)

#there's some agricultural census information included up to 1940 when the USDA took over this aspect of things
#so I'll just focus on the more regular human population metrics.



#1890census

census1890<-read.csv("Ohio_census/Census_1890.csv")

#need to create a new dataframe with only relevant, mergable info. Love a probram that assigns different column names 
#to the same data entity, ugh. Use data dictonaries to figure out which columns are the stuff we want

data1890<-census1890[c(1,8,9,11)]#name,population, area, Urban Population of Places With 2,500 People and Over

#so we'll want to rename for consistency, then get a bit of derrived data (humans/area, % urban), 
#add a year column for merging later

Year<-rep(1890, length(data1890$Geo_name))

names(data1890)<-c("County","Population", "Area", "Urban_pop")

cleaned1890<-cbind(Year, data1890)

cleaned1890$Density<-cleaned1890$Population/cleaned1890$Area
cleaned1890$Prop_urban<-cleaned1890$Urban_pop/cleaned1890$Population



#1900census

census1900<-read.csv("Ohio_census/Census_1900.csv")

#need to create a new dataframe with only relevant, mergable info. Love a probram that assigns different column names 
#to the same data entity, ugh. Use data dictonaries to figure out which columns are the stuff we want

data1900<-census1900[c(1,8,9,11)]#name,population, area, Urban Population of Places With 2,500 People and Over

#so we'll want to rename for consistency, then get a bit of derrived data (humans/area, % urban), 
#add a year column for merging later

Year<-rep(1900, length(data1900$Geo_name))

names(data1900)<-c("County","Population", "Area", "Urban_pop")

cleaned1900<-cbind(Year, data1900)

cleaned1900$Density<-cleaned1900$Population/cleaned1900$Area
cleaned1900$Prop_urban<-cleaned1900$Urban_pop/cleaned1900$Population



#1910census

census1910<-read.csv("Ohio_census/Census_1910.csv")

#need to create a new dataframe with only relevant, mergable info. Love a probram that assigns different column names 
#to the same data entity, ugh. Use data dictonaries to figure out which columns are the stuff we want

data1910<-census1910[c(1,8,9,11)]#name,population, area, Urban Population of Places With 2,500 People and Over

#so we'll want to rename for consistency, then get a bit of derrived data (humans/area, % urban), 
#add a year column for merging later

Year<-rep(1910, length(data1910$Geo_name))

names(data1910)<-c("County","Population", "Area", "Urban_pop")

cleaned1910<-cbind(Year, data1910)

cleaned1910$Density<-cleaned1910$Population/cleaned1910$Area
cleaned1910$Prop_urban<-cleaned1910$Urban_pop/cleaned1910$Population



#1920census

census1920<-read.csv("Ohio_census/Census_1920.csv")

#need to create a new dataframe with only relevant, mergable info. Love a probram that assigns different column names 
#to the same data entity, ugh. Use data dictonaries to figure out which columns are the stuff we want

data1920<-census1920[c(1,8,9,11)]#name,population, area, Urban Population of Places With 2,500 People and Over

#so we'll want to rename for consistency, then get a bit of derrived data (humans/area, % urban), 
#add a year column for merging later

Year<-rep(1920, length(data1920$Geo_name))

names(data1920)<-c("County","Population", "Area", "Urban_pop")

cleaned1920<-cbind(Year, data1920)

cleaned1920$Density<-cleaned1920$Population/cleaned1920$Area
cleaned1920$Prop_urban<-cleaned1920$Urban_pop/cleaned1920$Population



#1930census

census1930<-read.csv("Ohio_census/Census_1930.csv")

#need to create a new dataframe with only relevant, mergable info. Love a probram that assigns different column names 
#to the same data entity, ugh. Use data dictonaries to figure out which columns are the stuff we want

data1930<-census1930[c(1,8,9,11)]#name,population, area, Urban Population of Places With 2,500 People and Over

#so we'll want to rename for consistency, then get a bit of derrived data (humans/area, % urban), 
#add a year column for merging later

Year<-rep(1930, length(data1930$Geo_name))

names(data1930)<-c("County","Population", "Area", "Urban_pop")

cleaned1930<-cbind(Year, data1930)

cleaned1930$Density<-cleaned1930$Population/cleaned1930$Area
cleaned1930$Prop_urban<-cleaned1930$Urban_pop/cleaned1930$Population




#1940census

census1940<-read.csv("Ohio_census/Census_1940.csv")

#need to create a new dataframe with only relevant, mergable info. Love a probram that assigns different column names 
#to the same data entity, ugh. Use data dictonaries to figure out which columns are the stuff we want

data1940<-census1940[c(1,8,9,11)]#name,population, area, Urban Population of Places With 2,500 People and Over

#so we'll want to rename for consistency, then get a bit of derrived data (humans/area, % urban), 
#add a year column for merging later

Year<-rep(1940, length(data1940$Geo_name))

names(data1940)<-c("County","Population", "Area", "Urban_pop")

cleaned1940<-cbind(Year, data1940)

cleaned1940$Density<-cleaned1940$Population/cleaned1940$Area
cleaned1940$Prop_urban<-cleaned1940$Urban_pop/cleaned1940$Population

