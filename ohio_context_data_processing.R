#data are downloaded from Government sources or via Social Explorer.

#compute county centroids
#using Ohio Department of Transportation data from http://ogrip-geohio.opendata.arcgis.com/datasets/odot-county-boundaries

library(sf)
library(sp)
library(rgdal)
library(maptools)
library(raster)
library(ggplot2)

oh<- st_read("OH_county_shapes/ODOT_County_Boundaries.shp") 

plot(oh)

sf_cent <- st_centroid(oh)

plot(sf_cent)


counties_df<-fortify(sf_cent)
write.csv(counties_df, file="intermediate_data/oh_co_centroids.csv")

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

data1940<-census1940[c(1,7,8)]#name,population, area

#so we'll want to rename for consistency, then get a bit of derrived data (humans/area, % urban), 
#add a year column for merging later
#1940 has no metrics for urbanization so let's set those metrics to NA

Year<-rep(1940, length(data1940$Geo_Name))#tricky, they change the capitalization in this variable name here
Urban_pop<-rep(NA, length(data1940$Geo_Name))

names(data1940)<-c("County","Population", "Area")

cleaned1940<-cbind(Year, data1940, Urban_pop)

cleaned1940$Density<-cleaned1940$Population/cleaned1940$Area
cleaned1940$Prop_urban<-cleaned1940$Urban_pop/cleaned1940$Population



#1950census

census1950<-read.csv("Ohio_census/Census_1950.csv")

#need to create a new dataframe with only relevant, mergable info. Love a probram that assigns different column names 
#to the same data entity, ugh. Use data dictonaries to figure out which columns are the stuff we want

data1950<-census1950[c(1,7,8)]#name,population, area

#so we'll want to rename for consistency, then get a bit of derrived data (humans/area, % urban), 
#add a year column for merging later
#1950 has no metrics for urbanization so let's set those metrics to NA

Year<-rep(1950, length(data1950$Geo_Name))
Urban_pop<-rep(NA, length(data1950$Geo_Name))

names(data1950)<-c("County","Population", "Area")

cleaned1950<-cbind(Year, data1950, Urban_pop)

cleaned1950$Density<-cleaned1950$Population/cleaned1950$Area
cleaned1950$Prop_urban<-cleaned1950$Urban_pop/cleaned1950$Population


#1960census

census1960<-read.csv("Ohio_census/Census_1960.csv")

#need to create a new dataframe with only relevant, mergable info. Love a probram that assigns different column names 
#to the same data entity, ugh. Use data dictonaries to figure out which columns are the stuff we want

data1960<-census1960[c(1,11,12)]#name,population, area

#so we'll want to rename for consistency, then get a bit of derrived data (humans/area, % urban), 
#add a year column for merging later
#1960 has no metrics for urbanization so let's set those metrics to NA

Year<-rep(1960, length(data1960$Geo_name))#back to previous case. cute.
Urban_pop<-rep(NA, length(data1960$Geo_name))

names(data1960)<-c("County","Population", "Area")

cleaned1960<-cbind(Year, data1960, Urban_pop)

cleaned1960$Density<-cleaned1960$Population/cleaned1960$Area
cleaned1960$Prop_urban<-cleaned1960$Urban_pop/cleaned1960$Population


#1970census

census1970<-read.csv("Ohio_census/Census_1970.csv")

#need to create a new dataframe with only relevant, mergable info. Love a probram that assigns different column names 
#to the same data entity, ugh. Use data dictonaries to figure out which columns are the stuff we want

data1970<-census1970[c(2,9,10)]#name,population, area

#so we'll want to rename for consistency, then get a bit of derrived data (humans/area, % urban), 
#add a year column for merging later
#1970 has no metrics for urbanization so let's set those metrics to NA

Year<-rep(1970, length(data1970$Geo_NAME))#A NEW CASE CONVENTION! COOOOOOOOL
Urban_pop<-rep(NA, length(data1970$Geo_NAME))

names(data1970)<-c("County","Population", "Area")

cleaned1970<-cbind(Year, data1970, Urban_pop)

cleaned1970$Density<-cleaned1970$Population/cleaned1970$Area
cleaned1970$Prop_urban<-cleaned1970$Urban_pop/cleaned1970$Population




#1980census

census1980<-read.csv("Ohio_census/Census_1980.csv")

#need to create a new dataframe with only relevant, mergable info. Love a probram that assigns different column names 
#to the same data entity, ugh. Use data dictonaries to figure out which columns are the stuff we want

data1980<-census1980[c(1,9,10)]#name,population, area

#so we'll want to rename for consistency, then get a bit of derrived data (humans/area, % urban), 
#add a year column for merging later
#1980 has no metrics for urbanization so let's set those metrics to NA

Year<-rep(1980, length(data1980$Geo_Name))#and back to Name
Urban_pop<-rep(NA, length(data1980$Geo_Name))

names(data1980)<-c("County","Population", "Area")

cleaned1980<-cbind(Year, data1980, Urban_pop)

cleaned1980$Density<-cleaned1980$Population/cleaned1980$Area
cleaned1980$Prop_urban<-cleaned1980$Urban_pop/cleaned1980$Population


#1990census

census1990<-read.csv("Ohio_census/Census_1990.csv")

#need to create a new dataframe with only relevant, mergable info. Love a probram that assigns different column names 
#to the same data entity, ugh. Use data dictonaries to figure out which columns are the stuff we want

data1990<-census1990[c(1,10,11,15)]#name,population, area, "urban persons"

#so we'll want to rename for consistency, then get a bit of derrived data (humans/area, % urban), 
#add a year column for merging later
#1990 has no metrics for urbanization so let's set those metrics to NA

Year<-rep(1990, length(data1990$Geo_NAME))#and back to NAME

names(data1990)<-c("County","Population", "Area", "Urban_pop")

cleaned1990<-cbind(Year, data1990)

cleaned1990$Density<-cleaned1990$Population/cleaned1990$Area
cleaned1990$Prop_urban<-cleaned1990$Urban_pop/cleaned1990$Population


#2000census

census2000<-read.csv("Ohio_census/Census_2000.csv")

#need to create a new dataframe with only relevant, mergable info. Love a probram that assigns different column names 
#to the same data entity, ugh. Use data dictonaries to figure out which columns are the stuff we want

data2000<-census2000[c(1,12,18,14)]#name,population, area, "urban population"

#so we'll want to rename for consistency, then get a bit of derrived data (humans/area, % urban), 
#add a year column for merging later
#2000 has no metrics for urbanization so let's set those metrics to NA

Year<-rep(2000, length(data2000$Geo_NAME))

names(data2000)<-c("County","Population", "Area", "Urban_pop")

cleaned2000<-cbind(Year, data2000)

cleaned2000$Density<-cleaned2000$Population/cleaned2000$Area
cleaned2000$Prop_urban<-cleaned2000$Urban_pop/cleaned2000$Population



#2010census

census2010<-read.csv("Ohio_census/Census_2010.csv")

#need to create a new dataframe with only relevant, mergable info. Love a probram that assigns different column names 
#to the same data entity, ugh. Use data dictonaries to figure out which columns are the stuff we want

data2010<-census2010[c(1,12,13)]#name,population, area

#so we'll want to rename for consistency, then get a bit of derrived data (humans/area, % urban), 
#add a year column for merging later
#2010 has no metrics for urbanization so let's set those metrics to NA

Year<-rep(2010, length(data2010$Geo_NAME))
Urban_pop<-rep(NA, length(data2010$Geo_NAME))

names(data2010)<-c("County","Population", "Area")

cleaned2010<-cbind(Year, data2010, Urban_pop)

cleaned2010$Density<-cleaned2010$Population/cleaned2010$Area
cleaned2010$Prop_urban<-cleaned2010$Urban_pop/cleaned2010$Population



#okay let's bring it all together and then do the last bit of QC

allcensus<-rbind(cleaned1890, cleaned1900, cleaned1910, cleaned1920, cleaned1930, cleaned1940,
                 cleaned1950, cleaned1960, cleaned1970, cleaned1980, cleaned1990, cleaned2000, cleaned2010)


str(allcensus)
#so county names are the first obvious problem, looks like they're both called by their names and then also by 
# x.....space county
allcensus$County<-gsub(" County", "", allcensus$County)

str(allcensus)
levels(as.factor(allcensus$County))
length(levels(as.factor(allcensus$County)))

#ok, looks good, let's export the data
write.csv(allcensus, file="intermediate_data/oh_co_census.csv")

#ok, now to figure out how to merge these bad boys together

#gotta start with naming conventions- one data frame has abbreviated name, one has whole name

COUNTY_CD<-levels(as.factor(counties_df$COUNTY_CD))
County<-levels(as.factor(allcensus$County))

#and LB data
ladybeetle<-read.csv(file="specimen_data/LB_museumData_2020.csv", header=T, stringsAsFactors = T)
museum_name<-levels(as.factor(ladybeetle$County))

namematch<-cbind(COUNTY_CD,County)
#okay, looks like things are lining up, we can use this as a key for merging data


#Want to allign census with County, Decade in ladybeetle data

#first ned to change names of Decade coloumn

names(allcensus)[names(allcensus) == "Year"] <- "Decade"

lb_census<-merge(ladybeetle, allcensus, by=c("County", "Decade"), all.x=T)

#ok, looks like that did the trick, now let's merge in the centroid coordinates.

#first need to index them my the actual county name, not the county abbreviation

counties_df_name<-merge(counties_df, namematch)

#note these data are given in NAD83/ South Ohio projection, if we need to convert to lat and long we can do this:
#counties_ll <- st_transform(counties_df_name, "+proj=longlat +ellps=WGS84 +datum=WGS84")


#ok, now we can merge the cooridinates
lb_allcontext<-merge(lb_census, counties_df_name, by="County", all.x=T)

write.csv(lb_allcontext, file="intermediate_data/lb_alldata.csv")

