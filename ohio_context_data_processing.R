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

#plot(oh)

sf_cent <- st_centroid(oh)

#plot(sf_cent)


counties_df<-fortify(sf_cent)
#write.csv(counties_df, file="intermediate_data/oh_co_centroids.csv")

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
#write.csv(allcensus, file="intermediate_data/oh_co_census.csv")

#and now the land use land cover data- need to extrapolate form the given time points

COUNTY_CD<-levels(as.factor(counties_df$COUNTY_CD))
County<-levels(as.factor(allcensus$County))
namematch<-cbind(COUNTY_CD,County)
#okay, looks like things are lining up, we can use this as a key for merging data
#need to change names of year columnm in census
names(allcensus)[names(allcensus) == "Year"] <- "Decade"
#and pull out the dacades we have data for
Decades<-levels(as.factor(allcensus$Decade))

#Need a list of all county-decade combinations

county.decade<-data.frame(matrix(vector(), 0, 2,
                                 dimnames=list(c(), c("County", "Decade"))),
                          stringsAsFactors=F)
for (i in 1:length(County)){
  countyi<-County[i]
  for (j in 1:length(Decades)){
    decadej<-Decades[j]
    comboij<-cbind(countyi, decadej)
    county.decade<-rbind(county.decade, comboij)
  }
  
}
#rename columns
colnames(county.decade)<- c("County", "Decade")

#bring in landcover data

LULC<-read.csv(file="specimen_data/LULC_percent_long.csv", header=T, stringsAsFactors = F)

library(stringr)

#convert atributes to be consistent with other conventions
LULC$County<-str_to_sentence(LULC$COUNTY)
LULC$COUNTY<-NULL

# we have stop points at 1938, 1970, 1992 and 2016. We'll assume linear changes in landcover between each
#of theser time points, but we want an estimate for each land cover type at each decade 1930 on- so let's make a loop to compute

landclass<-unique(LULC$Class)
output<-data.frame(matrix(vector(), 0, 4,),
                          stringsAsFactors=F)

for (i in 1:length(landclass)){
  classdata<-LULC[which(LULC$Class==landclass[i]),]
  for(j in 1:length(County)){
    countydata<-classdata[which(classdata$County==County[j]),]
    #find slope and intercept of first segment, 1938-1970
    x1<-1938
    x2<-1970
    y1<-countydata[1,4]
    y2<-countydata[2,4]
    slopei<-(y2-y1)/(x2-x1)
    bi<-y1-(slopei*x1)
    decclass<-c(1930, 1940,1950, 1960)
    for (k in 1:length(decclass)){
      perccover<-slopei*decclass[k]+bi
      outvec<-c(landclass[i], County[j], decclass[k], perccover)
      output<-rbind(output, outvec)
    }
    #now find the slope and extrapolate for 1970-1992
    x1<-1970
    x2<-1992
    y1<-countydata[2,4]
    y2<-countydata[3,4]
    slopei<-(y2-y1)/(x2-x1)
    bi<-y1-(slopei*x1)
    decclass<-c(1970, 1980,1990)
    for (k in 1:length(decclass)){
      perccover<-slopei*decclass[k]+bi
      outvec<-c(landclass[i], County[j], decclass[k], perccover)
      output<-rbind(output, outvec)
    }
    #And now for 1992-2016
    x1<-1992
    x2<-2016
    y1<-countydata[3,4]
    y2<-countydata[4,4]
    slopei<-(y2-y1)/(x2-x1)
    bi<-y1-(slopei*x1)
    decclass<-c(2000, 2010)
    for (k in 1:length(decclass)){
      perccover<-slopei*decclass[k]+bi
      outvec<-c(landclass[i], County[j], decclass[k], perccover)
      output<-rbind(output, outvec)
    }
    
  }
}

names(output)<-c("Class", "County", "Decade", "Percentage")

#convert to wide format by county, decade
library(reshape2)

LULC.wide<-dcast(output, Decade+County~Class)

#ok, we're good! Export an intermediate product
#write.csv(LULC.wide, file="intermediate_data/oh_LULC.csv")

