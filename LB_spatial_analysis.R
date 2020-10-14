#bring in the data with the specimen info and environmental data integrated 
source("ohio_context_data_processing.R")

library(tibble)
library(reshape2)
#gotta start with naming conventions- one data frame has abbreviated name, one has whole name

COUNTY_CD<-levels(as.factor(counties_df$COUNTY_CD))
County<-levels(as.factor(allcensus$County))
namematch<-cbind(COUNTY_CD,County)
#okay, looks like things are lining up, we can use this as a key for merging data
#need to change names of year columnm in census
names(allcensus)[names(allcensus) == "Year"] <- "Decade"
#and pull out the dacades we have data for
Decades<-levels(as.factor(allcensus$Decade))


#and LB data
ladybeetle<-read.csv(file="specimen_data/LB_museumData_2020.csv", header=T, stringsAsFactors = T)

#we need to use our occurence data to create counts and pseudo-absences. We can do that by creating 
#a grid of county-decades by species:
ladybeetle.wide<-dcast(ladybeetle, Decade+County~Name, length)

#but wait! not all counties has any ladybeetles reported in all years. We need to create an 
#interaction of county and year  and then merge that in, add the pseudo-absences for those as well!

#first create an empty dataframe to dump this in:
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

#now need to merge the ladybeetle wide data into this interaction frame so we can create our pseudoabsences

ladybeetle.allcombos<-merge(county.decade, ladybeetle.wide, all.x=T, by=c("County", "Decade"))
#now replace all those new NAs with zero
ladybeetle.allcombos[is.na(ladybeetle.allcombos)]<-0


#Want to allign census with County, Decade in ladybeetle data



lb_census<-merge(ladybeetle, allcensus, by=c("County", "Decade"), all.x=T)

#ok, looks like that did the trick, now let's merge in the centroid coordinates.

#first need to index them my the actual county name, not the county abbreviation

counties_df_name<-merge(counties_df, namematch)

#note these data are given in NAD83/ South Ohio projection, to convert to lat and long we can do this:
counties_ll <- st_transform(counties_df_name, "+proj=longlat +ellps=WGS84 +datum=WGS84")

#get the geometry into good ol'd fashioned lat and long

coords <- do.call(rbind, st_geometry(counties_ll$geometry)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

counties_ll$lon<-coords$lon
counties_ll$lat<-coords$lat

#ok, now we can merge the cooridinates
lb_allcontext<-merge(lb_census, counties_ll, by="County", all.x=T)

#all right! Let's make a plot!

library(maptools)
library(ggplot2)
library(plyr)


oh<- st_read("OH_county_shapes/ODOT_County_Boundaries.shp")
oh2<- st_transform(oh, "+proj=longlat +ellps=WGS84 +datum=WGS84")
#merge in the context data


ohiomap<- ggplot()+ geom_sf(data=oh2, fill="white")+theme_classic()+
  geom_point(data=lb_allcontext, aes(x=lon, y=lat))+xlab("Longitude")+ylab("Latitude")

ohiomap


#ok, let's make a demo with one of the moderately rare species that is probably not in all counties

anatismali<-lb_allcontext[which(lb_allcontext$Species=="mali"),]

#Count the number of captures per county

amali.density<-ddply(anatismali, c("County", "lon", "lat"), summarize,
                     N=length(Name))

amalimap<- ggplot()+ geom_sf(data=oh2, aes(fill=Regions))+
  theme_classic()+
  geom_point(data=anatismali, aes(x=lon, y=lat))#+xlab("Longitude")+ylab("Latitude")

amalimap

#all right, let's do some distribution modelling!


library(dismo)

