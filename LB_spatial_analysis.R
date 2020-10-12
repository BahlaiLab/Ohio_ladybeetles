#bring in the data with the specimen info and environmental data integrated 

library(tibble)
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


oh<- st_read("OH_county_shapes/ODOT_County_Boundaries.shp")
oh2<- st_transform(oh, "+proj=longlat +ellps=WGS84 +datum=WGS84")
#merge in the context data
lb_allcontext_but_centroids<-lb_allcontext
lb_allcontext_but_centroids$geometry<-NULL
oh3<-merge(oh2, lb_allcontext_but_centroids, by="COUNTY_CD")

ohiomap<- ggplot()+ geom_sf(data=oh3, fill="white")+theme_classic()+
  geom_point(data=lb_allcontext, aes(x=lon, y=lat))+xlab("Longitude")+ylab("Latitude")

ohiomap


#ok, let's make a demo with one of the moderately rare species that is probably not in all counties

anatismali<-lb_allcontext[which(lb_allcontext$Species=="mali"),]

amalimap<- ggplot()+ geom_sf(data=oh3, aes(fill=Regions))+
  theme_classic()+
  geom_point(data=anatismali, aes(x=lon, y=lat))#+xlab("Longitude")+ylab("Latitude")

amalimap

#all right, let's do some distribution modelling!


library(dismo)

