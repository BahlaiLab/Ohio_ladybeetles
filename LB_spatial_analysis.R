#bring in the data with the specimen info and environmental data integrated 
source("ohio_context_data_processing.R")

library(tibble)
library(reshape2)
library(dplyr) 
library(ggplot2)
library(scales)
library(magrittr)
library(spatial)
library(sp)
library(gstat)
library(ggmap)
library(maptools)
library(viridis)
library(raster)
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

#we'll need the region data and the invasion data from the ladybeetle dataframe, let's just pull
#those data out and then remove duplicates
ohregions<-ladybeetle[,c(6,7)]
ohregions<-ohregions[!duplicated(ohregions), ]

#now invasions
invdecade<-ladybeetle[,c(14,16)]
invdecade<-invdecade[!duplicated(invdecade), ]

#now need to merge the ladybeetle wide data into this interaction frame so we can create our pseudoabsences

ladybeetle.allcombos<-merge(county.decade, ladybeetle.wide, all.x=T, by=c("County", "Decade"))
#now replace all those new NAs with zero
ladybeetle.allcombos[is.na(ladybeetle.allcombos)]<-0

#total ladybeetles per countyXdecade, can use in model later
ladybeetle.allcombos$Totalcount<-rowSums(ladybeetle.allcombos[3:33])

#now remelt that data

ladybeetle.long<-melt(ladybeetle.allcombos, id=c("County", "Decade","Totalcount"))
colnames(ladybeetle.long)<- c("County", "Decade", "Totalcount", "Name", "Count")



#Want to allign census with County, Decade in ladybeetle data

lb_census<-merge(ladybeetle.long, allcensus, by=c("County", "Decade"), all.x=T)

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

#also need data for the raw captures with spatial coordinates
lb_raw<-merge(ladybeetle, counties_ll, by="County", all.x=T)

#all right! Let's make a plot!


#now get ohio map data- this gives us a space to plot things

oh<- st_read("OH_county_shapes/ODOT_County_Boundaries.shp")
oh2<- st_transform(oh, "+proj=longlat +ellps=WGS84 +datum=WGS84")
#merge in the context data for regions
oh3<-merge(oh2, namematch)

oh4<-merge(oh3, ohregions, by=c("County"), all.x=T)



ohiomap<- ggplot()+ geom_sf(data=oh4, aes(fill=Regions))+theme_classic()+
  geom_point(data=lb_allcontext, aes(x=lon, y=lat))+xlab("Longitude")+ylab("Latitude")

ohiomap


#ok, let's make a demo with one of the moderately rare species that is probably not in all counties

anatismali<-lb_allcontext[which(lb_allcontext$Name=="Anatis mali"),]

#Count the number of captures per county
#and f this I'm using plyr
library(plyr)

amali.density<-ddply(anatismali, c("County","Decade", "lon", "lat"), summarize,
                     Count=sum(Count))

#and a nonzero dataset for the mapping
amali.density.1<-amali.density[which(amali.density$Count>0),]


#first let's just do a little plot that scales points by number captured
amalimap<- ggplot()+ geom_sf(data=oh4, aes(fill=Regions))+
  theme_classic()+
  geom_point(data=amali.density.1, aes(x=lon, y=lat, size=Count), pch=21, color="black")+
  xlab("Longitude")+ylab("Latitude")

amalimap

#now a map with binned density


amalimap2<- ggplot()+ geom_sf(data=oh4)+
  theme_classic()+
  stat_bin2d(data=amali.density.1, aes(x = lon, y = lat),
             size = 1, bins = 3, alpha = 0.8)+
  xlab("Longitude")+ylab("Latitude")

amalimap2

#now a map with smoothed density- let's use lb raw counts 

density_plot<- ggplot()+ geom_sf(data=oh4)+
  theme_classic()+
  stat_density2d(data=lb_raw, aes(x = lon, y = lat, fill=after_stat(level)),
                 geom="polygon", alpha=0.7)+
  scale_fill_viridis()+
  geom_point(data=lb_raw, aes(x=lon, y=lat), pch=21, color="black", position="jitter")+
  xlab("Longitude")+ylab("Latitude")+
  facet_wrap(~Decade30)

density_plot


#ok, maybe I can make it interpolate

#turn LB_all context to a spatial data frame
lb_spat <-  SpatialPointsDataFrame(coords = lb_allcontext[,c("lon", "lat")], 
                                   data = lb_allcontext,
                                   proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# get the min/max range for lat/long to make an empty grid 
x.range <- as.numeric(c(min(oh$LONG_WEST_), max(oh$LONG_EAST_)))  
  # min/max longitude of the interpolation area
y.range <- as.numeric(c(min(oh$LAT_SOUTH_), max(oh$LAT_NORTH_)))  
  # min/max latitude of the interpolation area  
# from the range, exapnd the coordinates to make a regular grid
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.075), 
                   y = seq(from = y.range[1], to = y.range[2], by = 0.075))  # expand 

coordinates(grd) <- ~x + y
sp::gridded(grd) <- TRUE
proj4string(grd) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

plot(grd, cex = 1.5, col = "grey")
points(lb_spat, pch = 1, col = "red", cex = 1)

#and now we have a plot of the county centroids on a grid. cute.

#inverse density weight

idw <- gstat::idw(formula = Count ~ 1, locations = lb_spat, newdata = grd)


# grab output of IDW for plotting
idw.output = as.data.frame(idw)  # output is defined as a data table
# set the names of the idw.output columns
# basic ggplot using geom_tile to display our interpolated grid within no map
ggplot() + 
  geom_sf(data=oh4)+
  geom_tile(data = idw.output, aes(x = x, y = y, fill = var1.pred), alpha=0.75) + 
  geom_point(data = lb_allcontext, aes(x = lon, y = lat), shape = 21, color = "red") +
  scale_fill_viridis()+ 
  theme_bw() 


######################################

#gam time!

library(mgcv)
library(GGally)

#ok, let's look at what really drives ladybeetle captures. our complete dataset
#based on density of captures and pseudoabsences

# lb_allcontext is our main data frame for this analysis
#let's clean it up a bit so we can do things a bit more efficiently downstream
lb_allcontext1<-lb_allcontext
lb_allcontext1[11:19]<-NULL
lb_allcontext1[13:25]<-NULL
#and marge in the land classification data
lb_allcontext2<-merge(lb_allcontext1, ohregions, by="County")


#going to make a model analysis using just Coleomegilla maculata because it's common and
#present the whole sampling period

cmac<-lb_allcontext2[which(lb_allcontext2$Name=="Coleomegilla maculata"),]
cmac$Name<-NULL

#also let's get rid of a few other metrics that are probably not important/incomplete
cmac$Population<-NULL
cmac$Area<-NULL
cmac$Urban_pop<-NULL
cmac$Prop_urban<-NULL
#and Decade is behaving like a factor's fix that
cmac$Decade<-as.numeric(cmac$Decade)

ggpairs(cmac, columns=c(2:9), ggplot2::aes(colour=County))

#Let's take a closer look at some key relationships- namely sampling effort vs captures:
plot(cmac$Totalcount, cmac$Count)
#doesn't look super strong, but I feel like I should include this as a linear predictor regardless


#try model that is linear with Totalcount and has a gaussian process-based spatial relationship
cmac.gam<-gam(Count~s(lon, lat, bs="gp")+Totalcount, data=cmac)
summary(cmac.gam)

plot(cmac.gam)

#now let's try the same model but accounting for human population density and decade
cmac.gam1<-gam(Count~s(lon, lat, bs="gp")+Totalcount+s(Density)+s(Decade), data=cmac)
summary(cmac.gam1)

#let's see what we gain by including a couple other terms
cmac.gam2<-gam(Count~s(lon, lat, bs="gp")+Totalcount+
                 s(Density)+s(Decade)+s(ELEVATION1)+Regions, data=cmac)
summary(cmac.gam2)
#looks like Region, elevation are not so important for Cmac, but I wonder if number of if number of invasives 


