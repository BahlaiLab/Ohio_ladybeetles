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
invdecade<-ladybeetle[,c(15,17,18)]
invdecade<-invdecade[!duplicated(invdecade), ]

#now need to merge the ladybeetle wide data into this interaction frame so we can create our pseudoabsences

ladybeetle.allcombos<-merge(county.decade, ladybeetle.wide, all.x=T, by=c("County", "Decade"))
#now replace all those new NAs with zero
ladybeetle.allcombos[is.na(ladybeetle.allcombos)]<-0

#total ladybeetles per countyXdecade, can use in model later
ladybeetle.allcombos$Totalcount<-rowSums(ladybeetle.allcombos[3:30])

#now remelt that data

ladybeetle.long<-melt(ladybeetle.allcombos, id=c("County", "Decade","Totalcount"))
colnames(ladybeetle.long)<- c("County", "Decade", "Totalcount", "Name", "Count")



#Want to allign census with County, Decade in ladybeetle data

lb_census<-merge(ladybeetle.long, allcensus, by=c("County", "Decade"), all.x=T)

#and now merge in all landscape data
lb_geo<-merge(lb_census, LULC.wide, by=c("County", "Decade"), all.x=T)

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
lb_allcontext<-merge(lb_geo, counties_ll, by="County", all.x=T)

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
library(visreg)
library(mgcViz)
library(grid)
library(gridExtra)
library(ggplotify)
library(cowplot)

#ok, let's look at what really drives ladybeetle captures. our complete dataset
#based on density of captures and pseudoabsences

# lb_allcombos is our main data frame for this analysis- need to merge in the context data
#because we need it wide format by species

#Want to allign census with County, Decade in ladybeetle data

lb_census_wide<-merge(ladybeetle.allcombos, allcensus, by=c("County", "Decade"), all.x=T)
#and bring in the LULC data

lb_geo_wide<-merge(lb_census_wide, LULC.wide, by=c("County", "Decade"), all.x=T)

#ok, looks like that did the trick, now let's merge in the centroid coordinates.

#ok, now we can merge the cooridinates
lb_all_wide<-merge(lb_geo_wide, counties_ll, by="County", all.x=T)


#and merge in the land classification data
lb_all<-merge(lb_all_wide, ohregions, by="County")
lb_all2<-merge(lb_all, invdecade, by="Decade")

#so let's think about each species as a potential response AND predictor variable
#let's turn the species names into names that can be used as variables in the models:

names(lb_all2) <- gsub(x = names(lb_all2), pattern = " ", replacement = ".")  

#need a few more calculations like total invasive, total aphidophagous

#total invasive
lb_all2$Totalinvasive<-lb_all2$Coccinella.septempunctata+lb_all2$Coccinella.undecimpunctata+
  lb_all2$Harmonia.axyridis+lb_all2$Hippodamia.variegata+lb_all2$Propylea.quatuordecimpunctata

#total aphidophagous- first calculate non-aphidophagous and then subtract
lb_all2$Nonaphidophagous<-lb_all2$`Hyperaspis undulata`+lb_all2$Neoharmonia.venusta+
  lb_all2$Psyllobora.vigintimaculata
lb_all2$Aphidophagous<-lb_all2$Totalcount-lb_all2$Nonaphidophagous

#and Decade is behaving like a factor- fix that
lb_all2$Decade<-as.numeric(lb_all2$Decade)
#landscape parameters, too
lb_all2$Agriculture<-as.numeric(lb_all2$Agriculture)
lb_all2$Forest<-as.numeric(lb_all2$Forest)
lb_all2$Developed<-as.numeric(lb_all2$Developed)

#also because gams aren't terribly happy with missing data and all lanscape data is mission prior to 1930
#let's just cull those data out

lb_all2<-lb_all2[which(lb_all2$Decade>1929),]

summary(lb_all2)

#native species

#Coleomegilla maculata

#first, how many captures are we working with?
sum(lb_all2$Coleomegilla.maculata)


#try model that is linear with Totalcount (minus the captures of C.mac to make it independent) 
#and has a gaussian process-based spatial relationship
cmac.gam<-gam(Coleomegilla.maculata~s(lon, lat, bs="gp")+I(Totalcount-Coleomegilla.maculata), data=lb_all2)
summary(cmac.gam)
AIC(cmac.gam)
b<-getViz(cmac.gam)

cmac.gam.inv<-gam(Coleomegilla.maculata~s(lon, lat, by=Invasion, bs="gp")+I(Totalcount-Coleomegilla.maculata), data=lb_all2)
summary(cmac.gam.inv)
AIC(cmac.gam.inv)
b.inv<-getViz(cmac.gam.inv)

text_high<-textGrob("Highest")
text_low<-textGrob("Lowest")
text_key<-textGrob("Predicted captures")
text_labelA<-textGrob("A", gp=gpar(fontface="bold"))

cmacmap<- as_grob(plot(sm(b, select=1)))+theme_classic()+
  xlab("Longitude")+ylab("Latitude")+ggtitle(NULL)+
  theme(aspect.ratio=1,legend.background=element_blank(), 
        legend.title = element_blank(), legend.text = element_blank())+
  annotation_custom(text_high, xmin=-79.7,xmax=-79.7,ymin=40.55,ymax=40.55)+
  annotation_custom(text_low, xmin=-79.7,xmax=-79.7,ymin=39.70,ymax=39.70)+
  annotation_custom(text_key, xmin=-79.9,xmax=-79.9,ymin=40.75,ymax=40.75)+
  annotation_custom(text_labelA, xmin=-84.6,xmax=-84.6,ymin=41.85,ymax=41.85)+
  coord_cartesian(clip = "off")

cmacmap
cmac.gb<-grid.grabExpr(print(cmacmap))


#make the maps for each of the invasion periods

text_labelB<-textGrob("B", gp=gpar(fontface="bold"))
text_labelC<-textGrob("C", gp=gpar(fontface="bold"))
text_labelD<-textGrob("D", gp=gpar(fontface="bold"))
text_labelE<-textGrob("E", gp=gpar(fontface="bold"))

cmacmap.inv.1<- plot(b.inv, select=1)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)+
  annotation_custom(text_labelB, xmin=-84.6,xmax=-84.6,ymin=41.85,ymax=41.85)+
  coord_cartesian(clip = "off")

cmacmap.inv.1
cmac1.gb<-grid.grabExpr(print(cmacmap.inv.1))

cmacmap.inv.2<- plot(b.inv, select=2)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)+
  annotation_custom(text_labelC, xmin=-84.6,xmax=-84.6,ymin=41.85,ymax=41.85)+
  coord_cartesian(clip = "off")

cmacmap.inv.2
cmac2.gb<-grid.grabExpr(print(cmacmap.inv.2))

cmacmap.inv.3<- plot(b.inv, select=3)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)+
  annotation_custom(text_labelD, xmin=-84.6,xmax=-84.6,ymin=41.85,ymax=41.85)+
  coord_cartesian(clip = "off")

cmacmap.inv.3
cmac3.gb<-grid.grabExpr(print(cmacmap.inv.3))

cmacmap.inv.4<-plot(b.inv, select=4)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)+
  annotation_custom(text_labelE, xmin=-84.6,xmax=-84.6,ymin=41.85,ymax=41.85)+
  coord_cartesian(clip = "off")

cmacmap.inv.4
cmac4.gb<-grid.grabExpr(print(cmacmap.inv.4))

#All right, let's put these together nicely

cmac.4<-plot_grid(cmac.gb, plot_grid(cmac1.gb,cmac2.gb,cmac3.gb,cmac4.gb, ncol=4), ncol=2)
cmac.4



#So let's use the sampling+spatial autocorrelation model as our 'base model'
#iterative process-use AIC as selection criterion
cmac.gam1<-gam(Coleomegilla.maculata~+I(Totalcount-Coleomegilla.maculata)+
                 s(I(Aphidophagous-Coleomegilla.maculata))+
                 s(Agriculture)+
                 s(Forest)+
                 s(Developed)+
                 s(lon, lat, bs="gp")+
                 s(Density), data=lb_all2)
summary(cmac.gam1)
AIC(cmac.gam1)


#looks like we don't see a super strong signal from much other than Harmonia and C7 on Cmac

visreg(cmac.gam1, "Aphidophagous", "Invasion", ylab="Captures", overlay=T, xlim=c(0,120), ylim=c(0,100))

visreg(cmac.gam1, "Developed",  ylab="Captures")

visreg(cmac.gam1, "Forest", ylab="Captures")

visreg(cmac.gam1, "Density", ylab="Captures")

visreg(cmac.gam1, "Totalcount", ylab="Captures")


#Coccinella novemnota

#first, how many captures are we working with?
sum(lb_all2$Coccinella.novemnotata)

#also because this species was never collected after 1990 let's create a dataset for the extirpated 
pre1995<-lb_all2[which(lb_all2$Decade<=1990),]

#try model that is linear with Totalcount and has a gaussian process-based spatial relationship
c9.gam<-gam(Coccinella.novemnotata~s(lon, lat, bs="gp")+Totalcount, data=pre1995)
summary(c9.gam)

plot(c9.gam)

#oy, so the spatial term is definitely too much for these data

#now let's try the same model but accounting for human population density and decade
#iterative process-use GCV as nodel selection criterion

c9.gam1<-gam(Coccinella.novemnotata~Totalcount+
                 s(Aphidophagous, k=4)+s(Decade, k=4), data=pre1995)
summary(c9.gam1)

#there's not a lot of signal coming out of the c9 data- wondering if landscape will make all the difference 
#because it doesn't seem to be invasions

visreg(c9.gam1, "Aphidophagous",  ylab="Captures")

visreg(c9.gam1, "Decade", ylab="Captures")


#Adalia bipunctata 
#Hippodamia covergens

#scale feeding- Chilocorus stigma

#non-native species 
#Coccinella septempunctata
#Harmonia axyridis


