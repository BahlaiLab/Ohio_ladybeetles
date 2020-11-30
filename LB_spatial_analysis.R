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

#also trim up some polygonsfor giving the map edges
allstates<-map_data ("state")
ohiopoly<-allstates[which(allstates$region=="ohio"),]

#native species
##################################################################################################
#Coleomegilla maculata

#first, how many captures are we working with?
sum(lb_all2$Coleomegilla.maculata)


#try model that is linear with Totalcount (minus the captures of C.mac to make it independent) 
#and has a gaussian process-based spatial relationship
cmac.gam<-gam(Coleomegilla.maculata~s(lon, lat, bs="gp")+
                offset(log(1+Totalcount-Coleomegilla.maculata)), 
              data=lb_all2, family="nb")
summary(cmac.gam)
AIC(cmac.gam)
b<-getViz(cmac.gam)

cmac.gam.inv<-gam(Coleomegilla.maculata~s(lon, lat, by=Decade30, bs="gp")+
                    offset(log(1+Totalcount-Coleomegilla.maculata)), 
                  data=lb_all2, family="nb")
summary(cmac.gam.inv)
AIC(cmac.gam.inv)
b.inv<-getViz(cmac.gam.inv)

text_high<-textGrob("Highest")
text_low<-textGrob("Lowest")
text_key<-textGrob("Predicted captures")

cmacmap<- plot(b, select=1)+theme_classic()+
  xlab("Longitude")+ylab("Latitude")+ggtitle(NULL)+
  theme(aspect.ratio=1,legend.background=element_blank(), 
        legend.title = element_blank(), legend.text = element_blank(),
        plot.margin=unit(c(1, 3, 0.5, 1), "lines"))+
  annotation_custom(text_high, xmin=-79.7,xmax=-79.7,ymin=40.55,ymax=40.55)+
  annotation_custom(text_low, xmin=-79.7,xmax=-79.7,ymin=39.70,ymax=39.70)+
  annotation_custom(text_key, xmin=-79.9,xmax=-79.9,ymin=40.77,ymax=40.77)+
  coord_cartesian(clip = "off")

cmacmap
cmac.gb<-grid.grabExpr(print(cmacmap))


#make the maps for each of the invasion periods


cmacmap.inv.1<- plot(b.inv, select=1)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

cmacmap.inv.1
cmac1.gb<-grid.grabExpr(print(cmacmap.inv.1))

cmacmap.inv.2<- plot(b.inv, select=2)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

cmacmap.inv.2
cmac2.gb<-grid.grabExpr(print(cmacmap.inv.2))

cmacmap.inv.3<- plot(b.inv, select=3)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

cmacmap.inv.3
cmac3.gb<-grid.grabExpr(print(cmacmap.inv.3))

cmacmap.inv.4<-plot(b.inv, select=4)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)
cmacmap.inv.4
cmac4.gb<-grid.grabExpr(print(cmacmap.inv.4))

#All right, let's put these together nicely

cmac.4<-plot_grid(cmac1.gb,cmac2.gb,cmac3.gb,cmac4.gb, ncol=2, labels=c('B', 'C', 'D', 'E'))
cmac.4

cmac.all<-plot_grid(cmac.gb, cmac.4, ncol=2, rel_widths=c(6,4), labels=c('A', NULL))
cmac.all

pdf("plots/cmac_distribution.pdf", height=5, width=11)
grid.draw(cmac.all)
dev.off()

#So let's use the sampling as our 'base model' and take out the spatial stuff because 
#it will be autocorrelated with other values
#iterative process-use AIC as selection criterion
cmac.gam1<-gam(Coleomegilla.maculata~offset(log(1+Totalcount-Coleomegilla.maculata))+
                 s(log(1+Totalinvasive), sp=0.5)+
                 s(Agriculture, sp=0.5)+
                 s(Forest, sp=0.5)+
                 s(Developed, sp=0.5)+
                 s(Density, sp=0.5), 
               data=lb_all2, family="nb")
summary(cmac.gam1)
AIC(cmac.gam1)

visreg(cmac.gam1, "Totalinvasive",  ylab="Captures")
visreg(cmac.gam1, "Agriculture",  ylab="Captures")
visreg(cmac.gam1, "Forest", ylab="Captures")
visreg(cmac.gam1, "Developed",  ylab="Captures")
visreg(cmac.gam1, "Density", ylab="Captures")

#not a lot of super strong signals from anything in the "global model"

#replace totalinvasive with two major invasives

cmac.gam2<-gam(Coleomegilla.maculata~offset(log(1+Totalcount-Coleomegilla.maculata))+
                 s(log(1+Coccinella.septempunctata), sp=0.5, k=4)+
                 s(log(1+Harmonia.axyridis), sp=0.5, k=4)+
                 s(Agriculture, sp=0.5)+
                 s(Forest, sp=0.5)+
                 s(Developed, sp=0.5)+
                 s(Density, sp=0.5), 
               data=lb_all2, family="nb")
summary(cmac.gam2)
AIC(cmac.gam2)

visreg(cmac.gam2, "Coccinella.septempunctata",  ylab="Captures")
visreg(cmac.gam2, "Harmonia.axyridis",  ylab="Captures")
visreg(cmac.gam2, "Agriculture",  ylab="Captures")
visreg(cmac.gam2, "Forest", ylab="Captures")
visreg(cmac.gam2, "Developed",  ylab="Captures")
visreg(cmac.gam2, "Density", ylab="Captures")

#Model selection to whittle down landscape parameters in final model (intermediate form statistics recorded in excel file):

cmac.gam3<-gam(Coleomegilla.maculata~offset(log(1+Totalcount-Coleomegilla.maculata))+
                 s(log(1+Coccinella.septempunctata), sp=0.5, k=4)+
                 s(log(1+Harmonia.axyridis), sp=0.5, k=4)+
                 s(Forest, sp=0.5), 
               data=lb_all2, family="nb")
summary(cmac.gam3)
AIC(cmac.gam3)

cmac.c7<-visreg(cmac.gam3, "Coccinella.septempunctata",  ylab="Residual captures",
       xlab=expression(paste(italic("Coccinella septempunctata "), "captures")), 
       gg=T, 
       line=list(col="darkred"),
       fill=list(col="mistyrose1", fill="mistyrose1"),
       points=list(size=1, pch=24, fill="darkred", col="black"))+
    theme_classic()
cmac.c7
cmac.c7.gb<-grid.grabExpr(print(cmac.c7))

cmac.ha<-visreg(cmac.gam3, "Harmonia.axyridis",  ylab="Residual captures", 
       xlab=expression(paste(italic("Harmonia axyridis "), "captures")), 
       gg=T, 
       line=list(col="darkorange"),
       fill=list(col="burlywood1", fill="burlywood1"),
       points=list(size=1, pch=25, fill="darkorange", col="black"))+
  theme_classic()
cmac.ha
cmac.ha.gb<-grid.grabExpr(print(cmac.ha))

cmac.forest<-visreg(cmac.gam3, "Forest", ylab="Residual Captures", xlab="% Forest cover", 
       gg=T, 
       line=list(col="darkgreen"),
       fill=list(col="lightgreen", fill="lightgreen"),
       points=list(size=1, pch=21, fill="darkgreen", col="black"))+
  theme_classic()
cmac.forest
cmac.forest.gb<-grid.grabExpr(print(cmac.forest))


cmac.smooths<-plot_grid(cmac.c7, cmac.ha, cmac.forest, ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C'))
cmac.smooths

pdf("plots/cmac_smooths.pdf", height=3, width=9)
grid.draw(cmac.smooths)
dev.off()

##################################################################################################
#Coccinella novemnota

#first, how many captures are we working with?
sum(lb_all2$Coccinella.novemnotata)

#also because this species was never collected after 1990 let's create a dataset for the extirpated 
pre1995<-lb_all2[which(lb_all2$Decade<=1990),]

#try model that is linear with Totalcount (minus the captures of C.9 to make it independent) 
#and has a gaussian process-based spatial relationship
c9.gam<-gam(Coccinella.novemnotata~s(lon, lat, bs="gp")+
              offset(log(1+Totalcount-Coccinella.novemnotata)), 
            data=pre1995, family="nb")
summary(c9.gam)
AIC(c9.gam)
b<-getViz(c9.gam)

c9.gam.inv<-gam(Coccinella.novemnotata~s(lon, lat, by=Decade30, bs="gp")+
                  offset(log(1+Totalcount-Coccinella.novemnotata)), 
                data=pre1995, family="nb")
summary(c9.gam.inv)
AIC(c9.gam.inv)
b.inv<-getViz(c9.gam.inv)

text_high<-textGrob("Highest")
text_low<-textGrob("Lowest")
text_key<-textGrob("Predicted captures")

c9map<- plot(b, select=1)+theme_classic()+
  xlab("Longitude")+ylab("Latitude")+ggtitle(NULL)+
  theme(aspect.ratio=1,legend.background=element_blank(), 
        legend.title = element_blank(), legend.text = element_blank(),
        plot.margin=unit(c(1, 3, 0.5, 1), "lines"))+
  annotation_custom(text_high, xmin=-79.7,xmax=-79.7,ymin=40.55,ymax=40.55)+
  annotation_custom(text_low, xmin=-79.7,xmax=-79.7,ymin=39.70,ymax=39.70)+
  annotation_custom(text_key, xmin=-79.9,xmax=-79.9,ymin=40.77,ymax=40.77)+
  coord_cartesian(clip = "off")

c9map
c9.gb<-grid.grabExpr(print(c9map))


#make the maps for each of the invasion periods


c9map.inv.1<- plot(b.inv, select=1)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

c9map.inv.1
c91.gb<-grid.grabExpr(print(c9map.inv.1))

c9map.inv.2<- plot(b.inv, select=2)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

c9map.inv.2
c92.gb<-grid.grabExpr(print(c9map.inv.2))

c9map.inv.3<- plot(b.inv, select=3)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

c9map.inv.3
c93.gb<-grid.grabExpr(print(c9map.inv.3))



#All right, let's put these together nicely

c9.4<-plot_grid(c91.gb,c92.gb,c93.gb,NULL, ncol=2, labels=c('B', 'C', 'D', 'E'))
c9.4

c9.all<-plot_grid(c9.gb, c9.4, ncol=2, rel_widths=c(6,4), labels=c('A', NULL))
c9.all

pdf("plots/c9_distribution.pdf", height=5, width=11)
grid.draw(c9.all)
dev.off()

#So let's use the sampling as our 'base model' and take out the spatial stuff because 
#it will be autocorrelated with other values
#iterative process-use AIC as selection criterion
c9.gam1<-gam(Coccinella.novemnotata~offset(log(1+Totalcount-Coccinella.novemnotata))+
               s(log(1+Totalinvasive), sp=0.5, k=4)+
               s(Agriculture, sp=0.5)+
               s(Forest, sp=0.5)+
               s(Developed, sp=0.5)+
               s(Density, sp=0.5), 
             data=pre1995, family="nb")
summary(c9.gam1)
AIC(c9.gam1)

visreg(c9.gam1, "Totalinvasive",  ylab="Captures")
visreg(c9.gam1, "Agriculture",  ylab="Captures")
visreg(c9.gam1, "Forest", ylab="Captures")
visreg(c9.gam1, "Developed",  ylab="Captures")
visreg(c9.gam1, "Density", ylab="Captures")

#not a lot of super strong signals from anything in the "global model"

#for C9, only invasive present coincident with it  for any time is C7, but for consistency let's do that

c9.gam2<-gam(Coccinella.novemnotata~offset(log(1+Totalcount-Coccinella.novemnotata))+
               s(log(1+Coccinella.septempunctata), sp=0.5, k=4)+
               s(Agriculture, sp=0.5)+
               s(Forest, sp=0.5)+
               s(Developed, sp=0.5)+
               s(Density, sp=0.5), 
             data=pre1995, family="nb")
summary(c9.gam2)
AIC(c9.gam2)

visreg(c9.gam2, "Coccinella.septempunctata",  ylab="Captures")
visreg(c9.gam2, "Agriculture",  ylab="Captures")
visreg(c9.gam2, "Forest", ylab="Captures")
visreg(c9.gam2, "Developed",  ylab="Captures")
visreg(c9.gam2, "Density", ylab="Captures")

#Looks like totalinvasives is beeter for C9
#Model selection to whittle down landscape parameters in final model (intermediate form statistics recorded in excel file):

c9.gam3<-gam(Coccinella.novemnotata~offset(log(1+Totalcount-Coccinella.novemnotata))+
               s(log(1+Totalinvasive), sp=0.5, k=4)+
               s(Agriculture, sp=0.5)+
               s(Density, sp=0.5), 
             data=pre1995, family="nb")
summary(c9.gam3)
AIC(c9.gam3)

c9.inv<-visreg(c9.gam3, "Totalinvasive",  ylab="Residual captures",
              xlab=expression(paste("Total invasive captures")), 
              gg=T, 
              line=list(col="darkred"),
              fill=list(col="mistyrose1", fill="mistyrose1"),
              points=list(size=1, pch=24, fill="darkred", col="black"))+
  theme_classic()
c9.inv
c9.inv.gb<-grid.grabExpr(print(c9.inv))


c9.agriculture<-visreg(c9.gam3, "Agriculture", ylab="Residual captures", xlab="% Agriculture cover", 
                  gg=T, 
                  line=list(col="darkolivegreen4"),
                  fill=list(col="darkolivegreen1", fill="darkolivegreen1"),
                  points=list(size=1, pch=22, fill="darkolivegreen4", col="black"))+
  theme_classic()
c9.agriculture
c9.agriculture.gb<-grid.grabExpr(print(c9.agriculture))

c9.density<-visreg(c9.gam3, "Density", ylab="Residual captures", xlab="Human population density", 
                       gg=T, 
                       line=list(col="navyblue"),
                       fill=list(col="lightsteelblue1", fill="lightsteelblue1"),
                       points=list(size=1, pch=23, fill="navyblue", col="black"))+
  theme_classic()
c9.density
c9.density.gb<-grid.grabExpr(print(c9.density))

c9.smooths<-plot_grid(c9.inv, c9.agriculture, c9.density, ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C'))
c9.smooths

pdf("plots/c9_smooths.pdf", height=3, width=9)
grid.draw(c9.smooths)
dev.off()


##################################################################################################
#Adalia bipunctata 

#first, how many captures are we working with?
sum(lb_all2$Adalia.bipunctata)

#also because this species was never collected after 1990 like C9 we need to use that same pre 1995 dataset


#try model that is linear with Totalcount (minus the captures of C.9 to make it independent) 
#and has a gaussian process-based spatial relationship
abi.gam<-gam(Adalia.bipunctata~s(lon, lat, bs="gp")+
               offset(log(1+Totalcount-Adalia.bipunctata)), 
             data=pre1995, family="nb")
summary(abi.gam)
AIC(abi.gam)
b<-getViz(abi.gam)

abi.gam.inv<-gam(Adalia.bipunctata~s(lon, lat, by=Decade30, bs="gp")+
                   offset(log(1+Totalcount-Adalia.bipunctata)), 
                 data=pre1995, family="nb")
summary(abi.gam.inv)
AIC(abi.gam.inv)
b.inv<-getViz(abi.gam.inv)

text_high<-textGrob("Highest")
text_low<-textGrob("Lowest")
text_key<-textGrob("Predicted captures")

abimap<- plot(b, select=1)+theme_classic()+
  xlab("Longitude")+ylab("Latitude")+ggtitle(NULL)+
  theme(aspect.ratio=1,legend.background=element_blank(), 
        legend.title = element_blank(), legend.text = element_blank(),
        plot.margin=unit(c(1, 3, 0.5, 1), "lines"))+
  annotation_custom(text_high, xmin=-79.7,xmax=-79.7,ymin=40.55,ymax=40.55)+
  annotation_custom(text_low, xmin=-79.7,xmax=-79.7,ymin=39.70,ymax=39.70)+
  annotation_custom(text_key, xmin=-79.9,xmax=-79.9,ymin=40.77,ymax=40.77)+
  coord_cartesian(clip = "off")

abimap
abi.gb<-grid.grabExpr(print(abimap))


#make the maps for each of the invasion periods


abimap.inv.1<- plot(b.inv, select=1)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

abimap.inv.1
abi1.gb<-grid.grabExpr(print(abimap.inv.1))

abimap.inv.2<- plot(b.inv, select=2)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

abimap.inv.2
abi2.gb<-grid.grabExpr(print(abimap.inv.2))

abimap.inv.3<- plot(b.inv, select=3)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

abimap.inv.3
abi3.gb<-grid.grabExpr(print(abimap.inv.3))



#All right, let's put these together nicely

abi.4<-plot_grid(abi1.gb,abi2.gb,abi3.gb,NULL, ncol=2, labels=c('B', 'C', 'D', 'E'))
abi.4

abi.all<-plot_grid(abi.gb, abi.4, ncol=2, rel_widths=c(6,4), labels=c('A', NULL))
abi.all

pdf("plots/abi_distribution.pdf", height=5, width=11)
grid.draw(abi.all)
dev.off()

#So let's use the sampling as our 'base model' and take out the spatial stuff because 
#it will be autocorrelated with other values
#iterative process-use AIC as selection criterion
abi.gam1<-gam(Adalia.bipunctata~offset(log(1+Totalcount-Adalia.bipunctata))+
                s(log(1+Totalinvasive), sp=0.5, k=4)+
                s(Agriculture, sp=0.5)+
                s(Forest, sp=0.5)+
                s(Developed, sp=0.5)+
                s(Density, sp=0.5), 
              data=pre1995, family="nb")
summary(abi.gam1)
AIC(abi.gam1)

visreg(abi.gam1, "Totalinvasive",  ylab="Captures")
visreg(abi.gam1, "Agriculture",  ylab="Captures")
visreg(abi.gam1, "Forest", ylab="Captures")
visreg(abi.gam1, "Developed",  ylab="Captures")
visreg(abi.gam1, "Density", ylab="Captures")

#not a lot of super strong signals from anything in the "global model"

#for ABI, only invasive present coincident with it  for any time is C7, but for consistency let's do that

abi.gam2<-gam(Adalia.bipunctata~offset(log(1+Totalcount-Adalia.bipunctata))+
                s(log(1+Coccinella.septempunctata), sp=0.5, k=4)+
                s(Agriculture, sp=0.5)+
                s(Forest, sp=0.5)+
                s(Developed, sp=0.5)+
                s(Density, sp=0.5), 
              data=pre1995, family="nb")
summary(abi.gam2)
AIC(abi.gam2)

visreg(abi.gam2, "Coccinella.septempunctata",  ylab="Captures")
visreg(abi.gam2, "Agriculture",  ylab="Captures")
visreg(abi.gam2, "Forest", ylab="Captures")
visreg(abi.gam2, "Developed",  ylab="Captures")
visreg(abi.gam2, "Density", ylab="Captures")

#Looks like totalinvasives is better for ABI
#Model selection to whittle down landscape parameters in final model (intermediate form statistics recorded in excel file):

abi.gam3<-gam(Adalia.bipunctata~offset(log(1+Totalcount-Adalia.bipunctata))+
                s(log(1+Totalinvasive), sp=0.5, k=4)+
                s(Agriculture, sp=0.5)+
                s(Developed, sp=0.5), 
              data=pre1995, family="nb")
summary(abi.gam3)
AIC(abi.gam3)

abi.inv<-visreg(abi.gam3, "Totalinvasive",  ylab="Residual captures",
                xlab=expression(paste("Total invasive captures")), 
                gg=T, 
                line=list(col="darkred"),
                fill=list(col="mistyrose1", fill="mistyrose1"),
                points=list(size=1, pch=24, fill="darkred", col="black"))+
  theme_classic()
abi.inv
abi.inv.gb<-grid.grabExpr(print(abi.inv))


abi.agriculture<-visreg(abi.gam3, "Agriculture", ylab="Residual captures", xlab="% Agriculture cover", 
                        gg=T, 
                        line=list(col="darkolivegreen4"),
                        fill=list(col="darkolivegreen1", fill="darkolivegreen1"),
                        points=list(size=1, pch=22, fill="darkolivegreen4", col="black"))+
  theme_classic()
abi.agriculture
abi.agriculture.gb<-grid.grabExpr(print(abi.agriculture))

abi.developed<-visreg(abi.gam3, "Developed", ylab="Residual captures", xlab="% Developed cover", 
                    gg=T, 
                    line=list(col="slategray4"),
                    fill=list(col="slategray2", fill="slategray2"),
                    points=list(size=1, pch=23, fill="slategray4", col="black"))+
  theme_classic()
abi.developed
abi.developed.gb<-grid.grabExpr(print(abi.developed))

abi.smooths<-plot_grid(abi.inv, abi.agriculture, abi.developed, ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C'))
abi.smooths

pdf("plots/abi_smooths.pdf", height=3, width=9)
grid.draw(abi.smooths)
dev.off()

###########################################################################################

#Hippodamia covergens



###########################################################################################

#scale feeding- Chilocorus stigma


###########################################################################################
#non-native species 
#Coccinella septempunctata
#Harmonia axyridis


