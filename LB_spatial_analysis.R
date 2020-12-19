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



# ohiomap<- ggplot()+ geom_sf(data=oh4, aes(fill=Regions))+theme_classic()+
#   geom_point(data=lb_allcontext, aes(x=lon, y=lat))+xlab("Longitude")+ylab("Latitude")
# 
# ohiomap
# 
# 
# #ok, let's make a demo with one of the moderately rare species that is probably not in all counties
# 
# anatismali<-lb_allcontext[which(lb_allcontext$Name=="Anatis mali"),]

# #Count the number of captures per county
# #and f this I'm using plyr
# library(plyr)
# 
# amali.density<-ddply(anatismali, c("County","Decade", "lon", "lat"), summarize,
#                      Count=sum(Count))
# 
# #and a nonzero dataset for the mapping
# amali.density.1<-amali.density[which(amali.density$Count>0),]
# 
# 
# #first let's just do a little plot that scales points by number captured
# amalimap<- ggplot()+ geom_sf(data=oh4, aes(fill=Regions))+
#   theme_classic()+
#   geom_point(data=amali.density.1, aes(x=lon, y=lat, size=Count), pch=21, color="black")+
#   xlab("Longitude")+ylab("Latitude")
# 
# amalimap
# 
# #now a map with binned density
# 
# 
# amalimap2<- ggplot()+ geom_sf(data=oh4)+
#   theme_classic()+
#   stat_bin2d(data=amali.density.1, aes(x = lon, y = lat),
#              size = 1, bins = 3, alpha = 0.8)+
#   xlab("Longitude")+ylab("Latitude")
# 
# amalimap2
# 
# #now a map with smoothed density- let's use lb raw counts 
# 
# density_plot<- ggplot()+ geom_sf(data=oh4)+
#   theme_classic()+
#   stat_density2d(data=lb_raw, aes(x = lon, y = lat, fill=after_stat(level)),
#                  geom="polygon", alpha=0.7)+
#   scale_fill_viridis()+
#   geom_point(data=lb_raw, aes(x=lon, y=lat), pch=21, color="black", position="jitter")+
#   xlab("Longitude")+ylab("Latitude")+
#   facet_wrap(~Decade30)
# 
# density_plot


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

# coordinates(grd) <- ~x + y
# sp::gridded(grd) <- TRUE
# proj4string(grd) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
# 
# plot(grd, cex = 1.5, col = "grey")
# points(lb_spat, pch = 1, col = "red", cex = 1)
# 
# #and now we have a plot of the county centroids on a grid. cute.
# 
# #inverse density weight
# 
# idw <- gstat::idw(formula = Count ~ 1, locations = lb_spat, newdata = grd)
# 
# 
# # grab output of IDW for plotting
# idw.output = as.data.frame(idw)  # output is defined as a data table
# # set the names of the idw.output columns
# # basic ggplot using geom_tile to display our interpolated grid within no map
# ggplot() + 
#   geom_sf(data=oh4)+
#   geom_tile(data = idw.output, aes(x = x, y = y, fill = var1.pred), alpha=0.75) + 
#   geom_point(data = lb_allcontext, aes(x = lon, y = lat), shape = 21, color = "red") +
#   scale_fill_viridis()+ 
#   theme_bw() 


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
library(ggpubr)

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

#need a few more calculations like total invasive, total aphidophagous, proporation invasive

#total invasive
lb_all2$Totalinvasive<-lb_all2$Coccinella.septempunctata+lb_all2$Coccinella.undecimpunctata+
  lb_all2$Harmonia.axyridis+lb_all2$Hippodamia.variegata+lb_all2$Propylea.quatuordecimpunctata

#proportion invasive
lb_all2$Propinvasive<-ifelse(lb_all2$Totalcount>0, lb_all2$Totalinvasive/lb_all2$Totalcount, 0)

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
lb_all2$Nontarget<-as.numeric(lb_all2$Nontarget)

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
                 s(Decade, sp=0.5, k=4)+
                 s(lon, sp=0.5)+
                 s(lat, sp=0.5)+
                 s(log(1+Totalinvasive), sp=0.5)+
                 s(Agriculture, sp=0.5)+
                 s(Forest, sp=0.5)+
                 s(Developed, sp=0.5), 
               data=lb_all2, family="nb")
summary(cmac.gam1)
AIC(cmac.gam1)

visreg(cmac.gam1, "Decade",  ylab="Captures")
visreg(cmac.gam1, "lon",  ylab="Captures")
visreg(cmac.gam1, "lat",  ylab="Captures")
visreg(cmac.gam1, "Totalinvasive",  ylab="Captures")
visreg(cmac.gam1, "Agriculture",  ylab="Captures")
visreg(cmac.gam1, "Forest", ylab="Captures")
visreg(cmac.gam1, "Developed",  ylab="Captures")


#not a lot of super strong signals from anything in the "global model"

#replace totalinvasive with two major invasives

cmac.gam2<-gam(Coleomegilla.maculata~offset(log(1+Totalcount-Coleomegilla.maculata))+
                 s(Decade, sp=0.5, k=4)+
                 s(lon, sp=0.5)+
                 s(lat, sp=0.5)+
                 s(Propinvasive, sp=0.5)+
                 s(log(1+Coccinella.septempunctata), sp=0.5, k=4)+
                 s(log(1+Harmonia.axyridis), sp=0.5, k=4)+
                 s(Agriculture, sp=0.5)+
                 s(Forest, sp=0.5)+
                 s(Developed, sp=0.5),  
               data=lb_all2, family="nb")
summary(cmac.gam2)
AIC(cmac.gam2)

visreg(cmac.gam2, "Decade",  ylab="Captures")
#visreg(cmac.gam2, "lon",  ylab="Captures")
visreg(cmac.gam2, "lat",  ylab="Captures")
visreg(cmac.gam2, "Propinvasive",  ylab="Captures")
#visreg(cmac.gam2, "Coccinella.septempunctata",  ylab="Captures")
#visreg(cmac.gam2, "Harmonia.axyridis",  ylab="Captures")
visreg(cmac.gam2, "Agriculture",  ylab="Captures")
visreg(cmac.gam2, "Forest", ylab="Captures")
visreg(cmac.gam2, "Developed",  ylab="Captures")



#Model selection to whittle down landscape parameters in final model (intermediate form statistics recorded in excel file):

cmac.gam3<-gam(Coleomegilla.maculata~offset(log(1+Totalcount-Coleomegilla.maculata))+
                 s(Decade, sp=0.5, k=4)+
                 s(lat, sp=0.5)+
                 s(Propinvasive, sp=0.5)+
                 s(Agriculture, sp=0.5),  
               data=lb_all2, family="nb")
summary(cmac.gam3)
AIC(cmac.gam3)

#for all the 'final' models as chosen by AIC, check the concurvity to identify variables where concurvity is 
#likely to be causing an issue in the final fit. Eliminate variables with overall estimated concurvity >0.8 
# (do this one by one and recalculate, record stats in the model selection spreadhseet)

concurvity(cmac.gam3)
#ok, can stay as is!

cmac.decade<-visreg(cmac.gam3, "Decade",  ylab="Residual captures",
       xlab=expression(paste("Year")), 
       gg=T, 
       line=list(col="gray19"),
       fill=list(col="gray57", fill="gray57"),
       points=list(size=1, pch=21, fill="gray19", col="black"))+
    theme_classic()
cmac.decade

cmac.lat<-visreg(cmac.gam3, "lat",  ylab="Residual captures",
                    xlab=expression(paste("Latitude")), 
                    gg=T, 
                    line=list(col="gray28"),
                    fill=list(col="gray", fill="gray"),
                    points=list(size=1, pch=21, fill="gray28", col="black"))+
  theme_classic()
cmac.lat

cmac.pi<-visreg(cmac.gam3, "Propinvasive",  ylab="Residual captures", 
       xlab=expression(paste("Proportion invasive")), 
       gg=T, 
       line=list(col="darkmagenta"),
       fill=list(col="plum1", fill="plum1"),
       points=list(size=1, pch=22, fill="darkmagenta", col="black"))+
  theme_classic()
cmac.pi


cmac.agriculture<-visreg(cmac.gam3, "Agriculture", ylab="Residual Captures", xlab="% Agriculture cover", 
       gg=T, 
       line=list(col="darkolivegreen4"),
       fill=list(col="darkolivegreen1", fill="darkolivegreen1"),
       points=list(size=1, pch=23, fill="darkolivegreen4", col="black"))+
  theme_classic()
cmac.agriculture

#create graphical objects for placeholders in plots
noeffect<-text_grob(paste("No effect"), color="black")
notpresent<-text_grob(paste("Not present"), color="black")

cmac.smooths<-plot_grid(cmac.decade, noeffect, cmac.lat, 
                        cmac.pi, noeffect, noeffect,
                        cmac.agriculture, noeffect, noeffect,
                        ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C', 
                                                              'D', 'E', 'F',
                                                              'G', 'H', 'I'))
cmac.smooths

pdf("plots/cmac_smooths.pdf", height=9, width=9)
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

c9.4<-plot_grid(c91.gb, c92.gb, c93.gb, notpresent, ncol=2, labels=c('B', 'C', 'D', 'E'))
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
               s(Decade, sp=0.5, k=4)+
               s(lon, sp=0.5)+
               s(lat, sp=0.5)+
               s(log(1+Totalinvasive), sp=0.5, k=4)+
               s(Agriculture, sp=0.5)+
               s(Forest, sp=0.5)+
               s(Developed, sp=0.5), 
             data=pre1995, family="nb")
summary(c9.gam1)
AIC(c9.gam1)

visreg(c9.gam1, "Decade",  ylab="Captures")
visreg(c9.gam1, "lon",  ylab="Captures")
visreg(c9.gam1, "lat",  ylab="Captures")
visreg(c9.gam1, "Totalinvasive",  ylab="Captures")
visreg(c9.gam1, "Agriculture",  ylab="Captures")
visreg(c9.gam1, "Forest", ylab="Captures")
visreg(c9.gam1, "Developed",  ylab="Captures")


#only Decade is popping out right now but let's see what happens when we narrow down sources of variation

#replace totalinvasive with two major invasives

c9.gam2<-gam(Coccinella.novemnotata~offset(log(1+Totalcount-Coccinella.novemnotata))+
               s(Decade, sp=0.5, k=4)+
               s(lon, sp=0.5)+
               s(lat, sp=0.5)+
               s(Propinvasive, sp=0.5)+
               s(log(1+Coccinella.septempunctata), sp=0.5, k=4)+
               s(Agriculture, sp=0.5)+
               s(Forest, sp=0.5)+
               s(Developed, sp=0.5),  
             data=pre1995, family="nb")
summary(c9.gam2)
AIC(c9.gam2)

visreg(c9.gam2, "Decade",  ylab="Captures")
#visreg(c9.gam2, "lon",  ylab="Captures")
visreg(c9.gam2, "lat",  ylab="Captures")
visreg(c9.gam2, "Propinvasive",  ylab="Captures")
#visreg(c9.gam2, "Coccinella.septempunctata",  ylab="Captures")
#visreg(c9.gam2, "Harmonia.axyridis",  ylab="Captures")
visreg(c9.gam2, "Agriculture",  ylab="Captures")
visreg(c9.gam2, "Forest", ylab="Captures")
visreg(c9.gam2, "Developed",  ylab="Captures")



#Model selection to whittle down landscape parameters in final model (intermediate form statistics recorded in excel file):

c9.gam3<-gam(Coccinella.novemnotata~offset(log(1+Totalcount-Coccinella.novemnotata))+
               s(Decade, sp=0.5, k=4)+
               s(lon, sp=0.5)+
               s(lat, sp=0.5)+
               s(Agriculture, sp=0.5),  
             data=pre1995, family="nb")
summary(c9.gam3)
AIC(c9.gam3)

#for all the 'final' models as chosen by AIC, check the concurvity to identify variables where concurvity is 
#likely to be causing an issue in the final fit. Eliminate variables with overall estimated concurvity >0.8 
# (do this one by one and recalculate, record stats in the model selection spreadhseet)

concurvity(c9.gam3)
#ok, can stay as is!

c9.decade<-visreg(c9.gam3, "Decade",  ylab="Residual captures",
                  xlab=expression(paste("Year")), 
                  gg=T, 
                  line=list(col="gray19"),
                  fill=list(col="gray57", fill="gray57"),
                  points=list(size=1, pch=21, fill="gray19", col="black"))+
  theme_classic()
c9.decade

c9.lon<-visreg(c9.gam3, "lon",  ylab="Residual captures",
               xlab=expression(paste("Longitude")), 
               gg=T, 
               line=list(col="gray28"),
               fill=list(col="gray", fill="gray"),
               points=list(size=1, pch=21, fill="gray28", col="black"))+
  theme_classic()
c9.lon

c9.lat<-visreg(c9.gam3, "lat",  ylab="Residual captures",
               xlab=expression(paste("Latitude")), 
               gg=T, 
               line=list(col="gray28"),
               fill=list(col="gray", fill="gray"),
               points=list(size=1, pch=21, fill="gray28", col="black"))+
  theme_classic()
c9.lat

# c9.pi<-visreg(c9.gam3, "Propinvasive",  ylab="Residual captures", 
#               xlab=expression(paste("Proportion invasive")), 
#               gg=T, 
#               line=list(col="darkmagenta"),
#               fill=list(col="plum1", fill="plum1"),
#               points=list(size=1, pch=22, fill="darkmagenta", col="black"))+
#   theme_classic()
# c9.pi


c9.agriculture<-visreg(c9.gam3, "Agriculture", ylab="Residual Captures", xlab="% Agriculture cover", 
                       gg=T, 
                       line=list(col="darkolivegreen4"),
                       fill=list(col="darkolivegreen1", fill="darkolivegreen1"),
                       points=list(size=1, pch=23, fill="darkolivegreen4", col="black"))+
  theme_classic()
c9.agriculture


#create graphical objects for placeholders in plots
noeffect<-text_grob(paste("No effect"), color="black")
notpresent<-text_grob(paste("Not present"), color="black")

c9.smooths<-plot_grid(c9.decade, c9.lon, c9.lat, 
                      noeffect, noeffect, noeffect,
                      c9.agriculture, noeffect, noeffect,
                      ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C', 
                                                            'D', 'E', 'F',
                                                            'G', 'H', 'I'))
c9.smooths

pdf("plots/c9_smooths.pdf", height=9, width=9)
grid.draw(c9.smooths)
dev.off()



##################################################################################################
#Adalia bipunctata 

#first, how many captures are we working with?
sum(lb_all2$Adalia.bipunctata)

#also because this species was never collected after 1990 like C9 we need to use that same pre 1995 dataset


#try model that is linear with Totalcount (minus the captures of abi to make it independent) 
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

abi.4<-plot_grid(abi1.gb, abi2.gb, abi3.gb, notpresent, ncol=2, labels=c('B', 'C', 'D', 'E'))
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
                s(Decade, sp=0.5, k=4)+
                s(lon, sp=0.5)+
                s(lat, sp=0.5)+
                s(log(1+Totalinvasive), sp=0.5, k=4)+
                s(Agriculture, sp=0.5)+
                s(Forest, sp=0.5)+
                s(Developed, sp=0.5), 
              data=pre1995, family="nb")
summary(abi.gam1)
AIC(abi.gam1)

visreg(abi.gam1, "Decade",  ylab="Captures")
visreg(abi.gam1, "lon",  ylab="Captures")
visreg(abi.gam1, "lat",  ylab="Captures")
visreg(abi.gam1, "Totalinvasive",  ylab="Captures")
visreg(abi.gam1, "Agriculture",  ylab="Captures")
visreg(abi.gam1, "Forest", ylab="Captures")
visreg(abi.gam1, "Developed",  ylab="Captures")


#replace totalinvasive with two major invasives

abi.gam2<-gam(Adalia.bipunctata~offset(log(1+Totalcount-Adalia.bipunctata))+
                s(Decade, sp=0.5, k=4)+
                s(lon, sp=0.5)+
                s(lat, sp=0.5)+
                s(Propinvasive, sp=0.5)+
                #s(log(1+Coccinella.septempunctata), sp=0.5, k=4)+
                s(Agriculture, sp=0.5)+
                s(Forest, sp=0.5)+
                s(Developed, sp=0.5),  
              data=pre1995, family="nb")
summary(abi.gam2)
AIC(abi.gam2)

visreg(abi.gam2, "Decade",  ylab="Captures")
#visreg(abi.gam2, "lon",  ylab="Captures")
visreg(abi.gam2, "lat",  ylab="Captures")
visreg(abi.gam2, "Propinvasive",  ylab="Captures")
#visreg(abi.gam2, "Coccinella.septempunctata",  ylab="Captures")
#visreg(abi.gam2, "Harmonia.axyridis",  ylab="Captures")
visreg(abi.gam2, "Agriculture",  ylab="Captures")
visreg(abi.gam2, "Forest", ylab="Captures")
visreg(abi.gam2, "Developed",  ylab="Captures")


#Model selection to whittle down landscape parameters in final model (intermediate form statistics recorded in excel file):

abi.gam3<-gam(Adalia.bipunctata~offset(log(1+Totalcount-Adalia.bipunctata))+
                s(lon, sp=0.5)+
                s(Propinvasive, sp=0.5)+
                s(Agriculture, sp=0.5)+
                s(Developed, sp=0.5),  
              data=pre1995, family="nb")
summary(abi.gam3)
AIC(abi.gam3)


#for all the 'final' models as chosen by AIC, check the concurvity to identify variables where concurvity is 
#likely to be causing an issue in the final fit. Eliminate variables with overall estimated concurvity >0.8 
# (do this one by one and recalculate, record stats in the model selection spreadhseet)

concurvity(abi.gam3)
#ok, can stay as is!


# abi.decade<-visreg(abi.gam3, "Decade",  ylab="Residual captures",
#                    xlab=expression(paste("Year")), 
#                    gg=T, 
#                    line=list(col="gray19"),
#                    fill=list(col="gray57", fill="gray57"),
#                    points=list(size=1, pch=21, fill="gray19", col="black"))+
#   theme_classic()
# abi.decade

abi.lon<-visreg(abi.gam3, "lon",  ylab="Residual captures",
                xlab=expression(paste("Longitude")), 
                gg=T, 
                line=list(col="gray28"),
                fill=list(col="gray", fill="gray"),
                points=list(size=1, pch=21, fill="gray28", col="black"))+
  theme_classic()
abi.lon

# abi.lat<-visreg(abi.gam3, "lat",  ylab="Residual captures",
#                 xlab=expression(paste("Latitude")), 
#                 gg=T, 
#                 line=list(col="gray28"),
#                 fill=list(col="gray", fill="gray"),
#                 points=list(size=1, pch=21, fill="gray28", col="black"))+
#   theme_classic()
# abi.lat

abi.pi<-visreg(abi.gam3, "Propinvasive",  ylab="Residual captures",
              xlab=expression(paste("Proportion invasive")),
              gg=T,
              line=list(col="darkmagenta"),
              fill=list(col="plum1", fill="plum1"),
              points=list(size=1, pch=22, fill="darkmagenta", col="black"))+
  theme_classic()
abi.pi


abi.agriculture<-visreg(abi.gam3, "Agriculture", ylab="Residual Captures", xlab="% Agriculture cover", 
                        gg=T, 
                        line=list(col="darkolivegreen4"),
                        fill=list(col="darkolivegreen1", fill="darkolivegreen1"),
                        points=list(size=1, pch=23, fill="darkolivegreen4", col="black"))+
  theme_classic()
abi.agriculture

abi.developed<-visreg(abi.gam3, "Developed", ylab="Residual Captures", xlab="% Developed cover",
                       gg=T,
                       line=list(col="slategray4"),
                       fill=list(col="slategray2", fill="slategray2"),
                       points=list(size=1, pch=23, fill="slategray4", col="black"))+
  theme_classic()
abi.developed



#create graphical objects for placeholders in plots
noeffect<-text_grob(paste("No effect"), color="black")
notpresent<-text_grob(paste("Not present"), color="black")

abi.smooths<-plot_grid(noeffect, abi.lon, noeffect, 
                       abi.pi, noeffect, noeffect,
                       abi.agriculture, noeffect, abi.developed,
                       ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C', 
                                                             'D', 'E', 'F',
                                                             'G', 'H', 'I'))
abi.smooths

pdf("plots/abi_smooths.pdf", height=9, width=9)
grid.draw(abi.smooths)
dev.off()


###########################################################################################

#Hippodamia covergens


#first, how many captures are we working with?
sum(lb_all2$Hippodamia.convergens)


#try model that is linear with Totalcount (minus the captures of hcon to make it independent) 
#and has a gaussian process-based spatial relationship
hcon.gam<-gam(Hippodamia.convergens~s(lon, lat, bs="gp")+
                offset(log(1+Totalcount-Hippodamia.convergens)), 
              data=lb_all2, family="nb")
summary(hcon.gam)
AIC(hcon.gam)
b<-getViz(hcon.gam)

hcon.gam.inv<-gam(Hippodamia.convergens~s(lon, lat, by=Decade30, bs="gp")+
                    offset(log(1+Totalcount-Hippodamia.convergens)), 
                  data=lb_all2, family="nb")
summary(hcon.gam.inv)
AIC(hcon.gam.inv)
b.inv<-getViz(hcon.gam.inv)


hconmap<- plot(b, select=1)+theme_classic()+
  xlab("Longitude")+ylab("Latitude")+ggtitle(NULL)+
  theme(aspect.ratio=1,legend.background=element_blank(), 
        legend.title = element_blank(), legend.text = element_blank(),
        plot.margin=unit(c(1, 3, 0.5, 1), "lines"))+
  annotation_custom(text_high, xmin=-79.7,xmax=-79.7,ymin=40.55,ymax=40.55)+
  annotation_custom(text_low, xmin=-79.7,xmax=-79.7,ymin=39.70,ymax=39.70)+
  annotation_custom(text_key, xmin=-79.9,xmax=-79.9,ymin=40.77,ymax=40.77)+
  coord_cartesian(clip = "off")

hconmap
hcon.gb<-grid.grabExpr(print(hconmap))


#make the maps for each of the invasion periods

hconmap.inv.1<- plot(b.inv, select=1)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

hconmap.inv.1
hcon1.gb<-grid.grabExpr(print(hconmap.inv.1))

hconmap.inv.2<- plot(b.inv, select=2)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

hconmap.inv.2
hcon2.gb<-grid.grabExpr(print(hconmap.inv.2))

hconmap.inv.3<- plot(b.inv, select=3)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

hconmap.inv.3
hcon3.gb<-grid.grabExpr(print(hconmap.inv.3))

hconmap.inv.4<- plot(b.inv, select=4)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

hconmap.inv.4
hcon4.gb<-grid.grabExpr(print(hconmap.inv.4))

#All right, let's put these together nicely

hcon.4<-plot_grid(hcon1.gb, hcon2.gb, hcon3.gb, hcon4.gb, ncol=2, labels=c('B', 'C', 'D', 'E'))
hcon.4

hcon.all<-plot_grid(hcon.gb, hcon.4, ncol=2, rel_widths=c(6,4), labels=c('A', NULL))
hcon.all

pdf("plots/hcon_distribution.pdf", height=5, width=11)
grid.draw(hcon.all)
dev.off()

#So let's use the sampling as our 'base model' and take out the spatial stuff because 
#it will be autocorrelated with other values
#iterative process-use AIC as selection criterion
hcon.gam1<-gam(Hippodamia.convergens~offset(log(1+Totalcount-Hippodamia.convergens))+
                 s(Decade, sp=0.5, k=4)+
                 s(lon, sp=0.5)+
                 s(lat, sp=0.5)+
                 s(log(1+Totalinvasive), sp=0.5, k=4)+
                 s(Agriculture, sp=0.5)+
                 s(Forest, sp=0.5)+
                 s(Developed, sp=0.5), 
               data=lb_all2, family="nb")
summary(hcon.gam1)
AIC(hcon.gam1)

visreg(hcon.gam1, "Decade",  ylab="Captures")
visreg(hcon.gam1, "lon",  ylab="Captures")
visreg(hcon.gam1, "lat",  ylab="Captures")
visreg(hcon.gam1, "Totalinvasive",  ylab="Captures")
visreg(hcon.gam1, "Agriculture",  ylab="Captures")
visreg(hcon.gam1, "Forest", ylab="Captures")
visreg(hcon.gam1, "Developed",  ylab="Captures")


#replace totalinvasive with two major invasives

hcon.gam2<-gam(Hippodamia.convergens~offset(log(1+Totalcount-Hippodamia.convergens))+
                 s(Decade, sp=0.5, k=4)+
                 s(lon, sp=0.5)+
                 s(lat, sp=0.5)+
                 s(Propinvasive, sp=0.5)+
                 #s(log(1+Coccinella.septempunctata), sp=0.5, k=4)+
                 #s(log(1+Harmonia.axyridis), sp=0.5, k=4)+
                 s(Agriculture, sp=0.5)+
                 s(Forest, sp=0.5)+
                 s(Developed, sp=0.5),  
               data=lb_all2, family="nb")
summary(hcon.gam2)
AIC(hcon.gam2)

visreg(hcon.gam2, "Decade",  ylab="Captures")
#visreg(hcon.gam2, "lon",  ylab="Captures")
visreg(hcon.gam2, "lat",  ylab="Captures")
visreg(hcon.gam2, "Propinvasive",  ylab="Captures")
#visreg(hcon.gam2, "Coccinella.septempunctata",  ylab="Captures")
#visreg(hcon.gam2, "Harmonia.axyridis",  ylab="Captures")
visreg(hcon.gam2, "Agriculture",  ylab="Captures")
visreg(hcon.gam2, "Forest", ylab="Captures")
visreg(hcon.gam2, "Developed",  ylab="Captures")



#Model selection to whittle down landscape parameters in final model (intermediate form statistics recorded in excel file):

hcon.gam3<-gam(Hippodamia.convergens~offset(log(1+Totalcount-Hippodamia.convergens))+
                 s(Decade, sp=0.5, k=4)+
                 s(lat, sp=0.5)+
                 s(log(1+Coccinella.septempunctata), sp=0.5, k=4)+
                 s(log(1+Harmonia.axyridis), sp=0.5, k=4)+
                 s(Developed, sp=0.5),  
               data=lb_all2, family="nb")
summary(hcon.gam3)
AIC(hcon.gam3)

#for all the 'final' models as chosen by AIC, check the concurvity to identify variables where concurvity is 
#likely to be causing an issue in the final fit. Eliminate variables with overall estimated concurvity >0.8 
# (do this one by one and recalculate, record stats in the model selection spreadhseet)

concurvity(hcon.gam3)
#ok, can stay as is!

hcon.decade<-visreg(hcon.gam3, "Decade",  ylab="Residual captures",
                   xlab=expression(paste("Year")),
                   gg=T,
                   line=list(col="gray19"),
                   fill=list(col="gray57", fill="gray57"),
                   points=list(size=1, pch=21, fill="gray19", col="black"))+
  theme_classic()
hcon.decade

# hcon.lon<-visreg(hcon.gam3, "lon",  ylab="Residual captures",
#                  xlab=expression(paste("Longitude")), 
#                  gg=T, 
#                  line=list(col="gray28"),
#                  fill=list(col="gray", fill="gray"),
#                  points=list(size=1, pch=21, fill="gray28", col="black"))+
#   theme_classic()
# hcon.lon

hcon.lat<-visreg(hcon.gam3, "lat",  ylab="Residual captures",
                xlab=expression(paste("Latitude")),
                gg=T,
                line=list(col="gray28"),
                fill=list(col="gray", fill="gray"),
                points=list(size=1, pch=21, fill="gray28", col="black"))+
  theme_classic()
hcon.lat

# hcon.pi<-visreg(hcon.gam3, "Propinvasive",  ylab="Residual captures",
#                 xlab=expression(paste("Proportion invasive")),
#                 gg=T,
#                 line=list(col="darkmagenta"),
#                 fill=list(col="plum1", fill="plum1"),
#                 points=list(size=1, pch=22, fill="darkmagenta", col="black"))+
#   theme_classic()
# hcon.pi

hcon.c7<-visreg(hcon.gam3, "Coccinella.septempunctata",  ylab="Residual captures",
                xlab=expression(paste("Captures of ", italic("Coccinella septempunctata"))),
                gg=T,
                line=list(col="darkred"),
                fill=list(col="mistyrose", fill="mistyrose"),
                points=list(size=1, pch=22, fill="darkred", col="black"))+
  theme_classic()
hcon.c7

hcon.ha<-visreg(hcon.gam3, "Harmonia.axyridis",  ylab="Residual captures",
                xlab=expression(paste("Captures of ", italic("Harmonia axyridis"))),
                gg=T,
                line=list(col="darkorange"),
                fill=list(col="burlywood1", fill="burlywood1"),
                points=list(size=1, pch=22, fill="darkorange", col="black"))+
  theme_classic()
hcon.ha


# hcon.agriculture<-visreg(hcon.gam3, "Agriculture", ylab="Residual Captures", xlab="% Agriculture cover", 
#                          gg=T, 
#                          line=list(col="darkolivegreen4"),
#                          fill=list(col="darkolivegreen1", fill="darkolivegreen1"),
#                          points=list(size=1, pch=23, fill="darkolivegreen4", col="black"))+
#   theme_classic()
# hcon.agriculture

hcon.developed<-visreg(hcon.gam3, "Developed", ylab="Residual Captures", xlab="% Developed cover",
                         gg=T,
                         line=list(col="slategray4"),
                         fill=list(col="slategray2", fill="slategray2"),
                         points=list(size=1, pch=23, fill="slategray4", col="black"))+
  theme_classic()
hcon.developed




#create graphical objects for placeholders in plots
noeffect<-text_grob(paste("No effect"), color="black")
notpresent<-text_grob(paste("Not present"), color="black")

hcon.smooths<-plot_grid(hcon.decade, noeffect, hcon.lat, 
                        noeffect, hcon.c7, hcon.ha,
                        noeffect, noeffect, hcon.developed,
                        ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C', 
                                                              'D', 'E', 'F',
                                                              'G', 'H', 'I'))
hcon.smooths

pdf("plots/hcon_smooths.pdf", height=9, width=9)
grid.draw(hcon.smooths)
dev.off()



###########################################################################################

#scale feeding- Chilocorus stigma


#first, how many captures are we working with?
sum(lb_all2$Chilocorus.stigma)


#try model that is linear with Totalcount (minus the captures of cstig to make it independent) 
#and has a gaussian process-based spatial relationship
cstig.gam<-gam(Chilocorus.stigma~s(lon, lat, bs="gp")+
                 offset(log(1+Totalcount-Chilocorus.stigma)), 
               data=lb_all2, family="nb")
summary(cstig.gam)
AIC(cstig.gam)
b<-getViz(cstig.gam)

cstig.gam.inv<-gam(Chilocorus.stigma~s(lon, lat, by=Decade30, bs="gp")+
                     offset(log(1+Totalcount-Chilocorus.stigma)), 
                   data=lb_all2, family="nb")
summary(cstig.gam.inv)
AIC(cstig.gam.inv)
b.inv<-getViz(cstig.gam.inv)


cstigmap<- plot(b, select=1)+theme_classic()+
  xlab("Longitude")+ylab("Latitude")+ggtitle(NULL)+
  theme(aspect.ratio=1,legend.background=element_blank(), 
        legend.title = element_blank(), legend.text = element_blank(),
        plot.margin=unit(c(1, 3, 0.5, 1), "lines"))+
  annotation_custom(text_high, xmin=-79.7,xmax=-79.7,ymin=40.55,ymax=40.55)+
  annotation_custom(text_low, xmin=-79.7,xmax=-79.7,ymin=39.70,ymax=39.70)+
  annotation_custom(text_key, xmin=-79.9,xmax=-79.9,ymin=40.77,ymax=40.77)+
  coord_cartesian(clip = "off")

cstigmap
cstig.gb<-grid.grabExpr(print(cstigmap))


#make the maps for each of the invasion periods

cstigmap.inv.1<- plot(b.inv, select=1)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

cstigmap.inv.1
cstig1.gb<-grid.grabExpr(print(cstigmap.inv.1))

cstigmap.inv.2<- plot(b.inv, select=2)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

cstigmap.inv.2
cstig2.gb<-grid.grabExpr(print(cstigmap.inv.2))

cstigmap.inv.3<- plot(b.inv, select=3)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

cstigmap.inv.3
cstig3.gb<-grid.grabExpr(print(cstigmap.inv.3))

cstigmap.inv.4<- plot(b.inv, select=4)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

cstigmap.inv.4
cstig4.gb<-grid.grabExpr(print(cstigmap.inv.4))

#All right, let's put these together nicely

cstig.4<-plot_grid(cstig1.gb, cstig2.gb, cstig3.gb, cstig4.gb, ncol=2, labels=c('B', 'C', 'D', 'E'))
cstig.4

cstig.all<-plot_grid(cstig.gb, cstig.4, ncol=2, rel_widths=c(6,4), labels=c('A', NULL))
cstig.all

pdf("plots/cstig_distribution.pdf", height=5, width=11)
grid.draw(cstig.all)
dev.off()

#So let's use the sampling as our 'base model' and take out the spatial stuff because 
#it will be autocorrelated with other values
#iterative process-use AIC as selection criterion
cstig.gam1<-gam(Chilocorus.stigma~offset(log(1+Totalcount-Chilocorus.stigma))+
                  s(Decade, sp=0.5, k=4)+
                  s(lon, sp=0.5)+
                  s(lat, sp=0.5)+
                  s(log(1+Totalinvasive), sp=0.5, k=4)+
                  s(Agriculture, sp=0.5)+
                  s(Forest, sp=0.5)+
                  s(Developed, sp=0.5), 
                data=lb_all2, family="nb")
summary(cstig.gam1)
AIC(cstig.gam1)

visreg(cstig.gam1, "Decade",  ylab="Captures")
visreg(cstig.gam1, "lon",  ylab="Captures")
visreg(cstig.gam1, "lat",  ylab="Captures")
visreg(cstig.gam1, "Totalinvasive",  ylab="Captures")
visreg(cstig.gam1, "Agriculture",  ylab="Captures")
visreg(cstig.gam1, "Forest", ylab="Captures")
visreg(cstig.gam1, "Developed",  ylab="Captures")



#replace totalinvasive with two major invasives

cstig.gam2<-gam(Chilocorus.stigma~offset(log(1+Totalcount-Chilocorus.stigma))+
                  s(Decade, sp=0.5, k=4)+
                  s(lon, sp=0.5)+
                  s(lat, sp=0.5)+
                  s(Propinvasive, sp=0.5)+
                  s(log(1+Coccinella.septempunctata), sp=0.5, k=4)+
                  s(log(1+Harmonia.axyridis), sp=0.5, k=4)+
                  s(Agriculture, sp=0.5)+
                  s(Forest, sp=0.5)+
                  s(Developed, sp=0.5),  
                data=lb_all2, family="nb")
summary(cstig.gam2)
AIC(cstig.gam2)

visreg(cstig.gam2, "Decade",  ylab="Captures")
#visreg(cstig.gam2, "lon",  ylab="Captures")
visreg(cstig.gam2, "lat",  ylab="Captures")
visreg(cstig.gam2, "Propinvasive",  ylab="Captures")
#visreg(cstig.gam2, "Coccinella.septempunctata",  ylab="Captures")
#visreg(cstig.gam2, "Harmonia.axyridis",  ylab="Captures")
visreg(cstig.gam2, "Agriculture",  ylab="Captures")
visreg(cstig.gam2, "Forest", ylab="Captures")
visreg(cstig.gam2, "Developed",  ylab="Captures")


#Model selection to whittle down landscape parameters in final model (intermediate form statistics recorded in excel file):

cstig.gam3<-gam(Chilocorus.stigma~offset(log(1+Totalcount-Chilocorus.stigma))+
                  s(lon, sp=0.5)+
                  s(lat, sp=0.5)+
                  s(Propinvasive, sp=0.5)+
                  s(Forest, sp=0.5),  
                data=lb_all2, family="nb")
summary(cstig.gam3)
AIC(cstig.gam3)

#for all the 'final' models as chosen by AIC, check the concurvity to identify variables where concurvity is 
#likely to be causing an issue in the final fit. Eliminate variables with overall estimated concurvity >0.8 
# (do this one by one and recalculate, record stats in the model selection spreadhseet)

concurvity(cstig.gam3)
#ok, can stay as is!

# cstig.decade<-visreg(cstig.gam3, "Decade",  ylab="Residual captures",
#                      xlab=expression(paste("Year")),
#                      gg=T,
#                      line=list(col="gray19"),
#                      fill=list(col="gray57", fill="gray57"),
#                      points=list(size=1, pch=21, fill="gray19", col="black"))+
#   theme_classic()
# cstig.decade

cstig.lon<-visreg(cstig.gam3, "lon",  ylab="Residual captures",
                 xlab=expression(paste("Longitude")),
                 gg=T,
                 line=list(col="gray28"),
                 fill=list(col="gray", fill="gray"),
                 points=list(size=1, pch=21, fill="gray28", col="black"))+
  theme_classic()
cstig.lon

cstig.lat<-visreg(cstig.gam3, "lat",  ylab="Residual captures",
                  xlab=expression(paste("Latitude")),
                  gg=T,
                  line=list(col="gray28"),
                  fill=list(col="gray", fill="gray"),
                  points=list(size=1, pch=21, fill="gray28", col="black"))+
  theme_classic()
cstig.lat

cstig.pi<-visreg(cstig.gam3, "Propinvasive",  ylab="Residual captures",
                 xlab=expression(paste("Proportion invasive")),
                 gg=T,
                 line=list(col="darkmagenta"),
                 fill=list(col="plum1", fill="plum1"),
                 points=list(size=1, pch=22, fill="darkmagenta", col="black"))+
  theme_classic()
cstig.pi

# cstig.c7<-visreg(cstig.gam3, "Coccinella.septempunctata",  ylab="Residual captures",
#                  xlab=expression(paste("Captures of ", italic("Coccinella septempunctata"))),
#                  gg=T,
#                  line=list(col="darkred"),
#                  fill=list(col="mistyrose", fill="mistyrose"),
#                  points=list(size=1, pch=22, fill="darkred", col="black"))+
#   theme_classic()
# cstig.c7

# cstig.ha<-visreg(cstig.gam3, "Harmonia.axyridis",  ylab="Residual captures",
#                  xlab=expression(paste("Captures of ", italic("Harmonia axyridis"))),
#                  gg=T,
#                  line=list(col="darkorange"),
#                  fill=list(col="burlywood1", fill="burlywood1"),
#                  points=list(size=1, pch=22, fill="darkorange", col="black"))+
#   theme_classic()
# cstig.ha


# cstig.agriculture<-visreg(cstig.gam3, "Agriculture", ylab="Residual Captures", xlab="% Agriculture cover", 
#                          gg=T, 
#                          line=list(col="darkolivegreen4"),
#                          fill=list(col="darkolivegreen1", fill="darkolivegreen1"),
#                          points=list(size=1, pch=23, fill="darkolivegreen4", col="black"))+
#   theme_classic()
# cstig.agriculture

cstig.forest<-visreg(cstig.gam3, "Forest", ylab="Residual Captures", xlab="% Forest cover",
                        gg=T,
                        line=list(col="darkgreen"),
                        fill=list(col="lightgreen", fill="lightgreen"),
                        points=list(size=1, pch=23, fill="darkgreen", col="black"))+
  theme_classic()
cstig.forest
# cstig.developed<-visreg(cstig.gam3, "Developed", ylab="Residual Captures", xlab="% Developed cover",
#                         gg=T,
#                         line=list(col="slategray4"),
#                         fill=list(col="slategray2", fill="slategray2"),
#                         points=list(size=1, pch=23, fill="slategray4", col="black"))+
#   theme_classic()
# cstig.developed



#create graphical objects for placeholders in plots
noeffect<-text_grob(paste("No effect"), color="black")
notpresent<-text_grob(paste("Not present"), color="black")

cstig.smooths<-plot_grid(noeffect, cstig.lon, cstig.lat, 
                         cstig.pi, noeffect, noeffect,
                         cstig.forest, noeffect, noeffect,
                         ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C', 
                                                               'D', 'E', 'F',
                                                               'G', 'H', 'I'))
cstig.smooths

pdf("plots/cstig_smooths.pdf", height=9, width=9)
grid.draw(cstig.smooths)
dev.off()



###########################################################################################
#non-native species 
###########################################################################################

#Coccinella septempunctata

#first, how many captures are we working with?
sum(lb_all2$Coccinella.septempunctata)

#this species arrived in the 1970s but only 4 were caught before 1980, so let’s use data starting at 1980 
after1980<-lb_all2[which(lb_all2$Decade>=1980),]

#try model that is linear with Totalcount (minus the captures of C.7 to make it independent) 
#and has a gaussian process-based spatial relationship
c7.gam<-gam(Coccinella.septempunctata~s(lon, lat, bs="gp")+
              offset(log(1+Totalcount-Coccinella.septempunctata)), 
            data=after1980, family="nb")
summary(c7.gam)
AIC(c7.gam)
b<-getViz(c7.gam)

c7.gam.inv<-gam(Coccinella.septempunctata~s(lon, lat, by=Decade30, bs="gp")+
                  offset(log(1+Totalcount-Coccinella.septempunctata)), 
                data=after1980, family="nb")
summary(c7.gam.inv)
AIC(c7.gam.inv)
b.inv<-getViz(c7.gam.inv)

text_high<-textGrob("Highest")
text_low<-textGrob("Lowest")
text_key<-textGrob("Predicted captures")

c7map<- plot(b, select=1)+theme_classic()+
  xlab("Longitude")+ylab("Latitude")+ggtitle(NULL)+
  theme(aspect.ratio=1,legend.background=element_blank(), 
        legend.title = element_blank(), legend.text = element_blank(),
        plot.margin=unit(c(1, 3, 0.5, 1), "lines"))+
  annotation_custom(text_high, xmin=-79.7,xmax=-79.7,ymin=40.55,ymax=40.55)+
  annotation_custom(text_low, xmin=-79.7,xmax=-79.7,ymin=39.70,ymax=39.70)+
  annotation_custom(text_key, xmin=-79.9,xmax=-79.9,ymin=40.77,ymax=40.77)+
  coord_cartesian(clip = "off")

c7map
c7.gb<-grid.grabExpr(print(c7map))


#make the maps for each of the invasion periods (only around in 3 and 4)

c7map.inv.3<- plot(b.inv, select=1)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

c7map.inv.3
c73.gb<-grid.grabExpr(print(c7map.inv.3))

c7map.inv.4<- plot(b.inv, select=2)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

c7map.inv.4
c74.gb<-grid.grabExpr(print(c7map.inv.4))



#All right, let's put these together nicely

c7.4<-plot_grid(notpresent,notpresent,c73.gb,c74.gb, ncol=2, labels=c('B', 'C', 'D', 'E'))
c7.4

c7.all<-plot_grid(c7.gb, c7.4, ncol=2, rel_widths=c(6,4), labels=c('A', NULL))
c7.all

pdf("plots/c7_distribution.pdf", height=5, width=11)
grid.draw(c7.all)
dev.off()

#So let's use the sampling as our 'base model' and take out the spatial stuff because 
#it will be autocorrelated with other values
#iterative process-use AIC as selection criterion
#So let's use the sampling as our 'base model' and take out the spatial stuff because 
#it will be autocorrelated with other values
#iterative process-use AIC as selection criterion
c7.gam1<-gam(Coccinella.septempunctata~offset(log(1+Totalcount-Coccinella.septempunctata))+
               s(Decade, sp=0.5, k=4)+
               s(lon, sp=0.5)+
               s(lat, sp=0.5)+
               s(log(1+Aphidophagous-Coccinella.septempunctata), sp=0.5, k=4)+
               s(Agriculture, sp=0.5)+
               s(Forest, sp=0.5)+
               s(Developed, sp=0.5), 
             data=after1980, family="nb")
summary(c7.gam1)
AIC(c7.gam1)

visreg(c7.gam1, "Decade",  ylab="Captures")
visreg(c7.gam1, "lon",  ylab="Captures")
visreg(c7.gam1, "lat",  ylab="Captures")
visreg(c7.gam1, "Aphidophagous",  ylab="Captures")
visreg(c7.gam1, "Agriculture",  ylab="Captures")
visreg(c7.gam1, "Forest", ylab="Captures")
visreg(c7.gam1, "Developed",  ylab="Captures")


#replace totalinvasive with two major invasives

c7.gam2<-gam(Coccinella.septempunctata~offset(log(1+Totalcount-Coccinella.septempunctata))+
               s(Decade, sp=0.5, k=4)+
               s(lon, sp=0.5)+
               s(lat, sp=0.5)+
               s(log(1+Aphidophagous-Coccinella.septempunctata), sp=0.5, k=4)+
               s(log(1+Harmonia.axyridis), sp=0.5, k=4)+
               s(Agriculture, sp=0.5)+
               s(Forest, sp=0.5)+
               s(Developed, sp=0.5),  
             data=after1980, family="nb")
summary(c7.gam2)
AIC(c7.gam2)

visreg(c7.gam2, "Decade",  ylab="Captures")
#visreg(c7.gam2, "lon",  ylab="Captures")
visreg(c7.gam2, "lat",  ylab="Captures")
visreg(c7.gam2, "Propinvasive",  ylab="Captures")
#visreg(c7.gam2, "Coccinella.septempunctata",  ylab="Captures")
#visreg(c7.gam2, "Harmonia.axyridis",  ylab="Captures")
visreg(c7.gam2, "Agriculture",  ylab="Captures")
visreg(c7.gam2, "Forest", ylab="Captures")
visreg(c7.gam2, "Developed",  ylab="Captures")


#Model selection to whittle down landscape parameters in final model (intermediate form statistics recorded in excel file):

c7.gam3<-gam(Coccinella.septempunctata~offset(log(1+Totalcount-Coccinella.septempunctata))+
               s(Agriculture, sp=0.5),  
             data=after1980, family="nb")
summary(c7.gam3)
AIC(c7.gam3)

#for all the 'final' models as chosen by AIC, check the concurvity to identify variables where concurvity is 
#likely to be causing an issue in the final fit. Eliminate variables with overall estimated concurvity >0.8 
# (do this one by one and recalculate, record stats in the model selection spreadhseet)

concurvity(c7.gam3)
#must eliminate a few of the lanscape variables!

# c7.decade<-visreg(c7.gam3, "Decade",  ylab="Residual captures",
#                   xlab=expression(paste("Year")),
#                   gg=T,
#                   line=list(col="gray19"),
#                   fill=list(col="gray57", fill="gray57"),
#                   points=list(size=1, pch=21, fill="gray19", col="black"))+
#   theme_classic()
# c7.decade
# 
# c7.lon<-visreg(c7.gam3, "lon",  ylab="Residual captures",
#                  xlab=expression(paste("Longitude")),
#                  gg=T,
#                  line=list(col="gray28"),
#                  fill=list(col="gray", fill="gray"),
#                  points=list(size=1, pch=21, fill="gray28", col="black"))+
#   theme_classic()
# c7.lon
# 
# c7.lat<-visreg(c7.gam3, "lat",  ylab="Residual captures",
#                xlab=expression(paste("Latitude")),
#                gg=T,
#                line=list(col="gray28"),
#                fill=list(col="gray", fill="gray"),
#                points=list(size=1, pch=21, fill="gray28", col="black"))+
#   theme_classic()
# c7.lat

# c7.pi<-visreg(c7.gam3, "Propinvasive",  ylab="Residual captures",
#               xlab=expression(paste("Proportion invasive")),
#               gg=T,
#               line=list(col="darkmagenta"),
#               fill=list(col="plum1", fill="plum1"),
#               points=list(size=1, pch=22, fill="darkmagenta", col="black"))+
#   theme_classic()
# c7.pi

# c7.c7<-visreg(c7.gam3, "Coccinella.septempunctata",  ylab="Residual captures",
#               xlab=expression(paste("Captures of ", italic("Coccinella septempunctata"))),
#               gg=T,
#               line=list(col="darkred"),
#               fill=list(col="mistyrose", fill="mistyrose"),
#               points=list(size=1, pch=22, fill="darkred", col="black"))+
#   theme_classic()
# c7.c7

# c7.ha<-visreg(c7.gam3, "Harmonia.axyridis",  ylab="Residual captures",
#               xlab=expression(paste("Captures of ", italic("Harmonia axyridis"))),
#               gg=T,
#               line=list(col="darkorange"),
#               fill=list(col="burlywood1", fill="burlywood1"),
#               points=list(size=1, pch=22, fill="darkorange", col="black"))+
#   theme_classic()
# c7.ha


c7.agriculture<-visreg(c7.gam3, "Agriculture", ylab="Residual Captures", xlab="% Agriculture cover",
                         gg=T,
                         line=list(col="darkolivegreen4"),
                         fill=list(col="darkolivegreen1", fill="darkolivegreen1"),
                         points=list(size=1, pch=23, fill="darkolivegreen4", col="black"))+
  theme_classic()
c7.agriculture

# c7.forest<-visreg(c7.gam3, "Forest", ylab="Residual Captures", xlab="% Forest cover",
#                      gg=T,
#                      line=list(col="darkgreen"),
#                      fill=list(col="lightgreen", fill="lightgreen"),
#                      points=list(size=1, pch=23, fill="darkgreen", col="black"))+
#   theme_classic()
# c7.forest

# c7.developed<-visreg(c7.gam3, "Developed", ylab="Residual Captures", xlab="% Developed cover",
#                      gg=T,
#                      line=list(col="slategray4"),
#                      fill=list(col="slategray2", fill="slategray2"),
#                      points=list(size=1, pch=23, fill="slategray4", col="black"))+
#   theme_classic()
# c7.developed



#create graphical objects for placeholders in plots
noeffect<-text_grob(paste("No effect"), color="black")
notpresent<-text_grob(paste("Not present"), color="black")

c7.smooths<-plot_grid(noeffect, noeffect, noeffect, 
                      noeffect, noeffect, noeffect,
                      c7.agriculture, noeffect, noeffect,
                      ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C', 
                                                            'D', 'E', 'F',
                                                            'G', 'H', 'I'))
c7.smooths

pdf("plots/c7_smooths.pdf", height=9, width=9)
grid.draw(c7.smooths)
dev.off()



###########################################################################################
#Harmonia axyridis


#first, how many captures are we working with?
sum(lb_all2$Harmonia.axyridis)

#this species arrived in the 1990s 
after1990<-lb_all2[which(lb_all2$Decade>=1990),]

#try model that is linear with Totalcount (minus the captures of C.7 to make it independent) 
#and has a gaussian process-based spatial relationship
ha.gam<-gam(Harmonia.axyridis~s(lon, lat, bs="gp")+
              offset(log(1+Totalcount-Harmonia.axyridis)), 
            data=after1990, family="nb")
summary(ha.gam)
AIC(ha.gam)
b<-getViz(ha.gam)

ha.gam.inv<-gam(Harmonia.axyridis~s(lon, lat, by=Decade30, bs="gp")+
                  offset(log(1+Totalcount-Harmonia.axyridis)), 
                data=after1990, family="nb")
summary(ha.gam.inv)
AIC(ha.gam.inv)
b.inv<-getViz(ha.gam.inv)

text_high<-textGrob("Highest")
text_low<-textGrob("Lowest")
text_key<-textGrob("Predicted captures")

hamap<- plot(b, select=1)+theme_classic()+
  xlab("Longitude")+ylab("Latitude")+ggtitle(NULL)+
  theme(aspect.ratio=1,legend.background=element_blank(), 
        legend.title = element_blank(), legend.text = element_blank(),
        plot.margin=unit(c(1, 3, 0.5, 1), "lines"))+
  annotation_custom(text_high, xmin=-79.7,xmax=-79.7,ymin=40.55,ymax=40.55)+
  annotation_custom(text_low, xmin=-79.7,xmax=-79.7,ymin=39.70,ymax=39.70)+
  annotation_custom(text_key, xmin=-79.9,xmax=-79.9,ymin=40.77,ymax=40.77)+
  coord_cartesian(clip = "off")

hamap
ha.gb<-grid.grabExpr(print(hamap))


#make the maps for each of the invasion periods (only around in 3 and 4)

hamap.inv.3<- plot(b.inv, select=1)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

hamap.inv.3
ha3.gb<-grid.grabExpr(print(hamap.inv.3))

hamap.inv.4<- plot(b.inv, select=2)+theme_classic()+
  xlab(NULL)+ylab(NULL)+ggtitle(NULL)+
  theme(legend.position="none", aspect.ratio=1)

hamap.inv.4
ha4.gb<-grid.grabExpr(print(hamap.inv.4))



#All right, let's put these together nicely

ha.4<-plot_grid(notpresent,notpresent,ha3.gb,ha4.gb, ncol=2, labels=c('B', 'C', 'D', 'E'))
ha.4

ha.all<-plot_grid(ha.gb, ha.4, ncol=2, rel_widths=c(6,4), labels=c('A', NULL))
ha.all

pdf("plots/ha_distribution.pdf", height=5, width=11)
grid.draw(ha.all)
dev.off()

#So let's use the sampling as our 'base model' and take out the spatial stuff because 
#it will be autocorrelated with other values
#iterative process-use AIC as selection criterion
#So let's use the sampling as our 'base model' and take out the spatial stuff because 
#it will be autocorrelated with other values
#iterative process-use AIC as selection criterion
ha.gam1<-gam(Harmonia.axyridis~offset(log(1+Totalcount-Harmonia.axyridis))+
               s(Decade, sp=0.5, k=3)+
               s(lon, sp=0.5)+
               s(lat, sp=0.5)+
               s(log(1+Aphidophagous-Harmonia.axyridis), sp=0.5, k=4)+
               s(Agriculture, sp=0.5)+
               s(Forest, sp=0.5)+
               s(Developed, sp=0.5), 
             data=after1990, family="nb")
summary(ha.gam1)
AIC(ha.gam1)

visreg(ha.gam1, "Decade",  ylab="Captures")
visreg(ha.gam1, "lon",  ylab="Captures")
visreg(ha.gam1, "lat",  ylab="Captures")
visreg(ha.gam1, "Aphidophagous",  ylab="Captures")
visreg(ha.gam1, "Agriculture",  ylab="Captures")
visreg(ha.gam1, "Forest", ylab="Captures")
visreg(ha.gam1, "Developed",  ylab="Captures")



#replace totalinvasive with two major invasives

ha.gam2<-gam(Harmonia.axyridis~offset(log(1+Totalcount-Harmonia.axyridis))+
               s(Decade, sp=0.5, k=3)+
               s(lon, sp=0.5)+
               s(lat, sp=0.5)+
               s(log(1+Aphidophagous-Harmonia.axyridis), sp=0.5, k=4)+
               s(log(1+Coccinella.septempunctata), sp=0.5, k=4)+
               s(Agriculture, sp=0.5)+
               s(Forest, sp=0.5)+
               s(Developed, sp=0.5),  
             data=after1990, family="nb")
summary(ha.gam2)
AIC(ha.gam2)

visreg(ha.gam2, "Decade",  ylab="Captures")
#visreg(ha.gam2, "lon",  ylab="Captures")
visreg(ha.gam2, "lat",  ylab="Captures")
visreg(ha.gam2, "Propinvasive",  ylab="Captures")
#visreg(ha.gam2, "Harmonia.axyridis",  ylab="Captures")
#visreg(ha.gam2, "Harmonia.axyridis",  ylab="Captures")
visreg(ha.gam2, "Agriculture",  ylab="Captures")
visreg(ha.gam2, "Forest", ylab="Captures")
visreg(ha.gam2, "Developed",  ylab="Captures")



#Model selection to whittle down landscape parameters in final model (intermediate form statistics recorded in excel file):

ha.gam3<-gam(Harmonia.axyridis~offset(log(1+Totalcount-Harmonia.axyridis))+
               s(Decade, sp=0.5, k=3)+
               s(lat, sp=0.5)+
               s(log(1+Aphidophagous-Harmonia.axyridis), sp=0.5, k=4)+
               s(Forest, sp=0.5)+
               s(Developed, sp=0.5),  
             data=after1990, family="nb")
summary(ha.gam3)
AIC(ha.gam3)

#for all the 'final' models as chosen by AIC, check the concurvity to identify variables where concurvity is 
#likely to be causing an issue in the final fit. Eliminate variables with overall estimated concurvity >0.8 
# (do this one by one and recalculate, record stats in the model selection spreadhseet)

concurvity(ha.gam3)
#needs a bit of tweaking


ha.decade<-visreg(ha.gam3, "Decade",  ylab="Residual captures",
                  xlab=expression(paste("Year")),
                  gg=T,
                  line=list(col="gray19"),
                  fill=list(col="gray57", fill="gray57"),
                  points=list(size=1, pch=21, fill="gray19", col="black"))+
  theme_classic()
ha.decade

# ha.lon<-visreg(ha.gam3, "lon",  ylab="Residual captures",
#                xlab=expression(paste("Longitude")),
#                gg=T,
#                line=list(col="gray28"),
#                fill=list(col="gray", fill="gray"),
#                points=list(size=1, pch=21, fill="gray28", col="black"))+
#   theme_classic()
# ha.lon

ha.lat<-visreg(ha.gam3, "lat",  ylab="Residual captures",
               xlab=expression(paste("Latitude")),
               gg=T,
               line=list(col="gray28"),
               fill=list(col="gray", fill="gray"),
               points=list(size=1, pch=21, fill="gray28", col="black"))+
  theme_classic()
ha.lat

ha.pi<-visreg(ha.gam3, "Aphidophagous",  ylab="Residual captures",
              xlab=expression(paste("Other Aphidophagous")),
              gg=T,
              line=list(col="maroon4"),
              fill=list(col="palevioletred", fill="palevioletred"),
              points=list(size=1, pch=22, fill="maroon4", col="black"))+
  theme_classic()
ha.pi

# ha.ha<-visreg(ha.gam3, "Harmonia.axyridis",  ylab="Residual captures",
#               xlab=expression(paste("Captures of ", italic("Coccinella septempunctata"))),
#               gg=T,
#               line=list(col="darkred"),
#               fill=list(col="mistyrose", fill="mistyrose"),
#               points=list(size=1, pch=22, fill="darkred", col="black"))+
#   theme_classic()
# ha.ha

# ha.ha<-visreg(ha.gam3, "Harmonia.axyridis",  ylab="Residual captures",
#               xlab=expression(paste("Captures of ", italic("Harmonia axyridis"))),
#               gg=T,
#               line=list(col="darkorange"),
#               fill=list(col="burlywood1", fill="burlywood1"),
#               points=list(size=1, pch=22, fill="darkorange", col="black"))+
#   theme_classic()
# ha.ha


# ha.agriculture<-visreg(ha.gam3, "Agriculture", ylab="Residual Captures", xlab="% Agriculture cover",
#                        gg=T,
#                        line=list(col="darkolivegreen4"),
#                        fill=list(col="darkolivegreen1", fill="darkolivegreen1"),
#                        points=list(size=1, pch=23, fill="darkolivegreen4", col="black"))+
#   theme_classic()
# ha.agriculture

ha.forest<-visreg(ha.gam3, "Forest", ylab="Residual Captures", xlab="% Forest cover",
                  gg=T,
                  line=list(col="darkgreen"),
                  fill=list(col="lightgreen", fill="lightgreen"),
                  points=list(size=1, pch=23, fill="darkgreen", col="black"))+
  theme_classic()
ha.forest

ha.developed<-visreg(ha.gam3, "Developed", ylab="Residual Captures", xlab="% Developed cover",
                     gg=T,
                     line=list(col="slategray4"),
                     fill=list(col="slategray2", fill="slategray2"),
                     points=list(size=1, pch=23, fill="slategray4", col="black"))+
  theme_classic()
ha.developed




#create graphical objects for placeholders in plots
noeffect<-text_grob(paste("No effect"), color="black")
notpresent<-text_grob(paste("Not present"), color="black")

ha.smooths<-plot_grid(ha.decade, noeffect, ha.lat, 
                      ha.pi, noeffect, noeffect,
                      noeffect, ha.forest, ha.developed,
                      ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C', 
                                                            'D', 'E', 'F',
                                                            'G', 'H', 'I'))
ha.smooths

pdf("plots/ha_smooths.pdf", height=12, width=9)
grid.draw(ha.smooths)
dev.off()
