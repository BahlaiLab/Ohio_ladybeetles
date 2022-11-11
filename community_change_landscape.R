#run ohio_context_data_processing.R and LB_spatial_analysis first

#need to reshape LULC object in wide format, but with the original year timepoints preserved
library(reshape2)

LULC.raw.wide<-dcast(LULC, Year+County~Class, value.var="Percentage")

#let's also use the raw ladybeetle collections for this as well- first need to create a classifier 
#so we can map these onto the timepoints we have solid landcover data for

lb_raw$YearGroup<-cut(lb_raw$Year, breaks=c(-Inf, 1938, 1970, 1992, 2016, Inf), labels=c("Before 1938","1939-1970","1971-1992","1993-2016","After 2017"))


lb.wide.yeargroups<-dcast(lb_raw, YearGroup+County~Name, length)

#gotta cull out county/groups with insufficient observations to NMDS

cutpoint<-rowSums(lb.wide.yeargroups[3:30])
lb.wide.yeargroups<-lb.wide.yeargroups[which(cutpoint>2), ]
#also very little data after 2017 so let's just cut that
lb.wide.yeargroups<-lb.wide.yeargroups[which(lb.wide.yeargroups$YearGroup!="After 2017"), ]

lb.wide.yeargroups.natives<-lb.wide.yeargroups[,-c(10,13,16,22,29)]
#gotta cull out county/groups with insufficient observations to NMDS again!

cutpoint1<-rowSums(lb.wide.yeargroups.natives[3:25])
lb.wide.yeargroups.natives<-lb.wide.yeargroups.natives[which(cutpoint1>2), ]


library(vegan)

landscape.ord<-metaMDS(LULC.raw.wide[3:6])
landscape.ord

plot(landscape.ord, disp='sites', type="n")
points(landscape.ord, display="sites", select=which(LULC.raw.wide$Year=="1938"), pch=19, col="pink")
points(landscape.ord, display="sites", select=which(LULC.raw.wide$Year=="1970"), pch=19, col="orange")
points(landscape.ord, display="sites", select=which(LULC.raw.wide$Year=="1992"), pch=19, col="green")
points(landscape.ord, display="sites", select=which(LULC.raw.wide$Year=="2016"), pch=19, col="blue")
ordiellipse(landscape.ord, LULC.raw.wide$Year, draw="polygon", col=c("pink", "orange", "green", "blue"), kind="se", conf=0.999, label=TRUE)

lb.ord<-metaMDS(lb.wide.yeargroups[3:30])
lb.ord

plot(lb.ord, disp='sites', type="n")
points(lb.ord, display="sites", select=which(lb.wide.yeargroups$YearGroup=="Before 1938"), pch=19, col="pink")
points(lb.ord, display="sites", select=which(lb.wide.yeargroups$YearGroup=="1939-1970"), pch=19, col="orange")
points(lb.ord, display="sites", select=which(lb.wide.yeargroups$YearGroup=="1971-1992"), pch=19, col="green")
points(lb.ord, display="sites", select=which(lb.wide.yeargroups$YearGroup=="1993-2016"), pch=19, col="blue")
ordiellipse(lb.ord, lb.wide.yeargroups$YearGroup, draw="polygon", col=c("pink", "orange", "green", "blue"), kind="se", conf=0.999, label=TRUE)

lb.ord.n<-metaMDS(lb.wide.yeargroups.natives[3:25])
lb.ord.n

plot(lb.ord.n, disp='sites', type="n")
points(lb.ord.n, display="sites", select=which(lb.wide.yeargroups.natives$YearGroup=="Before 1938"), pch=19, col="pink")
points(lb.ord.n, display="sites", select=which(lb.wide.yeargroups.natives$YearGroup=="1939-1970"), pch=19, col="orange")
points(lb.ord.n, display="sites", select=which(lb.wide.yeargroups.natives$YearGroup=="1971-1992"), pch=19, col="green")
points(lb.ord.n, display="sites", select=which(lb.wide.yeargroups.natives$YearGroup=="1993-2016"), pch=19, col="blue")
ordiellipse(lb.ord.n, lb.wide.yeargroups.natives$YearGroup, draw="polygon", col=c("pink", "orange", "green", "blue"), kind="se", conf=0.999, label=TRUE)

