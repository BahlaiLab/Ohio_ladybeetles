#run ohio_context_data_processing.R and LB_spatial_analysis first

#need to reshape LULC object in wide format, but with the original year timepoints preserved
library(reshape2)

LULC.raw.wide<-dcast(LULC, Year+County~Class, value.var="Percentage")

#let's also use the raw ladybeetle collections for this as well- first need to create a classifier 
#so we can map these onto the timepoints we have solid landcover data for

#first just cull out data after 2016
lb_raw<-lb_raw[which(lb_raw$Year<2017),]

lb_raw$YearGroup<-cut(lb_raw$Year, breaks=c(-Inf, 1938, 1970, 1992,  Inf), labels=c("1938","1970","1992","2016"))


lb.wide.yeargroups<-dcast(lb_raw, YearGroup+County~Name, length)

#gotta cull out county/groups with insufficient observations to NMDS

cutpoint<-rowSums(lb.wide.yeargroups[3:30])
lb.wide.yeargroups<-lb.wide.yeargroups[which(cutpoint>2), ]



lb.wide.yeargroups.natives<-lb.wide.yeargroups[,-c(11,14,17,23,29)]
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
text(landscape.ord, display="species", col="red", pch=1)
ordiellipse(landscape.ord, LULC.raw.wide$Year, draw="polygon", col=c("pink", "orange", "green", "blue"), kind="se", conf=0.999, label=TRUE)

lb.ord<-metaMDS(lb.wide.yeargroups[3:30])
lb.ord

plot(lb.ord, disp='sites', type="n")
points(lb.ord, display="sites", select=which(lb.wide.yeargroups$YearGroup=="1938"), pch=19, col="pink")
points(lb.ord, display="sites", select=which(lb.wide.yeargroups$YearGroup=="1970"), pch=19, col="orange")
points(lb.ord, display="sites", select=which(lb.wide.yeargroups$YearGroup=="1992"), pch=19, col="green")
points(lb.ord, display="sites", select=which(lb.wide.yeargroups$YearGroup=="2016"), pch=19, col="blue")
ordiellipse(lb.ord, lb.wide.yeargroups$YearGroup, draw="polygon", col=c("pink", "orange", "green", "blue"), kind="se", conf=0.999, label=TRUE)

lb.ord.n<-metaMDS(lb.wide.yeargroups.natives[3:25])
lb.ord.n

plot(lb.ord.n, disp='sites', type="n")
points(lb.ord.n, display="sites", select=which(lb.wide.yeargroups.natives$YearGroup=="1938"), pch=19, col="pink")
points(lb.ord.n, display="sites", select=which(lb.wide.yeargroups.natives$YearGroup=="1970"), pch=19, col="orange")
points(lb.ord.n, display="sites", select=which(lb.wide.yeargroups.natives$YearGroup=="1992"), pch=19, col="green")
points(lb.ord.n, display="sites", select=which(lb.wide.yeargroups.natives$YearGroup=="2016"), pch=19, col="blue")
ordiellipse(lb.ord.n, lb.wide.yeargroups.natives$YearGroup, draw="polygon", col=c("pink", "orange", "green", "blue"), kind="se", conf=0.999, label=TRUE)


#ok let's make this all prettier with a GGplot multipanel figure
library(ggplot2)

#install.packages("remotes")
#remotes::install_github("jfq3/ggordiplots")
library(ggordiplots)

#start with landscape data

landscape.centroids<-as.data.frame(scores(landscape.ord, "species"))
landscape.centroids$landuse <- rownames(landscape.centroids)


fudgexl<-c(0.1, -0.04, 0.15, 0.16)#jitter the vector labels a bit
fudgeyl<-c(-0.04, -0.09, 0.075, 0.03)

landnmds<-gg_ordiplot(landscape.ord, groups= LULC.raw.wide$Year, kind="se", conf=0.99, pt.size=-1, plot=F)

gglandnmds<-landnmds$plot+  
  geom_point(data=landnmds$df_ord, aes(x=x,y=y, color=Group, shape=Group))+
  scale_colour_manual(values=c("1938" = "darkred", "1970" = "darkorange", "1992"= "yellow3", "2016"="darkgreen"), 
                      labels=c("1938", "1970", "1992", "2016"), title("Year"))+
  scale_fill_manual(values=c("1938" = "darkred", "1970" = "darkorange", "1992"= "yellow3", "2016"="darkgreen"), 
                      labels=c("1938", "1970", "1992", "2016"), title("Year"))+
  scale_shape_manual(values=c("1938" = 1, "1970" = 2, "1992"= 3, "2016"=4), 
                     labels=c("1938", "1970", "1992", "2016"), title("Year"))+
  geom_polygon(data = landnmds$df_ellipse, aes(x = x, y = y,  fill=Group), show.legend = FALSE, color="black", alpha =0.25)+
  geom_label(data = landnmds$df_mean.ord, aes(x = x+fudgexl, y = y+fudgeyl, label = Group, color = Group), 
             show.legend = FALSE, fill="white", color="black", alpha=0.9)+
  geom_path(data = landnmds$df_mean.ord, aes(x = x, y = y), 
             show.legend = FALSE, color="black")+
  geom_point(data = landnmds$df_mean.ord, aes(x = x, y = y), 
            show.legend = FALSE, color="black", pch=16)+
  geom_label(data = landscape.centroids, aes(x = NMDS1, y = NMDS2, label = landuse), 
             show.legend = FALSE, color="black", size=3, alpha=0.1, label.size = NA)+
  theme_classic()+
  #theme(aspect.ratio = 0.9)+
  labs(x="NMDS1", y="NMDS2")

gglandnmds
gglandnmds.noleg<-gglandnmds+theme(legend.position ="none")


#all lbs


fudgexa<-c(0.2,-0.1,0,0)#jitter the vector labels a bit
fudgeya<-c(-0.3,-0.32,0.28,-0.23)

lbnmds<-gg_ordiplot(lb.ord, groups=lb.wide.yeargroups$YearGroup, kind="se", conf=0.99, pt.size=-1, plot=F)

gglbnmds<-lbnmds$plot+  
  geom_point(data=lbnmds$df_ord, aes(x=x,y=y, color=Group, shape=Group))+
  scale_colour_manual(values=c("1938" = "darkred", "1970" = "darkorange", "1992"= "yellow3", "2016"="darkgreen"), 
                      labels=c("Before 1938", "1939-1970", "1971-1992", "1993-2016"), title("Year"))+
  scale_fill_manual(values=c("1938" = "darkred", "1970" = "darkorange", "1992"= "yellow3", "2016"="darkgreen"), 
                    labels=c("1938", "1970", "1992", "2016"), title("Year"))+
  scale_shape_manual(values=c("1938" = 1, "1970" = 2, "1992"= 3, "2016"=4), 
                     labels=c("Before 1938", "1939-1970", "1971-1992", "1993-2016"), title("Year"))+
  geom_polygon(data = lbnmds$df_ellipse, aes(x = x, y = y,  fill=Group), show.legend = FALSE, color="black", alpha =0.25)+
  #geom_label(data = lbnmds$df_mean.ord, aes(x = x+fudgexa, y = y+fudgeya, label = Group, color = Group), 
            # show.legend = FALSE, fill="white", color="black", alpha=0.9)+
  geom_path(data = lbnmds$df_mean.ord, aes(x = x, y = y), 
            show.legend = FALSE, color="black")+
  geom_point(data = lbnmds$df_mean.ord, aes(x = x, y = y), 
             show.legend = FALSE, color="black", pch=16)+
  theme_classic()+
  theme(aspect.ratio = 0.9)+
  labs(x="NMDS1", y="NMDS2")

gglbnmds
gglbnmds.noleg<-gglbnmds+theme(legend.position ="none")


#native lbs only


fudgexn<-c(0.25,-0.1,0,0.1)#jitter the vector labels a bit
fudgeyn<-c(-0.24,-0.32,0.28,-0.25)

lbnnmds<-gg_ordiplot(lb.ord.n, groups=lb.wide.yeargroups.natives$YearGroup, kind="se", conf=0.99, pt.size=-1, plot=F)

gglbnnmds<-lbnnmds$plot+  
  geom_point(data=lbnnmds$df_ord, aes(x=x,y=y, color=Group, shape=Group))+
  scale_colour_manual(values=c("1938" = "darkred", "1970" = "darkorange", "1992"= "yellow3", "2016"="darkgreen"), 
                      labels=c("Before 1938", "1939-1970", "1971-1992", "1993-2016"), title("Year"))+
  scale_fill_manual(values=c("1938" = "darkred", "1970" = "darkorange", "1992"= "yellow3", "2016"="darkgreen"), 
                    labels=c("1938", "1970", "1992", "2016"), title("Year"))+
  scale_shape_manual(values=c("1938" = 1, "1970" = 2, "1992"= 3, "2016"=4), 
                     labels=c("Before 1938", "1939-1970", "1971-1992", "1993-2016"), title("Year"))+
  geom_polygon(data = lbnnmds$df_ellipse, aes(x = x, y = y,  fill=Group), show.legend = FALSE, color="black", alpha =0.25)+
  #geom_label(data = lbnnmds$df_mean.ord, aes(x = x+fudgexn, y = y+fudgeyn, label = Group, color = Group), 
            # show.legend = FALSE, fill="white", color="black", alpha=0.9)+
  geom_path(data = lbnnmds$df_mean.ord, aes(x = x, y = y), 
            show.legend = FALSE, color="black")+
  geom_point(data = lbnnmds$df_mean.ord, aes(x = x, y = y), 
             show.legend = FALSE, color="black", pch=16)+
  theme_classic()+
  theme(aspect.ratio = 0.9)+
  labs(x="NMDS1", y="NMDS2")

gglbnnmds
gglbnnmds.noleg<-gglbnnmds+theme(legend.position ="none")

#pull legend so it can be its own grob

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

leg1<-g_legend(gglbnnmds)
leg2<-g_legend(gglandnmds)

library(ggpubr)
blankspace<-text_grob(paste(""), color="black")

#aiight. stackin' time

library(cowplot)


nmds.land<-plot_grid(gglandnmds.noleg, leg2, blankspace,
              ncol=3, rel_widths=c(2, 0.5, 0.1), labels=c('A', '', ''), align="h", axis="b")

nmds.land

nmds.beetles<-plot_grid(gglbnmds.noleg,gglbnnmds.noleg, leg1,
                        ncol=3, rel_widths=c(1, 1, 0.5), labels=c('B', 'C', ''), align="h", axis="b")
nmds.beetles



nmds.all<-plot_grid(nmds.land, nmds.beetles, ncol=1, rel_heights = c(1.3, 1), labels=c('A', ''), align="v", axis="btl")

nmds.all

pdf("plots/NMDS_timepoints.pdf", height=7, width=6)
grid.draw(nmds.all)
dev.off()