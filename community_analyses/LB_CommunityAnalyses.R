###################################################################################
#
# Lady beetle museum data
# Restructure data set
# Conduct NMDS analyses to assess changes in community composition
# by geographic regions, 30 year intervals, and pre-post LB invasion
# Assess beta-diversity over timeline and deconstruct beta-diversity into
# nestedeness and turnover components
#
###################################################################################

setwd("~/The Ohio State University/PostDoc/BLBB Project/Museum Files/Ohio_ladybeetles/community_analyses")

lb <- read.csv("LB_MuseumData_2020.csv")

colnames(lb)

library(reshape2)

library(vegan)

#install.packages("devtools")
#library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

library(plyr)

# Look at species by decade and by LB invasion timeline

lb2 <- lb[,c(2,14,16,17,15)]

colnames(lb2)
summary(lb2)

lb2.matrix <- dcast(lb2, Year + Invasion + Decade + Decade30 ~ Name, length)
#write.csv(lb.matrix, file = "LB2.csv")

str(lb2.matrix)

# change decade to a factor
lb2.matrix$Decade <- as.factor(lb2.matrix$Decade)
str(lb2.matrix)

# run NMDS model for decade

rankindex(lb2.matrix$Decade, lb2.matrix[5:35])
nmds.yr <- metaMDS(lb2.matrix[5:35], distance = "bray", trymax = 1000, autotransform = TRUE)
nmds.yr
plot(nmds.yr)
stressplot(nmds.yr)
nmds.yr$points
nmds.yr$stress

# calculate distance matrix for LB species

dist.lb.yr <- vegdist(lb2.matrix[5:35], method = "bray")

# test for differences in LB species composition across 30 year period
adonis(dist.lb.yr ~ as.factor(lb2.matrix$Decade30), permutations = 999)
pairwise.adonis(dist.lb.yr, lb2.matrix$Decade30)

# quick plot to visualize results
colors()
levels(lb2.matrix$Decade30)
#png("LB_30YearIntervals.png", width = 1000, height = 800, pointsize = 20)
ordiplot(nmds.yr, type="n", xlim = c(-3, 3), ylim = c(-1.5, 1.5))
points(nmds.yr, dis="sites", pch = 19, cex=1.5, 
       col=c("darkorange1", "darkgoldenrod1", "black", "chartreuse4", "royalblue3")[as.numeric(lb2.matrix$Decade30)])
ordiellipse(nmds.yr, groups=lb2.matrix$Decade30, display="sites", draw="lines", conf=0.90)
legend("topleft", legend = c("1890-1910","1920-1940","1950-1970", "1980-2000", "2010-2018"), 
       pch = 19, cex=1.1, bty="n", col=c("darkorange1", "darkgoldenrod1", "black", "chartreuse4", "royalblue3"))
#dev.off()

# test for differences in LB species composition across invasion timeline
adonis(dist.lb.yr ~ as.factor(lb2.matrix$Invasion), permutations = 999)
pairwise.adonis(dist.lb.yr, lb2.matrix$Invasion)
levels(lb2.matrix$Invasion)

# quick plot to visualize results
levels(lb2.matrix$Invasion)
#png("LB_InvasionHistory.png", width = 1000, height = 800, pointsize = 20)
ordiplot(nmds.yr, type="n", xlim = c(-3, 3), ylim = c(-1.5, 1.5))
points(nmds.yr, dis="sites", pch = 19, cex=1.5, 
       col=c("darkorange1", "darkgoldenrod1", "chartreuse4", "royalblue3")[as.numeric(lb2.matrix$Invasion)])
ordiellipse(nmds.yr, groups=lb2.matrix$Invasion, display="sites", draw="lines", conf=0.90)
legend("topleft", legend = c("Pre-invasion","Post-C.sept", "Post-H.axyridis", "Post-P.quat"), 
       pch = 19, cex=1.1, bty="n", col=c("darkorange1", "darkgoldenrod1", "chartreuse4", "royalblue3"))
#dev.off()

##############

# Look at species across regions of Ohio

colnames(lb)

lb3 <- lb[,c(2,7,6)]

colnames(lb3)

lb3.matrix <- dcast(lb3, Regions + County ~ Name, length)

str(lb3.matrix)

# run NMDS model for county

rankindex(lb3.matrix$Regions, lb3.matrix[3:33])
nmds.co <- metaMDS(lb3.matrix[3:33], distance = "bray", trymax = 500, autotransform = TRUE)
nmds.co
plot(nmds.co)
stressplot(nmds.co)
nmds.co$points
nmds.co$stress

# calculate distance matrix for LB species

dist.lb.co <- vegdist(lb3.matrix[3:33], method = "bray")

# test for differences in LB species composition across Ohio regions
adonis(dist.lb.co ~ as.factor(lb3.matrix$Regions), permutations = 999)
pairwise.adonis(dist.lb.co, lb3.matrix$Regions)

# quick plot to visualize results
levels(lb3.matrix$Regions)
#png("LB_GeographicRegions.png", width = 1000, height = 800, pointsize = 20)
ordiplot(nmds.co, type="n", xlim = c(-2, 2), ylim = c(-1.5, 1.5))
points(nmds.co, dis="sites", pch = 19, cex=1.5, col=c("darkorange1", "darkgoldenrod1", "chartreuse4", "royalblue3")[as.numeric(lb3.matrix$Regions)])
ordiellipse(nmds.co, groups=lb3.matrix$Regions, display="sites", draw="lines", conf=0.90)
legend("topleft", legend = c("Appalachian Plateau","Indiana-Ohio Till Plain",
                             "Lake Erie Glaciated Plateau", "Western Lake Erie Basin"), 
       pch = 19, cex=1.1, bty="n", col=c("darkorange1", "darkgoldenrod1", "chartreuse4", "royalblue3"))
#dev.off()

##############################################

# compare LB community composition collected from sticky cards in BLBB study
# to museum specimens collected from the same years

Yr09 <- lb[which(lb$Year == "2009"),]
Yr10 <- lb[which(lb$Year == "2010"),]
Yr13 <- lb[which(lb$Year == "2013"),]
Yr14 <- lb[which(lb$Year == "2014"),]
lb4 <- rbind(Yr09, Yr10, Yr13, Yr14)

rm(Yr09,Yr10,Yr13,Yr14)

colnames(lb4)

blbb <- lb4[,c(2,7,19,14)]

colnames(blbb)

lb4.matrix <- dcast(blbb, Year + Regions + Source ~ Name, length)
str(lb4.matrix)

# change year to a factor
lb4.matrix <- within(lb4.matrix, {
  Year <- factor(Year)
})
str(lb4.matrix)
str(lb)

# compared to full dataset, 2009-10 and 2013-14 have fewer species represented
# reduced dataset contains 16 fewer species (15 in blbb, 31 in full dataset)

# run NMDS model

rankindex(lb4.matrix$Source, lb4.matrix[4:18])
nmds.blbb <- metaMDS(lb4.matrix[4:18], distance = "bray", trymax = 500, autotransform = TRUE)
nmds.blbb
plot(nmds.blbb)
stressplot(nmds.blbb)
nmds.blbb$points
nmds.blbb$stress

# calculate distance matrix for LB species

dist.blbb <- vegdist(lb4.matrix[4:18], method = "bray")

# test for differences in species composition between museum and BLBB data
adonis(dist.blbb ~ as.factor(lb4.matrix$Source), permutations = 999)

# quick plot to visualize results
levels(lb4.matrix$Source)
#png("LB_BLBBandMuseum.png", width = 1000, height = 800, pointsize = 20)
ordiplot(nmds.blbb, type="n", xlim = c(-2, 2), ylim = c(-1.5, 1.5))
points(nmds.blbb, dis="sites", pch = 19, cex=1.5, col=c("firebrick3", "dodgerblue3")[as.numeric(lb4.matrix$Source)])
ordiellipse(nmds.blbb, groups=lb4.matrix$Source, display="sites", draw="lines", conf=0.90)
legend("bottomleft", legend = c("Buckeye Lady Beetle Blitz","Museum"), 
       pch = 19, cex=1.1, bty="n", col=c("firebrick3", "dodgerblue3"))
#dev.off()

# test for interaction between data source and year
adonis2(dist.blbb ~ lb4.matrix$Source*lb4.matrix$Year, permutations = 999)

# test for interaction between data source and geographic region
adonis2(dist.blbb ~ lb4.matrix$Source*lb4.matrix$Regions, permutations = 999)


#############################################

# partitioning beta diversity over time

if (!suppressWarnings(require(betapart))) install.packages("betapart")
citation("betapart")

str(lb)
colnames(lb)

lb5 <- lb[,c(2,16,7)]

colnames(lb5)

lb5.matrix <- dcast(lb5, Regions + Decade30 ~ Name, length)
str(lb5.matrix)

levels(lb5.matrix$Decade30)

# pull out data from each 30 year interval
# change to presence/absence
# remove factor columns, change regions to row names

time1 <- lb5.matrix[which(lb5.matrix$Decade30 == "1890-1910"),]
time1[time1>0]<- 1
str(time1)
rownames(time1) <- time1[,1]
time1 <- time1[,-1:-2]
str(time1)

time2 <- lb5.matrix[which(lb5.matrix$Decade30 == "1920-1940"),]
time2[time2>0]<- 1
str(time2)
rownames(time2) <- time2[,1]
time2 <- time2[,-1:-2]
str(time2)

time3 <- lb5.matrix[which(lb5.matrix$Decade30 == "1950-1970"),]
time3[time3>0]<- 1
str(time3)
rownames(time3) <- time3[,1]
time3 <- time3[,-1:-2]
str(time3)

time4 <- lb5.matrix[which(lb5.matrix$Decade30 == "1980-2000"),]
time4[time4>0]<- 1
str(time4)
rownames(time4) <- time4[,1]
time4 <- time4[,-1:-2]
str(time4)

time5 <- lb5.matrix[which(lb5.matrix$Decade30 == "2010-2018"),]
time5[time5>0]<- 1
str(time5)
rownames(time5) <- time5[,1]
time5 <- time5[,-1:-2]
str(time5)

# create beta part object for analyses

t1 <- betapart.core(time1)
t2 <- betapart.core(time2)
t3 <- betapart.core(time3)
t4 <- betapart.core(time4)
t5 <- betapart.core(time5)

# assess temporal change in community composition among the 30 year intervals

t12 <- beta.temp(t1, t2, index.family = "sorensen")
t12

t23 <- beta.temp(t2, t3, index.family = "sorensen")
t23

t34 <- beta.temp(t3, t4, index.family = "sorensen")
t34

t45 <- beta.temp(t4, t5, index.family = "sorensen")
t45

# create stacked bar plots for each geograpnical region

if (!suppressWarnings(require(ggplot2))) install.packages("ggplot2")
citation("ggplot2")

theme_set(theme_bw())

levels(lb5.matrix$Decade30)

# create a data frame for each geographical region based on beta.temp outputs
# beta.sim = species turnover; beta.sne = species nestedness
#beta.sor = total betadiversity (i.e., beta.sim + beta.sne = beta.sor)

# Appalachian Plateau
AP <- data.frame(
  Turnover = c(0, 0.05, 0.19, 0),
  Nestedness = c(0.42, 0.04, 0.02, 0.23)
)

AP$interval <- c("1890-1940", "1920-1970", "1950-2000", "1980-2018")

AP <- melt(AP, id.vars = "interval")

ggplot(AP, aes(fill = variable, x = interval, y = value)) + 
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal() + xlab("Time Interval") + ylab("Beta-diversity") +
  ggtitle("Appalachian Plateau")


# Indiana-Ohio Till Plain
TP <- data.frame(
  Turnover = c(0.06, 0.16, 0.15, 0),
  Nestedness = c(0.08, 0.04, 0.02, 0.40)
)

TP$interval <- c("1890-1940", "1920-1970", "1950-2000", "1980-2018")

TP <- melt(TP, id.vars = "interval")

ggplot(TP, aes(fill = variable, x = interval, y = value)) + 
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal() + xlab("Time Interval") + ylab("Beta-diversity") +
  ggtitle("Indiana-Ohio Till Plain")

# Lake Erie Glaciated Plateau
GP <- data.frame(
  Turnover = c(0, 0.11, 0.16, 0.05),
  Nestedness = c(0.16, 0, 0.02, 0.05)
)

GP$interval <- c("1890-1940", "1920-1970", "1950-2000", "1980-2018")

GP <- melt(GP, id.vars = "interval")

ggplot(GP, aes(fill = variable, x = interval, y = value)) + 
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal() + xlab("Time Interval") + ylab("Beta-diversity") +
  ggtitle("Lake Erie Glaciated Plateau")


# Western Lake Erie Basin
WB <- data.frame(
  Turnover = c(0.31, 0.21, 0.20, 0),
  Nestedness = c(0.02, 0.02, 0.07, 0.24)
)

WB$interval <- c("1890-1940", "1920-1970", "1950-2000", "1980-2018")

WB <- melt(WB, id.vars = "interval")

ggplot(WB, aes(fill = variable, x = interval, y = value)) + 
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal() + xlab("Time Interval") + ylab("Beta-diversity") +
  ggtitle("Western Lake Erie Basin")


# assess changes in beta diversity over 30 year intervals for the entire state of Ohio

lb6 <- lb[,c(2,16)]

colnames(lb6)

lb6.matrix <- dcast(lb6, Decade30 ~ Name, length)
str(lb6.matrix)

# add a column for site, which will become the row names later
lb6.matrix$site <- c("OH", "OH", "OH", "OH", "OH")

# pull out data from each 30 year interval
# change to presence/absence
# remove factor columns, change site to row names

time1.1 <- lb6.matrix[which(lb6.matrix$Decade30 == "1890-1910"),]
rownames(time1.1) <- time1.1[,33]
time1.1[time1.1>0]<- 1
time1.1 <- time1.1[,-1]
time1.1 <- time1.1[,-32]
str(time1.1)

time2.2 <- lb6.matrix[which(lb6.matrix$Decade30 == "1920-1940"),]
rownames(time2.2) <- time2.2[,33]
time2.2[time2.2>0]<- 1
time2.2 <- time2.2[,-1]
time2.2 <- time2.2[,-32]
str(time2.2)

time3.3 <- lb6.matrix[which(lb6.matrix$Decade30 == "1950-1970"),]
rownames(time3.3) <- time3.3[,33]
time3.3[time3.3>0]<- 1
time3.3 <- time3.3[,-1]
time3.3 <- time3.3[,-32]
str(time3.3)

time4.4 <- lb6.matrix[which(lb6.matrix$Decade30 == "1980-2000"),]
rownames(time4.4) <- time4.4[,33]
time4.4[time4.4>0]<- 1
time4.4 <- time4.4[,-1]
time4.4 <- time4.4[,-32]
str(time4.4)

time5.5 <- lb6.matrix[which(lb6.matrix$Decade30 == "2010-2018"),]
rownames(time5.5) <- time5.5[,33]
time5.5[time5.5>0]<- 1
time5.5 <- time5.5[,-1]
time5.5 <- time5.5[,-32]
str(time5.5)

t1.1 <- betapart.core(time1.1)
t2.2 <- betapart.core(time2.2)
t3.3 <- betapart.core(time3.3)
t4.4 <- betapart.core(time4.4)
t5.5 <- betapart.core(time5.5)

# assess temporal change in community composition among the 30 year intervals for the state

t12.oh <- beta.temp(t1.1, t2.2, index.family = "sorensen")
t12.oh

t23.oh <- beta.temp(t2.2, t3.3, index.family = "sorensen")
t23.oh

t34.oh <- beta.temp(t3.3, t4.4, index.family = "sorensen")
t34.oh

t45.oh <- beta.temp(t4.4, t5.5, index.family = "sorensen")
t45.oh


# create a data frame for each geographical region based on beta.temp outputs
# beta.sim = species turnover; beta.sne = species nestedness
#beta.sor = total betadiversity (i.e., beta.sim + beta.sne = beta.sor)

OH <- data.frame(
  Turnover = c(0, 0.08, 0.17, 0.05),
  Nestedness = c(0.17, 0.02, 0.02, 0.09 )
)

OH$interval <- c("1890-1910/1920-1940", "1920-1940/1950-1970", "1950-1970/1980-2000", "1980-2000/2010-2018")

OH <- melt(OH, id.vars = "interval")

png("LB_BetaDiversity.png", width = 1000, height = 800, pointsize = 20)
ggplot(OH, aes(fill = variable, x = interval, y = value)) + 
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal() + xlab("Time Interval") + ylab("Beta-diversity") +
  ggtitle("Ohio") + coord_flip() + theme(text = element_text(size = 20))
dev.off()


######################################################################################

# look at differences in LB communities among urban and rural areas

# create the LB community matrix
colnames(lb)

lb7 <- lb[,c(2,8,6)]

colnames(lb7)

lb7.matrix <- dcast(lb7, County + Landscape ~ Name, length)
str(lb7.matrix)

# look at overall differences between rural and urban areas
# urban areas include the counties with the top 6 Ohio cities
# Columbus, Cleveland, Cincinnati, Toledo, Akron, Dayton
# Franklin Co, Cuyahoga Co, Hamilton Co, Lucas Co, Summit Co, and Montgomery Co

# run NMDS model

rankindex(lb7.matrix$Landscape, lb7.matrix[3:33])
nmds.land <- metaMDS(lb7.matrix[3:33], distance = "bray", trymax = 1000, autotransform = TRUE)
nmds.land
plot(nmds.land)
stressplot(nmds.land)
nmds.land$points
nmds.land$stress

# calculate distance matrix for LB species

dist.land <- vegdist(lb7.matrix[3:33], method = "bray")

# test for differences in species composition between museum and BLBB data
adonis2(dist.land ~ as.factor(lb7.matrix$Landscape), permutations = 999)

# pull in census data and average across years by county
census <- read.csv("~/The Ohio State University/PostDoc/BLBB Project/Museum Files/Ohio_ladybeetles/intermediate_data/oh_co_census.csv")

levels(census$County)
summary(census)
census <- na.omit(census)

county <- lb7.matrix[1:2]

census.by.county <- ddply(census, c("County"), summarise,
                       avg.pop = mean(Population), avg.area = mean(Area), avg.urban.pop = mean(Urban_pop),
                       avg.density = mean(Density), avg.prop.urban = mean(Prop_urban))

census.by.county <- merge(county, census.by.county, by=c("County"), all.x = TRUE)
census.by.county$Landscape <- NULL

fit.census <- envfit(nmds.land ~ census.by.county, perm = 999) #this wont work for some reason

# calculate total abundance of each species
# then create logical vector for most abundant species
lb_abund <- colSums(lb7.matrix[3:33])
lb_abund
com_sp <- lb_abund > 100
com_sp

pchvec <- c(19, 15)

levels(lb7.matrix$Landscape)
png("LB_Landscape_species_100_smalltext.png", width = 1000, height = 800, pointsize = 20)
ordiplot(nmds.land, type="n", xlim = c(-2, 2), ylim = c(-1.8, 1.5))
points(nmds.land, dis="sites", pch = pchvec[lb7.matrix$Landscape], cex=1.5, col=c("gray45", "black")[as.numeric(lb7.matrix$Landscape)])
ordiellipse(nmds.land, groups=lb7.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.land, display = "species", cex = 0.6, col = "black", air = 0.1, select = com_sp)
legend("topleft", legend = c("Rural","Urban"), pch = 19, cex=1.1, bty="n", col=c("gray45", "black"))
dev.off()


################################################
pchvec <- c(19, 15)
summary(lb)
str(lb)

lb$Decade <- as.factor(lb$Decade)
levels(lb$Decade)

# 1900
lb.1900 <- lb[which(lb$Decade == "1900"),]
lb.1900 <- lb.1900[,c(2,8,6)]
lb.1900.matrix <- dcast(lb.1900, County + Landscape ~ Name, length)
nmds.1900 <- metaMDS(lb.1900.matrix[3:18], distance = "bray", trymax = 1000, autotransform = TRUE)
nmds.1900
plot(nmds.1900)
dist.1900 <- vegdist(lb.1900.matrix[3:18], method = "bray")
adonis2(dist.1900 ~ as.factor(lb.1900.matrix$Landscape), permutations = 999)

lb.abund.1900 <- colSums(lb.1900.matrix[3:18])
lb.abund.1900
com.sp.1900 <- lb.abund.1900 > 5
com.sp.1900

ordiplot(nmds.1900, type="n", xlim = c(-3, 3), ylim = c(-2, 3))
points(nmds.1900, dis="sites", pch = pchvec[lb.1900.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.1900.matrix$Landscape)])
ordiellipse(nmds.1900, groups=lb.1900.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.1900, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.1900)
legend("topleft", legend = c("Rural","Urban"), pch = 19, cex=1.1, bty="n", col=c("gray45", "black"))
text(3.4, 2.75, "1900", pos = 4, font = 2, cex = 1.5)


# 1930
lb.1930 <- lb[which(lb$Decade == "1930"),]
lb.1930 <- lb.1930[,c(2,8,6)]
lb.1930.matrix <- dcast(lb.1930, County + Landscape ~ Name, length)
nmds.1930 <- metaMDS(lb.1930.matrix[3:25], distance = "bray", trymax = 1000, autotransform = TRUE)
nmds.1930
plot(nmds.1930)
dist.1930 <- vegdist(lb.1930.matrix[3:25], method = "bray")
adonis2(dist.1930 ~ as.factor(lb.1930.matrix$Landscape), permutations = 999)

lb.abund.1930 <- colSums(lb.1930.matrix[3:25])
lb.abund.1930
com.sp.1930 <- lb.abund.1930 > 45
com.sp.1930

ordiplot(nmds.1930, type="n", xlim = c(-2, 2), ylim = c(-2, 1.5))
points(nmds.1930, dis="sites", pch = pchvec[lb.1930.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.1930.matrix$Landscape)])
ordiellipse(nmds.1930, groups=lb.1930.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.1930, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.1930)
legend("topleft", legend = c("Rural","Urban"), pch = 19, cex=1.1, bty="n", col=c("gray45", "black"))
text(2.4, 1.3, "1930", pos = 4, font = 2, cex = 1.5)


# 1950
lb.1950 <- lb[which(lb$Decade == "1950"),]
lb.1950 <- lb.1950[,c(2,8,6)]
lb.1950.matrix <- dcast(lb.1950, County + Landscape ~ Name, length)
nmds.1950 <- metaMDS(lb.1950.matrix[3:23], distance = "bray", trymax = 1000, autotransform = TRUE)
nmds.1950
plot(nmds.1950)
dist.1950 <- vegdist(lb.1950.matrix[3:23], method = "bray")
adonis2(dist.1950 ~ as.factor(lb.1950.matrix$Landscape), permutations = 999)

lb.abund.1950 <- colSums(lb.1950.matrix[3:23])
lb.abund.1950
com.sp.1950 <- lb.abund.1950 > 10
com.sp.1950

ordiplot(nmds.1950, type="n", xlim = c(-3, 3), ylim = c(-2.5, 2.5))
points(nmds.1950, dis="sites", pch = pchvec[lb.1950.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.1950.matrix$Landscape)])
ordiellipse(nmds.1950, groups=lb.1950.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.1950, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.1950)
legend("topleft", legend = c("Rural","Urban"), pch = 19, cex=1.1, bty="n", col=c("gray45", "black"))
text(3.4, 2.2, "1950", pos = 4, font = 2, cex = 1.5)


# 1960
lb.1960 <- lb[which(lb$Decade == "1960"),]
lb.1960 <- lb.1960[,c(2,8,6)]
lb.1960.matrix <- dcast(lb.1960, County + Landscape ~ Name, length)
nmds.1960 <- metaMDS(lb.1960.matrix[3:21], distance = "bray", trymax = 1000, autotransform = TRUE)
nmds.1960
plot(nmds.1960)
dist.1960 <- vegdist(lb.1960.matrix[3:21], method = "bray")
adonis2(dist.1960 ~ as.factor(lb.1960.matrix$Landscape), permutations = 999)

lb.abund.1960 <- colSums(lb.1960.matrix[3:21])
lb.abund.1960
com.sp.1960 <- lb.abund.1960 > 30
com.sp.1960

ordiplot(nmds.1960, type="n", xlim = c(-5, 2), ylim = c(-2.5, 2.5))
points(nmds.1960, dis="sites", pch = pchvec[lb.1960.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.1960.matrix$Landscape)])
ordiellipse(nmds.1960, groups=lb.1960.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.1960, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.1960)
legend("topleft", legend = c("Rural","Urban"), pch = 19, cex=1.1, bty="n", col=c("gray45", "black"))
text(1.9, 2.2, "1960", pos = 4, font = 2, cex = 1.5)


# 1980
lb.1980 <- lb[which(lb$Decade == "1980"),]
lb.1980 <- lb.1980[,c(2,8,6)]
lb.1980.matrix <- dcast(lb.1980, County + Landscape ~ Name, length)
nmds.1980 <- metaMDS(lb.1980.matrix[3:22], distance = "bray", trymax = 1000, autotransform = TRUE)
nmds.1980
plot(nmds.1980)
dist.1980 <- vegdist(lb.1980.matrix[3:22], method = "bray")
adonis2(dist.1980 ~ as.factor(lb.1980.matrix$Landscape), permutations = 999)

lb.abund.1980 <- colSums(lb.1980.matrix[3:22])
lb.abund.1960
com.sp.1980 <- lb.abund.1980 > 50
com.sp.1980

ordiplot(nmds.1980, type="n", xlim = c(-2, 4), ylim = c(-2, 2))
points(nmds.1980, dis="sites", pch = pchvec[lb.1980.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.1980.matrix$Landscape)])
ordiellipse(nmds.1980, groups=lb.1980.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.1980, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.1980)
legend("topleft", legend = c("Rural","Urban"), pch = 19, cex=1.1, bty="n", col=c("gray45", "black"))
text(3.7, 1.75, "1980", pos = 4, font = 2, cex = 1.5)


# 2000
lb.2000 <- lb[which(lb$Decade == "2000"),]
lb.2000 <- lb.2000[,c(2,8,6)]
lb.2000.matrix <- dcast(lb.2000, County + Landscape ~ Name, length)
nmds.2000 <- metaMDS(lb.2000.matrix[3:22], distance = "euclidean", trymax = 1000, autotransform = TRUE)
nmds.2000
plot(nmds.2000)
dist.2000 <- vegdist(lb.2000.matrix[3:22], method = "euclidean")
adonis2(dist.2000 ~ as.factor(lb.2000.matrix$Landscape), permutations = 999)

lb.abund.2000 <- colSums(lb.2000.matrix[3:22])
lb.abund.2000
com.sp.2000 <- lb.abund.2000 > 50
com.sp.2000

ordiplot(nmds.2000, type="n", xlim = c(-1, 1), ylim = c(-0.8, 0.8))
points(nmds.2000, dis="sites", pch = pchvec[lb.2000.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.2000.matrix$Landscape)])
ordiellipse(nmds.2000, groups=lb.2000.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.2000, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.2000)
legend("topleft", legend = c("Rural","Urban"), pch = 19, cex=1.1, bty="n", col=c("gray45", "black"))
text(1.1, 0.7, "2000", pos = 4, font = 2, cex = 1.5)


#################################################################################

# Combined nmds figure for LB communities overtime in rural vs urban counties

png("NMDS_LB_Landscape_Time.png", width = 1600, height = 2000, pointsize = 30)

par(mfrow=c(3,2))
par(mar=c(5,4,3,2))

ordiplot(nmds.1900, type="n", xlim = c(-3, 3), ylim = c(-2, 3))
points(nmds.1900, dis="sites", pch = pchvec[lb.1900.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.1900.matrix$Landscape)])
ordiellipse(nmds.1900, groups=lb.1900.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.1900, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.1900)
legend("topleft", legend = c("Rural","Urban"), pch = 19, cex=1.1, bty="n", col=c("gray45", "black"))
text(2.5, 2.75, "1900", pos = 4, font = 2, cex = 1.5)

ordiplot(nmds.1930, type="n", xlim = c(-2, 2), ylim = c(-2, 1.5))
points(nmds.1930, dis="sites", pch = pchvec[lb.1930.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.1930.matrix$Landscape)])
ordiellipse(nmds.1930, groups=lb.1930.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.1930, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.1930)
text(1.8, 1.3, "1930", pos = 4, font = 2, cex = 1.5)

ordiplot(nmds.1950, type="n", xlim = c(-3, 3), ylim = c(-2.5, 2.5))
points(nmds.1950, dis="sites", pch = pchvec[lb.1950.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.1950.matrix$Landscape)])
ordiellipse(nmds.1950, groups=lb.1950.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.1950, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.1950)
text(2.5, 2.2, "1950", pos = 4, font = 2, cex = 1.5)

ordiplot(nmds.1960, type="n", xlim = c(-5, 2), ylim = c(-2.5, 2.5))
points(nmds.1960, dis="sites", pch = pchvec[lb.1960.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.1960.matrix$Landscape)])
ordiellipse(nmds.1960, groups=lb.1960.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.1960, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.1960)
text(1.1, 2.2, "1960", pos = 4, font = 2, cex = 1.5)

ordiplot(nmds.1980, type="n", xlim = c(-2, 4), ylim = c(-2, 2))
points(nmds.1980, dis="sites", pch = pchvec[lb.1980.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.1980.matrix$Landscape)])
ordiellipse(nmds.1980, groups=lb.1980.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.1980, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.1980)
text(3.2, 1.85, "1980", pos = 4, font = 2, cex = 1.5)

ordiplot(nmds.2000, type="n", xlim = c(-1, 1), ylim = c(-0.8, 0.8))
points(nmds.2000, dis="sites", pch = pchvec[lb.2000.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.2000.matrix$Landscape)])
ordiellipse(nmds.2000, groups=lb.2000.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.2000, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.2000)
text(0.8, 0.7, "2000", pos = 4, font = 2, cex = 1.5)

dev.off()


###############################################################################

# pre- and post-invasion

pchvec <- c(19, 15)
summary(lb)
str(lb)
levels(lb$Invasion)

# Pre-invasion
lb.pre <- lb[which(lb$Invasion == "1Pre-invasion"),]
lb.pre <- lb.pre[,c(2,8,6)]
lb.pre.matrix <- dcast(lb.pre, County + Landscape ~ Name, length)
nmds.pre <- metaMDS(lb.pre.matrix[3:27], distance = "bray", trymax = 1000, autotransform = TRUE)
nmds.pre
plot(nmds.pre)
dist.pre <- vegdist(lb.pre.matrix[3:27], method = "bray")
adonis2(dist.pre ~ as.factor(lb.pre.matrix$Landscape), permutations = 999)

lb.abund.pre <- colSums(lb.pre.matrix[3:27])
lb.abund.pre
com.sp.pre <- lb.abund.pre > 100
com.sp.pre

ordiplot(nmds.pre, type="n", xlim = c(-2, 2), ylim = c(-1.5, 1.5))
points(nmds.pre, dis="sites", pch = pchvec[lb.pre.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.pre.matrix$Landscape)])
ordiellipse(nmds.pre, groups=lb.pre.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.pre, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.pre)
legend("topleft", legend = c("Rural","Urban"), pch = 19, cex=1.1, bty="n", col=c("gray45", "black"))
text(1.2, 1.3, "Pre-invasion", pos = 4, font = 2, cex = 1.5)


# Post-invasion of LBs
levels(lb$Invasion)
lb.post1 <- lb[which(lb$Invasion == "2Csept"),]
lb.post2 <- lb[which(lb$Invasion == "3Haxyridis"),]
lb.post3 <- lb[which(lb$Invasion == "4Pquat"),]

lb.post <- rbind(lb.post1, lb.post2, lb.post3)
levels(lb.post$Invasion)
levels(lb.post1$Invasion)

lb.post <- lb.post[,c(2,8,6)]
lb.post.matrix <- dcast(lb.post, County + Landscape ~ Name, length)
nmds.post <- metaMDS(lb.post.matrix[3:29], distance = "bray", trymax = 1000, autotransform = TRUE)
nmds.post
plot(nmds.post)
dist.post <- vegdist(lb.post.matrix[3:29], method = "bray")
adonis2(dist.post ~ as.factor(lb.post.matrix$Landscape), permutations = 999)

lb.abund.post <- colSums(lb.post.matrix[3:29])
lb.abund.post
com.sp.post <- lb.abund.post > 50
com.sp.post

ordiplot(nmds.post, type="n", xlim = c(-2, 2), ylim = c(-1.5, 1.5))
points(nmds.post, dis="sites", pch = pchvec[lb.post.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.post.matrix$Landscape)])
ordiellipse(nmds.post, groups=lb.post.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.post, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.post)
legend("topleft", legend = c("Rural","Urban"), pch = 19, cex=1.1, bty="n", col=c("gray45", "black"))
text(1, 1.3, "Post-invasion", pos = 4, font = 2, cex = 1.5)

#############

# make a combined figure for pre- and post-invasion

png("NMDS_LB_Invasion.png", width = 2000, height = 800, pointsize = 20)
layout(matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE))
layout.show(2)

ordiplot(nmds.pre, type="n", xlim = c(-2, 2), ylim = c(-1.5, 1.5))
points(nmds.pre, dis="sites", pch = pchvec[lb.pre.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.pre.matrix$Landscape)])
ordiellipse(nmds.pre, groups=lb.pre.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.pre, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.pre)
legend("topleft", legend = c("Rural","Urban"), pch = 19, cex=1.1, bty="n", col=c("gray45", "black"))
text(1.2, 1.3, "Pre-invasion", pos = 4, font = 2, cex = 1.5)

ordiplot(nmds.post, type="n", xlim = c(-2, 2), ylim = c(-1.5, 1.5))
points(nmds.post, dis="sites", pch = pchvec[lb.post.matrix$Landscape], cex=1.5, 
       col=c("gray45", "black")[as.numeric(lb.post.matrix$Landscape)])
ordiellipse(nmds.post, groups=lb.post.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.post, display = "species", cex = 0.8, col = "black", air = 0.1, select = com.sp.post)
text(1, 1.3, "Post-invasion", pos = 4, font = 2, cex = 1.5)

dev.off()