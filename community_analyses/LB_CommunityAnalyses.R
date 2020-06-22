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

fit.census <- envfit(nmds.land ~ census.by.county, perm = 999)

# calculate total abundance of each species
# then create logical vector for most abundant species
lb_abund <- colSums(lb7.matrix[3:33])
lb_abund
com_sp <- lb_abund > 300
com_sp

pchvec <- c(19, 15)

levels(lb7.matrix$Landscape)
#png("LB_Landscape_species.png", width = 1000, height = 800, pointsize = 20)
ordiplot(nmds.land, type="n", xlim = c(-2, 2), ylim = c(-1.8, 1.5))
points(nmds.land, dis="sites", pch = pchvec[lb7.matrix$Landscape], cex=1.5, col=c("gray45", "black")[as.numeric(lb7.matrix$Landscape)])
ordiellipse(nmds.land, groups=lb7.matrix$Landscape, display="sites", lwd = 2.5, draw="lines", conf=0.90)
orditorp(nmds.land, display = "species", cex = 1.25, col = "black", air = 0.5, select = com_sp)
legend("topleft", legend = c("Rural","Urban"), pch = 19, cex=1.1, bty="n", col=c("gray45", "black"))
#dev.off()





