###################################################################################
#
# Lady beetle museum data
#
# Restructure data set
#
# Assess taxonomic beta-diversity over timeline and partition beta-diversity into
# nestedeness and turnover components
#
# KI Perry; 5 October 2020
#
###################################################################################

#setwd("~/The Ohio State University/PostDoc - Gardiner/BLBB Project/Museum Files/Ohio_ladybeetles/community_analyses")
setwd("E:/Postdoc - Gardiner Lab/BLBB Project/Museum Files/Ohio_ladybeetles/community_analyses")

lb <- read.csv("LB_MuseumData_2020_v2.csv")

colnames(lb)
levels(lb$Name)

#install.packages("reshape2")
library(reshape2)

#install.packages("vegan")
library(vegan)

#install.packages("devtools")
#library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
#library(pairwiseAdonis)

#install.packages("plyr")
library(plyr)

#install.packages("betapart")
library(betapart)

########
# Question: Does lady beetle taxonomic beta diversity change over time, and if so, is the mechanism species turnover,
# species nestedness, or both?

# first, investigate descriptive patterns over time across Ohio
lb.b <- lb[,c(2,15,19)]

# subset data to look at only museum specimens (i.e. remove BLBB data)
lb.b <- lb.b[c(1:4210),]
lb.b <- lb.b[,-3]

colnames(lb.b)

lb.b.matrix <- dcast(lb.b, Decade ~ Name, length)
str(lb.b.matrix)

# remove 1890
lb.b.matrix <- lb.b.matrix[-1,]
str(lb.b.matrix)

# add a column for site, which will become the row names later
lb.b.matrix$site <- c("OH", "OH", "OH", "OH", "OH", "OH", "OH", "OH", "OH", "OH", "OH", "OH")
str(lb.b.matrix)

#### Assess descriptive patterns for changes in lady beetle beta diversity
#### across decades in Ohio

# pull out data from each decade
# change to presence/absence
# remove factor columns, change site to row names

# removed data from 1890
#time1 <- lb.b.matrix[which(lb.b.matrix$Decade == "1890"),]
#rownames(time1) <- time1[,33]
#time1[time1>0]<- 1
#time1 <- time1[,-1]
#time1 <- time1[,-32]
#str(time1)

time2 <- lb.b.matrix[which(lb.b.matrix$Decade == "1900"),]
rownames(time2) <- time2[,30]
time2[time2>0]<- 1
time2 <- time2[,-1]
time2 <- time2[,-29]
str(time2)

time3 <- lb.b.matrix[which(lb.b.matrix$Decade == "1910"),]
rownames(time3) <- time3[,30]
time3[time3>0]<- 1
time3 <- time3[,-1]
time3 <- time3[,-29]
str(time3)

time4 <- lb.b.matrix[which(lb.b.matrix$Decade == "1920"),]
rownames(time4) <- time4[,30]
time4[time4>0]<- 1
time4 <- time4[,-1]
time4 <- time4[,-29]
str(time4)

time5 <- lb.b.matrix[which(lb.b.matrix$Decade == "1930"),]
rownames(time5) <- time5[,30]
time5[time5>0]<- 1
time5 <- time5[,-1]
time5 <- time5[,-29]
str(time5)

time6 <- lb.b.matrix[which(lb.b.matrix$Decade == "1940"),]
rownames(time6) <- time6[,30]
time6[time6>0]<- 1
time6 <- time6[,-1]
time6 <- time6[,-29]
str(time6)

time7 <- lb.b.matrix[which(lb.b.matrix$Decade == "1950"),]
rownames(time7) <- time7[,30]
time7[time7>0]<- 1
time7 <- time7[,-1]
time7 <- time7[,-29]
str(time7)

time8 <- lb.b.matrix[which(lb.b.matrix$Decade == "1960"),]
rownames(time8) <- time8[,30]
time8[time8>0]<- 1
time8 <- time8[,-1]
time8 <- time8[,-29]
str(time8)

time9 <- lb.b.matrix[which(lb.b.matrix$Decade == "1970"),]
rownames(time9) <- time9[,30]
time9[time9>0]<- 1
time9 <- time9[,-1]
time9 <- time9[,-29]
str(time9)

time10 <- lb.b.matrix[which(lb.b.matrix$Decade == "1980"),]
rownames(time10) <- time10[,30]
time10[time10>0]<- 1
time10 <- time10[,-1]
time10 <- time10[,-29]
str(time10)

time11 <- lb.b.matrix[which(lb.b.matrix$Decade == "1990"),]
rownames(time11) <- time11[,30]
time11[time11>0]<- 1
time11 <- time11[,-1]
time11 <- time11[,-29]
str(time11)

time12 <- lb.b.matrix[which(lb.b.matrix$Decade == "2000"),]
rownames(time12) <- time12[,30]
time12[time12>0]<- 1
time12 <- time12[,-1]
time12 <- time12[,-29]
str(time12)

time13 <- lb.b.matrix[which(lb.b.matrix$Decade == "2010"),]
rownames(time13) <- time13[,30]
time13[time13>0]<- 1
time13 <- time13[,-1]
time13 <- time13[,-29]
str(time13)

# create beta part object for analyses

#t1 <- betapart.core(time1)
t2 <- betapart.core(time2)
t3 <- betapart.core(time3)
t4 <- betapart.core(time4)
t5 <- betapart.core(time5)
t6 <- betapart.core(time6)
t7 <- betapart.core(time7)
t8 <- betapart.core(time8)
t9 <- betapart.core(time9)
t10 <- betapart.core(time10)
t11 <- betapart.core(time11)
t12 <- betapart.core(time12)
t13 <- betapart.core(time13)

# assess temporal change in community composition among the decades for the state

#t12 <- beta.temp(t1, t2, index.family = "sorensen")
#t12

t23 <- beta.temp(t2, t3, index.family = "sorensen")
t23

t34 <- beta.temp(t3, t4, index.family = "sorensen")
t34

t45 <- beta.temp(t4, t5, index.family = "sorensen")
t45

t56 <- beta.temp(t5, t6, index.family = "sorensen")
t56

t67 <- beta.temp(t6, t7, index.family = "sorensen")
t67

t78 <- beta.temp(t7, t8, index.family = "sorensen")
t78

t89 <- beta.temp(t8, t9, index.family = "sorensen")
t89

t910 <- beta.temp(t9, t10, index.family = "sorensen")
t910

t1011 <- beta.temp(t10, t11, index.family = "sorensen")
t1011

t1112 <- beta.temp(t11, t12, index.family = "sorensen")
t1112

t1213 <- beta.temp(t12, t13, index.family = "sorensen")
t1213

# outputs from beta.temp used to make figure 2
# remove all objects in workspace
rm(list=ls())


# next, investigate descriptive patterns for Ohio regions over time

lb <- read.csv("LB_MuseumData_2020_v2.csv")

colnames(lb)
levels(lb$Name)

lb.b <- lb[,c(2,15,7,19)]

# subset data to look at only museum specimens (i.e. remove BLBB data)
lb.b <- lb.b[c(1:4210),]
lb.b <- lb.b[,-4]

colnames(lb.b)

lb.b.matrix <- dcast(lb.b, Regions + Decade ~ Name, length)
str(lb.b.matrix)

# remove single collection of Mulsantina luteodorsa because it hasn't been
# verified by experts
lb.b.matrix <- lb.b.matrix[,-24]
str(lb.b.matrix)

lb.b.matrix$Decade <- as.factor(lb.b.matrix$Decade)
lb.b.matrix$Regions <- as.factor(lb.b.matrix$Regions)
levels(lb.b.matrix$Decade)
levels(lb.b.matrix$Regions)
str(lb.b.matrix)

# pull out data from each decade
# change to presence/absence
# remove factor columns, change regions to row names

# not enough collections from 1890s, and collections not from entire decade
#time1 <- lb.b.matrix[which(lb.b.matrix$Decade == "1890"),]
#time1[time1>0]<- 1
#str(time1)
#rownames(time1) <- time1[,1]
#time1 <- time1[,-1:-2]
#str(time1)

time2 <- lb.b.matrix[which(lb.b.matrix$Decade == "1900"),]
time2[time2>0]<- 1
str(time2)
rownames(time2) <- time2[,1]
time2 <- time2[,-1:-2]
str(time2)

time3 <- lb.b.matrix[which(lb.b.matrix$Decade == "1910"),]
time3[time3>0]<- 1
str(time3)
rownames(time3) <- time3[,1]
time3 <- time3[,-1:-2]
str(time3)

# need to remove AP from time 3 in order to make a comparison between
# time 3 and time 2
time3.2 <- lb.b.matrix[which(lb.b.matrix$Decade == "1910"),]
time3.2 <- time3.2[-1,]
time3.2[time3.2>0]<- 1
str(time3.2)
rownames(time3.2) <- time3.2[,1]
time3.2 <- time3.2[,-1:-2]
str(time3.2)

time4 <- lb.b.matrix[which(lb.b.matrix$Decade == "1920"),]
time4[time4>0]<- 1
str(time4)
rownames(time4) <- time4[,1]
time4 <- time4[,-1:-2]
str(time4)

time5 <- lb.b.matrix[which(lb.b.matrix$Decade == "1930"),]
time5[time5>0]<- 1
str(time5)
rownames(time5) <- time5[,1]
time5 <- time5[,-1:-2]
str(time5)

time6 <- lb.b.matrix[which(lb.b.matrix$Decade == "1940"),]
time6[time6>0]<- 1
str(time6)
rownames(time6) <- time6[,1]
time6 <- time6[,-1:-2]
str(time6)

time7 <- lb.b.matrix[which(lb.b.matrix$Decade == "1950"),]
time7[time7>0]<- 1
str(time7)
rownames(time7) <- time7[,1]
time7 <- time7[,-1:-2]
str(time7)

time8 <- lb.b.matrix[which(lb.b.matrix$Decade == "1960"),]
time8[time8>0]<- 1
str(time8)
rownames(time8) <- time8[,1]
time8 <- time8[,-1:-2]
str(time8)

time9 <- lb.b.matrix[which(lb.b.matrix$Decade == "1970"),]
time9[time9>0]<- 1
str(time9)
rownames(time9) <- time9[,1]
time9 <- time9[,-1:-2]
str(time9)

time10 <- lb.b.matrix[which(lb.b.matrix$Decade == "1980"),]
time10[time10>0]<- 1
str(time10)
rownames(time10) <- time10[,1]
time10 <- time10[,-1:-2]
str(time10)

time11 <- lb.b.matrix[which(lb.b.matrix$Decade == "1990"),]
time11[time11>0]<- 1
str(time11)
rownames(time11) <- time11[,1]
time11 <- time11[,-1:-2]
str(time11)

time12 <- lb.b.matrix[which(lb.b.matrix$Decade == "2000"),]
time12[time12>0]<- 1
str(time12)
rownames(time12) <- time12[,1]
time12 <- time12[,-1:-2]
str(time12)

time13 <- lb.b.matrix[which(lb.b.matrix$Decade == "2010"),]
time13[time13>0]<- 1
str(time13)
rownames(time13) <- time13[,1]
time13 <- time13[,-1:-2]
str(time13)

# create beta part object for analyses

#t1 <- betapart.core(time1)
t2 <- betapart.core(time2)
t3 <- betapart.core(time3)
t3.2 <- betapart.core(time3.2)
t4 <- betapart.core(time4)
t5 <- betapart.core(time5)
t6 <- betapart.core(time6)
t7 <- betapart.core(time7)
t8 <- betapart.core(time8)
t9 <- betapart.core(time9)
t10 <- betapart.core(time10)
t11 <- betapart.core(time11)
t12 <- betapart.core(time12)
t13 <- betapart.core(time13)

# assess temporal change in community composition among decades

#t12 <- beta.temp(t1, t2, index.family = "sorensen")
#t12

# comparison between 1900 and 1910 cannot be made due to no samples from AP region in 1900
# need to used revised dataset time.3.2
t23 <- beta.temp(t2, t3.2, index.family = "sorensen")
t23

t34 <- beta.temp(t3, t4, index.family = "sorensen")
t34

t45 <- beta.temp(t4, t5, index.family = "sorensen")
t45

t56 <- beta.temp(t5, t6, index.family = "sorensen")
t56

t67 <- beta.temp(t6, t7, index.family = "sorensen")
t67

t78 <- beta.temp(t7, t8, index.family = "sorensen")
t78

t89 <- beta.temp(t8, t9, index.family = "sorensen")
t89

t910 <- beta.temp(t9, t10, index.family = "sorensen")
t910

t1011 <- beta.temp(t10, t11, index.family = "sorensen")
t1011

t1112 <- beta.temp(t11, t12, index.family = "sorensen")
t1112

t1213 <- beta.temp(t12, t13, index.family = "sorensen")
t1213

# outputs from beta.temp used to make table
# remove all objects in workspace
rm(list=ls())


# now assess pairwise beta diversity for Ohio regions across decades


lb <- read.csv("LB_MuseumData_2020_v2.csv")

colnames(lb)

lb.b <- lb[,c(2,15,7,16,19)]
lb.b <- lb[,c(2,15,7,19)] #decade only

# subset data to look at only museum specimens (i.e. remove BLBB data)
lb.b <- lb.b[c(1:4210),]
lb.b <- lb.b[,-5]

colnames(lb.b)

lb.b.matrix <- dcast(lb.b, Regions + Decade + HDecade ~ Name, length)
lb.b.matrix <- dcast(lb.b, Regions + Decade ~ Name, length) #decade only
str(lb.b.matrix)

write.csv(lb.b.matrix, "LBCounts_by regions.csv")

# make sure predictor variables are factors
# this is important for conversion to presence/absence below
lb.b.matrix$Decade <- as.factor(lb.b.matrix$Decade)
lb.b.matrix$Regions <- as.factor(lb.b.matrix$Regions)
lb.b.matrix$HDecade <- as.factor(lb.b.matrix$HDecade)

levels(lb.b.matrix$Decade)
levels(lb.b.matrix$Regions)
levels(lb.b.matrix$HDecade)
str(lb.b.matrix)

# remove single collection of Mulsantina luteodorsa because it hasn't been
# verified by experts
lb.b.matrix <- lb.b.matrix[,-25]
lb.b.matrix <- lb.b.matrix[,-24] #decade only
str(lb.b.matrix)

# convert to presence/absence
lb.b.matrix[lb.b.matrix>0]<-1
str(lb.b.matrix)

# not enough data to look at pairwise comparisons among two decades with 
# specimens pooled by regions; splitting it up into half decades
# pull out data from each decade

time2 <- lb.b.matrix[which(lb.b.matrix$Decade == "1900"),]
str(time2)

time3 <- lb.b.matrix[which(lb.b.matrix$Decade == "1910"),]
str(time3)

time4 <- lb.b.matrix[which(lb.b.matrix$Decade == "1920"),]
str(time4)

time5 <- lb.b.matrix[which(lb.b.matrix$Decade == "1930"),]
str(time5)

time6 <- lb.b.matrix[which(lb.b.matrix$Decade == "1940"),]
str(time6)

time7 <- lb.b.matrix[which(lb.b.matrix$Decade == "1950"),]
str(time7)

time8 <- lb.b.matrix[which(lb.b.matrix$Decade == "1960"),]
str(time8)

time9 <- lb.b.matrix[which(lb.b.matrix$Decade == "1970"),]
str(time9)

time10 <- lb.b.matrix[which(lb.b.matrix$Decade == "1980"),]
str(time10)

time11 <- lb.b.matrix[which(lb.b.matrix$Decade == "1990"),]
str(time11)

time12 <- lb.b.matrix[which(lb.b.matrix$Decade == "2000"),]
str(time12)

time13 <- lb.b.matrix[which(lb.b.matrix$Decade == "2010"),]
str(time13)


# merge datasets to facilitate comparisons over time
# calculate betapart.core object for the two time intervals
# and then calculate pairwise betadiveristy among regions and decades
# run NMDS models and plot figures for each betadiversity component

# PERMANOVA tests whether the group centroid of beetle communities
# differs in multivariate space (e.g. different community composition)

# BETADISPER tests whether the dispersion of a treatment from its spatial median is different
# between groups (i.e. species redundancy across space)
# analysis of multivariate homogeneity of group dispersions (variances)
# multivariate analogue of Levene's test for homogenetiy of variances


# 1900, 1910  ####################################################################################
time.23 <-rbind(time2, time3)
str(time.23)
time.23.core <- betapart.core(time.23[3:29])
b.23.pair <- beta.pair(time.23.core, index.family = "sorensen")
str(b.23.pair)
b.23.multi <- beta.multi(time.23.core, index.family = "sorensen")
str(b.23.multi)


## beta.sor - total betadiversity
nmds.23.sor <- metaMDS(b.23$beta.sor, trymax = 500, autotransform = TRUE)
nmds.23.sor
stressplot(nmds.23.sor)
goodness(nmds.23.sor)
nmds.23.sor$stress
plot(nmds.23.sor)

# PERMANOVA
adonis2(b.23$beta.sor ~ time.23$Decade, permutations = 999)
pairwise.adonis(b.23$beta.sor, time.23$Decade)
adonis2(b.23$beta.sor ~ time.23$Regions, permutations = 999)
pairwise.adonis(b.23$beta.sor, time.23$Regions)

# BETADISPER
beta.sor.23 <- betadisper(b.23$beta.sor, time.23$Decade, type = c("median"))
beta.sor.23
anova(beta.sor.23)
plot(beta.sor.23)
boxplot(beta.sor.23, ylab = "Distance to median")
TukeyHSD(beta.sor.23, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.23.sim <- metaMDS(b.23$beta.sim, trymax = 500, autotransform = TRUE)
nmds.23.sim
stressplot(nmds.23.sim)
goodness(nmds.23.sim)
nmds.23.sim$stress
plot(nmds.23.sim)

# PERMANOVA
adonis2(b.23$beta.sim ~ time.23$Decade, permutations = 999)
pairwise.adonis(b.23$beta.sim, time.23$Decade)
adonis2(b.23$beta.sim ~ time.23$Regions, permutations = 999)
pairwise.adonis(b.23$beta.sim, time.23$Regions)

# BETADISPER
beta.sim.23 <- betadisper(b.23$beta.sim, time.23$Decade, type = c("median"))
beta.sim.23
anova(beta.sim.23)
plot(beta.sim.23)
boxplot(beta.sim.23, ylab = "Distance to median")
TukeyHSD(beta.sim.23, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.23.sne <- metaMDS(b.23$beta.sne, trymax = 500, autotransform = TRUE)
nmds.23.sne
stressplot(nmds.23.sne)
goodness(nmds.23.sne)
nmds.23.sne$stress
plot(nmds.23.sne)

# PERMANOVA
adonis2(b.23$beta.sne ~ time.23$Decade, permutations = 999)
pairwise.adonis(b.23$beta.sne, time.23$Decade)
adonis2(b.23$beta.sne ~ time.23$Regions, permutations = 999)
pairwise.adonis(b.23$beta.sne, time.23$Regions)

# BETADISPER
beta.sne.23 <- betadisper(b.23$beta.sne, time.23$Decade, type = c("median"))
beta.sne.23
anova(beta.sne.23)
plot(beta.sne.23)
boxplot(beta.sne.23, ylab = "Distance to median")
TukeyHSD(beta.sne.23, which = "group", conf.level = 0.95)


# 1910, 1920  ####################################################################################
time.34 <-rbind(time3, time4)
str(time.34)
time.34.core <- betapart.core(time.34[3:29])
b.34 <- beta.pair(time.34.core, index.family = "sorensen")
str(b.34)
b.34.multi <- beta.multi(time.34.core, index.family = "sorensen")
str(b.34.multi)


## beta.sor - total betadiversity
nmds.34.sor <- metaMDS(b.34$beta.sor, trymax = 500, autotransform = TRUE)
nmds.34.sor
stressplot(nmds.34.sor)
goodness(nmds.34.sor)
nmds.34.sor$stress
plot(nmds.34.sor)

# PERMANOVA
adonis2(b.34$beta.sor ~ time.34$Decade, permutations = 999)
pairwise.adonis(b.34$beta.sor, time.34$Decade)
adonis2(b.34$beta.sor ~ time.34$Regions, permutations = 999)
pairwise.adonis(b.34$beta.sor, time.34$Regions)

# BETADISPER
beta.sor.34 <- betadisper(b.34$beta.sor, time.34$Decade, type = c("median"))
beta.sor.34
anova(beta.sor.34)
plot(beta.sor.34)
boxplot(beta.sor.34, ylab = "Distance to median")
TukeyHSD(beta.sor.34, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.34.sim <- metaMDS(b.34$beta.sim, trymax = 500, autotransform = TRUE)
nmds.34.sim
stressplot(nmds.34.sim)
goodness(nmds.34.sim)
nmds.34.sim$stress
plot(nmds.34.sim)

# PERMANOVA
adonis2(b.34$beta.sim ~ time.34$Decade, permutations = 999)
pairwise.adonis(b.34$beta.sim, time.34$Decade)
adonis2(b.34$beta.sim ~ time.34$Regions, permutations = 999)
pairwise.adonis(b.34$beta.sim, time.34$Regions)

# BETADISPER
beta.sim.34 <- betadisper(b.34$beta.sim, time.34$Decade, type = c("median"))
beta.sim.34
anova(beta.sim.34)
plot(beta.sim.34)
boxplot(beta.sim.34, ylab = "Distance to median")
TukeyHSD(beta.sim.34, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.34.sne <- metaMDS(b.34$beta.sne, trymax = 500, autotransform = TRUE)
nmds.34.sne
stressplot(nmds.34.sne)
goodness(nmds.34.sne)
nmds.34.sne$stress
plot(nmds.34.sne)

# PERMANOVA
adonis2(b.34$beta.sne ~ time.34$Decade, permutations = 999)
pairwise.adonis(b.34$beta.sne, time.34$Decade)
adonis2(b.34$beta.sne ~ time.34$Regions, permutations = 999)
pairwise.adonis(b.34$beta.sne, time.34$Regions)

# BETADISPER
beta.sne.34 <- betadisper(b.34$beta.sne, time.34$Decade, type = c("median"))
beta.sne.34
anova(beta.sne.34)
plot(beta.sne.34)
boxplot(beta.sne.34, ylab = "Distance to median")
TukeyHSD(beta.sne.34, which = "group", conf.level = 0.95)


# 1920. 1930  ####################################################################################
time.45 <-rbind(time4, time5)
str(time.45)
time.45.core <- betapart.core(time.45[3:29])
b.45 <- beta.pair(time.45.core, index.family = "sorensen")
str(b.45)
b.45.multi <- beta.multi(time.45.core, index.family = "sorensen")
str(b.45.multi)


## beta.sor - total betadiversity
nmds.45.sor <- metaMDS(b.45$beta.sor, trymax = 500, autotransform = TRUE)
nmds.45.sor
stressplot(nmds.45.sor)
goodness(nmds.45.sor)
nmds.45.sor$stress
plot(nmds.45.sor)

# PERMANOVA
adonis2(b.45$beta.sor ~ time.45$Decade, permutations = 999)
pairwise.adonis(b.45$beta.sor, time.45$Decade)
adonis2(b.45$beta.sor ~ time.45$Regions, permutations = 999)
pairwise.adonis(b.45$beta.sor, time.45$Regions)

# BETADISPER
beta.sor.45 <- betadisper(b.45$beta.sor, time.45$Decade, type = c("median"))
beta.sor.45
anova(beta.sor.45)
plot(beta.sor.45)
boxplot(beta.sor.45, ylab = "Distance to median")
TukeyHSD(beta.sor.45, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.45.sim <- metaMDS(b.45$beta.sim, trymax = 500, autotransform = TRUE)
nmds.45.sim
stressplot(nmds.45.sim)
goodness(nmds.45.sim)
nmds.45.sim$stress
plot(nmds.45.sim)

# PERMANOVA
adonis2(b.45$beta.sim ~ time.45$Decade, permutations = 999)
pairwise.adonis(b.45$beta.sim, time.45$Decade)
adonis2(b.45$beta.sim ~ time.45$Regions, permutations = 999)
pairwise.adonis(b.45$beta.sim, time.45$Regions)

# BETADISPER
beta.sim.45 <- betadisper(b.45$beta.sim, time.45$Decade, type = c("median"))
beta.sim.45
anova(beta.sim.45)
plot(beta.sim.45)
boxplot(beta.sim.45, ylab = "Distance to median")
TukeyHSD(beta.sim.45, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.45.sne <- metaMDS(b.45$beta.sne, trymax = 500, autotransform = TRUE)
nmds.45.sne
stressplot(nmds.45.sne)
goodness(nmds.45.sne)
nmds.45.sne$stress
plot(nmds.45.sne)

# PERMANOVA
adonis2(b.45$beta.sne ~ time.45$Decade, permutations = 999)
pairwise.adonis(b.45$beta.sne, time.45$Decade)
adonis2(b.45$beta.sne ~ time.45$Regions, permutations = 999)
pairwise.adonis(b.45$beta.sne, time.45$Regions)

# BETADISPER
beta.sne.45 <- betadisper(b.45$beta.sne, time.45$Decade, type = c("median"))
beta.sne.45
anova(beta.sne.45)
plot(beta.sne.45)
boxplot(beta.sne.45, ylab = "Distance to median")
TukeyHSD(beta.sne.45, which = "group", conf.level = 0.95)


# 1930, 1940  ####################################################################################
time.56 <-rbind(time5, time6)
str(time.56)
time.56.core <- betapart.core(time.56[3:29])
b.56 <- beta.pair(time.56.core, index.family = "sorensen")
str(b.56)
b.56.multi <- beta.multi(time.56.core, index.family = "sorensen")
str(b.56.multi)


## beta.sor - total betadiversity
nmds.56.sor <- metaMDS(b.56$beta.sor, trymax = 500, autotransform = TRUE)
nmds.56.sor
stressplot(nmds.56.sor)
goodness(nmds.56.sor)
nmds.56.sor$stress
plot(nmds.56.sor)

# PERMANOVA
adonis2(b.56$beta.sor ~ time.56$Decade, permutations = 999)
pairwise.adonis(b.56$beta.sor, time.56$Decade)
adonis2(b.56$beta.sor ~ time.56$Regions, permutations = 999)
pairwise.adonis(b.56$beta.sor, time.56$Regions)

# BETADISPER
beta.sor.56 <- betadisper(b.56$beta.sor, time.56$Decade, type = c("median"))
beta.sor.56
anova(beta.sor.56)
plot(beta.sor.56)
boxplot(beta.sor.56, ylab = "Distance to median")
TukeyHSD(beta.sor.56, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.56.sim <- metaMDS(b.56$beta.sim, trymax = 500, autotransform = TRUE)
nmds.56.sim
stressplot(nmds.56.sim)
goodness(nmds.56.sim)
nmds.56.sim$stress
plot(nmds.56.sim)

# PERMANOVA
adonis2(b.56$beta.sim ~ time.56$Decade, permutations = 999)
pairwise.adonis(b.56$beta.sim, time.56$Decade)
adonis2(b.56$beta.sim ~ time.56$Regions, permutations = 999)
pairwise.adonis(b.56$beta.sim, time.56$Regions)

# BETADISPER
beta.sim.56 <- betadisper(b.56$beta.sim, time.56$Decade, type = c("median"))
beta.sim.56
anova(beta.sim.56)
plot(beta.sim.56)
boxplot(beta.sim.56, ylab = "Distance to median")
TukeyHSD(beta.sim.56, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.56.sne <- metaMDS(b.56$beta.sne, trymax = 500, autotransform = TRUE)
nmds.56.sne
stressplot(nmds.56.sne)
goodness(nmds.56.sne)
nmds.56.sne$stress
plot(nmds.56.sne)

# PERMANOVA
adonis2(b.56$beta.sne ~ time.56$Decade, permutations = 999)
pairwise.adonis(b.56$beta.sne, time.56$Decade)
adonis2(b.56$beta.sne ~ time.56$Regions, permutations = 999)
pairwise.adonis(b.56$beta.sne, time.56$Regions)

# BETADISPER
beta.sne.56 <- betadisper(b.56$beta.sne, time.56$Decade, type = c("median"))
beta.sne.56
anova(beta.sne.56)
plot(beta.sne.56)
boxplot(beta.sne.56, ylab = "Distance to median")
TukeyHSD(beta.sne.56, which = "group", conf.level = 0.95)


# 1940, 1950  ####################################################################################
time.67 <-rbind(time6, time7)
str(time.67)
time.67.core <- betapart.core(time.67[3:29])
b.67 <- beta.pair(time.67.core, index.family = "sorensen")
str(b.67)
b.67.multi <- beta.multi(time.67.core, index.family = "sorensen")
str(b.67.multi)


## beta.sor - total betadiversity
nmds.67.sor <- metaMDS(b.67$beta.sor, trymax = 500, autotransform = TRUE)
nmds.67.sor
stressplot(nmds.67.sor)
goodness(nmds.67.sor)
nmds.67.sor$stress
plot(nmds.67.sor)

# PERMANOVA
adonis2(b.67$beta.sor ~ time.67$Decade, permutations = 999)
pairwise.adonis(b.67$beta.sor, time.67$Decade)
adonis2(b.67$beta.sor ~ time.67$Regions, permutations = 999)
pairwise.adonis(b.67$beta.sor, time.67$Regions)

# BETADISPER
beta.sor.67 <- betadisper(b.67$beta.sor, time.67$Decade, type = c("median"))
beta.sor.67
anova(beta.sor.67)
plot(beta.sor.67)
boxplot(beta.sor.67, ylab = "Distance to median")
TukeyHSD(beta.sor.67, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.67.sim <- metaMDS(b.67$beta.sim, trymax = 500, autotransform = TRUE)
nmds.67.sim
stressplot(nmds.67.sim)
goodness(nmds.67.sim)
nmds.67.sim$stress
plot(nmds.67.sim)

# PERMANOVA
adonis2(b.67$beta.sim ~ time.67$Decade, permutations = 999)
pairwise.adonis(b.67$beta.sim, time.67$Decade)
adonis2(b.67$beta.sim ~ time.67$Regions, permutations = 999)
pairwise.adonis(b.67$beta.sim, time.67$Regions)

# BETADISPER
beta.sim.67 <- betadisper(b.67$beta.sim, time.67$Decade, type = c("median"))
beta.sim.67
anova(beta.sim.67)
plot(beta.sim.67)
boxplot(beta.sim.67, ylab = "Distance to median")
TukeyHSD(beta.sim.67, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.67.sne <- metaMDS(b.67$beta.sne, trymax = 500, autotransform = TRUE)
nmds.67.sne
stressplot(nmds.67.sne)
goodness(nmds.67.sne)
nmds.67.sne$stress
plot(nmds.67.sne)

# PERMANOVA
adonis2(b.67$beta.sne ~ time.67$Decade, permutations = 999)
pairwise.adonis(b.67$beta.sne, time.67$Decade)
adonis2(b.67$beta.sne ~ time.67$Regions, permutations = 999)
pairwise.adonis(b.67$beta.sne, time.67$Regions)

# BETADISPER
beta.sne.67 <- betadisper(b.67$beta.sne, time.67$Decade, type = c("median"))
beta.sne.67
anova(beta.sne.67)
plot(beta.sne.67)
boxplot(beta.sne.67, ylab = "Distance to median")
TukeyHSD(beta.sne.67, which = "group", conf.level = 0.95)


# 1950, 1960  ####################################################################################
time.78 <-rbind(time7, time8)
str(time.78)
time.78.core <- betapart.core(time.78[3:29])
b.78 <- beta.pair(time.78.core, index.family = "sorensen")
str(b.78)
b.78.multi <- beta.multi(time.78.core, index.family = "sorensen")
str(b.78.multi)


## beta.sor - total betadiversity
nmds.78.sor <- metaMDS(b.78$beta.sor, trymax = 500, autotransform = TRUE)
nmds.78.sor
stressplot(nmds.78.sor)
goodness(nmds.78.sor)
nmds.78.sor$stress
plot(nmds.78.sor)

# PERMANOVA
adonis2(b.78$beta.sor ~ time.78$Decade, permutations = 999)
pairwise.adonis(b.78$beta.sor, time.78$Decade)
adonis2(b.78$beta.sor ~ time.78$Regions, permutations = 999)
pairwise.adonis(b.78$beta.sor, time.78$Regions)

# BETADISPER
beta.sor.78 <- betadisper(b.78$beta.sor, time.78$Decade, type = c("median"))
beta.sor.78
anova(beta.sor.78)
plot(beta.sor.78)
boxplot(beta.sor.78, ylab = "Distance to median")
TukeyHSD(beta.sor.78, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.78.sim <- metaMDS(b.78$beta.sim, trymax = 500, autotransform = TRUE)
nmds.78.sim
stressplot(nmds.78.sim)
goodness(nmds.78.sim)
nmds.78.sim$stress
plot(nmds.78.sim)

# PERMANOVA
adonis2(b.78$beta.sim ~ time.78$Decade, permutations = 999)
pairwise.adonis(b.78$beta.sim, time.78$Decade)
adonis2(b.78$beta.sim ~ time.78$Regions, permutations = 999)
pairwise.adonis(b.78$beta.sim, time.78$Regions)

# BETADISPER
beta.sim.78 <- betadisper(b.78$beta.sim, time.78$Decade, type = c("median"))
beta.sim.78
anova(beta.sim.78)
plot(beta.sim.78)
boxplot(beta.sim.78, ylab = "Distance to median")
TukeyHSD(beta.sim.78, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.78.sne <- metaMDS(b.78$beta.sne, trymax = 500, autotransform = TRUE)
nmds.78.sne
stressplot(nmds.78.sne)
goodness(nmds.78.sne)
nmds.78.sne$stress
plot(nmds.78.sne)

# PERMANOVA
adonis2(b.78$beta.sne ~ time.78$Decade, permutations = 999)
pairwise.adonis(b.78$beta.sne, time.78$Decade)
adonis2(b.78$beta.sne ~ time.78$Regions, permutations = 999)
pairwise.adonis(b.78$beta.sne, time.78$Regions)

# BETADISPER
beta.sne.78 <- betadisper(b.78$beta.sne, time.78$Decade, type = c("median"))
beta.sne.78
anova(beta.sne.78)
plot(beta.sne.78)
boxplot(beta.sne.78, ylab = "Distance to median")
TukeyHSD(beta.sne.78, which = "group", conf.level = 0.95)


# 1960, 1970  ####################################################################################
time.89 <-rbind(time8, time9)
str(time.89)
time.89.core <- betapart.core(time.89[3:29])
b.89 <- beta.pair(time.89.core, index.family = "sorensen")
str(b.89)
b.89.multi <- beta.multi(time.89.core, index.family = "sorensen")
str(b.89.multi)


## beta.sor - total betadiversity
nmds.89.sor <- metaMDS(b.89$beta.sor, trymax = 500, autotransform = TRUE)
nmds.89.sor
stressplot(nmds.89.sor)
goodness(nmds.89.sor)
nmds.89.sor$stress
plot(nmds.89.sor)

# PERMANOVA
adonis2(b.89$beta.sor ~ time.89$Decade, permutations = 999)
pairwise.adonis(b.89$beta.sor, time.89$Decade)
adonis2(b.89$beta.sor ~ time.89$Regions, permutations = 999)
pairwise.adonis(b.89$beta.sor, time.89$Regions)

# BETADISPER
beta.sor.89 <- betadisper(b.89$beta.sor, time.89$Decade, type = c("median"))
beta.sor.89
anova(beta.sor.89)
plot(beta.sor.89)
boxplot(beta.sor.89, ylab = "Distance to median")
TukeyHSD(beta.sor.89, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.89.sim <- metaMDS(b.89$beta.sim, trymax = 500, autotransform = TRUE)
nmds.89.sim
stressplot(nmds.89.sim)
goodness(nmds.89.sim)
nmds.89.sim$stress
plot(nmds.89.sim)

# PERMANOVA
adonis2(b.89$beta.sim ~ time.89$Decade, permutations = 999)
pairwise.adonis(b.89$beta.sim, time.89$Decade)
adonis2(b.89$beta.sim ~ time.89$Regions, permutations = 999)
pairwise.adonis(b.89$beta.sim, time.89$Regions)

# BETADISPER
beta.sim.89 <- betadisper(b.89$beta.sim, time.89$Decade, type = c("median"))
beta.sim.89
anova(beta.sim.89)
plot(beta.sim.89)
boxplot(beta.sim.89, ylab = "Distance to median")
TukeyHSD(beta.sim.89, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.89.sne <- metaMDS(b.89$beta.sne, trymax = 500, autotransform = TRUE)
nmds.89.sne
stressplot(nmds.89.sne)
goodness(nmds.89.sne)
nmds.89.sne$stress
plot(nmds.89.sne)

# PERMANOVA
adonis2(b.89$beta.sne ~ time.89$Decade, permutations = 999)
pairwise.adonis(b.89$beta.sne, time.89$Decade)
adonis2(b.89$beta.sne ~ time.89$Regions, permutations = 999)
pairwise.adonis(b.89$beta.sne, time.89$Regions)

# BETADISPER
beta.sne.89 <- betadisper(b.89$beta.sne, time.89$Decade, type = c("median"))
beta.sne.89
anova(beta.sne.89)
plot(beta.sne.89)
boxplot(beta.sne.89, ylab = "Distance to median")
TukeyHSD(beta.sne.89, which = "group", conf.level = 0.95)


# 1970, 1980  ####################################################################################
time.910 <-rbind(time9, time10)
str(time.910)
time.910.core <- betapart.core(time.910[3:29])
b.910 <- beta.pair(time.910.core, index.family = "sorensen")
str(b.910)
b.910.multi <- beta.multi(time.910.core, index.family = "sorensen")
str(b.910.multi)


## beta.sor - total betadiversity
nmds.910.sor <- metaMDS(b.910$beta.sor, trymax = 500, autotransform = TRUE)
nmds.910.sor
stressplot(nmds.910.sor)
goodness(nmds.910.sor)
nmds.910.sor$stress
plot(nmds.910.sor)

# PERMANOVA
adonis2(b.910$beta.sor ~ time.910$Decade, permutations = 999)
pairwise.adonis(b.910$beta.sor, time.910$Decade)
adonis2(b.910$beta.sor ~ time.910$Regions, permutations = 999)
pairwise.adonis(b.910$beta.sor, time.910$Regions)

# BETADISPER
beta.sor.910 <- betadisper(b.910$beta.sor, time.910$Decade, type = c("median"))
beta.sor.910
anova(beta.sor.910)
plot(beta.sor.910)
boxplot(beta.sor.910, ylab = "Distance to median")
TukeyHSD(beta.sor.910, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.910.sim <- metaMDS(b.910$beta.sim, trymax = 500, autotransform = TRUE)
nmds.910.sim
stressplot(nmds.910.sim)
goodness(nmds.910.sim)
nmds.910.sim$stress
plot(nmds.910.sim)

# PERMANOVA
adonis2(b.910$beta.sim ~ time.910$Decade, permutations = 999)
pairwise.adonis(b.910$beta.sim, time.910$Decade)
adonis2(b.910$beta.sim ~ time.910$Regions, permutations = 999)
pairwise.adonis(b.910$beta.sim, time.910$Regions)

# BETADISPER
beta.sim.910 <- betadisper(b.910$beta.sim, time.910$Decade, type = c("median"))
beta.sim.910
anova(beta.sim.910)
plot(beta.sim.910)
boxplot(beta.sim.910, ylab = "Distance to median")
TukeyHSD(beta.sim.910, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.910.sne <- metaMDS(b.910$beta.sne, trymax = 500, autotransform = TRUE)
nmds.910.sne
stressplot(nmds.910.sne)
goodness(nmds.910.sne)
nmds.910.sne$stress
plot(nmds.910.sne)

# PERMANOVA
adonis2(b.910$beta.sne ~ time.910$Decade, permutations = 999)
pairwise.adonis(b.910$beta.sne, time.910$Decade)
adonis2(b.910$beta.sne ~ time.910$Regions, permutations = 999)
pairwise.adonis(b.910$beta.sne, time.910$Regions)

# BETADISPER
beta.sne.910 <- betadisper(b.910$beta.sne, time.910$Decade, type = c("median"))
beta.sne.910
anova(beta.sne.910)
plot(beta.sne.910)
boxplot(beta.sne.910, ylab = "Distance to median")
TukeyHSD(beta.sne.910, which = "group", conf.level = 0.95)


# 1980, 1990  ####################################################################################
time.1011 <-rbind(time10, time11)
str(time.1011)
time.1011.core <- betapart.core(time.1011[3:29])
b.1011 <- beta.pair(time.1011.core, index.family = "sorensen")
str(b.1011)
b.1011.multi <- beta.multi(time.1011.core, index.family = "sorensen")
str(b.1011.multi)


## beta.sor - total betadiversity
nmds.1011.sor <- metaMDS(b.1011$beta.sor, trymax = 500, autotransform = TRUE)
nmds.1011.sor
stressplot(nmds.1011.sor)
goodness(nmds.1011.sor)
nmds.1011.sor$stress
plot(nmds.1011.sor)

# PERMANOVA
adonis2(b.1011$beta.sor ~ time.1011$Decade, permutations = 999)
pairwise.adonis(b.1011$beta.sor, time.1011$Decade)
adonis2(b.1011$beta.sor ~ time.1011$Regions, permutations = 999)
pairwise.adonis(b.1011$beta.sor, time.1011$Regions)

# BETADISPER
beta.sor.1011 <- betadisper(b.1011$beta.sor, time.1011$Decade, type = c("median"))
beta.sor.1011
anova(beta.sor.1011)
plot(beta.sor.1011)
boxplot(beta.sor.1011, ylab = "Distance to median")
TukeyHSD(beta.sor.1011, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.1011.sim <- metaMDS(b.1011$beta.sim, trymax = 500, autotransform = TRUE)
nmds.1011.sim
stressplot(nmds.1011.sim)
goodness(nmds.1011.sim)
nmds.1011.sim$stress
plot(nmds.1011.sim)

# PERMANOVA
adonis2(b.1011$beta.sim ~ time.1011$Decade, permutations = 999)
pairwise.adonis(b.1011$beta.sim, time.1011$Decade)
adonis2(b.1011$beta.sim ~ time.1011$Regions, permutations = 999)
pairwise.adonis(b.1011$beta.sim, time.1011$Regions)

# BETADISPER
beta.sim.1011 <- betadisper(b.1011$beta.sim, time.1011$Decade, type = c("median"))
beta.sim.1011
anova(beta.sim.1011)
plot(beta.sim.1011)
boxplot(beta.sim.1011, ylab = "Distance to median")
TukeyHSD(beta.sim.1011, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.1011.sne <- metaMDS(b.1011$beta.sne, trymax = 500, autotransform = TRUE)
nmds.1011.sne
stressplot(nmds.1011.sne)
goodness(nmds.1011.sne)
nmds.1011.sne$stress
plot(nmds.1011.sne)

# PERMANOVA
adonis2(b.1011$beta.sne ~ time.1011$Decade, permutations = 999)
pairwise.adonis(b.1011$beta.sne, time.1011$Decade)
adonis2(b.1011$beta.sne ~ time.1011$Regions, permutations = 999)
pairwise.adonis(b.1011$beta.sne, time.1011$Regions)

# BETADISPER
beta.sne.1011 <- betadisper(b.1011$beta.sne, time.1011$Decade, type = c("median"))
beta.sne.1011
anova(beta.sne.1011)
plot(beta.sne.1011)
boxplot(beta.sne.1011, ylab = "Distance to median")
TukeyHSD(beta.sne.1011, which = "group", conf.level = 0.95)


# 1990, 2000  ####################################################################################
time.1112 <-rbind(time11, time12)
str(time.1112)
time.1112.core <- betapart.core(time.1112[3:29])
b.1112 <- beta.pair(time.1112.core, index.family = "sorensen")
str(b.1112)
b.1112.multi <- beta.multi(time.1112.core, index.family = "sorensen")
str(b.1112.multi)


## beta.sor - total betadiversity
nmds.1112.sor <- metaMDS(b.1112$beta.sor, trymax = 500, autotransform = TRUE)
nmds.1112.sor
stressplot(nmds.1112.sor)
goodness(nmds.1112.sor)
nmds.1112.sor$stress
plot(nmds.1112.sor)

# PERMANOVA
adonis2(b.1112$beta.sor ~ time.1112$Decade, permutations = 999)
pairwise.adonis(b.1112$beta.sor, time.1112$Decade)
adonis2(b.1112$beta.sor ~ time.1112$Regions, permutations = 999)
pairwise.adonis(b.1112$beta.sor, time.1112$Regions)

# BETADISPER
beta.sor.1112 <- betadisper(b.1112$beta.sor, time.1112$Decade, type = c("median"))
beta.sor.1112
anova(beta.sor.1112)
plot(beta.sor.1112)
boxplot(beta.sor.1112, ylab = "Distance to median")
TukeyHSD(beta.sor.1112, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.1112.sim <- metaMDS(b.1112$beta.sim, trymax = 500, autotransform = TRUE)
nmds.1112.sim
stressplot(nmds.1112.sim)
goodness(nmds.1112.sim)
nmds.1112.sim$stress
plot(nmds.1112.sim)

# PERMANOVA
adonis2(b.1112$beta.sim ~ time.1112$Decade, permutations = 999)
pairwise.adonis(b.1112$beta.sim, time.1112$Decade)
adonis2(b.1112$beta.sim ~ time.1112$Regions, permutations = 999)
pairwise.adonis(b.1112$beta.sim, time.1112$Regions)

# BETADISPER
beta.sim.1112 <- betadisper(b.1112$beta.sim, time.1112$Decade, type = c("median"))
beta.sim.1112
anova(beta.sim.1112)
plot(beta.sim.1112)
boxplot(beta.sim.1112, ylab = "Distance to median")
TukeyHSD(beta.sim.1112, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.1112.sne <- metaMDS(b.1112$beta.sne, trymax = 500, autotransform = TRUE)
nmds.1112.sne
stressplot(nmds.1112.sne)
goodness(nmds.1112.sne)
nmds.1112.sne$stress
plot(nmds.1112.sne)

# PERMANOVA
adonis2(b.1112$beta.sne ~ time.1112$Decade, permutations = 999)
pairwise.adonis(b.1112$beta.sne, time.1112$Decade)
adonis2(b.1112$beta.sne ~ time.1112$Regions, permutations = 999)
pairwise.adonis(b.1112$beta.sne, time.1112$Regions)

# BETADISPER
beta.sne.1112 <- betadisper(b.1112$beta.sne, time.1112$Decade, type = c("median"))
beta.sne.1112
anova(beta.sne.1112)
plot(beta.sne.1112)
boxplot(beta.sne.1112, ylab = "Distance to median")
TukeyHSD(beta.sne.1112, which = "group", conf.level = 0.95)


# 2000, 2010  ####################################################################################
time.1213 <-rbind(time12, time13)
str(time.1213)
time.1213.core <- betapart.core(time.1213[3:29])
b.1213 <- beta.pair(time.1213.core, index.family = "sorensen")
str(b.1213)
b.1213.multi <- beta.multi(time.1213.core, index.family = "sorensen")
str(b.1213.multi)


## beta.sor - total betadiversity
nmds.1213.sor <- metaMDS(b.1213$beta.sor, trymax = 500, autotransform = TRUE)
nmds.1213.sor
stressplot(nmds.1213.sor)
goodness(nmds.1213.sor)
nmds.1213.sor$stress
plot(nmds.1213.sor)

# PERMANOVA
adonis2(b.1213$beta.sor ~ time.1213$Decade, permutations = 999)
pairwise.adonis(b.1213$beta.sor, time.1213$Decade)
adonis2(b.1213$beta.sor ~ time.1213$Regions, permutations = 999)
pairwise.adonis(b.1213$beta.sor, time.1213$Regions)

# BETADISPER
beta.sor.1213 <- betadisper(b.1213$beta.sor, time.1213$Decade, type = c("median"))
beta.sor.1213
anova(beta.sor.1213)
plot(beta.sor.1213)
boxplot(beta.sor.1213, ylab = "Distance to median")
TukeyHSD(beta.sor.1213, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.1213.sim <- metaMDS(b.1213$beta.sim, trymax = 500, autotransform = TRUE)
nmds.1213.sim
stressplot(nmds.1213.sim)
goodness(nmds.1213.sim)
nmds.1213.sim$stress
plot(nmds.1213.sim)

# PERMANOVA
adonis2(b.1213$beta.sim ~ time.1213$Decade, permutations = 999)
pairwise.adonis(b.1213$beta.sim, time.1213$Decade)
adonis2(b.1213$beta.sim ~ time.1213$Regions, permutations = 999)
pairwise.adonis(b.1213$beta.sim, time.1213$Regions)

# BETADISPER
beta.sim.1213 <- betadisper(b.1213$beta.sim, time.1213$Decade, type = c("median"))
beta.sim.1213
anova(beta.sim.1213)
plot(beta.sim.1213)
boxplot(beta.sim.1213, ylab = "Distance to median")
TukeyHSD(beta.sim.1213, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.1213.sne <- metaMDS(b.1213$beta.sne, trymax = 500, autotransform = TRUE)
nmds.1213.sne
stressplot(nmds.1213.sne)
goodness(nmds.1213.sne)
nmds.1213.sne$stress
plot(nmds.1213.sne)

# PERMANOVA
adonis2(b.1213$beta.sne ~ time.1213$Decade, permutations = 999)
pairwise.adonis(b.1213$beta.sne, time.1213$Decade)
adonis2(b.1213$beta.sne ~ time.1213$Regions, permutations = 999)
pairwise.adonis(b.1213$beta.sne, time.1213$Regions)

# BETADISPER
beta.sne.1213 <- betadisper(b.1213$beta.sne, time.1213$Decade, type = c("median"))
beta.sne.1213
anova(beta.sne.1213)
plot(beta.sne.1213)
boxplot(beta.sne.1213, ylab = "Distance to median")
TukeyHSD(beta.sne.1213, which = "group", conf.level = 0.95)






################################################################################
# Which species are contributing to these patterns?

colnames(lb)

lb.b <- lb[,c(2,15,19)]

# subset data to look at only museum specimens (i.e. remove BLBB data)
lb.b <- lb.b[c(1:4210),]
lb.b <- lb.b[,-3]

colnames(lb.b)

lb.b.matrix <- dcast(lb.b, Decade ~ Name, length)
str(lb.b.matrix)

lb.b.matrix$Decade <- as.factor(lb.b.matrix$Decade)

levels(lb.b.matrix$Decade)
str(lb.b.matrix)

write.csv(lb.b.matrix, file = "collections by decade.csv")

lb.2 <- read.csv("collections by decade.csv")

str(lb.2)

png("Adalia bipunctata.png")
barplot(Abipunctata ~ Decade, data = lb.2)
dev.off()

png("Anatis labiculata.png")
barplot(Alabiculata ~ Decade, data = lb.2)
dev.off()

png("Anatis mali.png")
barplot(Amali ~ Decade, data = lb.2)
dev.off()

png("Anisosticta bitriangularis.png")
barplot(Abitriangularis ~ Decade, data = lb.2)
dev.off()

png("Brachiacantha ursina.png")
barplot(Bursina ~ Decade, data = lb.2)
dev.off()

png("Ceratomegilla fuscilabris.png")
barplot(Cfuscilabris ~ Decade, data = lb.2)
dev.off()

png("Chilocorus stigma.png")
barplot(Cstigma ~ Decade, data = lb.2)
dev.off()

png("Coccinella novemnotata.png")
barplot(Cnovemnotata ~ Decade, data = lb.2)
dev.off()

png("Coccinella septempunctata.png")
barplot(Cseptem ~ Decade, data = lb.2)
dev.off()

png("Coccinella transversoguttata.png")
barplot(Ctransvers ~ Decade, data = lb.2)
dev.off()

png("Coccinella trifasciata.png")
barplot(Ctrifasciata ~ Decade, data = lb.2)
dev.off()

png("Coccinella undecimpunctata.png")
barplot(Cundecim ~ Decade, data = lb.2)
dev.off()

png("Coleomegilla maculata.png")
barplot(Cmaculata ~ Decade, data = lb.2)
dev.off()

png("Cycloneda munda.png")
barplot(Cmunda ~ Decade, data = lb.2)
dev.off()

png("Cycloneda sanguinea.png")
barplot(Csanguinea ~ Decade, data = lb.2)
dev.off()

png("Harmonia axyridis.png")
barplot(Haxyridis ~ Decade, data = lb.2)
dev.off()

png("Hippodamia convergens.png")
barplot(Hconvergens ~ Decade, data = lb.2)
dev.off()

png("Hippodamia glacialis.png")
barplot(Hglacialis ~ Decade, data = lb.2)
dev.off()

png("Hippodamia parenthesis.png")
barplot(Hparenthesis ~ Decade, data = lb.2)
dev.off()

png("Hippodamia quindecimmaculata.png")
barplot(Hquindec ~ Decade, data = lb.2)
dev.off()

png("Hippodamia transversoguttata.png")
barplot(Htransver ~ Decade, data = lb.2)
dev.off()

png("Hippodamia tredecimpunctata.png")
barplot(Htredecim ~ Decade, data = lb.2)
dev.off()

png("Hippodamia variegata.png")
barplot(Hvariegata ~ Decade, data = lb.2)
dev.off()

png("Hyperaspis undulata.png")
barplot(Hundulata ~ Decade, data = lb.2)
dev.off()

png("Mulsantina luteodorsa.png")
barplot(Mluteodorsa ~ Decade, data = lb.2)
dev.off()

png("Mulsantina picta.png")
barplot(Mpicta ~ Decade, data = lb.2)
dev.off()

png("Myzia pullata.png")
barplot(Mpullata ~ Decade, data = lb.2)
dev.off()

png("Neoharmonia venusta.png")
barplot(Nvenusta ~ Decade, data = lb.2)
dev.off()

png("Olla v-nigrum.png")
barplot(Ovnigrum ~ Decade, data = lb.2)
dev.off()

png("Propylea quatuordecimpunctata.png")
barplot(Pquatuor ~ Decade, data = lb.2)
dev.off()

png("Psyllobora vigintimaculata.png")
barplot(Pviginti ~ Decade, data = lb.2)
dev.off()


##################################################################################



