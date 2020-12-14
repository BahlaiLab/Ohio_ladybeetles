###################################################################################
#
# Lady beetle museum data
#
# Restructure data set
#
# Assess taxonomic beta-diversity over time (by decade) and partition beta-diversity into
# nestedeness and turnover components
#
# Analyses for all lady beetle species and for aphidophagous species
#
# KI Perry; 1 December 2020
#
###################################################################################

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

###################################################################################
# Question: Does lady beetle taxonomic beta-diversity change over time, and if so, are these
# patterns driven by species turnover, species nestedness, or both?

# first, investigate descriptive patterns over time across the state of Ohio
lb.b <- lb[,c(2,15,19)]

# subset data to look at only museum specimens (i.e. remove BLBB data)
lb.b <- lb.b[c(1:4210),]
lb.b <- lb.b[,-3]

colnames(lb.b)

lb.b.matrix <- dcast(lb.b, Decade ~ Name, length)
str(lb.b.matrix)

# remove 1890 due to few collections
lb.b.matrix <- lb.b.matrix[-1,]
str(lb.b.matrix)

# remove single collection of Mulsantina luteodorsa because it hasn't been
# verified by LB experts
lb.b.matrix <- lb.b.matrix[,-23]
str(lb.b.matrix)

# change decade variable to factor
lb.b.matrix$Decade <- as.factor(lb.b.matrix$Decade)
levels(lb.b.matrix$Decade)

# change dataset to presence/absence (1/0)
lb.b.matrix[lb.b.matrix>0]<-1
str(lb.b.matrix)

# add a column for site (i.e. Ohio) so can make row names later
lb.b.matrix$site <- c("OH", "OH", "OH", "OH", "OH", "OH", "OH", "OH", "OH", "OH", "OH", "OH")
str(lb.b.matrix)

#### Assess descriptive patterns for changes in total lady beetle beta diversity
#### across decades in Ohio

# pull out data from each decade
# remove factor columns, change site to row names

time1 <- lb.b.matrix[which(lb.b.matrix$Decade == "1900"),]
rownames(time1) <- time1[,29]
time1 <- time1[,-1]
time1 <- time1[,-28]
str(time1)

time2 <- lb.b.matrix[which(lb.b.matrix$Decade == "1910"),]
rownames(time2) <- time2[,29]
time2 <- time2[,-1]
time2 <- time2[,-28]
str(time2)

time3 <- lb.b.matrix[which(lb.b.matrix$Decade == "1920"),]
rownames(time3) <- time3[,29]
time3 <- time3[,-1]
time3 <- time3[,-28]
str(time3)

time4 <- lb.b.matrix[which(lb.b.matrix$Decade == "1930"),]
rownames(time4) <- time4[,29]
time4 <- time4[,-1]
time4 <- time4[,-28]
str(time4)

time5 <- lb.b.matrix[which(lb.b.matrix$Decade == "1940"),]
rownames(time5) <- time5[,29]
time5 <- time5[,-1]
time5 <- time5[,-28]
str(time5)

time6 <- lb.b.matrix[which(lb.b.matrix$Decade == "1950"),]
rownames(time6) <- time6[,29]
time6 <- time6[,-1]
time6 <- time6[,-28]
str(time6)

time7 <- lb.b.matrix[which(lb.b.matrix$Decade == "1960"),]
rownames(time7) <- time7[,29]
time7 <- time7[,-1]
time7 <- time7[,-28]
str(time7)

time8 <- lb.b.matrix[which(lb.b.matrix$Decade == "1970"),]
rownames(time8) <- time8[,29]
time8 <- time8[,-1]
time8 <- time8[,-28]
str(time8)

time9 <- lb.b.matrix[which(lb.b.matrix$Decade == "1980"),]
rownames(time9) <- time9[,29]
time9 <- time9[,-1]
time9 <- time9[,-28]
str(time9)

time10 <- lb.b.matrix[which(lb.b.matrix$Decade == "1990"),]
rownames(time10) <- time10[,29]
time10 <- time10[,-1]
time10 <- time10[,-28]
str(time10)

time11 <- lb.b.matrix[which(lb.b.matrix$Decade == "2000"),]
rownames(time11) <- time11[,29]
time11 <- time11[,-1]
time11 <- time11[,-28]
str(time11)

time12 <- lb.b.matrix[which(lb.b.matrix$Decade == "2010"),]
rownames(time12) <- time12[,29]
time12 <- time12[,-1]
time12 <- time12[,-28]
str(time12)

# create beta part object for each decade

t1 <- betapart.core(time1)
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

# assess temporal change in community composition among the decades for all of Ohio

t12 <- beta.temp(t1, t2, index.family = "sorensen")
t12

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

# outputs from beta.temp used to make figure 2A

#####
# Now, repeat these analyses with only aphidophagous lady beetle species

str(lb.b.matrix)

# remove species that do not primarily feed on aphids

lb.b.matrix.a <- lb.b.matrix[,-3:-4] # Anatis labiculata & Anatis mali
lb.b.matrix.a <- lb.b.matrix.a[,-5] # Chilocorus stigma
lb.b.matrix.a <- lb.b.matrix.a[,-19] # Hyperaspis undulata
lb.b.matrix.a <- lb.b.matrix.a[,-21] # Neoharmonia ventusta
lb.b.matrix.a <- lb.b.matrix.a[,-23] # Psyllobora vigintimaculata

#### Assess descriptive patterns for changes in aphidophagous lady beetle beta diversity
#### across decades in Ohio

# pull out data from each decade
# remove factor columns, change site to row names

time1.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1900"),]
rownames(time1.a) <- time1.a[,23]
time1.a <- time1.a[,-1]
time1.a <- time1.a[,-22]
str(time1.a)

time2.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1910"),]
rownames(time2.a) <- time2.a[,23]
time2.a <- time2.a[,-1]
time2.a <- time2.a[,-22]
str(time2.a)

time3.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1920"),]
rownames(time3.a) <- time3.a[,23]
time3.a <- time3.a[,-1]
time3.a <- time3.a[,-22]
str(time3.a)

time4.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1930"),]
rownames(time4.a) <- time4.a[,23]
time4.a <- time4.a[,-1]
time4.a <- time4.a[,-22]
str(time4.a)

time5.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1940"),]
rownames(time5.a) <- time5.a[,23]
time5.a <- time5.a[,-1]
time5.a <- time5.a[,-22]
str(time5.a)

time6.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1950"),]
rownames(time6.a) <- time6.a[,23]
time6.a <- time6.a[,-1]
time6.a <- time6.a[,-22]
str(time6.a)

time7.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1960"),]
rownames(time7.a) <- time7.a[,23]
time7.a <- time7.a[,-1]
time7.a <- time7.a[,-22]
str(time7.a)

time8.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1970"),]
rownames(time8.a) <- time8.a[,23]
time8.a <- time8.a[,-1]
time8.a <- time8.a[,-22]
str(time8.a)

time9.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1980"),]
rownames(time9.a) <- time9.a[,23]
time9.a <- time9.a[,-1]
time9.a <- time9.a[,-22]
str(time9.a)

time10.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1990"),]
rownames(time10.a) <- time10.a[,23]
time10.a <- time10.a[,-1]
time10.a <- time10.a[,-22]
str(time10.a)

time11.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "2000"),]
rownames(time11.a) <- time11.a[,23]
time11.a <- time11.a[,-1]
time11.a <- time11.a[,-22]
str(time11.a)

time12.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1900"),]
rownames(time12.a) <- time12.a[,23]
time12.a <- time12.a[,-1]
time12.a <- time12.a[,-22]
str(time12.a)

# create beta part object for each decade

t1.a <- betapart.core(time1.a)
t2.a <- betapart.core(time2.a)
t3.a <- betapart.core(time3.a)
t4.a <- betapart.core(time4.a)
t5.a <- betapart.core(time5.a)
t6.a <- betapart.core(time6.a)
t7.a <- betapart.core(time7.a)
t8.a <- betapart.core(time8.a)
t9.a <- betapart.core(time9.a)
t10.a <- betapart.core(time10.a)
t11.a <- betapart.core(time11.a)
t12.a <- betapart.core(time12.a)

# assess temporal change in community composition among the decades for all of Ohio

t12.a <- beta.temp(t1.a, t2.a, index.family = "sorensen")
t12.a

t23.a <- beta.temp(t2.a, t3.a, index.family = "sorensen")
t23.a

t34.a <- beta.temp(t3.a, t4.a, index.family = "sorensen")
t34.a

t45.a <- beta.temp(t4.a, t5.a, index.family = "sorensen")
t45.a

t56.a <- beta.temp(t5.a, t6.a, index.family = "sorensen")
t56.a

t67.a <- beta.temp(t6.a, t7.a, index.family = "sorensen")
t67.a

t78.a <- beta.temp(t7.a, t8.a, index.family = "sorensen")
t78.a

t89.a <- beta.temp(t8.a, t9.a, index.family = "sorensen")
t89.a

t910.a <- beta.temp(t9.a, t10.a, index.family = "sorensen")
t910.a

t1011.a <- beta.temp(t10.a, t11.a, index.family = "sorensen")
t1011.a

t1112.a <- beta.temp(t11.a, t12.a, index.family = "sorensen")
t1112.a

# outputs from beta.temp used to make figure 2B

# remove all objects in workspace
rm(list=ls())

#########################################################################################
#########################################################################################
#########################################################################################

# Next, investigate changes in LB community composition among decades
# and regions in Ohio

# start with all lady beetle species

lb <- read.csv("LB_MuseumData_2020_v2.csv")

colnames(lb)

lb.b <- lb[,c(2,15,7,16,19)]

# subset data to look at only museum specimens (i.e. remove BLBB data)
lb.b <- lb.b[c(1:4210),]
lb.b <- lb.b[,-5]

colnames(lb.b)

lb.b.matrix <- dcast(lb.b, Decade + Regions + HDecade ~ Name, length)
str(lb.b.matrix)

# remove 1890 due to few collections
lb.b.matrix <- lb.b.matrix[-1:-3,]
str(lb.b.matrix)

# remove single collection of Mulsantina luteodorsa because it hasn't been
# verified by LB experts
lb.b.matrix <- lb.b.matrix[,-25]
str(lb.b.matrix)

# make sure predictor variables are factors
lb.b.matrix$Decade <- as.factor(lb.b.matrix$Decade)
lb.b.matrix$Regions <- as.factor(lb.b.matrix$Regions)
lb.b.matrix$HDecade <- as.factor(lb.b.matrix$HDecade)

levels(lb.b.matrix$Decade)
levels(lb.b.matrix$Regions)
levels(lb.b.matrix$HDecade)
str(lb.b.matrix)

# change dataset to presence/absence (1/0)
lb.b.matrix[lb.b.matrix>0]<-1
str(lb.b.matrix)

# pull out data from each decade
# data pooled by half decade and region

time1 <- lb.b.matrix[which(lb.b.matrix$Decade == "1900"),]
str(time1)

time2 <- lb.b.matrix[which(lb.b.matrix$Decade == "1910"),]
str(time2)

time3 <- lb.b.matrix[which(lb.b.matrix$Decade == "1920"),]
str(time3)

time4 <- lb.b.matrix[which(lb.b.matrix$Decade == "1930"),]
str(time4)

time5 <- lb.b.matrix[which(lb.b.matrix$Decade == "1940"),]
str(time5)

time6 <- lb.b.matrix[which(lb.b.matrix$Decade == "1950"),]
str(time6)

time7 <- lb.b.matrix[which(lb.b.matrix$Decade == "1960"),]
str(time7)

time8 <- lb.b.matrix[which(lb.b.matrix$Decade == "1970"),]
str(time8)

time9 <- lb.b.matrix[which(lb.b.matrix$Decade == "1980"),]
str(time9)

time10 <- lb.b.matrix[which(lb.b.matrix$Decade == "1990"),]
str(time10)

time11 <- lb.b.matrix[which(lb.b.matrix$Decade == "2000"),]
str(time11)

time12 <- lb.b.matrix[which(lb.b.matrix$Decade == "2010"),]
str(time12)

# merge datasets to facilitate comparisons over time
# calculate betapart.core object for the two time intervals
# and then calculate pairwise betadiveristy among regions and decades
# run NMDS models and plot figures for each beta-diversity component

# PERMANOVA tests whether the group centroid of beetle communities
# differs in multivariate space (e.g. different community composition)

# BETADISPER tests whether the dispersion of a treatment from its spatial median is different
# between groups (i.e. species redundancy across space)
# analysis of multivariate homogeneity of group dispersions (variances)
# multivariate analogue of Levene's test for homogenetiy of variances

# 1900, 1910  ####################################################################################
time.12 <-rbind(time1, time2)
str(time.12)
time.12.core <- betapart.core(time.12[4:30])
b.12 <- beta.pair(time.12.core, index.family = "sorensen")
str(b.12)

## beta.sor - total betadiversity
nmds.12.sor <- metaMDS(b.12$beta.sor, trymax = 500, autotransform = TRUE)
nmds.12.sor
stressplot(nmds.12.sor)
goodness(nmds.12.sor)
nmds.12.sor$stress
plot(nmds.12.sor)

# PERMANOVA
adonis2(b.12$beta.sor ~ time.12$Decade, permutations = 999)
pairwise.adonis(b.12$beta.sor, time.12$Decade)
adonis2(b.12$beta.sor ~ time.12$Regions, permutations = 999)
pairwise.adonis(b.12$beta.sor, time.12$Regions)

# BETADISPER
beta.sor.12 <- betadisper(b.12$beta.sor, time.12$Decade, type = c("median"))
beta.sor.12
anova(beta.sor.12)
plot(beta.sor.12)
boxplot(beta.sor.12, ylab = "Distance to median")
TukeyHSD(beta.sor.12, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.12.sim <- metaMDS(b.12$beta.sim, trymax = 500, autotransform = TRUE)
nmds.12.sim
stressplot(nmds.12.sim)
goodness(nmds.12.sim)
nmds.12.sim$stress
plot(nmds.12.sim)

# PERMANOVA
adonis2(b.12$beta.sim ~ time.12$Decade, permutations = 999)
pairwise.adonis(b.12$beta.sim, time.12$Decade)
adonis2(b.12$beta.sim ~ time.12$Regions, permutations = 999)
pairwise.adonis(b.12$beta.sim, time.12$Regions)

# BETADISPER
beta.sim.12 <- betadisper(b.12$beta.sim, time.12$Decade, type = c("median"))
beta.sim.12
anova(beta.sim.12)
plot(beta.sim.12)
boxplot(beta.sim.12, ylab = "Distance to median")
TukeyHSD(beta.sim.12, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.12.sne <- metaMDS(b.12$beta.sne, trymax = 500, autotransform = TRUE)
nmds.12.sne
stressplot(nmds.12.sne)
goodness(nmds.12.sne)
nmds.12.sne$stress
plot(nmds.12.sne)

# PERMANOVA
adonis2(b.12$beta.sne ~ time.12$Decade, permutations = 999)
pairwise.adonis(b.12$beta.sne, time.12$Decade)
adonis2(b.12$beta.sne ~ time.12$Regions, permutations = 999)
pairwise.adonis(b.12$beta.sne, time.12$Regions)

# BETADISPER
beta.sne.12 <- betadisper(b.12$beta.sne, time.12$Decade, type = c("median"))
beta.sne.12
anova(beta.sne.12)
plot(beta.sne.12)
boxplot(beta.sne.12, ylab = "Distance to median")
TukeyHSD(beta.sne.12, which = "group", conf.level = 0.95)

# 1910, 1920  ####################################################################################
time.23 <-rbind(time2, time3)
str(time.23)
time.23.core <- betapart.core(time.23[4:30])
b.23 <- beta.pair(time.23.core, index.family = "sorensen")
str(b.23)

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

# 1920. 1930  ####################################################################################
time.34 <-rbind(time3, time4)
str(time.34)
time.34.core <- betapart.core(time.34[4:30])
b.34 <- beta.pair(time.34.core, index.family = "sorensen")
str(b.34)

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

# 1930, 1940  ####################################################################################
time.45 <-rbind(time4, time5)
str(time.45)
time.45.core <- betapart.core(time.45[4:30])
b.45 <- beta.pair(time.45.core, index.family = "sorensen")
str(b.45)

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


# 1940, 1950  ####################################################################################
time.56 <-rbind(time5, time6)
str(time.56)
time.56.core <- betapart.core(time.56[4:30])
b.56 <- beta.pair(time.56.core, index.family = "sorensen")
str(b.56)

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

# 1950, 1960  ####################################################################################
time.67 <-rbind(time6, time7)
str(time.67)
time.67.core <- betapart.core(time.67[4:30])
b.67 <- beta.pair(time.67.core, index.family = "sorensen")
str(b.67)

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


# 1960, 1970  ####################################################################################
time.78 <-rbind(time7, time8)
str(time.78)
time.78.core <- betapart.core(time.78[4:30])
b.78 <- beta.pair(time.78.core, index.family = "sorensen")
str(b.78)

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


# 1970, 1980  ####################################################################################
time.89 <-rbind(time8, time9)
str(time.89)
time.89.core <- betapart.core(time.89[4:30])
b.89 <- beta.pair(time.89.core, index.family = "sorensen")
str(b.89)

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


# 1980, 1990  ####################################################################################
time.910 <-rbind(time9, time10)
str(time.910)
time.910.core <- betapart.core(time.910[4:30])
b.910 <- beta.pair(time.910.core, index.family = "sorensen")
str(b.910)

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


# 1990, 2000  ####################################################################################
time.1011 <-rbind(time10, time11)
str(time.1011)
time.1011.core <- betapart.core(time.1011[4:30])
b.1011 <- beta.pair(time.1011.core, index.family = "sorensen")
str(b.1011)

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


# 2000, 2010  ####################################################################################
time.1112 <-rbind(time11, time12)
str(time.1112)
time.1112.core <- betapart.core(time.1112[4:30])
b.1112 <- beta.pair(time.1112.core, index.family = "sorensen")
str(b.1112)

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


######
######
######
# Now, repeat these analyses with only aphidophagous lady beetle species

str(lb.b.matrix)

# remove species that do not primarily feed on aphids

lb.b.matrix.a <- lb.b.matrix[,-5:-6] # Anatis labiculata & Anatis mali
lb.b.matrix.a <- lb.b.matrix.a[,-7] # Chilocorus stigma
lb.b.matrix.a <- lb.b.matrix.a[,-21] # Hyperaspis undulata
lb.b.matrix.a <- lb.b.matrix.a[,-23] # Neoharmonia ventusta
lb.b.matrix.a <- lb.b.matrix.a[,-25] # Psyllobora vigintimaculata

# pull out data from each decade
# data pooled by half decade and region

time1.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1900"),]
str(time1.a)

time2.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1910"),]
str(time2.a)

time3.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1920"),]
str(time3.a)

time4.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1930"),]
str(time4.a)

time5.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1940"),]
str(time5.a)

time6.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1950"),]
str(time6.a)

time7.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1960"),]
str(time7.a)

time8.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1970"),]
str(time8.a)

time9.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1980"),]
str(time9.a)

time10.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "1990"),]
str(time10.a)

time11.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "2000"),]
str(time11.a)

time12.a <- lb.b.matrix.a[which(lb.b.matrix.a$Decade == "2010"),]
str(time12.a)

# merge datasets to facilitate comparisons over time
# calculate betapart.core object for the two time intervals
# and then calculate pairwise betadiveristy among regions and decades
# run NMDS models and plot figures for each beta-diversity component

# PERMANOVA tests whether the group centroid of beetle communities
# differs in multivariate space (e.g. different community composition)

# BETADISPER tests whether the dispersion of a treatment from its spatial median is different
# between groups (i.e. species redundancy across space)
# analysis of multivariate homogeneity of group dispersions (variances)
# multivariate analogue of Levene's test for homogenetiy of variances

# 1900, 1910  ####################################################################################
time.12.a <-rbind(time1.a, time2.a)
str(time.12.a)
time.12.a.core <- betapart.core(time.12.a[4:24])
b.12.a <- beta.pair(time.12.a.core, index.family = "sorensen")
str(b.12.a)

## beta.sor - total betadiversity
nmds.12.a.sor <- metaMDS(b.12.a$beta.sor, trymax = 500, autotransform = TRUE)
nmds.12.a.sor
stressplot(nmds.12.a.sor)
goodness(nmds.12.a.sor)
nmds.12.a.sor$stress
plot(nmds.12.a.sor)

# PERMANOVA
adonis2(b.12.a$beta.sor ~ time.12.a$Decade, permutations = 999)
pairwise.adonis(b.12.a$beta.sor, time.12.a$Decade)
adonis2(b.12.a$beta.sor ~ time.12.a$Regions, permutations = 999)
pairwise.adonis(b.12.a$beta.sor, time.12.a$Regions)

# BETADISPER
beta.sor.12.a <- betadisper(b.12.a$beta.sor, time.12.a$Decade, type = c("median"))
beta.sor.12.a
anova(beta.sor.12.a)
plot(beta.sor.12.a)
boxplot(beta.sor.12.a, ylab = "Distance to median")
TukeyHSD(beta.sor.12.a, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.12.a.sim <- metaMDS(b.12.a$beta.sim, trymax = 500, autotransform = TRUE)
nmds.12.a.sim
stressplot(nmds.12.a.sim)
goodness(nmds.12.a.sim)
nmds.12.a.sim$stress
plot(nmds.12.a.sim)

# PERMANOVA
adonis2(b.12.a$beta.sim ~ time.12.a$Decade, permutations = 999)
pairwise.adonis(b.12.a$beta.sim, time.12.a$Decade)
adonis2(b.12.a$beta.sim ~ time.12.a$Regions, permutations = 999)
pairwise.adonis(b.12.a$beta.sim, time.12.a$Regions)

# BETADISPER
beta.sim.12.a <- betadisper(b.12.a$beta.sim, time.12.a$Decade, type = c("median"))
beta.sim.12.a
anova(beta.sim.12.a)
plot(beta.sim.12.a)
boxplot(beta.sim.12.a, ylab = "Distance to median")
TukeyHSD(beta.sim.12.a, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.12.a.sne <- metaMDS(b.12.a$beta.sne, trymax = 500, autotransform = TRUE)
nmds.12.a.sne
stressplot(nmds.12.a.sne)
goodness(nmds.12.a.sne)
nmds.12.a.sne$stress
plot(nmds.12.a.sne)

# PERMANOVA
adonis2(b.12.a$beta.sne ~ time.12.a$Decade, permutations = 999)
pairwise.adonis(b.12.a$beta.sne, time.12.a$Decade)
adonis2(b.12.a$beta.sne ~ time.12.a$Regions, permutations = 999)
pairwise.adonis(b.12.a$beta.sne, time.12.a$Regions)

# BETADISPER
beta.sne.12.a <- betadisper(b.12.a$beta.sne, time.12.a$Decade, type = c("median"))
beta.sne.12.a
anova(beta.sne.12.a)
plot(beta.sne.12.a)
boxplot(beta.sne.12.a, ylab = "Distance to median")
TukeyHSD(beta.sne.12.a, which = "group", conf.level = 0.95)


# 1910, 1920  ####################################################################################
time.23.a <-rbind(time2.a, time3.a)
str(time.23.a)
time.23.a.core <- betapart.core(time.23.a[4:24])
b.23.a <- beta.pair(time.23.a.core, index.family = "sorensen")
str(b.23.a)

## beta.sor - total betadiversity
nmds.23.a.sor <- metaMDS(b.23.a$beta.sor, trymax = 500, autotransform = TRUE)
nmds.23.a.sor
stressplot(nmds.23.a.sor)
goodness(nmds.23.a.sor)
nmds.23.a.sor$stress
plot(nmds.23.a.sor)

# PERMANOVA
adonis2(b.23.a$beta.sor ~ time.23.a$Decade, permutations = 999)
pairwise.adonis(b.23.a$beta.sor, time.23.a$Decade)
adonis2(b.23.a$beta.sor ~ time.23.a$Regions, permutations = 999)
pairwise.adonis(b.23.a$beta.sor, time.23.a$Regions)

# BETADISPER
beta.sor.23.a <- betadisper(b.23.a$beta.sor, time.23.a$Decade, type = c("median"))
beta.sor.23.a
anova(beta.sor.23.a)
plot(beta.sor.23.a)
boxplot(beta.sor.23.a, ylab = "Distance to median")
TukeyHSD(beta.sor.23.a, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.23.a.sim <- metaMDS(b.23.a$beta.sim, trymax = 500, autotransform = TRUE)
nmds.23.a.sim
stressplot(nmds.23.a.sim)
goodness(nmds.23.a.sim)
nmds.23.a.sim$stress
plot(nmds.23.a.sim)

# PERMANOVA
adonis2(b.23.a$beta.sim ~ time.23.a$Decade, permutations = 999)
pairwise.adonis(b.23.a$beta.sim, time.23.a$Decade)
adonis2(b.23.a$beta.sim ~ time.23.a$Regions, permutations = 999)
pairwise.adonis(b.23.a$beta.sim, time.23.a$Regions)

# BETADISPER
beta.sim.23.a <- betadisper(b.23.a$beta.sim, time.23.a$Decade, type = c("median"))
beta.sim.23.a
anova(beta.sim.23.a)
plot(beta.sim.23.a)
boxplot(beta.sim.23.a, ylab = "Distance to median")
TukeyHSD(beta.sim.23.a, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.23.a.sne <- metaMDS(b.23.a$beta.sne, trymax = 500, autotransform = TRUE)
nmds.23.a.sne
stressplot(nmds.23.a.sne)
goodness(nmds.23.a.sne)
nmds.23.a.sne$stress
plot(nmds.23.a.sne)

# PERMANOVA
adonis2(b.23.a$beta.sne ~ time.23.a$Decade, permutations = 999)
pairwise.adonis(b.23.a$beta.sne, time.23.a$Decade)
adonis2(b.23.a$beta.sne ~ time.23.a$Regions, permutations = 999)
pairwise.adonis(b.23.a$beta.sne, time.23.a$Regions)

# BETADISPER
beta.sne.23.a <- betadisper(b.23.a$beta.sne, time.23.a$Decade, type = c("median"))
beta.sne.23.a
anova(beta.sne.23.a)
plot(beta.sne.23.a)
boxplot(beta.sne.23.a, ylab = "Distance to median")
TukeyHSD(beta.sne.23.a, which = "group", conf.level = 0.95)


# 1920. 1930  ####################################################################################
time.34.a <-rbind(time3.a, time4.a)
str(time.34.a)
time.34.a.core <- betapart.core(time.34.a[4:24])
b.34.a <- beta.pair(time.34.a.core, index.family = "sorensen")
str(b.34.a)

## beta.sor - total betadiversity
nmds.34.a.sor <- metaMDS(b.34.a$beta.sor, trymax = 500, autotransform = TRUE)
nmds.34.a.sor
stressplot(nmds.34.a.sor)
goodness(nmds.34.a.sor)
nmds.34.a.sor$stress
plot(nmds.34.a.sor)

# PERMANOVA
adonis2(b.34.a$beta.sor ~ time.34.a$Decade, permutations = 999)
pairwise.adonis(b.34.a$beta.sor, time.34.a$Decade)
adonis2(b.34.a$beta.sor ~ time.34.a$Regions, permutations = 999)
pairwise.adonis(b.34.a$beta.sor, time.34.a$Regions)

# BETADISPER
beta.sor.34.a <- betadisper(b.34.a$beta.sor, time.34.a$Decade, type = c("median"))
beta.sor.34.a
anova(beta.sor.34.a)
plot(beta.sor.34.a)
boxplot(beta.sor.34.a, ylab = "Distance to median")
TukeyHSD(beta.sor.34.a, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.34.a.sim <- metaMDS(b.34.a$beta.sim, trymax = 500, autotransform = TRUE)
nmds.34.a.sim
stressplot(nmds.34.a.sim)
goodness(nmds.34.a.sim)
nmds.34.a.sim$stress
plot(nmds.34.a.sim)

# PERMANOVA
adonis2(b.34.a$beta.sim ~ time.34.a$Decade, permutations = 999)
pairwise.adonis(b.34.a$beta.sim, time.34.a$Decade)
adonis2(b.34.a$beta.sim ~ time.34.a$Regions, permutations = 999)
pairwise.adonis(b.34.a$beta.sim, time.34.a$Regions)

# BETADISPER
beta.sim.34.a <- betadisper(b.34.a$beta.sim, time.34.a$Decade, type = c("median"))
beta.sim.34.a
anova(beta.sim.34.a)
plot(beta.sim.34.a)
boxplot(beta.sim.34.a, ylab = "Distance to median")
TukeyHSD(beta.sim.34.a, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.34.a.sne <- metaMDS(b.34.a$beta.sne, trymax = 500, autotransform = TRUE)
nmds.34.a.sne
stressplot(nmds.34.a.sne)
goodness(nmds.34.a.sne)
nmds.34.a.sne$stress
plot(nmds.34.a.sne)

# PERMANOVA
adonis2(b.34.a$beta.sne ~ time.34.a$Decade, permutations = 999)
pairwise.adonis(b.34.a$beta.sne, time.34.a$Decade)
adonis2(b.34.a$beta.sne ~ time.34.a$Regions, permutations = 999)
pairwise.adonis(b.34.a$beta.sne, time.34.a$Regions)

# BETADISPER
beta.sne.34.a <- betadisper(b.34.a$beta.sne, time.34.a$Decade, type = c("median"))
beta.sne.34.a
anova(beta.sne.34.a)
plot(beta.sne.34.a)
boxplot(beta.sne.34.a, ylab = "Distance to median")
TukeyHSD(beta.sne.34.a, which = "group", conf.level = 0.95)


# 1930, 1940  ####################################################################################
time.45.a <-rbind(time4.a, time5.a)
str(time.45.a)
time.45.a.core <- betapart.core(time.45.a[4:24])
b.45.a <- beta.pair(time.45.a.core, index.family = "sorensen")
str(b.45.a)

## beta.sor - total betadiversity
nmds.45.a.sor <- metaMDS(b.45.a$beta.sor, trymax = 500, autotransform = TRUE)
nmds.45.a.sor
stressplot(nmds.45.a.sor)
goodness(nmds.45.a.sor)
nmds.45.a.sor$stress
plot(nmds.45.a.sor)

# PERMANOVA
adonis2(b.45.a$beta.sor ~ time.45.a$Decade, permutations = 999)
pairwise.adonis(b.45.a$beta.sor, time.45.a$Decade)
adonis2(b.45.a$beta.sor ~ time.45.a$Regions, permutations = 999)
pairwise.adonis(b.45.a$beta.sor, time.45.a$Regions)

# BETADISPER
beta.sor.45.a <- betadisper(b.45.a$beta.sor, time.45.a$Decade, type = c("median"))
beta.sor.45.a
anova(beta.sor.45.a)
plot(beta.sor.45.a)
boxplot(beta.sor.45.a, ylab = "Distance to median")
TukeyHSD(beta.sor.45.a, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.45.a.sim <- metaMDS(b.45.a$beta.sim, trymax = 500, autotransform = TRUE)
nmds.45.a.sim
stressplot(nmds.45.a.sim)
goodness(nmds.45.a.sim)
nmds.45.a.sim$stress
plot(nmds.45.a.sim)

# PERMANOVA
adonis2(b.45.a$beta.sim ~ time.45.a$Decade, permutations = 999)
pairwise.adonis(b.45.a$beta.sim, time.45.a$Decade)
adonis2(b.45.a$beta.sim ~ time.45.a$Regions, permutations = 999)
pairwise.adonis(b.45.a$beta.sim, time.45.a$Regions)

# BETADISPER
beta.sim.45.a <- betadisper(b.45.a$beta.sim, time.45.a$Decade, type = c("median"))
beta.sim.45.a
anova(beta.sim.45.a)
plot(beta.sim.45.a)
boxplot(beta.sim.45.a, ylab = "Distance to median")
TukeyHSD(beta.sim.45.a, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.45.a.sne <- metaMDS(b.45.a$beta.sne, trymax = 500, autotransform = TRUE)
nmds.45.a.sne
stressplot(nmds.45.a.sne)
goodness(nmds.45.a.sne)
nmds.45.a.sne$stress
plot(nmds.45.a.sne)

# PERMANOVA
adonis2(b.45.a$beta.sne ~ time.45.a$Decade, permutations = 999)
pairwise.adonis(b.45.a$beta.sne, time.45.a$Decade)
adonis2(b.45.a$beta.sne ~ time.45.a$Regions, permutations = 999)
pairwise.adonis(b.45.a$beta.sne, time.45.a$Regions)

# BETADISPER
beta.sne.45.a <- betadisper(b.45.a$beta.sne, time.45.a$Decade, type = c("median"))
beta.sne.45.a
anova(beta.sne.45.a)
plot(beta.sne.45.a)
boxplot(beta.sne.45.a, ylab = "Distance to median")
TukeyHSD(beta.sne.45.a, which = "group", conf.level = 0.95)


# 1940, 1950  ####################################################################################
time.56.a <-rbind(time5.a, time6.a)
str(time.56.a)
time.56.a.core <- betapart.core(time.56.a[4:24])
b.56.a <- beta.pair(time.56.a.core, index.family = "sorensen")
str(b.56.a)

## beta.sor - total betadiversity
nmds.56.a.sor <- metaMDS(b.56.a$beta.sor, trymax = 500, autotransform = TRUE)
nmds.56.a.sor
stressplot(nmds.56.a.sor)
goodness(nmds.56.a.sor)
nmds.56.a.sor$stress
plot(nmds.56.a.sor)

# PERMANOVA
adonis2(b.56.a$beta.sor ~ time.56.a$Decade, permutations = 999)
pairwise.adonis(b.56.a$beta.sor, time.56.a$Decade)
adonis2(b.56.a$beta.sor ~ time.56.a$Regions, permutations = 999)
pairwise.adonis(b.56.a$beta.sor, time.56.a$Regions)

# BETADISPER
beta.sor.56.a <- betadisper(b.56.a$beta.sor, time.56.a$Decade, type = c("median"))
beta.sor.56.a
anova(beta.sor.56.a)
plot(beta.sor.56.a)
boxplot(beta.sor.56.a, ylab = "Distance to median")
TukeyHSD(beta.sor.56.a, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.56.a.sim <- metaMDS(b.56.a$beta.sim, trymax = 500, autotransform = TRUE)
nmds.56.a.sim
stressplot(nmds.56.a.sim)
goodness(nmds.56.a.sim)
nmds.56.a.sim$stress
plot(nmds.56.a.sim)

# PERMANOVA
adonis2(b.56.a$beta.sim ~ time.56.a$Decade, permutations = 999)
pairwise.adonis(b.56.a$beta.sim, time.56.a$Decade)
adonis2(b.56.a$beta.sim ~ time.56.a$Regions, permutations = 999)
pairwise.adonis(b.56.a$beta.sim, time.56.a$Regions)

# BETADISPER
beta.sim.56.a <- betadisper(b.56.a$beta.sim, time.56.a$Decade, type = c("median"))
beta.sim.56.a
anova(beta.sim.56.a)
plot(beta.sim.56.a)
boxplot(beta.sim.56.a, ylab = "Distance to median")
TukeyHSD(beta.sim.56.a, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.56.a.sne <- metaMDS(b.56.a$beta.sne, trymax = 500, autotransform = TRUE)
nmds.56.a.sne
stressplot(nmds.56.a.sne)
goodness(nmds.56.a.sne)
nmds.56.a.sne$stress
plot(nmds.56.a.sne)

# PERMANOVA
adonis2(b.56.a$beta.sne ~ time.56.a$Decade, permutations = 999)
pairwise.adonis(b.56.a$beta.sne, time.56.a$Decade)
adonis2(b.56.a$beta.sne ~ time.56.a$Regions, permutations = 999)
pairwise.adonis(b.56.a$beta.sne, time.56.a$Regions)

# BETADISPER
beta.sne.56.a <- betadisper(b.56.a$beta.sne, time.56.a$Decade, type = c("median"))
beta.sne.56.a
anova(beta.sne.56.a)
plot(beta.sne.56.a)
boxplot(beta.sne.56.a, ylab = "Distance to median")
TukeyHSD(beta.sne.56.a, which = "group", conf.level = 0.95)


# 1950, 1960  ####################################################################################
time.67.a <-rbind(time6.a, time7.a)
str(time.67.a)
time.67.a.core <- betapart.core(time.67.a[4:24])
b.67.a <- beta.pair(time.67.a.core, index.family = "sorensen")
str(b.67.a)

## beta.sor - total betadiversity
nmds.67.a.sor <- metaMDS(b.67.a$beta.sor, trymax = 500, autotransform = TRUE)
nmds.67.a.sor
stressplot(nmds.67.a.sor)
goodness(nmds.67.a.sor)
nmds.67.a.sor$stress
plot(nmds.67.a.sor)

# PERMANOVA
adonis2(b.67.a$beta.sor ~ time.67.a$Decade, permutations = 999)
pairwise.adonis(b.67.a$beta.sor, time.67.a$Decade)
adonis2(b.67.a$beta.sor ~ time.67.a$Regions, permutations = 999)
pairwise.adonis(b.67.a$beta.sor, time.67.a$Regions)

# BETADISPER
beta.sor.67.a <- betadisper(b.67.a$beta.sor, time.67.a$Decade, type = c("median"))
beta.sor.67.a
anova(beta.sor.67.a)
plot(beta.sor.67.a)
boxplot(beta.sor.67.a, ylab = "Distance to median")
TukeyHSD(beta.sor.67.a, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.67.a.sim <- metaMDS(b.67.a$beta.sim, trymax = 500, autotransform = TRUE)
nmds.67.a.sim
stressplot(nmds.67.a.sim)
goodness(nmds.67.a.sim)
nmds.67.a.sim$stress
plot(nmds.67.a.sim)

# PERMANOVA
adonis2(b.67.a$beta.sim ~ time.67.a$Decade, permutations = 999)
pairwise.adonis(b.67.a$beta.sim, time.67.a$Decade)
adonis2(b.67.a$beta.sim ~ time.67.a$Regions, permutations = 999)
pairwise.adonis(b.67.a$beta.sim, time.67.a$Regions)

# BETADISPER
beta.sim.67.a <- betadisper(b.67.a$beta.sim, time.67.a$Decade, type = c("median"))
beta.sim.67.a
anova(beta.sim.67.a)
plot(beta.sim.67.a)
boxplot(beta.sim.67.a, ylab = "Distance to median")
TukeyHSD(beta.sim.67.a, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.67.a.sne <- metaMDS(b.67.a$beta.sne, trymax = 500, autotransform = TRUE)
nmds.67.a.sne
stressplot(nmds.67.a.sne)
goodness(nmds.67.a.sne)
nmds.67.a.sne$stress
plot(nmds.67.a.sne)

# PERMANOVA
adonis2(b.67.a$beta.sne ~ time.67.a$Decade, permutations = 999)
pairwise.adonis(b.67.a$beta.sne, time.67.a$Decade)
adonis2(b.67.a$beta.sne ~ time.67.a$Regions, permutations = 999)
pairwise.adonis(b.67.a$beta.sne, time.67.a$Regions)

# BETADISPER
beta.sne.67.a <- betadisper(b.67.a$beta.sne, time.67.a$Decade, type = c("median"))
beta.sne.67.a
anova(beta.sne.67.a)
plot(beta.sne.67.a)
boxplot(beta.sne.67.a, ylab = "Distance to median")
TukeyHSD(beta.sne.67.a, which = "group", conf.level = 0.95)


# 1960, 1970  ####################################################################################
time.78.a <-rbind(time7.a, time8.a)
str(time.78.a)
time.78.a.core <- betapart.core(time.78.a[4:24])
b.78.a <- beta.pair(time.78.a.core, index.family = "sorensen")
str(b.78.a)

## beta.sor - total betadiversity
nmds.78.a.sor <- metaMDS(b.78.a$beta.sor, trymax = 500, autotransform = TRUE)
nmds.78.a.sor
stressplot(nmds.78.a.sor)
goodness(nmds.78.a.sor)
nmds.78.a.sor$stress
plot(nmds.78.a.sor)

# PERMANOVA
adonis2(b.78.a$beta.sor ~ time.78.a$Decade, permutations = 999)
pairwise.adonis(b.78.a$beta.sor, time.78.a$Decade)
adonis2(b.78.a$beta.sor ~ time.78.a$Regions, permutations = 999)
pairwise.adonis(b.78.a$beta.sor, time.78.a$Regions)

# BETADISPER
beta.sor.78.a <- betadisper(b.78.a$beta.sor, time.78.a$Decade, type = c("median"))
beta.sor.78.a
anova(beta.sor.78.a)
plot(beta.sor.78.a)
boxplot(beta.sor.78.a, ylab = "Distance to median")
TukeyHSD(beta.sor.78.a, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.78.a.sim <- metaMDS(b.78.a$beta.sim, trymax = 500, autotransform = TRUE)
nmds.78.a.sim
stressplot(nmds.78.a.sim)
goodness(nmds.78.a.sim)
nmds.78.a.sim$stress
plot(nmds.78.a.sim)

# PERMANOVA
adonis2(b.78.a$beta.sim ~ time.78.a$Decade, permutations = 999)
pairwise.adonis(b.78.a$beta.sim, time.78.a$Decade)
adonis2(b.78.a$beta.sim ~ time.78.a$Regions, permutations = 999)
pairwise.adonis(b.78.a$beta.sim, time.78.a$Regions)

# BETADISPER
beta.sim.78.a <- betadisper(b.78.a$beta.sim, time.78.a$Decade, type = c("median"))
beta.sim.78.a
anova(beta.sim.78.a)
plot(beta.sim.78.a)
boxplot(beta.sim.78.a, ylab = "Distance to median")
TukeyHSD(beta.sim.78.a, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.78.a.sne <- metaMDS(b.78.a$beta.sne, trymax = 500, autotransform = TRUE)
nmds.78.a.sne
stressplot(nmds.78.a.sne)
goodness(nmds.78.a.sne)
nmds.78.a.sne$stress
plot(nmds.78.a.sne)

# PERMANOVA
adonis2(b.78.a$beta.sne ~ time.78.a$Decade, permutations = 999)
pairwise.adonis(b.78.a$beta.sne, time.78.a$Decade)
adonis2(b.78.a$beta.sne ~ time.78.a$Regions, permutations = 999)
pairwise.adonis(b.78.a$beta.sne, time.78.a$Regions)

# BETADISPER
beta.sne.78.a <- betadisper(b.78.a$beta.sne, time.78.a$Decade, type = c("median"))
beta.sne.78.a
anova(beta.sne.78.a)
plot(beta.sne.78.a)
boxplot(beta.sne.78.a, ylab = "Distance to median")
TukeyHSD(beta.sne.78.a, which = "group", conf.level = 0.95)


# 1970, 1980  ####################################################################################
time.89.a <-rbind(time8.a, time9.a)
str(time.89.a)
time.89.a.core <- betapart.core(time.89.a[4:24])
b.89.a <- beta.pair(time.89.a.core, index.family = "sorensen")
str(b.89.a)

## beta.sor - total betadiversity
nmds.89.a.sor <- metaMDS(b.89.a$beta.sor, trymax = 500, autotransform = TRUE)
nmds.89.a.sor
stressplot(nmds.89.a.sor)
goodness(nmds.89.a.sor)
nmds.89.a.sor$stress
plot(nmds.89.a.sor)

# PERMANOVA
adonis2(b.89.a$beta.sor ~ time.89.a$Decade, permutations = 999)
pairwise.adonis(b.89.a$beta.sor, time.89.a$Decade)
adonis2(b.89.a$beta.sor ~ time.89.a$Regions, permutations = 999)
pairwise.adonis(b.89.a$beta.sor, time.89.a$Regions)

# BETADISPER
beta.sor.89.a <- betadisper(b.89.a$beta.sor, time.89.a$Decade, type = c("median"))
beta.sor.89.a
anova(beta.sor.89.a)
plot(beta.sor.89.a)
boxplot(beta.sor.89.a, ylab = "Distance to median")
TukeyHSD(beta.sor.89.a, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.89.a.sim <- metaMDS(b.89.a$beta.sim, trymax = 500, autotransform = TRUE)
nmds.89.a.sim
stressplot(nmds.89.a.sim)
goodness(nmds.89.a.sim)
nmds.89.a.sim$stress
plot(nmds.89.a.sim)

# PERMANOVA
adonis2(b.89.a$beta.sim ~ time.89.a$Decade, permutations = 999)
pairwise.adonis(b.89.a$beta.sim, time.89.a$Decade)
adonis2(b.89.a$beta.sim ~ time.89.a$Regions, permutations = 999)
pairwise.adonis(b.89.a$beta.sim, time.89.a$Regions)

# BETADISPER
beta.sim.89.a <- betadisper(b.89.a$beta.sim, time.89.a$Decade, type = c("median"))
beta.sim.89.a
anova(beta.sim.89.a)
plot(beta.sim.89.a)
boxplot(beta.sim.89.a, ylab = "Distance to median")
TukeyHSD(beta.sim.89.a, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.89.a.sne <- metaMDS(b.89.a$beta.sne, trymax = 500, autotransform = TRUE)
nmds.89.a.sne
stressplot(nmds.89.a.sne)
goodness(nmds.89.a.sne)
nmds.89.a.sne$stress
plot(nmds.89.a.sne)

# PERMANOVA
adonis2(b.89.a$beta.sne ~ time.89.a$Decade, permutations = 999)
pairwise.adonis(b.89.a$beta.sne, time.89.a$Decade)
adonis2(b.89.a$beta.sne ~ time.89.a$Regions, permutations = 999)
pairwise.adonis(b.89.a$beta.sne, time.89.a$Regions)

# BETADISPER
beta.sne.89.a <- betadisper(b.89.a$beta.sne, time.89.a$Decade, type = c("median"))
beta.sne.89.a
anova(beta.sne.89.a)
plot(beta.sne.89.a)
boxplot(beta.sne.89.a, ylab = "Distance to median")
TukeyHSD(beta.sne.89.a, which = "group", conf.level = 0.95)


# 1980, 1990  ####################################################################################
time.910.a <-rbind(time9.a, time10.a)
str(time.910.a)
time.910.a.core <- betapart.core(time.910.a[4:24])
b.910.a <- beta.pair(time.910.a.core, index.family = "sorensen")
str(b.910.a)

## beta.sor - total betadiversity
nmds.910.a.sor <- metaMDS(b.910.a$beta.sor, trymax = 500, autotransform = TRUE)
nmds.910.a.sor
stressplot(nmds.910.a.sor)
goodness(nmds.910.a.sor)
nmds.910.a.sor$stress
plot(nmds.910.a.sor)

# PERMANOVA
adonis2(b.910.a$beta.sor ~ time.910.a$Decade, permutations = 999)
pairwise.adonis(b.910.a$beta.sor, time.910.a$Decade)
adonis2(b.910.a$beta.sor ~ time.910.a$Regions, permutations = 999)
pairwise.adonis(b.910.a$beta.sor, time.910.a$Regions)

# BETADISPER
beta.sor.910.a <- betadisper(b.910.a$beta.sor, time.910.a$Decade, type = c("median"))
beta.sor.910.a
anova(beta.sor.910.a)
plot(beta.sor.910.a)
boxplot(beta.sor.910.a, ylab = "Distance to median")
TukeyHSD(beta.sor.910.a, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.910.a.sim <- metaMDS(b.910.a$beta.sim, trymax = 500, autotransform = TRUE)
nmds.910.a.sim
stressplot(nmds.910.a.sim)
goodness(nmds.910.a.sim)
nmds.910.a.sim$stress
plot(nmds.910.a.sim)

# PERMANOVA
adonis2(b.910.a$beta.sim ~ time.910.a$Decade, permutations = 999)
pairwise.adonis(b.910.a$beta.sim, time.910.a$Decade)
adonis2(b.910.a$beta.sim ~ time.910.a$Regions, permutations = 999)
pairwise.adonis(b.910.a$beta.sim, time.910.a$Regions)

# BETADISPER
beta.sim.910.a <- betadisper(b.910.a$beta.sim, time.910.a$Decade, type = c("median"))
beta.sim.910.a
anova(beta.sim.910.a)
plot(beta.sim.910.a)
boxplot(beta.sim.910.a, ylab = "Distance to median")
TukeyHSD(beta.sim.910.a, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.910.a.sne <- metaMDS(b.910.a$beta.sne, trymax = 500, autotransform = TRUE)
nmds.910.a.sne
stressplot(nmds.910.a.sne)
goodness(nmds.910.a.sne)
nmds.910.a.sne$stress
plot(nmds.910.a.sne)

# PERMANOVA
adonis2(b.910.a$beta.sne ~ time.910.a$Decade, permutations = 999)
pairwise.adonis(b.910.a$beta.sne, time.910.a$Decade)
adonis2(b.910.a$beta.sne ~ time.910.a$Regions, permutations = 999)
pairwise.adonis(b.910.a$beta.sne, time.910.a$Regions)

# BETADISPER
beta.sne.910.a <- betadisper(b.910.a$beta.sne, time.910.a$Decade, type = c("median"))
beta.sne.910.a
anova(beta.sne.910.a)
plot(beta.sne.910.a)
boxplot(beta.sne.910.a, ylab = "Distance to median")
TukeyHSD(beta.sne.910.a, which = "group", conf.level = 0.95)


# 1990, 2000  ####################################################################################
time.1011.a <-rbind(time10.a, time11.a)
str(time.1011.a)
time.1011.a.core <- betapart.core(time.1011.a[4:24])
b.1011.a <- beta.pair(time.1011.a.core, index.family = "sorensen")
str(b.1011.a)

## beta.sor - total betadiversity
nmds.1011.a.sor <- metaMDS(b.1011.a$beta.sor, trymax = 500, autotransform = TRUE)
nmds.1011.a.sor
stressplot(nmds.1011.a.sor)
goodness(nmds.1011.a.sor)
nmds.1011.a.sor$stress
plot(nmds.1011.a.sor)

# PERMANOVA
adonis2(b.1011.a$beta.sor ~ time.1011.a$Decade, permutations = 999)
pairwise.adonis(b.1011.a$beta.sor, time.1011.a$Decade)
adonis2(b.1011.a$beta.sor ~ time.1011.a$Regions, permutations = 999)
pairwise.adonis(b.1011.a$beta.sor, time.1011.a$Regions)

# BETADISPER
beta.sor.1011.a <- betadisper(b.1011.a$beta.sor, time.1011.a$Decade, type = c("median"))
beta.sor.1011.a
anova(beta.sor.1011.a)
plot(beta.sor.1011.a)
boxplot(beta.sor.1011.a, ylab = "Distance to median")
TukeyHSD(beta.sor.1011.a, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.1011.a.sim <- metaMDS(b.1011.a$beta.sim, trymax = 500, autotransform = TRUE)
nmds.1011.a.sim
stressplot(nmds.1011.a.sim)
goodness(nmds.1011.a.sim)
nmds.1011.a.sim$stress
plot(nmds.1011.a.sim)

# PERMANOVA
adonis2(b.1011.a$beta.sim ~ time.1011.a$Decade, permutations = 999)
pairwise.adonis(b.1011.a$beta.sim, time.1011.a$Decade)
adonis2(b.1011.a$beta.sim ~ time.1011.a$Regions, permutations = 999)
pairwise.adonis(b.1011.a$beta.sim, time.1011.a$Regions)

# BETADISPER
beta.sim.1011.a <- betadisper(b.1011.a$beta.sim, time.1011.a$Decade, type = c("median"))
beta.sim.1011.a
anova(beta.sim.1011.a)
plot(beta.sim.1011.a)
boxplot(beta.sim.1011.a, ylab = "Distance to median")
TukeyHSD(beta.sim.1011.a, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.1011.a.sne <- metaMDS(b.1011.a$beta.sne, trymax = 500, autotransform = TRUE)
nmds.1011.a.sne
stressplot(nmds.1011.a.sne)
goodness(nmds.1011.a.sne)
nmds.1011.a.sne$stress
plot(nmds.1011.a.sne)

# PERMANOVA
adonis2(b.1011.a$beta.sne ~ time.1011.a$Decade, permutations = 999)
pairwise.adonis(b.1011.a$beta.sne, time.1011.a$Decade)
adonis2(b.1011.a$beta.sne ~ time.1011.a$Regions, permutations = 999)
pairwise.adonis(b.1011.a$beta.sne, time.1011.a$Regions)

# BETADISPER
beta.sne.1011.a <- betadisper(b.1011.a$beta.sne, time.1011.a$Decade, type = c("median"))
beta.sne.1011.a
anova(beta.sne.1011.a)
plot(beta.sne.1011.a)
boxplot(beta.sne.1011.a, ylab = "Distance to median")
TukeyHSD(beta.sne.1011.a, which = "group", conf.level = 0.95)


# 2000, 2010  ####################################################################################
time.1112.a <-rbind(time11.a, time12.a)
str(time.1112.a)
time.1112.a.core <- betapart.core(time.1112.a[4:24])
b.1112.a <- beta.pair(time.1112.a.core, index.family = "sorensen")
str(b.1112.a)

## beta.sor - total betadiversity
nmds.1112.a.sor <- metaMDS(b.1112.a$beta.sor, trymax = 500, autotransform = TRUE)
nmds.1112.a.sor
stressplot(nmds.1112.a.sor)
goodness(nmds.1112.a.sor)
nmds.1112.a.sor$stress
plot(nmds.1112.a.sor)

# PERMANOVA
adonis2(b.1112.a$beta.sor ~ time.1112.a$Decade*time.1112.a$Regions, permutations = 999)
pairwise.adonis(b.1112.a$beta.sor, time.1112.a$Decade)
adonis2(b.1112.a$beta.sor ~ time.1112.a$Regions, permutations = 999)
pairwise.adonis(b.1112.a$beta.sor, time.1112.a$Regions)

# BETADISPER
beta.sor.1112.a <- betadisper(b.1112.a$beta.sor, time.1112.a$Decade, type = c("median"))
beta.sor.1112.a
anova(beta.sor.1112.a)
plot(beta.sor.1112.a)
boxplot(beta.sor.1112.a, ylab = "Distance to median")
TukeyHSD(beta.sor.1112.a, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.1112.a.sim <- metaMDS(b.1112.a$beta.sim, trymax = 500, autotransform = TRUE)
nmds.1112.a.sim
stressplot(nmds.1112.a.sim)
goodness(nmds.1112.a.sim)
nmds.1112.a.sim$stress
plot(nmds.1112.a.sim)

# PERMANOVA
adonis2(b.1112.a$beta.sim ~ time.1112.a$Decade * time.1112.a$Regions, permutations = 999)
pairwise.adonis(b.1112.a$beta.sim, time.1112.a$Decade)
adonis2(b.1112.a$beta.sim ~ time.1112.a$Regions, permutations = 999)
pairwise.adonis(b.1112.a$beta.sim, time.1112.a$Regions)

# BETADISPER
beta.sim.1112.a <- betadisper(b.1112.a$beta.sim, time.1112.a$Decade, type = c("median"))
beta.sim.1112.a
anova(beta.sim.1112.a)
plot(beta.sim.1112.a)
boxplot(beta.sim.1112.a, ylab = "Distance to median")
TukeyHSD(beta.sim.1112.a, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.1112.a.sne <- metaMDS(b.1112.a$beta.sne, trymax = 500, autotransform = TRUE)
nmds.1112.a.sne
stressplot(nmds.1112.a.sne)
goodness(nmds.1112.a.sne)
nmds.1112.a.sne$stress
plot(nmds.1112.a.sne)

# PERMANOVA
adonis2(b.1112.a$beta.sne ~ time.1112.a$Decade * time.1112.a$Regions, permutations = 999)
pairwise.adonis(b.1112.a$beta.sne, time.1112.a$Decade)
adonis2(b.1112.a$beta.sne ~ time.1112.a$Regions, permutations = 999)
pairwise.adonis(b.1112.a$beta.sne, time.1112.a$Regions)

# BETADISPER
beta.sne.1112.a <- betadisper(b.1112.a$beta.sne, time.1112.a$Decade, type = c("median"))
beta.sne.1112.a
anova(beta.sne.1112.a)
plot(beta.sne.1112.a)
boxplot(beta.sne.1112.a, ylab = "Distance to median")
TukeyHSD(beta.sne.1112.a, which = "group", conf.level = 0.95)


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


### now add code to compare museum specimens with BLBB!
