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

lb <- read.csv("specimen_data/LB_MuseumData_2020_v2.csv")

colnames(lb)
levels(as.factor(lb$Name))

#install.packages("reshape2")
library(reshape2)

#install.packages("vegan")
library(vegan)

#install.packages("devtools")
#library(devtools)
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

#install.packages("remotes")
#remotes::install_github("GuillemSalazar/EcolUtils")
library(EcolUtils)
citation("EcolUtils")

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


#####
# Now, repeat these analyses with only native lady beetle species

str(lb.b.matrix)

# remove the exotic species

lb.b.matrix.n <- lb.b.matrix[,-9] # Coccinella septempunctata
lb.b.matrix.n <- lb.b.matrix.n[,-11] # Coccinella undecimpunctata
lb.b.matrix.n <- lb.b.matrix.n[,-13] # Harmonia axyridis
lb.b.matrix.n <- lb.b.matrix.n[,-18] # Hippodamia variegata
lb.b.matrix.n <- lb.b.matrix.n[,-23] # Propylea quatuordecimpunctata

str(lb.b.matrix.n)

#### Assess descriptive patterns for changes in native lady beetle beta diversity
#### across decades in Ohio

# pull out data from each decade
# remove factor columns, change site to row names

time1.n <- lb.b.matrix.n[which(lb.b.matrix.n$Decade == "1900"),]
rownames(time1.n) <- time1.n[,24]
time1.n <- time1.n[,-1]
time1.n <- time1.n[,-23]
str(time1.n)

time2.n <- lb.b.matrix.n[which(lb.b.matrix.n$Decade == "1910"),]
rownames(time2.n) <- time2.n[,24]
time2.n <- time2.n[,-1]
time2.n <- time2.n[,-23]
str(time2.n)

time3.n <- lb.b.matrix.n[which(lb.b.matrix.n$Decade == "1920"),]
rownames(time3.n) <- time3.n[,24]
time3.n <- time3.n[,-1]
time3.n <- time3.n[,-23]
str(time3.n)

time4.n <- lb.b.matrix.n[which(lb.b.matrix.n$Decade == "1930"),]
rownames(time4.n) <- time4.n[,24]
time4.n <- time4.n[,-1]
time4.n <- time4.n[,-23]
str(time4.n)

time5.n <- lb.b.matrix.n[which(lb.b.matrix.n$Decade == "1940"),]
rownames(time5.n) <- time5.n[,24]
time5.n <- time5.n[,-1]
time5.n <- time5.n[,-23]
str(time5.n)

time6.n <- lb.b.matrix.n[which(lb.b.matrix.n$Decade == "1950"),]
rownames(time6.n) <- time6.n[,24]
time6.n <- time6.n[,-1]
time6.n <- time6.n[,-23]
str(time6.n)

time7.n <- lb.b.matrix.n[which(lb.b.matrix.n$Decade == "1960"),]
rownames(time7.n) <- time7.n[,24]
time7.n <- time7.n[,-1]
time7.n <- time7.n[,-23]
str(time7.n)

time8.n <- lb.b.matrix.n[which(lb.b.matrix.n$Decade == "1970"),]
rownames(time8.n) <- time8.n[,24]
time8.n <- time8.n[,-1]
time8.n <- time8.n[,-23]
str(time8.n)

time9.n <- lb.b.matrix.n[which(lb.b.matrix.n$Decade == "1980"),]
rownames(time9.n) <- time9.n[,24]
time9.n <- time9.n[,-1]
time9.n <- time9.n[,-23]
str(time9.n)

time10.n <- lb.b.matrix.n[which(lb.b.matrix.n$Decade == "1990"),]
rownames(time10.n) <- time10.n[,24]
time10.n <- time10.n[,-1]
time10.n <- time10.n[,-23]
str(time10.n)

time11.n <- lb.b.matrix.n[which(lb.b.matrix.n$Decade == "2000"),]
rownames(time11.n) <- time11.n[,24]
time11.n <- time11.n[,-1]
time11.n <- time11.n[,-23]
str(time11.n)

time12.n <- lb.b.matrix.n[which(lb.b.matrix.n$Decade == "1900"),]
rownames(time12.n) <- time12.n[,24]
time12.n <- time12.n[,-1]
time12.n <- time12.n[,-23]
str(time12.n)

# create beta part object for each decade

t1.n <- betapart.core(time1.n)
t2.n <- betapart.core(time2.n)
t3.n <- betapart.core(time3.n)
t4.n <- betapart.core(time4.n)
t5.n <- betapart.core(time5.n)
t6.n <- betapart.core(time6.n)
t7.n <- betapart.core(time7.n)
t8.n <- betapart.core(time8.n)
t9.n <- betapart.core(time9.n)
t10.n <- betapart.core(time10.n)
t11.n <- betapart.core(time11.n)
t12.n <- betapart.core(time12.n)

# assess temporal change in community composition among the decades for all of Ohio

t12.n <- beta.temp(t1.n, t2.n, index.family = "sorensen")
t12.n

t23.n <- beta.temp(t2.n, t3.n, index.family = "sorensen")
t23.n

t34.n <- beta.temp(t3.n, t4.n, index.family = "sorensen")
t34.n

t45.n <- beta.temp(t4.n, t5.n, index.family = "sorensen")
t45.n

t56.n <- beta.temp(t5.n, t6.n, index.family = "sorensen")
t56.n

t67.n <- beta.temp(t6.n, t7.n, index.family = "sorensen")
t67.n

t78.n <- beta.temp(t7.n, t8.n, index.family = "sorensen")
t78.n

t89.n <- beta.temp(t8.n, t9.n, index.family = "sorensen")
t89.n

t910.n <- beta.temp(t9.n, t10.n, index.family = "sorensen")
t910.n

t1011.n <- beta.temp(t10.n, t11.n, index.family = "sorensen")
t1011.n

t1112.n <- beta.temp(t11.n, t12.n, index.family = "sorensen")
t1112.n

# outputs from beta.temp used to make figure 2C

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

# interaction between decade and geographic region
adonis2(b.12$beta.sor ~ time.12$Decade * time.12$Regions, permutations = 999)

# BETADISPER
beta.sor.12.d <- betadisper(b.12$beta.sor, time.12$Decade, type = c("median"))
beta.sor.12.d
anova(beta.sor.12.d)
plot(beta.sor.12.d)
boxplot(beta.sor.12.d, ylab = "Distance to median")
TukeyHSD(beta.sor.12.d, which = "group", conf.level = 0.95)

beta.sor.12.r <- betadisper(b.12$beta.sor, time.12$Regions, type = c("median"))
beta.sor.12.r
anova(beta.sor.12.r)
plot(beta.sor.12.r)
boxplot(beta.sor.12.r, ylab = "Distance to median")
TukeyHSD(beta.sor.12.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.12$beta.sim ~ time.12$Decade * time.12$Regions, permutations = 999)

# BETADISPER
beta.sim.12.d <- betadisper(b.12$beta.sim, time.12$Decade, type = c("median"))
beta.sim.12.d
anova(beta.sim.12.d)
plot(beta.sim.12.d)
boxplot(beta.sim.12.d, ylab = "Distance to median")
TukeyHSD(beta.sim.12.d, which = "group", conf.level = 0.95)

beta.sim.12.r <- betadisper(b.12$beta.sim, time.12$Regions, type = c("median"))
beta.sim.12.r
anova(beta.sim.12.r)
plot(beta.sim.12.r)
boxplot(beta.sim.12.r, ylab = "Distance to median")
TukeyHSD(beta.sim.12.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.12$beta.sne ~ time.12$Decade * time.12$Regions, permutations = 999)

# BETADISPER
beta.sne.12.d <- betadisper(b.12$beta.sne, time.12$Decade, type = c("median"))
beta.sne.12.d
anova(beta.sne.12.d)
plot(beta.sne.12.d)
boxplot(beta.sne.12.d, ylab = "Distance to median")
TukeyHSD(beta.sne.12.d, which = "group", conf.level = 0.95)

beta.sne.12.r <- betadisper(b.12$beta.sne, time.12$Regions, type = c("median"))
beta.sne.12.r
anova(beta.sne.12.r)
plot(beta.sne.12.r)
boxplot(beta.sne.12.r, ylab = "Distance to median")
TukeyHSD(beta.sne.12.r, which = "group", conf.level = 0.95)

# 1910, 1920  ####################################################################################
time.23 <-rbind(time2, time3)
str(time.23)
time.23.core <- betapart.core(time.23[4:30])
b.23 <- beta.pair(time.23.core, index.family = "sorensen")
str(b.23)

time.23$Dec <- c("1910","1910","1910","1910","1910","1910","1910","1910","1920","1920","1920","1920","1920","1920")
time.23$Dec <- as.factor(time.23$Dec)
levels(time.23$Dec)

## beta.sor - total betadiversity
nmds.23.sor <- metaMDS(b.23$beta.sor, trymax = 500, autotransform = TRUE)
nmds.23.sor
stressplot(nmds.23.sor)
goodness(nmds.23.sor)
nmds.23.sor$stress
plot(nmds.23.sor)

#1910s-1920s
ordiplot(nmds.23.sor, type="n", xlim = c(-0.6, 0.6), ylim = c(-0.6, 0.4))
points(nmds.23.sor, dis="sites", cex=2, pch = pchvec1[time.23$Dec], col=colvec[time.23$Dec])
ordiellipse(nmds.23.sor, groups=time.23$Dec, display="sites", draw="lines",
            col=c("gray56", "black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1910s","1920s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=c(16, 17))

ordiplot(nmds.23.sne, type="n", xlim = c(-0.6, 0.6), ylim = c(-0.6, 0.4))
points(nmds.23.sne, dis="sites", cex=2, pch = pchvec1[time.23$Dec], col=colvec[time.23$Dec])
ordiellipse(nmds.23.sne, groups=time.23$Dec, display="sites", draw="lines",
            col=c("gray56", "black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1910s","1920s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=c(16, 17))

# PERMANOVA
adonis2(b.23$beta.sor ~ time.23$Decade, permutations = 999)
pairwise.adonis(b.23$beta.sor, time.23$Decade)
adonis2(b.23$beta.sor ~ time.23$Regions, permutations = 999)
pairwise.adonis(b.23$beta.sor, time.23$Regions)

# interaction between decade and region
adonis2(b.23$beta.sor ~ time.23$Decade * time.23$Regions, permutations = 999)


# BETADISPER
beta.sor.23.d <- betadisper(b.23$beta.sor, time.23$Decade, type = c("median"))
beta.sor.23.d
anova(beta.sor.23.d)
plot(beta.sor.23.d)
boxplot(beta.sor.23.d, ylab = "Distance to median")
TukeyHSD(beta.sor.23.d, which = "group", conf.level = 0.95)

beta.sor.23.r <- betadisper(b.23$beta.sor, time.23$Regions, type = c("median"))
beta.sor.23.r
anova(beta.sor.23.r)
plot(beta.sor.23.r)
boxplot(beta.sor.23.r, ylab = "Distance to median")
TukeyHSD(beta.sor.23.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.23$beta.sim ~ time.23$Decade * time.23$Regions, permutations = 999)

# BETADISPER
beta.sim.23.d <- betadisper(b.23$beta.sim, time.23$Decade, type = c("median"))
beta.sim.23.d
anova(beta.sim.23.d)
plot(beta.sim.23.d)
boxplot(beta.sim.23.d, ylab = "Distance to median")
TukeyHSD(beta.sim.23.d, which = "group", conf.level = 0.95)

beta.sim.23.r <- betadisper(b.23$beta.sim, time.23$Regions, type = c("median"))
beta.sim.23.r
anova(beta.sim.23.r)
plot(beta.sim.23.r)
boxplot(beta.sim.23.r, ylab = "Distance to median")
TukeyHSD(beta.sim.23.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.23$beta.sne ~ time.23$Decade * time.23$Regions, permutations = 999)

# BETADISPER
beta.sne.23.d <- betadisper(b.23$beta.sne, time.23$Decade, type = c("median"))
beta.sne.23.d
anova(beta.sne.23.d)
plot(beta.sne.23.d)
boxplot(beta.sne.23.d, ylab = "Distance to median")
TukeyHSD(beta.sne.23.d, which = "group", conf.level = 0.95)

beta.sne.23.r <- betadisper(b.23$beta.sne, time.23$Regions, type = c("median"))
beta.sne.23.r
anova(beta.sne.23.r)
plot(beta.sne.23.r)
boxplot(beta.sne.23.r, ylab = "Distance to median")
TukeyHSD(beta.sne.23.r, which = "group", conf.level = 0.95)

# 1920. 1930  ####################################################################################
time.34 <-rbind(time3, time4)
str(time.34)
time.34.core <- betapart.core(time.34[4:30])
b.34 <- beta.pair(time.34.core, index.family = "sorensen")
str(b.34)

time.34$Dec <- c("1920","1920","1920","1920","1920","1920","1930","1930","1930","1930","1930","1930","1930","1930")
time.34$Dec <- as.factor(time.34$Dec)
levels(time.34$Dec)

## beta.sor - total betadiversity
nmds.34.sor <- metaMDS(b.34$beta.sor, trymax = 500, autotransform = TRUE)
nmds.34.sor
stressplot(nmds.34.sor)
goodness(nmds.34.sor)
nmds.34.sor$stress
plot(nmds.34.sor)

colvec <- c("gray56","black")
pchvec1 <- c(16, 17)
pchvec2 <- c(16, 17, 15, 18)

png("LB_Museum_sor_1920-30.png", width = 1000, height = 800, pointsize = 20)
ordiplot(nmds.34.sor, type="n", xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5))
points(nmds.34.sor, dis="sites", cex=2, pch = pchvec2[time.34$Regions], col=colvec[time.34$Dec])
ordiellipse(nmds.34.sor, groups=time.34$Dec, display="sites", draw="lines",
            col=c("gray56", "black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1920s","1930s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
legend("topleft", legend = c("AP","IOTP","LEGP","WLEB"), cex=1.5, bty="n", 
       pch = c(16,17,15,18), col=c("black"))
dev.off()

# PERMANOVA
adonis2(b.34$beta.sor ~ time.34$Decade, permutations = 999)
pairwise.adonis(b.34$beta.sor, time.34$Decade)
adonis2(b.34$beta.sor ~ time.34$Regions, permutations = 999)
pairwise.adonis(b.34$beta.sor, time.34$Regions)

# interaction between decade and region
adonis2(b.34$beta.sor ~ time.34$Decade * time.34$Regions, permutations = 999)

# BETADISPER
beta.sor.34.d <- betadisper(b.34$beta.sor, time.34$Decade, type = c("median"))
beta.sor.34.d
anova(beta.sor.34.d)
plot(beta.sor.34.d)
boxplot(beta.sor.34.d, ylab = "Distance to median")
TukeyHSD(beta.sor.34.d, which = "group", conf.level = 0.95)

beta.sor.34.r <- betadisper(b.34$beta.sor, time.34$Regions, type = c("median"))
beta.sor.34.r
anova(beta.sor.34.r)
plot(beta.sor.34.r)
boxplot(beta.sor.34.r, ylab = "Distance to median")
TukeyHSD(beta.sor.34.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.34$beta.sim ~ time.34$Decade * time.34$Regions, permutations = 999)

# BETADISPER
beta.sim.34.d <- betadisper(b.34$beta.sim, time.34$Decade, type = c("median"))
beta.sim.34.d
anova(beta.sim.34.d)
plot(beta.sim.34.d)
boxplot(beta.sim.34.d, ylab = "Distance to median")
TukeyHSD(beta.sim.34.d, which = "group", conf.level = 0.95)

beta.sim.34.r <- betadisper(b.34$beta.sim, time.34$Regions, type = c("median"))
beta.sim.34.r
anova(beta.sim.34.r)
plot(beta.sim.34.r)
boxplot(beta.sim.34.r, ylab = "Distance to median")
TukeyHSD(beta.sim.34.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.34$beta.sne ~ time.34$Decade * time.34$Regions, permutations = 999)

# BETADISPER
beta.sne.34.d <- betadisper(b.34$beta.sne, time.34$Decade, type = c("median"))
beta.sne.34.d
anova(beta.sne.34.d)
plot(beta.sne.34.d)
boxplot(beta.sne.34.d, ylab = "Distance to median")
TukeyHSD(beta.sne.34.d, which = "group", conf.level = 0.95)

beta.sne.34.r <- betadisper(b.34$beta.sne, time.34$Regions, type = c("median"))
beta.sne.34.r
anova(beta.sne.34.r)
plot(beta.sne.34.r)
boxplot(beta.sne.34.r, ylab = "Distance to median")
TukeyHSD(beta.sne.34.r, which = "group", conf.level = 0.95)


# 1930, 1940  ####################################################################################
time.45 <-rbind(time4, time5)
str(time.45)
time.45.core <- betapart.core(time.45[4:30])
b.45 <- beta.pair(time.45.core, index.family = "sorensen")
str(b.45)

time.45$Dec <- c("1930","1930","1930","1930","1930","1930","1930","1930","1940","1940","1940","1940","1940","1940","1940","1940")
time.45$Dec <- as.factor(time.45$Dec)
levels(time.45$Dec)

## beta.sor - total betadiversity
nmds.45.sor <- metaMDS(b.45$beta.sor, trymax = 500, autotransform = TRUE)
nmds.45.sor
stressplot(nmds.45.sor)
goodness(nmds.45.sor)
nmds.45.sor$stress
plot(nmds.45.sor)

png("LB_Museum_sor_1930-40.png", width = 1000, height = 800, pointsize = 20)
ordiplot(nmds.45.sor, type="n", xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5))
points(nmds.45.sor, dis="sites", cex=2, pch = pchvec2[time.45$Regions], col=colvec[time.45$Dec])
ordiellipse(nmds.45.sor, groups=time.45$Dec, display="sites", draw="lines",
            col=c("gray56", "black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1930s","1940s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
legend("topleft", legend = c("AP","IOTP","LEGP","WLEB"), cex=1.5, bty="n", 
       pch = c(16,17,15,18), col=c("black"))
dev.off()

# PERMANOVA
adonis2(b.45$beta.sor ~ time.45$Decade, permutations = 999)
pairwise.adonis(b.45$beta.sor, time.45$Decade)
adonis2(b.45$beta.sor ~ time.45$Regions, permutations = 999)
pairwise.adonis(b.45$beta.sor, time.45$Regions)

# interaction between decade and region
adonis2(b.45$beta.sor ~ time.45$Decade * time.45$Regions, permutations = 999)

# BETADISPER
beta.sor.45.d <- betadisper(b.45$beta.sor, time.45$Decade, type = c("median"))
beta.sor.45.d
anova(beta.sor.45.d)
plot(beta.sor.45.d)
boxplot(beta.sor.45.d, ylab = "Distance to median")
TukeyHSD(beta.sor.45.d, which = "group", conf.level = 0.95)

beta.sor.45.r <- betadisper(b.45$beta.sor, time.45$Regions, type = c("median"))
beta.sor.45.r
anova(beta.sor.45.r)
plot(beta.sor.45.r)
boxplot(beta.sor.45.r, ylab = "Distance to median")
TukeyHSD(beta.sor.45.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.45$beta.sim ~ time.45$Decade * time.45$Regions, permutations = 999)

# BETADISPER
beta.sim.45.d <- betadisper(b.45$beta.sim, time.45$Decade, type = c("median"))
beta.sim.45.d
anova(beta.sim.45.d)
plot(beta.sim.45.d)
boxplot(beta.sim.45.d, ylab = "Distance to median")
TukeyHSD(beta.sim.45.d, which = "group", conf.level = 0.95)

beta.sim.45.r <- betadisper(b.45$beta.sim, time.45$Regions, type = c("median"))
beta.sim.45.r
anova(beta.sim.45.r)
plot(beta.sim.45.r)
boxplot(beta.sim.45.r, ylab = "Distance to median")
TukeyHSD(beta.sim.45.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.45$beta.sne ~ time.45$Decade * time.45$Regions, permutations = 999)

# BETADISPER
beta.sne.45.d <- betadisper(b.45$beta.sne, time.45$Decade, type = c("median"))
beta.sne.45.d
anova(beta.sne.45.d)
plot(beta.sne.45.d)
boxplot(beta.sne.45.d, ylab = "Distance to median")
TukeyHSD(beta.sne.45.d, which = "group", conf.level = 0.95)

beta.sne.45.r <- betadisper(b.45$beta.sne, time.45$Regions, type = c("median"))
beta.sne.45.r
anova(beta.sne.45.r)
plot(beta.sne.45.r)
boxplot(beta.sne.45.r, ylab = "Distance to median")
TukeyHSD(beta.sne.45.r, which = "group", conf.level = 0.95)


# 1940, 1950  ####################################################################################
time.56 <-rbind(time5, time6)
str(time.56)
time.56.core <- betapart.core(time.56[4:30])
b.56 <- beta.pair(time.56.core, index.family = "sorensen")
str(b.56)

time.56$Dec <- c("1940","1940","1940","1940","1940","1940","1940","1940","1950","1950","1950","1950","1950","1950","1950","1950")
time.56$Dec <- as.factor(time.56$Dec)
levels(time.56$Dec)

## beta.sor - total betadiversity
nmds.56.sor <- metaMDS(b.56$beta.sor, trymax = 500, autotransform = TRUE)
nmds.56.sor
stressplot(nmds.56.sor)
goodness(nmds.56.sor)
nmds.56.sor$stress
plot(nmds.56.sor)


png("LB_Museum_sor_1940-50.png", width = 1000, height = 800, pointsize = 20)
ordiplot(nmds.56.sor, type="n", xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.8))
points(nmds.56.sor, dis="sites", cex=2, pch = pchvec2[time.56$Regions], col=colvec[time.56$Dec])
ordiellipse(nmds.56.sor, groups=time.56$Regions, display="sites", draw="lines",
            col=c("black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1940s","1950s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
legend("topleft", legend = c("AP","IOTP","LEGP","WLEB"), cex=1.5, bty="n", 
       pch = c(16,17,15,18), col=c("black"))
dev.off()


# PERMANOVA
adonis2(b.56$beta.sor ~ time.56$Decade, permutations = 999)
pairwise.adonis(b.56$beta.sor, time.56$Decade)
adonis2(b.56$beta.sor ~ time.56$Regions, permutations = 999)
pairwise.adonis(b.56$beta.sor, time.56$Regions)

adonis2(b.56$beta.sor ~ time.56$Decade * time.56$Regions, permutations = 999)

# BETADISPER
beta.sor.56.d <- betadisper(b.56$beta.sor, time.56$Decade, type = c("median"))
beta.sor.56.d
anova(beta.sor.56.d)
plot(beta.sor.56.d)
boxplot(beta.sor.56.d, ylab = "Distance to median")
TukeyHSD(beta.sor.56.d, which = "group", conf.level = 0.95)

beta.sor.56.r <- betadisper(b.56$beta.sor, time.56$Regions, type = c("median"))
beta.sor.56.r
anova(beta.sor.56.r)
plot(beta.sor.56.r)
boxplot(beta.sor.56.r, ylab = "Distance to median")
TukeyHSD(beta.sor.56.r, which = "group", conf.level = 0.95)


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

# interactions between decade and region
adonis2(b.56$beta.sim ~ time.56$Decade * time.56$Regions, permutations = 999)

# BETADISPER
beta.sim.56.d <- betadisper(b.56$beta.sim, time.56$Decade, type = c("median"))
beta.sim.56.d
anova(beta.sim.56.d)
plot(beta.sim.56.d)
boxplot(beta.sim.56.d, ylab = "Distance to median")
TukeyHSD(beta.sim.56.d, which = "group", conf.level = 0.95)

beta.sim.56.r <- betadisper(b.56$beta.sim, time.56$Regions, type = c("median"))
beta.sim.56.r
anova(beta.sim.56.r)
plot(beta.sim.56.r)
boxplot(beta.sim.56.r, ylab = "Distance to median")
TukeyHSD(beta.sim.56.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.56$beta.sne ~ time.56$Decade * time.56$Regions, permutations = 999)

# BETADISPER
beta.sne.56.d <- betadisper(b.56$beta.sne, time.56$Decade, type = c("median"))
beta.sne.56.d
anova(beta.sne.56.d)
plot(beta.sne.56.d)
boxplot(beta.sne.56.d, ylab = "Distance to median")
TukeyHSD(beta.sne.56.d, which = "group", conf.level = 0.95)

beta.sne.56.r <- betadisper(b.56$beta.sne, time.56$Regions, type = c("median"))
beta.sne.56.r
anova(beta.sne.56.r)
plot(beta.sne.56.r)
boxplot(beta.sne.56.r, ylab = "Distance to median")
TukeyHSD(beta.sne.56.r, which = "group", conf.level = 0.95)

# 1950, 1960  ####################################################################################
time.67 <-rbind(time6, time7)
str(time.67)
time.67.core <- betapart.core(time.67[4:30])
b.67 <- beta.pair(time.67.core, index.family = "sorensen")
str(b.67)

time.67$Dec <- c("1950","1950","1950","1950","1950","1950","1950","1950","1960","1960","1960","1960","1960","1960","1960","1960")
time.67$Dec <- as.factor(time.67$Dec)
levels(time.67$Dec)

## beta.sor - total betadiversity
nmds.67.sor <- metaMDS(b.67$beta.sor, trymax = 500, autotransform = TRUE)
nmds.67.sor
stressplot(nmds.67.sor)
goodness(nmds.67.sor)
nmds.67.sor$stress
plot(nmds.67.sor)


png("LB_Museum_sor_1950-60.png", width = 1000, height = 800, pointsize = 20)
ordiplot(nmds.67.sor, type="n", xlim = c(-0.8, 0.8), ylim = c(-0.5, 0.5))
points(nmds.67.sor, dis="sites", cex=2, pch = pchvec2[time.67$Regions], col=colvec[time.67$Dec])
ordiellipse(nmds.67.sor, groups=time.67$Regions, display="sites", draw="lines",
            col=c("black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1950s","1960s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
legend("topleft", legend = c("AP","IOTP","LEGP","WLEB"), cex=1.5, bty="n", 
       pch = c(16,17,15,18), col=c("black"))
dev.off()


# PERMANOVA
adonis2(b.67$beta.sor ~ time.67$Decade, permutations = 999)
pairwise.adonis(b.67$beta.sor, time.67$Decade)
adonis2(b.67$beta.sor ~ time.67$Regions, permutations = 999)
pairwise.adonis(b.67$beta.sor, time.67$Regions)

# interaction between decade and region
adonis2(b.67$beta.sor ~ time.67$Decade * time.67$Regions, permutations = 999)

# BETADISPER
beta.sor.67.d <- betadisper(b.67$beta.sor, time.67$Decade, type = c("median"))
beta.sor.67.d
anova(beta.sor.67.d)
plot(beta.sor.67.d)
boxplot(beta.sor.67.d, ylab = "Distance to median")
TukeyHSD(beta.sor.67.d, which = "group", conf.level = 0.95)

beta.sor.67.r <- betadisper(b.67$beta.sor, time.67$Regions, type = c("median"))
beta.sor.67.r
anova(beta.sor.67.r)
plot(beta.sor.67.r)
boxplot(beta.sor.67.r, ylab = "Distance to median")
TukeyHSD(beta.sor.67.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.67$beta.sim ~ time.67$Decade * time.67$Regions, permutations = 999)

# BETADISPER
beta.sim.67.d <- betadisper(b.67$beta.sim, time.67$Decade, type = c("median"))
beta.sim.67.d
anova(beta.sim.67.d)
plot(beta.sim.67.d)
boxplot(beta.sim.67.d, ylab = "Distance to median")
TukeyHSD(beta.sim.67.d, which = "group", conf.level = 0.95)

beta.sim.67.r <- betadisper(b.67$beta.sim, time.67$Regions, type = c("median"))
beta.sim.67.r
anova(beta.sim.67.r)
plot(beta.sim.67.r)
boxplot(beta.sim.67.r, ylab = "Distance to median")
TukeyHSD(beta.sim.67.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.67$beta.sne ~ time.67$Decade * time.67$Regions, permutations = 999)

# BETADISPER
beta.sne.67.d <- betadisper(b.67$beta.sne, time.67$Decade, type = c("median"))
beta.sne.67.d
anova(beta.sne.67.d)
plot(beta.sne.67.d)
boxplot(beta.sne.67.d, ylab = "Distance to median")
TukeyHSD(beta.sne.67.d, which = "group", conf.level = 0.95)

beta.sne.67.r <- betadisper(b.67$beta.sne, time.67$Regions, type = c("median"))
beta.sne.67.r
anova(beta.sne.67.r)
plot(beta.sne.67.r)
boxplot(beta.sne.67.r, ylab = "Distance to median")
TukeyHSD(beta.sne.67.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.78$beta.sor ~ time.78$Decade * time.78$Regions, permutations = 999)

# BETADISPER
beta.sor.78.d <- betadisper(b.78$beta.sor, time.78$Decade, type = c("median"))
beta.sor.78.d
anova(beta.sor.78.d)
plot(beta.sor.78.d)
boxplot(beta.sor.78.d, ylab = "Distance to median")
TukeyHSD(beta.sor.78.d, which = "group", conf.level = 0.95)

beta.sor.78.r <- betadisper(b.78$beta.sor, time.78$Regions, type = c("median"))
beta.sor.78.r
anova(beta.sor.78.r)
plot(beta.sor.78.r)
boxplot(beta.sor.78.r, ylab = "Distance to median")
TukeyHSD(beta.sor.78.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.78$beta.sim ~ time.78$Decade * time.78$Regions, permutations = 999)

# BETADISPER
beta.sim.78.d <- betadisper(b.78$beta.sim, time.78$Decade, type = c("median"))
beta.sim.78.d
anova(beta.sim.78.d)
plot(beta.sim.78.d)
boxplot(beta.sim.78.d, ylab = "Distance to median")
TukeyHSD(beta.sim.78.d, which = "group", conf.level = 0.95)

beta.sim.78.r <- betadisper(b.78$beta.sim, time.78$Regions, type = c("median"))
beta.sim.78.r
anova(beta.sim.78.r)
plot(beta.sim.78.r)
boxplot(beta.sim.78.r, ylab = "Distance to median")
TukeyHSD(beta.sim.78.r, which = "group", conf.level = 0.95)


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

# interactions between decade and region
adonis2(b.78$beta.sne ~ time.78$Decade * time.78$Regions, permutations = 999)

# BETADISPER
beta.sne.78.d <- betadisper(b.78$beta.sne, time.78$Decade, type = c("median"))
beta.sne.78.d
anova(beta.sne.78.d)
plot(beta.sne.78.d)
boxplot(beta.sne.78.d, ylab = "Distance to median")
TukeyHSD(beta.sne.78.d, which = "group", conf.level = 0.95)

beta.sne.78.r <- betadisper(b.78$beta.sne, time.78$Regions, type = c("median"))
beta.sne.78.r
anova(beta.sne.78.r)
plot(beta.sne.78.r)
boxplot(beta.sne.78.r, ylab = "Distance to median")
TukeyHSD(beta.sne.78.r, which = "group", conf.level = 0.95)


# 1970, 1980  ####################################################################################
time.89 <-rbind(time8, time9)
str(time.89)
time.89.core <- betapart.core(time.89[4:30])
b.89 <- beta.pair(time.89.core, index.family = "sorensen")
str(b.89)

time.89$Dec <- c("1970","1970","1970","1970","1970","1970","1970","1970","1980","1980","1980","1980","1980","1980","1980","1980")
time.89$Dec <- as.factor(time.89$Dec)
levels(time.89$Dec)

## beta.sor - total betadiversity
nmds.89.sor <- metaMDS(b.89$beta.sor, trymax = 500, autotransform = TRUE)
nmds.89.sor
stressplot(nmds.89.sor)
goodness(nmds.89.sor)
nmds.89.sor$stress
plot(nmds.89.sor)

ordiplot(nmds.89.sor, type="n", xlim = c(-0.8, 0.2), ylim = c(-0.3, 0.4))
points(nmds.89.sor, dis="sites", cex=2, pch = pchvec2[time.89$Regions], col=colvec[time.89$Dec])
ordiellipse(nmds.89.sor, groups=time.89$Dec, display="sites", draw="lines",
            col=c("black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1970s","1980s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
legend("topleft", legend = c("AP","IOTP","LEGP","WLEB"), cex=1.5, bty="n", 
       pch = c(16,17,15,18), col=c("black"))

# PERMANOVA
adonis2(b.89$beta.sor ~ time.89$Decade, permutations = 999)
pairwise.adonis(b.89$beta.sor, time.89$Decade)
adonis2(b.89$beta.sor ~ time.89$Regions, permutations = 999)
pairwise.adonis(b.89$beta.sor, time.89$Regions)


# interaction between decade and region
adonis2(b.89$beta.sor ~ time.89$Decade * time.89$Regions, permutations = 999)

# BETADISPER
beta.sor.89.d <- betadisper(b.89$beta.sor, time.89$Decade, type = c("median"))
beta.sor.89.d
anova(beta.sor.89.d)
plot(beta.sor.89.d)
boxplot(beta.sor.89.d, ylab = "Distance to median")
TukeyHSD(beta.sor.89.d, which = "group", conf.level = 0.95)

beta.sor.89.r <- betadisper(b.89$beta.sor, time.89$Regions, type = c("median"))
beta.sor.89.r
anova(beta.sor.89.r)
plot(beta.sor.89.r)
boxplot(beta.sor.89.r, ylab = "Distance to median")
TukeyHSD(beta.sor.89.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.89$beta.sim ~ time.89$Decade * time.89$Regions, permutations = 999)

# BETADISPER
beta.sim.89.d <- betadisper(b.89$beta.sim, time.89$Decade, type = c("median"))
beta.sim.89.d
anova(beta.sim.89.d)
plot(beta.sim.89.d)
boxplot(beta.sim.89.d, ylab = "Distance to median")
TukeyHSD(beta.sim.89.d, which = "group", conf.level = 0.95)

beta.sim.89.r <- betadisper(b.89$beta.sim, time.89$Regions, type = c("median"))
beta.sim.89.r
anova(beta.sim.89.r)
plot(beta.sim.89.r)
boxplot(beta.sim.89.r, ylab = "Distance to median")
TukeyHSD(beta.sim.89.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.89$beta.sne ~ time.89$Decade * time.89$Regions, permutations = 999)

# BETADISPER
beta.sne.89.d <- betadisper(b.89$beta.sne, time.89$Decade, type = c("median"))
beta.sne.89.d
anova(beta.sne.89.d)
plot(beta.sne.89.d)
boxplot(beta.sne.89.d, ylab = "Distance to median")
TukeyHSD(beta.sne.89.d, which = "group", conf.level = 0.95)

beta.sne.89.r <- betadisper(b.89$beta.sne, time.89$Regions, type = c("median"))
beta.sne.89.r
anova(beta.sne.89.r)
plot(beta.sne.89.r)
boxplot(beta.sne.89.r, ylab = "Distance to median")
TukeyHSD(beta.sne.89.r, which = "group", conf.level = 0.95)


# 1980, 1990  ####################################################################################
time.910 <-rbind(time9, time10)
str(time.910)
time.910.core <- betapart.core(time.910[4:30])
b.910 <- beta.pair(time.910.core, index.family = "sorensen")
str(b.910)

time.910$Dec <- c("1980","1980","1980","1980","1980","1980","1980","1980","1990","1990","1990","1990","1990","1990","1990")
time.910$Dec <- as.factor(time.910$Dec)
levels(time.910$Dec)

## beta.sor - total betadiversity
nmds.910.sor <- metaMDS(b.910$beta.sor, trymax = 500, autotransform = TRUE)
nmds.910.sor
stressplot(nmds.910.sor)
goodness(nmds.910.sor)
nmds.910.sor$stress
plot(nmds.910.sor)

png("LB_Museum_sor_1980-90.png", width = 1000, height = 800, pointsize = 20)
ordiplot(nmds.910.sor, type="n", xlim = c(-0.6, 0.6), ylim = c(-0.4, 0.4))
points(nmds.910.sor, dis="sites", cex=2, pch = pchvec2[time.910$Regions], col=colvec[time.910$Dec])
ordiellipse(nmds.910.sor, groups=time.910$Dec, display="sites", draw="lines",
            col=c("black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1980s","1990s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
legend("topleft", legend = c("AP","IOTP","LEGP","WLEB"), cex=1.5, bty="n", 
       pch = c(16,17,15,18), col=c("black"))
dev.off()

# PERMANOVA
adonis2(b.910$beta.sor ~ time.910$Decade, permutations = 999)
pairwise.adonis(b.910$beta.sor, time.910$Decade)
adonis2(b.910$beta.sor ~ time.910$Regions, permutations = 999)
pairwise.adonis(b.910$beta.sor, time.910$Regions)

# interaction between decade and region
adonis2(b.910$beta.sor ~ time.910$Decade * time.910$Regions, permutations = 999)

# BETADISPER
beta.sor.910.d <- betadisper(b.910$beta.sor, time.910$Decade, type = c("median"))
beta.sor.910.d
anova(beta.sor.910.d)
plot(beta.sor.910.d)
boxplot(beta.sor.910.d, ylab = "Distance to median")
TukeyHSD(beta.sor.910.d, which = "group", conf.level = 0.95)

beta.sor.910.r <- betadisper(b.910$beta.sor, time.910$Regions, type = c("median"))
beta.sor.910.r
anova(beta.sor.910.r)
plot(beta.sor.910.r)
boxplot(beta.sor.910.r, ylab = "Distance to median")
TukeyHSD(beta.sor.910.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.910$beta.sim ~ time.910$Decade * time.910$Regions, permutations = 999)

# BETADISPER
beta.sim.910.d <- betadisper(b.910$beta.sim, time.910$Decade, type = c("median"))
beta.sim.910.d
anova(beta.sim.910.d)
plot(beta.sim.910.d)
boxplot(beta.sim.910.d, ylab = "Distance to median")
TukeyHSD(beta.sim.910.d, which = "group", conf.level = 0.95)

beta.sim.910.r <- betadisper(b.910$beta.sim, time.910$Regions, type = c("median"))
beta.sim.910.r
anova(beta.sim.910.r)
plot(beta.sim.910.r)
boxplot(beta.sim.910.r, ylab = "Distance to median")
TukeyHSD(beta.sim.910.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.910$beta.sne ~ time.910$Decade * time.910$Regions, permutations = 999)

# BETADISPER
beta.sne.910.d <- betadisper(b.910$beta.sne, time.910$Decade, type = c("median"))
beta.sne.910.d
anova(beta.sne.910.d)
plot(beta.sne.910.d)
boxplot(beta.sne.910.d, ylab = "Distance to median")
TukeyHSD(beta.sne.910.d, which = "group", conf.level = 0.95)

beta.sne.910.r <- betadisper(b.910$beta.sne, time.910$Regions, type = c("median"))
beta.sne.910.r
anova(beta.sne.910.r)
plot(beta.sne.910.r)
boxplot(beta.sne.910.r, ylab = "Distance to median")
TukeyHSD(beta.sne.910.r, which = "group", conf.level = 0.95)


# 1990, 2000  ####################################################################################
time.1011 <-rbind(time10, time11)
str(time.1011)
time.1011.core <- betapart.core(time.1011[4:30])
b.1011 <- beta.pair(time.1011.core, index.family = "sorensen")
str(b.1011)

time.1011$Dec <- c("1990","1990","1990","1990","1990","1990","1990","2000","2000","2000","2000","2000","2000","2000","2000")
time.1011$Dec <- as.factor(time.1011$Dec)
levels(time.1011$Dec)

## beta.sor - total betadiversity
nmds.1011.sor <- metaMDS(b.1011$beta.sor, trymax = 500, autotransform = TRUE)
nmds.1011.sor
stressplot(nmds.1011.sor)
goodness(nmds.1011.sor)
nmds.1011.sor$stress
plot(nmds.1011.sor)


png("LB_Museum_sor_1990-2000.png", width = 1000, height = 800, pointsize = 20)
ordiplot(nmds.1011.sor, type="n", xlim = c(-0.6, 0.8), ylim = c(-0.6, 0.4))
points(nmds.1011.sor, dis="sites", cex=2, pch = pchvec2[time.1011$Regions], col=colvec[time.1011$Dec])
ordiellipse(nmds.1011.sor, groups=time.1011$Dec, display="sites", draw="lines",
            col=c("black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1990s","2000s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
legend("topleft", legend = c("AP","IOTP","LEGP","WLEB"), cex=1.5, bty="n", 
       pch = c(16,17,15,18), col=c("black"))
dev.off()


# PERMANOVA
adonis2(b.1011$beta.sor ~ time.1011$Decade, permutations = 999)
pairwise.adonis(b.1011$beta.sor, time.1011$Decade)
adonis2(b.1011$beta.sor ~ time.1011$Regions, permutations = 999)
pairwise.adonis(b.1011$beta.sor, time.1011$Regions)

# interaction between decade and regions
adonis2(b.1011$beta.sor ~ time.1011$Decade * time.1011$Regions, permutations = 999)

# BETADISPER
beta.sor.1011.d <- betadisper(b.1011$beta.sor, time.1011$Decade, type = c("median"))
beta.sor.1011.d
anova(beta.sor.1011.d)
plot(beta.sor.1011.d)
boxplot(beta.sor.1011.d, ylab = "Distance to median")
TukeyHSD(beta.sor.1011.d, which = "group", conf.level = 0.95)

beta.sor.1011.r <- betadisper(b.1011$beta.sor, time.1011$Regions, type = c("median"))
beta.sor.1011.r
anova(beta.sor.1011.r)
plot(beta.sor.1011.r)
boxplot(beta.sor.1011.r, ylab = "Distance to median")
TukeyHSD(beta.sor.1011.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.1011$beta.sim ~ time.1011$Decade * time.1011$Regions, permutations = 999)

# BETADISPER
beta.sim.1011.d <- betadisper(b.1011$beta.sim, time.1011$Decade, type = c("median"))
beta.sim.1011.d
anova(beta.sim.1011.d)
plot(beta.sim.1011.d)
boxplot(beta.sim.1011.d, ylab = "Distance to median")
TukeyHSD(beta.sim.1011.d, which = "group", conf.level = 0.95)

beta.sim.1011.r <- betadisper(b.1011$beta.sim, time.1011$Regions, type = c("median"))
beta.sim.1011.r
anova(beta.sim.1011.r)
plot(beta.sim.1011.r)
boxplot(beta.sim.1011.r, ylab = "Distance to median")
TukeyHSD(beta.sim.1011.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.1011$beta.sne ~ time.1011$Decade * time.1011$Regions, permutations = 999)

# BETADISPER
beta.sne.1011.d <- betadisper(b.1011$beta.sne, time.1011$Decade, type = c("median"))
beta.sne.1011.d
anova(beta.sne.1011.d)
plot(beta.sne.1011.d)
boxplot(beta.sne.1011.d, ylab = "Distance to median")
TukeyHSD(beta.sne.1011.d, which = "group", conf.level = 0.95)

beta.sne.1011.r <- betadisper(b.1011$beta.sne, time.1011$Regions, type = c("median"))
beta.sne.1011.r
anova(beta.sne.1011.r)
plot(beta.sne.1011.r)
boxplot(beta.sne.1011.r, ylab = "Distance to median")
TukeyHSD(beta.sne.1011.r, which = "group", conf.level = 0.95)


# 2000, 2010  ####################################################################################
time.1112 <-rbind(time11, time12)
str(time.1112)
time.1112.core <- betapart.core(time.1112[4:30])
b.1112 <- beta.pair(time.1112.core, index.family = "sorensen")
str(b.1112)

time.1112$Dec <- c("2000","2000","2000","2000","2000","2000","2000","2000","2010","2010","2010","2010","2010","2010","2010","2010")
time.1112$Dec <- as.factor(time.1112$Dec)
levels(time.1112$Dec)

## beta.sor - total betadiversity
nmds.1112.sor <- metaMDS(b.1112$beta.sor, trymax = 500, autotransform = TRUE)
nmds.1112.sor
stressplot(nmds.1112.sor)
goodness(nmds.1112.sor)
nmds.1112.sor$stress
plot(nmds.1112.sor)


png("LB_Museum_sor_2000-2010.png", width = 1000, height = 800, pointsize = 20)
ordiplot(nmds.1112.sor, type="n", xlim = c(-0.4, 0.6), ylim = c(-0.4, 0.4))
points(nmds.1112.sor, dis="sites", cex=2, pch = pchvec2[time.1112$Regions], col=colvec[time.1112$Dec])
ordiellipse(nmds.1112.sor, groups=time.1112$Regions, display="sites", draw="lines",
            col=c("black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("2000s","2010s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
legend("topleft", legend = c("AP","IOTP","LEGP","WLEB"), cex=1.5, bty="n", 
       pch = c(16,17,15,18), col=c("black"))
dev.off()


# PERMANOVA
adonis2(b.1112$beta.sor ~ time.1112$Decade, permutations = 999)
pairwise.adonis(b.1112$beta.sor, time.1112$Decade)
adonis2(b.1112$beta.sor ~ time.1112$Regions, permutations = 999)
pairwise.adonis(b.1112$beta.sor, time.1112$Regions)

# interaction between decade and regions
adonis2(b.1112$beta.sor ~ time.1112$Decade * time.1112$Regions, permutations = 999)

# BETADISPER
beta.sor.1112.d <- betadisper(b.1112$beta.sor, time.1112$Decade, type = c("median"))
beta.sor.1112.d
anova(beta.sor.1112.d)
plot(beta.sor.1112.d)
boxplot(beta.sor.1112.d, ylab = "Distance to median")
TukeyHSD(beta.sor.1112.d, which = "group", conf.level = 0.95)

beta.sor.1112.r <- betadisper(b.1112$beta.sor, time.1112$Regions, type = c("median"))
beta.sor.1112.r
anova(beta.sor.1112.r)
plot(beta.sor.1112.r)
boxplot(beta.sor.1112.r, ylab = "Distance to median")
TukeyHSD(beta.sor.1112.r, which = "group", conf.level = 0.95)


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

#interaction between decade and regions
adonis2(b.1112$beta.sim ~ time.1112$Decade * time.1112$Regions, permutations = 999)

# BETADISPER
beta.sim.1112.d <- betadisper(b.1112$beta.sim, time.1112$Decade, type = c("median"))
beta.sim.1112.d
anova(beta.sim.1112.d)
plot(beta.sim.1112.d)
boxplot(beta.sim.1112.d, ylab = "Distance to median")
TukeyHSD(beta.sim.1112.d, which = "group", conf.level = 0.95)

beta.sim.1112.r <- betadisper(b.1112$beta.sim, time.1112$Regions, type = c("median"))
beta.sim.1112.r
anova(beta.sim.1112.r)
plot(beta.sim.1112.r)
boxplot(beta.sim.1112.r, ylab = "Distance to median")
TukeyHSD(beta.sim.1112.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.1112$beta.sne ~ time.1112$Decade * time.1112$Regions, permutations = 999)

# BETADISPER
beta.sne.1112.d <- betadisper(b.1112$beta.sne, time.1112$Decade, type = c("median"))
beta.sne.1112.d
anova(beta.sne.1112.d)
plot(beta.sne.1112.d)
boxplot(beta.sne.1112.d, ylab = "Distance to median")
TukeyHSD(beta.sne.1112.d, which = "group", conf.level = 0.95)

beta.sne.1112.r <- betadisper(b.1112$beta.sne, time.1112$Regions, type = c("median"))
beta.sne.1112.r
anova(beta.sne.1112.r)
plot(beta.sne.1112.r)
boxplot(beta.sne.1112.r, ylab = "Distance to median")
TukeyHSD(beta.sne.1112.r, which = "group", conf.level = 0.95)


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
time9.a <- time9.a[-5,] # no beetles
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

# interaction between decade and regions
adonis2(b.12.a$beta.sor ~ time.12.a$Decade * time.12.a$Regions, permutations = 999)

# BETADISPER
beta.sor.12.a.d <- betadisper(b.12.a$beta.sor, time.12.a$Decade, type = c("median"))
beta.sor.12.a.d
anova(beta.sor.12.a.d)
plot(beta.sor.12.a.d)
boxplot(beta.sor.12.a.d, ylab = "Distance to median")
TukeyHSD(beta.sor.12.a.d, which = "group", conf.level = 0.95)

beta.sor.12.a.r <- betadisper(b.12.a$beta.sor, time.12.a$Regions, type = c("median"))
beta.sor.12.a.r
anova(beta.sor.12.a.r)
plot(beta.sor.12.a.r)
boxplot(beta.sor.12.a.r, ylab = "Distance to median")
TukeyHSD(beta.sor.12.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.12.a$beta.sim ~ time.12.a$Decade * time.12.a$Regions, permutations = 999)

# BETADISPER
beta.sim.12.a.d <- betadisper(b.12.a$beta.sim, time.12.a$Decade, type = c("median"))
beta.sim.12.a.d
anova(beta.sim.12.a.d)
plot(beta.sim.12.a.d)
boxplot(beta.sim.12.a.d, ylab = "Distance to median")
TukeyHSD(beta.sim.12.a.d, which = "group", conf.level = 0.95)

beta.sim.12.a.r <- betadisper(b.12.a$beta.sim, time.12.a$Regions, type = c("median"))
beta.sim.12.a.r
anova(beta.sim.12.a.r)
plot(beta.sim.12.a.r)
boxplot(beta.sim.12.a.r, ylab = "Distance to median")
TukeyHSD(beta.sim.12.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.12.a$beta.sne ~ time.12.a$Decade * time.12.a$Regions, permutations = 999)

# BETADISPER
beta.sne.12.a.d <- betadisper(b.12.a$beta.sne, time.12.a$Decade, type = c("median"))
beta.sne.12.a.d
anova(beta.sne.12.a.d)
plot(beta.sne.12.a.d)
boxplot(beta.sne.12.a.d, ylab = "Distance to median")
TukeyHSD(beta.sne.12.a.d, which = "group", conf.level = 0.95)

beta.sne.12.a.r <- betadisper(b.12.a$beta.sne, time.12.a$Regions, type = c("median"))
beta.sne.12.a.r
anova(beta.sne.12.a.r)
plot(beta.sne.12.a.r)
boxplot(beta.sne.12.a.r, ylab = "Distance to median")
TukeyHSD(beta.sne.12.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.23.a$beta.sor ~ time.23.a$Decade * time.23.a$Regions, permutations = 999)

# BETADISPER
beta.sor.23.a.d <- betadisper(b.23.a$beta.sor, time.23.a$Decade, type = c("median"))
beta.sor.23.a.d
anova(beta.sor.23.a.d)
plot(beta.sor.23.a.d)
boxplot(beta.sor.23.a.d, ylab = "Distance to median")
TukeyHSD(beta.sor.23.a.d, which = "group", conf.level = 0.95)

beta.sor.23.a.r <- betadisper(b.23.a$beta.sor, time.23.a$Regions, type = c("median"))
beta.sor.23.a.r
anova(beta.sor.23.a.r)
plot(beta.sor.23.a.r)
boxplot(beta.sor.23.a.r, ylab = "Distance to median")
TukeyHSD(beta.sor.23.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.23.a$beta.sim ~ time.23.a$Decade * time.23.a$Regions, permutations = 999)

# BETADISPER
beta.sim.23.a.d <- betadisper(b.23.a$beta.sim, time.23.a$Decade, type = c("median"))
beta.sim.23.a.d
anova(beta.sim.23.a.d)
plot(beta.sim.23.a.d)
boxplot(beta.sim.23.a.d, ylab = "Distance to median")
TukeyHSD(beta.sim.23.a.d, which = "group", conf.level = 0.95)

beta.sim.23.a.r <- betadisper(b.23.a$beta.sim, time.23.a$Regions, type = c("median"))
beta.sim.23.a.r
anova(beta.sim.23.a.r)
plot(beta.sim.23.a.r)
boxplot(beta.sim.23.a.r, ylab = "Distance to median")
TukeyHSD(beta.sim.23.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.23.a$beta.sne ~ time.23.a$Decade * time.23.a$Regions, permutations = 999)

# BETADISPER
beta.sne.23.a.d <- betadisper(b.23.a$beta.sne, time.23.a$Decade, type = c("median"))
beta.sne.23.a.d
anova(beta.sne.23.a.d)
plot(beta.sne.23.a.d)
boxplot(beta.sne.23.a.d, ylab = "Distance to median")
TukeyHSD(beta.sne.23.a.d, which = "group", conf.level = 0.95)

beta.sne.23.a.r <- betadisper(b.23.a$beta.sne, time.23.a$Regions, type = c("median"))
beta.sne.23.a.r
anova(beta.sne.23.a.r)
plot(beta.sne.23.a.r)
boxplot(beta.sne.23.a.r, ylab = "Distance to median")
TukeyHSD(beta.sne.23.a.r, which = "group", conf.level = 0.95)


# 1920. 1930  ####################################################################################
time.34.a <-rbind(time3.a, time4.a)
str(time.34.a)
time.34.a$Decade <- as.factor(lb.b.matrix$Decade)
time.34.a.core <- betapart.core(time.34.a[4:24])
b.34.a <- beta.pair(time.34.a.core, index.family = "sorensen")
str(b.34.a)
time.34.a$Dec <- c("1920","1920","1920","1920","1920","1920","1930","1930","1930","1930","1930","1930","1930","1930")
time.34.a$Dec <- as.factor(time.34.a$Dec)
levels(time.34.a$Dec)

## beta.sor - total betadiversity
nmds.34.a.sor <- metaMDS(b.34.a$beta.sor, trymax = 500, autotransform = TRUE)
nmds.34.a.sor
stressplot(nmds.34.a.sor)
goodness(nmds.34.a.sor)
nmds.34.a.sor$stress
plot(nmds.34.a.sor)


colvec <- c("gray56","black")
pchvec1 <- c(16, 17)
pchvec2 <- c(16, 17, 15, 18)

png("LB_Museum_sor_1920-30.png", width = 1000, height = 800, pointsize = 20)
ordiplot(nmds.34.a.sor, type="n", xlim = c(-0.4, 0.4), ylim = c(-0.4, 0.4))
points(nmds.34.a.sor, dis="sites", cex=2, pch = pchvec2[time.34.a$Regions], col=colvec[time.34.a$Dec])
ordiellipse(nmds.34.a.sor, groups=time.34.a$Dec, display="sites", draw="lines",
            col=c("gray56", "black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1920s","1930s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
legend("topleft", legend = c("AP","IOTP","LEGP","WLEB"), cex=1.5, bty="n", 
       pch = c(16,17,15,18), col=c("black"))
dev.off()


# PERMANOVA
adonis2(b.34.a$beta.sor ~ time.34.a$Decade, permutations = 999)
pairwise.adonis(b.34.a$beta.sor, time.34.a$Decade)
adonis2(b.34.a$beta.sor ~ time.34.a$Regions, permutations = 999)
pairwise.adonis(b.34.a$beta.sor, time.34.a$Regions)

# interaction between decade and region
adonis2(b.34.a$beta.sor ~ time.34.a$Decade * time.34.a$Regions, permutations = 999)

# BETADISPER
beta.sor.34.a.d <- betadisper(b.34.a$beta.sor, time.34.a$Decade, type = c("median"))
beta.sor.34.a.d
anova(beta.sor.34.a.d)
plot(beta.sor.34.a.d)
boxplot(beta.sor.34.a.d, ylab = "Distance to median")
TukeyHSD(beta.sor.34.a.d, which = "group", conf.level = 0.95)

beta.sor.34.a.r <- betadisper(b.34.a$beta.sor, time.34.a$Regions, type = c("median"))
beta.sor.34.a.r
anova(beta.sor.34.a.r)
plot(beta.sor.34.a.r)
boxplot(beta.sor.34.a.r, ylab = "Distance to median")
TukeyHSD(beta.sor.34.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.34.a$beta.sim ~ time.34.a$Decade * time.34.a$Regions, permutations = 999)

# BETADISPER
beta.sim.34.a.d <- betadisper(b.34.a$beta.sim, time.34.a$Decade, type = c("median"))
beta.sim.34.a.d
anova(beta.sim.34.a.d)
plot(beta.sim.34.a.d)
boxplot(beta.sim.34.a.d, ylab = "Distance to median")
TukeyHSD(beta.sim.34.a.d, which = "group", conf.level = 0.95)

beta.sim.34.a.r <- betadisper(b.34.a$beta.sim, time.34.a$Regions, type = c("median"))
beta.sim.34.a.r
anova(beta.sim.34.a.r)
plot(beta.sim.34.a.r)
boxplot(beta.sim.34.a.r, ylab = "Distance to median")
TukeyHSD(beta.sim.34.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.34.a$beta.sne ~ time.34.a$Decade * time.34.a$Regions, permutations = 999)

# BETADISPER
beta.sne.34.a.d <- betadisper(b.34.a$beta.sne, time.34.a$Decade, type = c("median"))
beta.sne.34.a.d
anova(beta.sne.34.a.d)
plot(beta.sne.34.a.d)
boxplot(beta.sne.34.a.d, ylab = "Distance to median")
TukeyHSD(beta.sne.34.a.d, which = "group", conf.level = 0.95)

beta.sne.34.a.r <- betadisper(b.34.a$beta.sne, time.34.a$Regions, type = c("median"))
beta.sne.34.a.r
anova(beta.sne.34.a.r)
plot(beta.sne.34.a.r)
boxplot(beta.sne.34.a.r, ylab = "Distance to median")
TukeyHSD(beta.sne.34.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.45.a$beta.sor ~ time.45.a$Decade * time.45.a$Regions, permutations = 999)

# BETADISPER
beta.sor.45.a.d <- betadisper(b.45.a$beta.sor, time.45.a$Decade, type = c("median"))
beta.sor.45.a.d
anova(beta.sor.45.a.d)
plot(beta.sor.45.a.d)
boxplot(beta.sor.45.a.d, ylab = "Distance to median")
TukeyHSD(beta.sor.45.a.d, which = "group", conf.level = 0.95)

beta.sor.45.a.r <- betadisper(b.45.a$beta.sor, time.45.a$Regions, type = c("median"))
beta.sor.45.a.r
anova(beta.sor.45.a.r)
plot(beta.sor.45.a.r)
boxplot(beta.sor.45.a.r, ylab = "Distance to median")
TukeyHSD(beta.sor.45.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and region
adonis2(b.45.a$beta.sim ~ time.45.a$Decade * time.45.a$Regions, permutations = 999)

# BETADISPER
beta.sim.45.a.d <- betadisper(b.45.a$beta.sim, time.45.a$Decade, type = c("median"))
beta.sim.45.a.d
anova(beta.sim.45.a.d)
plot(beta.sim.45.a.d)
boxplot(beta.sim.45.a.d, ylab = "Distance to median")
TukeyHSD(beta.sim.45.a.d, which = "group", conf.level = 0.95)

beta.sim.45.a.r <- betadisper(b.45.a$beta.sim, time.45.a$Regions, type = c("median"))
beta.sim.45.a.r
anova(beta.sim.45.a.r)
plot(beta.sim.45.a.r)
boxplot(beta.sim.45.a.r, ylab = "Distance to median")
TukeyHSD(beta.sim.45.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.45.a$beta.sne ~ time.45.a$Decade * time.45.a$Regions, permutations = 999)

# BETADISPER
beta.sne.45.a.d <- betadisper(b.45.a$beta.sne, time.45.a$Decade, type = c("median"))
beta.sne.45.a.d
anova(beta.sne.45.a.d)
plot(beta.sne.45.a.d)
boxplot(beta.sne.45.a.d, ylab = "Distance to median")
TukeyHSD(beta.sne.45.a.d, which = "group", conf.level = 0.95)

beta.sne.45.a.r <- betadisper(b.45.a$beta.sne, time.45.a$Regions, type = c("median"))
beta.sne.45.a.r
anova(beta.sne.45.a.r)
plot(beta.sne.45.a.r)
boxplot(beta.sne.45.a.r, ylab = "Distance to median")
TukeyHSD(beta.sne.45.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.56.a$beta.sor ~ time.56.a$Decade * time.56.a$Regions, permutations = 999)

# BETADISPER
beta.sor.56.a.d <- betadisper(b.56.a$beta.sor, time.56.a$Decade, type = c("median"))
beta.sor.56.a.d
anova(beta.sor.56.a.d)
plot(beta.sor.56.a.d)
boxplot(beta.sor.56.a.d, ylab = "Distance to median")
TukeyHSD(beta.sor.56.a.d, which = "group", conf.level = 0.95)

beta.sor.56.a.r <- betadisper(b.56.a$beta.sor, time.56.a$Regions, type = c("median"))
beta.sor.56.a.r
anova(beta.sor.56.a.r)
plot(beta.sor.56.a.r)
boxplot(beta.sor.56.a.r, ylab = "Distance to median")
TukeyHSD(beta.sor.56.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.56.a$beta.sim ~ time.56.a$Decade * time.56.a$Regions, permutations = 999)

# BETADISPER
beta.sim.56.a.d <- betadisper(b.56.a$beta.sim, time.56.a$Decade, type = c("median"))
beta.sim.56.a.d
anova(beta.sim.56.a.d)
plot(beta.sim.56.a.d)
boxplot(beta.sim.56.a.d, ylab = "Distance to median")
TukeyHSD(beta.sim.56.a.d, which = "group", conf.level = 0.95)

beta.sim.56.a.r <- betadisper(b.56.a$beta.sim, time.56.a$Regions, type = c("median"))
beta.sim.56.a.r
anova(beta.sim.56.a.r)
plot(beta.sim.56.a.r)
boxplot(beta.sim.56.a.r, ylab = "Distance to median")
TukeyHSD(beta.sim.56.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.56.a$beta.sne ~ time.56.a$Decade * time.56.a$Regions, permutations = 999)

# BETADISPER
beta.sne.56.a.d <- betadisper(b.56.a$beta.sne, time.56.a$Decade, type = c("median"))
beta.sne.56.a.d
anova(beta.sne.56.a.d)
plot(beta.sne.56.a.d)
boxplot(beta.sne.56.a.d, ylab = "Distance to median")
TukeyHSD(beta.sne.56.a.d, which = "group", conf.level = 0.95)

beta.sne.56.a.r <- betadisper(b.56.a$beta.sne, time.56.a$Regions, type = c("median"))
beta.sne.56.a.r
anova(beta.sne.56.a.r)
plot(beta.sne.56.a.r)
boxplot(beta.sne.56.a.r, ylab = "Distance to median")
TukeyHSD(beta.sne.56.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.67.a$beta.sor ~ time.67.a$Decade * time.67.a$Regions, permutations = 999)

# BETADISPER
beta.sor.67.a.d <- betadisper(b.67.a$beta.sor, time.67.a$Decade, type = c("median"))
beta.sor.67.a.d
anova(beta.sor.67.a.d)
plot(beta.sor.67.a.d)
boxplot(beta.sor.67.a.d, ylab = "Distance to median")
TukeyHSD(beta.sor.67.a.d, which = "group", conf.level = 0.95)

beta.sor.67.a.r <- betadisper(b.67.a$beta.sor, time.67.a$Regions, type = c("median"))
beta.sor.67.a.r
anova(beta.sor.67.a.r)
plot(beta.sor.67.a.r)
boxplot(beta.sor.67.a.r, ylab = "Distance to median")
TukeyHSD(beta.sor.67.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.67.a$beta.sim ~ time.67.a$Decade * time.67.a$Regions, permutations = 999)

# BETADISPER
beta.sim.67.a.d <- betadisper(b.67.a$beta.sim, time.67.a$Decade, type = c("median"))
beta.sim.67.a.d
anova(beta.sim.67.a.d)
plot(beta.sim.67.a.d)
boxplot(beta.sim.67.a.d, ylab = "Distance to median")
TukeyHSD(beta.sim.67.a.d, which = "group", conf.level = 0.95)

beta.sim.67.a.r <- betadisper(b.67.a$beta.sim, time.67.a$Regions, type = c("median"))
beta.sim.67.a.r
anova(beta.sim.67.a.r)
plot(beta.sim.67.a.r)
boxplot(beta.sim.67.a.r, ylab = "Distance to median")
TukeyHSD(beta.sim.67.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.67.a$beta.sne ~ time.67.a$Decade * time.67.a$Regions, permutations = 999)

# BETADISPER
beta.sne.67.a.d <- betadisper(b.67.a$beta.sne, time.67.a$Decade, type = c("median"))
beta.sne.67.a.d
anova(beta.sne.67.a.d)
plot(beta.sne.67.a.d)
boxplot(beta.sne.67.a.d, ylab = "Distance to median")
TukeyHSD(beta.sne.67.a.d, which = "group", conf.level = 0.95)

beta.sne.67.a.r <- betadisper(b.67.a$beta.sne, time.67.a$Regions, type = c("median"))
beta.sne.67.a.r
anova(beta.sne.67.a.r)
plot(beta.sne.67.a.r)
boxplot(beta.sne.67.a.r, ylab = "Distance to median")
TukeyHSD(beta.sne.67.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.78.a$beta.sor ~ time.78.a$Decade * time.78.a$Regions, permutations = 999)

# BETADISPER
beta.sor.78.a.d <- betadisper(b.78.a$beta.sor, time.78.a$Decade, type = c("median"))
beta.sor.78.a.d
anova(beta.sor.78.a.d)
plot(beta.sor.78.a.d)
boxplot(beta.sor.78.a.d, ylab = "Distance to median")
TukeyHSD(beta.sor.78.a.d, which = "group", conf.level = 0.95)

beta.sor.78.a.r <- betadisper(b.78.a$beta.sor, time.78.a$Regions, type = c("median"))
beta.sor.78.a.r
anova(beta.sor.78.a.r)
plot(beta.sor.78.a.r)
boxplot(beta.sor.78.a.r, ylab = "Distance to median")
TukeyHSD(beta.sor.78.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.78.a$beta.sim ~ time.78.a$Decade * time.78.a$Regions, permutations = 999)

# BETADISPER
beta.sim.78.a.d <- betadisper(b.78.a$beta.sim, time.78.a$Decade, type = c("median"))
beta.sim.78.a.d
anova(beta.sim.78.a.d)
plot(beta.sim.78.a.d)
boxplot(beta.sim.78.a.d, ylab = "Distance to median")
TukeyHSD(beta.sim.78.a.d, which = "group", conf.level = 0.95)

beta.sim.78.a.r <- betadisper(b.78.a$beta.sim, time.78.a$Regions, type = c("median"))
beta.sim.78.a.r
anova(beta.sim.78.a.r)
plot(beta.sim.78.a.r)
boxplot(beta.sim.78.a.r, ylab = "Distance to median")
TukeyHSD(beta.sim.78.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.78.a$beta.sne ~ time.78.a$Decade * time.78.a$Regions, permutations = 999)

# BETADISPER
beta.sne.78.a.d <- betadisper(b.78.a$beta.sne, time.78.a$Decade, type = c("median"))
beta.sne.78.a.d
anova(beta.sne.78.a.d)
plot(beta.sne.78.a.d)
boxplot(beta.sne.78.a.d, ylab = "Distance to median")
TukeyHSD(beta.sne.78.a.d, which = "group", conf.level = 0.95)

beta.sne.78.a.r <- betadisper(b.78.a$beta.sne, time.78.a$Regions, type = c("median"))
beta.sne.78.a.r
anova(beta.sne.78.a.r)
plot(beta.sne.78.a.r)
boxplot(beta.sne.78.a.r, ylab = "Distance to median")
TukeyHSD(beta.sne.78.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.89.a$beta.sor ~ time.89.a$Decade * time.89.a$Regions, permutations = 999)

# BETADISPER
beta.sor.89.a.d <- betadisper(b.89.a$beta.sor, time.89.a$Decade, type = c("median"))
beta.sor.89.a.d
anova(beta.sor.89.a.d)
plot(beta.sor.89.a.d)
boxplot(beta.sor.89.a.d, ylab = "Distance to median")
TukeyHSD(beta.sor.89.a.d, which = "group", conf.level = 0.95)

beta.sor.89.a.r <- betadisper(b.89.a$beta.sor, time.89.a$Regions, type = c("median"))
beta.sor.89.a.r
anova(beta.sor.89.a.r)
plot(beta.sor.89.a.r)
boxplot(beta.sor.89.a.r, ylab = "Distance to median")
TukeyHSD(beta.sor.89.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.89.a$beta.sim ~ time.89.a$Decade * time.89.a$Regions, permutations = 999)

# BETADISPER
beta.sim.89.a.d <- betadisper(b.89.a$beta.sim, time.89.a$Decade, type = c("median"))
beta.sim.89.a.d
anova(beta.sim.89.a.d)
plot(beta.sim.89.a.d)
boxplot(beta.sim.89.a.d, ylab = "Distance to median")
TukeyHSD(beta.sim.89.a.d, which = "group", conf.level = 0.95)

beta.sim.89.a.r <- betadisper(b.89.a$beta.sim, time.89.a$Regions, type = c("median"))
beta.sim.89.a.r
anova(beta.sim.89.a.r)
plot(beta.sim.89.a.r)
boxplot(beta.sim.89.a.r, ylab = "Distance to median")
TukeyHSD(beta.sim.89.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.89.a$beta.sne ~ time.89.a$Decade * time.89.a$Regions, permutations = 999)

# BETADISPER
beta.sne.89.a.d <- betadisper(b.89.a$beta.sne, time.89.a$Decade, type = c("median"))
beta.sne.89.a.d
anova(beta.sne.89.a.d)
plot(beta.sne.89.a.d)
boxplot(beta.sne.89.a.d, ylab = "Distance to median")
TukeyHSD(beta.sne.89.a.d, which = "group", conf.level = 0.95)

beta.sne.89.a.r <- betadisper(b.89.a$beta.sne, time.89.a$Regions, type = c("median"))
beta.sne.89.a.r
anova(beta.sne.89.a.r)
plot(beta.sne.89.a.r)
boxplot(beta.sne.89.a.r, ylab = "Distance to median")
TukeyHSD(beta.sne.89.a.r, which = "group", conf.level = 0.95)


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

#interaction between decade and regions
adonis2(b.910.a$beta.sor ~ time.910.a$Decade * time.910.a$Regions, permutations = 999)

# BETADISPER
beta.sor.910.a.d <- betadisper(b.910.a$beta.sor, time.910.a$Decade, type = c("median"))
beta.sor.910.a.d
anova(beta.sor.910.a.d)
plot(beta.sor.910.a.d)
boxplot(beta.sor.910.a.d, ylab = "Distance to median")
TukeyHSD(beta.sor.910.a.d, which = "group", conf.level = 0.95)

beta.sor.910.a.r <- betadisper(b.910.a$beta.sor, time.910.a$Regions, type = c("median"))
beta.sor.910.a.r
anova(beta.sor.910.a.r)
plot(beta.sor.910.a.r)
boxplot(beta.sor.910.a.r, ylab = "Distance to median")
TukeyHSD(beta.sor.910.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.910.a$beta.sim ~ time.910.a$Decade * time.910.a$Regions, permutations = 999)

# BETADISPER
beta.sim.910.a.d <- betadisper(b.910.a$beta.sim, time.910.a$Decade, type = c("median"))
beta.sim.910.a.d
anova(beta.sim.910.a.d)
plot(beta.sim.910.a.d)
boxplot(beta.sim.910.a.d, ylab = "Distance to median")
TukeyHSD(beta.sim.910.a.d, which = "group", conf.level = 0.95)

beta.sim.910.a.r <- betadisper(b.910.a$beta.sim, time.910.a$Regions, type = c("median"))
beta.sim.910.a.r
anova(beta.sim.910.a.r)
plot(beta.sim.910.a.r)
boxplot(beta.sim.910.a.r, ylab = "Distance to median")
TukeyHSD(beta.sim.910.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.910.a$beta.sne ~ time.910.a$Decade * time.910.a$Regions, permutations = 999)

# BETADISPER
beta.sne.910.a.d <- betadisper(b.910.a$beta.sne, time.910.a$Decade, type = c("median"))
beta.sne.910.a.d
anova(beta.sne.910.a.d)
plot(beta.sne.910.a.d)
boxplot(beta.sne.910.a.d, ylab = "Distance to median")
TukeyHSD(beta.sne.910.a.d, which = "group", conf.level = 0.95)

beta.sne.910.a.r <- betadisper(b.910.a$beta.sne, time.910.a$Regions, type = c("median"))
beta.sne.910.a.r
anova(beta.sne.910.a.r)
plot(beta.sne.910.a.r)
boxplot(beta.sne.910.a.r, ylab = "Distance to median")
TukeyHSD(beta.sne.910.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.1011.a$beta.sor ~ time.1011.a$Decade * time.1011.a$Regions, permutations = 999)

# BETADISPER
beta.sor.1011.a.d <- betadisper(b.1011.a$beta.sor, time.1011.a$Decade, type = c("median"))
beta.sor.1011.a.d
anova(beta.sor.1011.a.d)
plot(beta.sor.1011.a.d)
boxplot(beta.sor.1011.a.d, ylab = "Distance to median")
TukeyHSD(beta.sor.1011.a.d, which = "group", conf.level = 0.95)

beta.sor.1011.a.r <- betadisper(b.1011.a$beta.sor, time.1011.a$Regions, type = c("median"))
beta.sor.1011.a.r
anova(beta.sor.1011.a.r)
plot(beta.sor.1011.a.r)
boxplot(beta.sor.1011.a.r, ylab = "Distance to median")
TukeyHSD(beta.sor.1011.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.1011.a$beta.sim ~ time.1011.a$Decade * time.1011.a$Regions, permutations = 999)

# BETADISPER
beta.sim.1011.a.d <- betadisper(b.1011.a$beta.sim, time.1011.a$Decade, type = c("median"))
beta.sim.1011.a.d
anova(beta.sim.1011.a.d)
plot(beta.sim.1011.a.d)
boxplot(beta.sim.1011.a.d, ylab = "Distance to median")
TukeyHSD(beta.sim.1011.a.d, which = "group", conf.level = 0.95)

beta.sim.1011.a.r <- betadisper(b.1011.a$beta.sim, time.1011.a$Regions, type = c("median"))
beta.sim.1011.a.r
anova(beta.sim.1011.a.r)
plot(beta.sim.1011.a.r)
boxplot(beta.sim.1011.a.r, ylab = "Distance to median")
TukeyHSD(beta.sim.1011.a.r, which = "group", conf.level = 0.95)


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

# interaction between decade and regions
adonis2(b.1011.a$beta.sne ~ time.1011.a$Decade * time.1011.a$Regions, permutations = 999)

# BETADISPER
beta.sne.1011.a.d <- betadisper(b.1011.a$beta.sne, time.1011.a$Decade, type = c("median"))
beta.sne.1011.a.d
anova(beta.sne.1011.a.d)
plot(beta.sne.1011.a.d)
boxplot(beta.sne.1011.a.d, ylab = "Distance to median")
TukeyHSD(beta.sne.1011.a.d, which = "group", conf.level = 0.95)

beta.sne.1011.a.r <- betadisper(b.1011.a$beta.sne, time.1011.a$Regions, type = c("median"))
beta.sne.1011.a.r
anova(beta.sne.1011.a.r)
plot(beta.sne.1011.a.r)
boxplot(beta.sne.1011.a.r, ylab = "Distance to median")
TukeyHSD(beta.sne.1011.a.r, which = "group", conf.level = 0.95)


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
adonis2(b.1112.a$beta.sor ~ time.1112.a$Decade, permutations = 999)
pairwise.adonis(b.1112.a$beta.sor, time.1112.a$Decade)
adonis2(b.1112.a$beta.sor ~ time.1112.a$Regions, permutations = 999)
pairwise.adonis(b.1112.a$beta.sor, time.1112.a$Regions)

# interaction between decade and regions
adonis2(b.1112.a$beta.sor ~ time.1112.a$Decade*time.1112.a$Regions, permutations = 999)

# BETADISPER
beta.sor.1112.a.d <- betadisper(b.1112.a$beta.sor, time.1112.a$Decade, type = c("median"))
beta.sor.1112.a.d
anova(beta.sor.1112.a.d)
plot(beta.sor.1112.a.d)
boxplot(beta.sor.1112.a.d, ylab = "Distance to median")
TukeyHSD(beta.sor.1112.a.d, which = "group", conf.level = 0.95)

beta.sor.1112.a.r <- betadisper(b.1112.a$beta.sor, time.1112.a$Regions, type = c("median"))
beta.sor.1112.a.r
anova(beta.sor.1112.a.r)
plot(beta.sor.1112.a.r)
boxplot(beta.sor.1112.a.r, ylab = "Distance to median")
TukeyHSD(beta.sor.1112.a.r, which = "group", conf.level = 0.95)


## beta.sim - turnover / replacement
nmds.1112.a.sim <- metaMDS(b.1112.a$beta.sim, trymax = 500, autotransform = TRUE)
nmds.1112.a.sim
stressplot(nmds.1112.a.sim)
goodness(nmds.1112.a.sim)
nmds.1112.a.sim$stress
plot(nmds.1112.a.sim)

# PERMANOVA
adonis2(b.1112.a$beta.sim ~ time.1112.a$Decade, permutations = 999)
pairwise.adonis(b.1112.a$beta.sim, time.1112.a$Decade)
adonis2(b.1112.a$beta.sim ~ time.1112.a$Regions, permutations = 999)
pairwise.adonis(b.1112.a$beta.sim, time.1112.a$Regions)

# interaction between decade and region
adonis2(b.1112.a$beta.sim ~ time.1112.a$Decade * time.1112.a$Regions, permutations = 999)

# BETADISPER
beta.sim.1112.a.d <- betadisper(b.1112.a$beta.sim, time.1112.a$Decade, type = c("median"))
beta.sim.1112.a.d
anova(beta.sim.1112.a.d)
plot(beta.sim.1112.a.d)
boxplot(beta.sim.1112.a.d, ylab = "Distance to median")
TukeyHSD(beta.sim.1112.a.d, which = "group", conf.level = 0.95)

beta.sim.1112.a.r <- betadisper(b.1112.a$beta.sim, time.1112.a$Regions, type = c("median"))
beta.sim.1112.a.r
anova(beta.sim.1112.a.r)
plot(beta.sim.1112.a.r)
boxplot(beta.sim.1112.a.r, ylab = "Distance to median")
TukeyHSD(beta.sim.1112.a.r, which = "group", conf.level = 0.95)


## beta.sne - nestedness / loss or gain
nmds.1112.a.sne <- metaMDS(b.1112.a$beta.sne, trymax = 500, autotransform = TRUE)
nmds.1112.a.sne
stressplot(nmds.1112.a.sne)
goodness(nmds.1112.a.sne)
nmds.1112.a.sne$stress
plot(nmds.1112.a.sne)

# PERMANOVA
adonis2(b.1112.a$beta.sne ~ time.1112.a$Decade, permutations = 999)
pairwise.adonis(b.1112.a$beta.sne, time.1112.a$Decade)
adonis2(b.1112.a$beta.sne ~ time.1112.a$Regions, permutations = 999)
pairwise.adonis(b.1112.a$beta.sne, time.1112.a$Regions)

# interaction between decade and region
adonis2(b.1112.a$beta.sne ~ time.1112.a$Decade * time.1112.a$Regions, permutations = 999)

# BETADISPER
beta.sne.1112.a.d <- betadisper(b.1112.a$beta.sne, time.1112.a$Decade, type = c("median"))
beta.sne.1112.a.d
anova(beta.sne.1112.a.d)
plot(beta.sne.1112.a.d)
boxplot(beta.sne.1112.a.d, ylab = "Distance to median")
TukeyHSD(beta.sne.1112.a.d, which = "group", conf.level = 0.95)

beta.sne.1112.a.r <- betadisper(b.1112.a$beta.sne, time.1112.a$Regions, type = c("median"))
beta.sne.1112.a.r
anova(beta.sne.1112.a.r)
plot(beta.sne.1112.a.r)
boxplot(beta.sne.1112.a.r, ylab = "Distance to median")
TukeyHSD(beta.sne.1112.a.r, which = "group", conf.level = 0.95)


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

# compare LB community composition collected from sticky cards in BLBB study
# to museum specimens collected from the same years


lb <- read.csv("specimen_data/LB_MuseumData_2020_v2.csv")

colnames(lb)

Yr09 <- lb[which(lb$Year == "2009"),]
Yr10 <- lb[which(lb$Year == "2010"),]
Yr13 <- lb[which(lb$Year == "2013"),]
Yr14 <- lb[which(lb$Year == "2014"),]
blbb <- rbind(Yr09, Yr10, Yr13, Yr14)

blbb.lb <- blbb[,c(2,7,20,14)]

colnames(blbb.lb)

blbb.matrix <- dcast(blbb.lb, Regions + Source + Year ~ Name, length)
str(blbb.matrix)

## double check counts for BLBB and museum specimens by species and year
blbb.counts <- dcast(blbb.lb, Source + Year ~ Name, length)

# make sure predictor variables are factors
blbb.matrix$Year <- as.factor(blbb.matrix$Year)
blbb.matrix$Regions <- as.factor(blbb.matrix$Regions)
blbb.matrix$Source <- as.factor(blbb.matrix$Source)

levels(blbb.matrix$Year)
levels(blbb.matrix$Regions)
levels(blbb.matrix$Source)
str(blbb.matrix)

# change dataset to presence/absence (1/0)
blbb.matrix[blbb.matrix>0]<-1
str(blbb.matrix)

write.csv(blbb.matrix, "blbb.csv")

blbb.core <- betapart.core(blbb.matrix[4:18])
blbb.b <- beta.pair(blbb.core, index.family = "sorensen")
str(blbb.b)

## beta.sor - total betadiversity
nmds.blbb.sor <- metaMDS(blbb.b$beta.sor, trymax = 500, autotransform = TRUE)
nmds.blbb.sor
stressplot(nmds.blbb.sor)
goodness(nmds.blbb.sor)
nmds.blbb.sor$stress
plot(nmds.blbb.sor)

# quick plot to visualize results
levels(blbb.matrix$Source)
levels(blbb.matrix$Regions)

colvec <- c("gray56", "black")
pchvec1 <- c(16, 17)
pchvec2 <- c(16, 17, 15, 18)

png("LB_BLBBandMuseum_sor.png", width = 1000, height = 800, pointsize = 20)
ordiplot(nmds.blbb.sor, type="n", xlim = c(-0.7, 0.7), ylim = c(-0.7, 0.7))
points(nmds.blbb.sor, dis="sites", pch = pchvec1[blbb.matrix$Source], cex=1.5, col=colvec[blbb.matrix$Source])
ordiellipse(nmds.blbb.sor, groups=blbb.matrix$Source, display="sites", lwd = 2.5, draw="lines", conf=0.90)
legend("bottomleft", legend = c("Buckeye Lady Beetle Blitz","Museum Collections"), 
       pch = pchvec1, cex=1.1, bty="n", col=colvec)
dev.off()

# PERMANOVA
adonis2(blbb.b$beta.sor ~ blbb.matrix$Year, permutations = 999)
pairwise.adonis(blbb.b$beta.sor, blbb.matrix$Year)
adonis2(blbb.b$beta.sor ~ blbb.matrix$Regions, permutations = 999)
pairwise.adonis(blbb.b$beta.sor, blbb.matrix$Regions)
adonis2(blbb.b$beta.sor ~ blbb.matrix$Source, permutations = 999)
pairwise.adonis(blbb.b$beta.sor, blbb.matrix$Source)

# interaction between source and year
adonis2(blbb.b$beta.sor ~ blbb.matrix$Source * blbb.matrix$Year, permutations = 999)

# interaction between source and regions
adonis2(blbb.b$beta.sor ~ blbb.matrix$Source * blbb.matrix$Regions, permutations = 999)


# BETADISPER
beta.sor.blbb.s <- betadisper(blbb.b$beta.sor, blbb.matrix$Source, type = c("median"))
beta.sor.blbb.s
anova(beta.sor.blbb.s)
plot(beta.sor.blbb.s)
boxplot(beta.sor.blbb.s, ylab = "Distance to median")
TukeyHSD(beta.sor.blbb.s, which = "group", conf.level = 0.95)

beta.sor.blbb.y <- betadisper(blbb.b$beta.sor, blbb.matrix$Year, type = c("median"))
beta.sor.blbb.y
anova(beta.sor.blbb.y)
plot(beta.sor.blbb.y)
boxplot(beta.sor.blbb.y, ylab = "Distance to median")
TukeyHSD(beta.sor.blbb.y, which = "group", conf.level = 0.95)

beta.sor.blbb.r <- betadisper(blbb.b$beta.sor, blbb.matrix$Regions, type = c("median"))
beta.sor.blbb.r
anova(beta.sor.blbb.r)
plot(beta.sor.blbb.r)
boxplot(beta.sor.blbb.r, ylab = "Distance to median")
TukeyHSD(beta.sor.blbb.r, which = "group", conf.level = 0.95)


## beta.sim - turnover
nmds.blbb.sim <- metaMDS(blbb.b$beta.sim, trymax = 500, autotransform = TRUE)
nmds.blbb.sim
stressplot(nmds.blbb.sim)
goodness(nmds.blbb.sim)
nmds.blbb.sim$stress
plot(nmds.blbb.sim)

# PERMANOVA
adonis2(blbb.b$beta.sim ~ blbb.matrix$Year, permutations = 999)
pairwise.adonis(blbb.b$beta.sim, blbb.matrix$Year)
adonis2(blbb.b$beta.sim ~ blbb.matrix$Regions, permutations = 999)
pairwise.adonis(blbb.b$beta.sim, blbb.matrix$Regions)
adonis2(blbb.b$beta.sim ~ blbb.matrix$Source, permutations = 999)
pairwise.adonis(blbb.b$beta.sim, blbb.matrix$Source)

# interaction between source and year
adonis2(blbb.b$beta.sim ~ blbb.matrix$Source * blbb.matrix$Year, permutations = 999)

# interaction between source and regions
adonis2(blbb.b$beta.sim ~ blbb.matrix$Source * blbb.matrix$Regions, permutations = 999)

# BETADISPER
beta.sim.blbb.s <- betadisper(blbb.b$beta.sim, blbb.matrix$Source, type = c("median"))
beta.sim.blbb.s
anova(beta.sim.blbb.s)
plot(beta.sim.blbb.s)
boxplot(beta.sim.blbb.s, ylab = "Distance to median")
TukeyHSD(beta.sim.blbb.s, which = "group", conf.level = 0.95)

beta.sim.blbb.y <- betadisper(blbb.b$beta.sim, blbb.matrix$Year, type = c("median"))
beta.sim.blbb.y
anova(beta.sim.blbb.y)
plot(beta.sim.blbb.y)
boxplot(beta.sim.blbb.y, ylab = "Distance to median")
TukeyHSD(beta.sim.blbb.y, which = "group", conf.level = 0.95)

beta.sim.blbb.r <- betadisper(blbb.b$beta.sim, blbb.matrix$Regions, type = c("median"))
beta.sim.blbb.r
anova(beta.sim.blbb.r)
plot(beta.sim.blbb.r)
boxplot(beta.sim.blbb.r, ylab = "Distance to median")
TukeyHSD(beta.sim.blbb.r, which = "group", conf.level = 0.95)


## beta.sne - nestedness
nmds.blbb.sne <- metaMDS(blbb.b$beta.sne, trymax = 500, autotransform = TRUE)
nmds.blbb.sne
stressplot(nmds.blbb.sne)
goodness(nmds.blbb.sne)
nmds.blbb.sne$stress
plot(nmds.blbb.sne)

# PERMANOVA
adonis2(blbb.b$beta.sne ~ blbb.matrix$Year, permutations = 999)
pairwise.adonis(blbb.b$beta.sne, blbb.matrix$Year)
adonis2(blbb.b$beta.sne ~ blbb.matrix$Regions, permutations = 999)
pairwise.adonis(blbb.b$beta.sne, blbb.matrix$Regions)
adonis2(blbb.b$beta.sne ~ blbb.matrix$Source, permutations = 999)
pairwise.adonis(blbb.b$beta.sne, blbb.matrix$Source)

# interaction between source and year
adonis2(blbb.b$beta.sne ~ blbb.matrix$Source * blbb.matrix$Year, permutations = 999)

# interaction between source and regions
adonis2(blbb.b$beta.sne ~ blbb.matrix$Source * blbb.matrix$Regions, permutations = 999)

# BETADISPER
beta.sne.blbb.s <- betadisper(blbb.b$beta.sne, blbb.matrix$Source, type = c("median"))
beta.sne.blbb.s
anova(beta.sne.blbb.s)
plot(beta.sne.blbb.s)
boxplot(beta.sne.blbb.s, ylab = "Distance to median")
TukeyHSD(beta.sne.blbb.s, which = "group", conf.level = 0.95)

beta.sne.blbb.y <- betadisper(blbb.b$beta.sne, blbb.matrix$Year, type = c("median"))
beta.sne.blbb.y
anova(beta.sne.blbb.y)
plot(beta.sne.blbb.y)
boxplot(beta.sne.blbb.y, ylab = "Distance to median")
TukeyHSD(beta.sne.blbb.y, which = "group", conf.level = 0.95)

beta.sne.blbb.r <- betadisper(blbb.b$beta.sne, blbb.matrix$Regions, type = c("median"))
beta.sne.blbb.r
anova(beta.sne.blbb.r)
plot(beta.sne.blbb.r)
boxplot(beta.sne.blbb.r, ylab = "Distance to median")
TukeyHSD(beta.sne.blbb.r, which = "group", conf.level = 0.95)

#############################################################################
#############################################################################
# Create NMDS figure for selected decade comparisons - aphidophagous species

colvec <- c("gray56", "black")
pchvec1 <- c(16, 17)
pchvec2 <- c(16, 17, 15, 18)

png("NMDS_LadyBeetles2.png", width = 1600, height = 2600, pointsize = 30)

par(mfrow=c(4,2))
par(mar=c(5,4,3,2))

#1920s-1930s
ordiplot(nmds.34.sor, type="n", xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.5))
points(nmds.34.sor, dis="sites", cex=2, pch = pchvec2[time.34$Regions], col=colvec[time.34$Dec])
ordiellipse(nmds.34.sor, groups=time.34$Dec, display="sites", draw="lines",
            col=c("gray56", "black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1920s","1930s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
legend("topleft", legend = c("AP","IOTP","LEGP","WLEB"), cex=1.5, bty="n", 
       pch = c(16,17,15,18), col=c("black"))
text(0.61, 0.44, "A", pos = 4, font = 2, cex = 1.8)
text(0.0, 0.44, "D p=0.014; R p=0.007", font = 1.5, cex = 1.5)

#1930s-1940s
ordiplot(nmds.45.sor, type="n", xlim = c(-0.8, 0.8), ylim = c(-0.5, 0.5),
         at=c(-0.8,-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6,0.8))
points(nmds.45.sor, dis="sites", cex=2, pch = pchvec2[time.45$Regions], col=colvec[time.45$Dec])
ordiellipse(nmds.45.sor, groups=time.45$Dec, display="sites", draw="lines",
            col=c("gray56", "black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1930s","1940s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
text(0.7, 0.5, "B", pos = 4, font = 2, cex = 1.8)
text(0.0, 0.5, "D p=0.0.003; R p=0.006", font = 1.5, cex = 1.5)

#1940s-1950s
ordiplot(nmds.56.sor, type="n", xlim = c(-0.5, 0.5), ylim = c(-0.5, 0.8),
         at=c(-0.8,-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6,0.8))
points(nmds.56.sor, dis="sites", cex=2, pch = pchvec2[time.56$Regions], col=colvec[time.56$Dec])
ordiellipse(nmds.56.sor, groups=time.56$Regions, display="sites", draw="lines",
            col=c("black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1940s","1950s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
text(0.79, 0.72, "C", pos = 4, font = 2, cex = 1.8)

#1950s-1960s
ordiplot(nmds.67.sor, type="n", xlim = c(-0.8, 0.8), ylim = c(-0.6, 0.6),
         at=c(-0.8,-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6,0.8))
points(nmds.67.sor, dis="sites", cex=2, pch = pchvec2[time.67$Regions], col=colvec[time.67$Dec])
ordiellipse(nmds.67.sor, groups=time.67$Regions, display="sites", draw="lines",
            col=c("black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1950s","1960s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
text(0.75, 0.53, "D", pos = 4, font = 2, cex = 1.8)

#1970s-1980s
ordiplot(nmds.89.sor, type="n", xlim = c(-0.8, 0.6), ylim = c(-0.3, 0.4),
         at=c(-0.8,-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6))
points(nmds.89.sor, dis="sites", cex=2, pch = pchvec2[time.89$Regions], col=colvec[time.89$Dec])
ordiellipse(nmds.89.sor, groups=time.89$Dec, display="sites", draw="lines",
            col=c("black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1970s","1980s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
text(0.5, 0.49, "E", pos = 4, font = 2, cex = 1.8)

#1980s-1990s
ordiplot(nmds.910.sor, type="n", xlim = c(-0.8, 0.6), ylim = c(-0.4, 0.6))
points(nmds.910.sor, dis="sites", cex=2, pch = pchvec2[time.910$Regions], col=colvec[time.910$Dec])
ordiellipse(nmds.910.sor, groups=time.910$Dec, display="sites", draw="lines",
            col=c("black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1980s","1990s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
text(0.52, 0.56, "F", pos = 4, font = 2, cex = 1.8)

#1990s-2000s
ordiplot(nmds.1011.sor, type="n", xlim = c(-0.8, 0.8), ylim = c(-0.8, 0.4),
         at=c(-0.8,-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6,0.8))
points(nmds.1011.sor, dis="sites", cex=2, pch = pchvec2[time.1011$Regions], col=colvec[time.1011$Dec])
ordiellipse(nmds.1011.sor, groups=time.1011$Dec, display="sites", draw="lines",
            col=c("black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1990s","2000s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
text(0.73, 0.32, "G", pos = 4, font = 2, cex = 1.8)

#2000s-2010s
ordiplot(nmds.1112.sor, type="n", xlim = c(-0.4, 0.8), ylim = c(-0.6, 0.4))
points(nmds.1112.sor, dis="sites", cex=2, pch = pchvec2[time.1112$Regions], col=colvec[time.1112$Dec])
ordiellipse(nmds.1112.sor, groups=time.1112$Regions, display="sites", draw="lines",
            col=c("black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("2000s","2010s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=19)
text(0.81, 0.35, "H", pos = 4, font = 2, cex = 1.8)

dev.off()


####
# Reduced figure, only focusing on effects of decade
colvec1 <- c("gray56", "black")
colvec2 <- c("black", "gray56")
pchvec1 <- c(16, 17)
pchvec2 <- c(17, 15)


png("NMDS_LadyBeetles3.png", width = 1600, height = 1600, pointsize = 30)

par(mfrow=c(2,2))
par(mar=c(5,4,3,2))

#1920s-1930s
ordiplot(nmds.34.sor, type="n", xlim = c(-0.6, 0.6), ylim = c(-0.6, 0.6),
         at=c(-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6))
points(nmds.34.sor, dis="sites", cex=2, pch = pchvec1[time.34$Dec], col=colvec[time.34$Dec])
ordiellipse(nmds.34.sor, groups=time.34$Dec, display="sites", draw="lines",
            col=c("gray56", "black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1920s","1930s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=c(16, 17))
text(0.54, 0.5, "A", pos = 4, font = 2, cex = 1.8)
text(0.0, 0.5, "p=0.014", font = 1.5, cex = 1.5)

#1930s-1940s
ordiplot(nmds.45.sor, type="n", xlim = c(-0.8, 0.8), ylim = c(-0.5, 0.5),
         at=c(-0.8,-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6,0.8))
points(nmds.45.sor, dis="sites", cex=2, pch = pchvec2[time.45$Dec], col=colvec2[time.45$Dec])
ordiellipse(nmds.45.sor, groups=time.45$Dec, display="sites", draw="lines",
            col=c("black", "gray56"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1930s","1940s"), cex=1.5, bty="n", col=c("black", "gray56"), pch=c(17, 15))
text(0.65, 0.61, "B", pos = 4, font = 2, cex = 1.8)
text(0.0, 0.61, "p=0.003", font = 1.5, cex = 1.5)

#1980s-1990s
ordiplot(nmds.910.sor, type="n", xlim = c(-0.8, 0.6), ylim = c(-0.4, 0.6))
points(nmds.910.sor, dis="sites", cex=2, pch = pchvec1[time.910$Dec], col=colvec[time.910$Dec])
ordiellipse(nmds.910.sor, groups=time.910$Dec, display="sites", draw="lines",
            col=c("gray56", "black"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1980s","1990s"), cex=1.5, bty="n", col=c("gray56", "black"), pch=c(16, 17))
text(0.47, 0.62, "C", pos = 4, font = 2, cex = 1.8)
text(-0.1, 0.62, "p=0.021", font = 1.5, cex = 1.5)

#1990s-2000s
ordiplot(nmds.1011.sor, type="n", xlim = c(-0.8, 0.8), ylim = c(-0.8, 0.6),
         at=c(-0.8,-0.6,-0.4,-0.2,0.0,0.2,0.4,0.6, 0.8))
points(nmds.1011.sor, dis="sites", cex=2, pch = pchvec2[time.1011$Dec], col=colvec2[time.1011$Dec])
ordiellipse(nmds.1011.sor, groups=time.1011$Dec, display="sites", draw="lines",
            col=c("black", "gray56"), lwd = 3, conf=0.90)
legend("bottomleft", legend = c("1990s","2000s"), cex=1.5, bty="n", col=c("black", "gray56"), pch=c(17, 15))
text(0.64, 0.5, "D", pos = 4, font = 2, cex = 1.8)
text(0.0, 0.5, "p=0.009", font = 1.5, cex = 1.5)

dev.off()

###################################################################################
# Question: Does lady beetle taxonomic beta-diversity change over time, and if so, are these
# patterns driven by species turnover, species nestedness, or both?
# revising analysis by binning across time intervals

lb <- read.csv("specimen_data/LB_MuseumData_2020_v2.csv")

# subset data to look at only museum specimens (i.e. remove BLBB data)
lb.b <- lb[c(1:4210),]

lb.b$YearGroup <-cut (lb.b$Year, breaks=c(-Inf, 1938, 1970, 1992,  Inf), labels=c("1938","1970","1992","2018"))

str(lb.b)
lb.b$Name <- as.factor(lb.b$Name)
levels(lb.b$Name)

# not enough data from each county even if we pool over these time periods. Have to pool across geographic regions
lb.b <- dcast(lb.b, YearGroup+Regions~Name, length)

colSums(lb.b[3:30])
# remove single collection of Mulsantina luteodorsa because it hasn't been
# verified by LB experts
lb.b <- lb.b[,-24]

rowSums(lb.b[3:29])

str(lb.b)

# change regions variable to factor
lb.b$Regions <- as.factor(lb.b$Regions)
str(lb.b)

# save a copy with total counts
lb2 <- lb.b

# change dataset to presence/absence (1/0)
lb.b[lb.b>0]<-1
str(lb.b)


# pull out data from each time period
# remove factor columns, change site to row names

time1 <- lb.b[which(lb.b$YearGroup == "1938"),]
rownames(time1) <- time1[,2]
time1 <- time1[,-1]
time1 <- time1[,-1]
str(time1)

time2 <- lb.b[which(lb.b$YearGroup == "1970"),]
rownames(time2) <- time2[,2]
time2 <- time2[,-1]
time2 <- time2[,-1]
str(time2)

time3 <- lb.b[which(lb.b$YearGroup == "1992"),]
rownames(time3) <- time3[,2]
time3 <- time3[,-1]
time3 <- time3[,-1]
str(time3)

time4 <- lb.b[which(lb.b$YearGroup == "2018"),]
rownames(time4) <- time4[,2]
time4 <- time4[,-1]
time4 <- time4[,-1]
str(time4)


# create beta part object for each decade

t1 <- betapart.core(time1)
t2 <- betapart.core(time2)
t3 <- betapart.core(time3)
t4 <- betapart.core(time4)


# assess temporal change in community composition among the decades for all of Ohio

t12 <- beta.temp(t1, t2, index.family = "sorensen")
t12$beta.sim

t23 <- beta.temp(t2, t3, index.family = "sorensen")
t23

t34 <- beta.temp(t3, t4, index.family = "sorensen")
t34
