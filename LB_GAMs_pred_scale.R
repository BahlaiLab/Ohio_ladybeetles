#let's make all the ladybeetle figures 'back transformed' into the scale of the data being taken, 
# assuming 500 ladybeetles captured per decade. This will essentially remake all the model figures but address 
#the fussier scale issues that make it harder to interpret things

#experimental pred code

#make prediction data for each  different variable. 100 points? yeah
#decade is 2000 when fixed, but need to switch so make it variable
d<-2000
#decade
newDecade <- with(lb_all2,
             data.frame(Decade = seq(min(Decade), max(Decade), length = 100),
                        Totalcount=500, 
                        lon=mean(lon), 
                        lat=mean(lat), 
                        Propinvasive=mean(Propinvasive),
                        Agriculture=mean(Agriculture, na.rm=T), 
                        Forest=mean(Forest, na.rm=T), 
                        Developed=mean(Developed, na.rm=T)))


#lon
newlon <- with(lb_all2,
                  data.frame(Decade = d,
                             Totalcount=500, 
                             lon=seq(min(lon), max(lon), length = 100), 
                             lat=mean(lat), 
                             Propinvasive=mean(Propinvasive),
                             Agriculture=mean(Agriculture, na.rm=T), 
                             Forest=mean(Forest, na.rm=T), 
                             Developed=mean(Developed, na.rm=T)))

#lat
newlat <- with(lb_all2,
               data.frame(Decade = d,
                          Totalcount=500, 
                          lon=mean(lon), 
                          lat=seq(min(lat), max(lat), length = 100), 
                          Propinvasive=mean(Propinvasive),
                          Agriculture=mean(Agriculture, na.rm=T), 
                          Forest=mean(Forest, na.rm=T), 
                          Developed=mean(Developed, na.rm=T)))
#Propinvasive
newPI <- with(lb_all2,
               data.frame(Decade = d,
                          Totalcount=500, 
                          lon=mean(lon), 
                          lat=mean(lat), 
                          Propinvasive=seq(min(Propinvasive), max(Propinvasive)-0.05, length = 100),
                          Agriculture=mean(Agriculture, na.rm=T), 
                          Forest=mean(Forest, na.rm=T), 
                          Developed=mean(Developed, na.rm=T)))
#Agriculture
newAgriculture <- with(lb_all2,
               data.frame(Decade = d,
                          Totalcount=500, 
                          lon=mean(lon), 
                          lat=mean(lat), 
                          Propinvasive=mean(Propinvasive),
                          Agriculture=seq(min(Agriculture, na.rm=T), max(Agriculture, na.rm=T), length = 100), 
                          Forest=mean(Forest, na.rm=T), 
                          Developed=mean(Developed, na.rm=T)))
#Forest
newForest <- with(lb_all2,
               data.frame(Decade = d,
                          Totalcount=500, 
                          lon=mean(lon), 
                          lat=mean(lat), 
                          Propinvasive=mean(Propinvasive),
                          Agriculture=mean(Agriculture, na.rm=T), 
                          Forest=seq(min(Forest, na.rm=T), max(Forest, na.rm=T), length = 100), 
                          Developed=mean(Developed, na.rm=T)))
#Developed
newDeveloped <- with(lb_all2,
               data.frame(Decade = d,
                          Totalcount=500, 
                          lon=mean(lon), 
                          lat=mean(lat), 
                          Propinvasive=mean(Propinvasive),
                          Agriculture=mean(Agriculture, na.rm=T), 
                          Forest=mean(Forest, na.rm=T), 
                          Developed=seq(min(Developed, na.rm=T), max(Developed, na.rm=T), length = 100)))


##########################################
#cmac

#pull in final model from other analysis, just cut the number of that species in the offset

cmac.gam4<-gam(Coleomegilla.maculata~offset(log(1+Totalcount))+
                 s(Decade, sp=0.5, k=4)+
                 s(lat, sp=0.5)+
                 s(Propinvasive, sp=0.5)+
                 s(Agriculture, sp=0.5),  
               data=lb_all2, family="nb")
summary(cmac.gam4)
AIC(cmac.gam4)
cmac.pred<-predict.gam(cmac.gam4, newDecade, se.fit = T, type="link")
cmac.pred<-cbind(newDecade,cmac.pred)
cmac.pred$lower<-cmac.pred$fit-2*cmac.pred$se.fit
cmac.pred$upper<-cmac.pred$fit+2*cmac.pred$se.fit

#decade
#set decade for rest of analyses
d<-2000

##
cmac.pred.decade<-ggplot(data=cmac.pred, aes(Decade, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey19", alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1929, 2011)+
  xlab(expression(paste("Year")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
cmac.pred.decade

#lat
cmac.pred<-predict.gam(cmac.gam4, newlat, se.fit = T, type="link")
cmac.pred<-cbind(newlat,cmac.pred)
cmac.pred$lower<-cmac.pred$fit-2*cmac.pred$se.fit
cmac.pred$upper<-cmac.pred$fit+2*cmac.pred$se.fit

cmac.pred.lat<-ggplot(data=cmac.pred, aes(lat, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey", alpha=0.6)+
  geom_line(col="gray28")+
  theme_classic()+
  xlab(expression(paste("Latitude")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
cmac.pred.lat


#prop invasive

cmac.pred<-predict.gam(cmac.gam4, newPI, se.fit = T, type="link")
cmac.pred<-cbind(newPI,cmac.pred)
cmac.pred$lower<-cmac.pred$fit-2*cmac.pred$se.fit
cmac.pred$upper<-cmac.pred$fit+2*cmac.pred$se.fit

cmac.pred.pi<-ggplot(data=cmac.pred, aes(Propinvasive, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="plum1", alpha=0.6)+
  geom_line(col="darkmagenta")+
  theme_classic()+
  xlab(expression(paste("Proportion invasive")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
cmac.pred.pi


#agriculture

cmac.pred<-predict.gam(cmac.gam4, newAgriculture, se.fit = T, type="link")
cmac.pred<-cbind(newAgriculture,cmac.pred)
cmac.pred$lower<-cmac.pred$fit-2*cmac.pred$se.fit
cmac.pred$upper<-cmac.pred$fit+2*cmac.pred$se.fit

cmac.pred.agriculture<-ggplot(data=cmac.pred, aes(Agriculture, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="darkolivegreen1", alpha=0.6)+
  geom_line(col="darkolivegreen")+
  theme_classic()+
  xlab(expression(paste("% Agriculture cover")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
cmac.pred.agriculture


#create graphical objects for placeholders in plots
noeffect<-text_grob(paste("No effect"), color="black")
notpresent<-text_grob(paste("Not present"), color="black")

cmac.predictions<-plot_grid(cmac.pred.decade, noeffect, cmac.pred.lat, 
                        cmac.pred.pi, noeffect, noeffect,
                        cmac.pred.agriculture, noeffect, noeffect,
                        ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C', 
                                                              'D', 'E', 'F',
                                                              'G', 'H', 'I'))
cmac.predictions

pdf("plots/cmac_predictions.pdf", height=9, width=9)
grid.draw(cmac.predictions)
dev.off()



######################################


#c9
#pull in final model from other analysis, just cut the number of that species in the offset

c9.gam4<-gam(Coccinella.novemnotata~offset(log(1+Totalcount))+
               s(Decade, sp=0.5, k=4)+
               s(lon, sp=0.5)+
               s(lat, sp=0.5)+
               s(Agriculture, sp=0.5),  
             data=pre1995, family="nb")
summary(c9.gam4)
AIC(c9.gam4)

c9.pred<-predict.gam(c9.gam4, newDecade, se.fit = T, type="link")
c9.pred<-cbind(newDecade,c9.pred)
c9.pred<-c9.pred[which(c9.pred$Decade<=1990),]
c9.pred$lower<-c9.pred$fit-2*c9.pred$se.fit
c9.pred$upper<-c9.pred$fit+2*c9.pred$se.fit


#decade
#set decade for rest of analyses
d<-1960


##
c9.pred.decade<-ggplot(data=c9.pred, aes(Decade, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey19", alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1929, 2011)+
  xlab(expression(paste("Year")))+ylab("Predicted captures")+
  scale_y_continuous(limits=c(-0.1, NA))
c9.pred.decade
#lon
c9.pred<-predict.gam(c9.gam4, newlon, se.fit = T, type="link")
c9.pred<-cbind(newlon,c9.pred)
c9.pred$lower<-c9.pred$fit-2*c9.pred$se.fit
c9.pred$upper<-c9.pred$fit+2*c9.pred$se.fit

c9.pred.lon<-ggplot(data=c9.pred, aes(lon, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey", alpha=0.6)+
  geom_line(col="gray28")+
  theme_classic()+
  xlab(expression(paste("Longitude")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
c9.pred.lon


#lat
c9.pred<-predict.gam(c9.gam4, newlat, se.fit = T, type="link")
c9.pred<-cbind(newlat,c9.pred)
c9.pred$lower<-c9.pred$fit-2*c9.pred$se.fit
c9.pred$upper<-c9.pred$fit+2*c9.pred$se.fit

c9.pred.lat<-ggplot(data=c9.pred, aes(lat, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey", alpha=0.6)+
  geom_line(col="gray28")+
  theme_classic()+
  xlab(expression(paste("Latitude")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
c9.pred.lat


#agriculture

c9.pred<-predict.gam(c9.gam4, newAgriculture, se.fit = T, type="link")
c9.pred<-cbind(newAgriculture,c9.pred)
c9.pred$lower<-c9.pred$fit-2*c9.pred$se.fit
c9.pred$upper<-c9.pred$fit+2*c9.pred$se.fit

c9.pred.agriculture<-ggplot(data=c9.pred, aes(Agriculture, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="darkolivegreen1", alpha=0.6)+
  geom_line(col="darkolivegreen")+
  theme_classic()+
  xlab(expression(paste("% Agriculture cover")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
c9.pred.agriculture


c9.predictions<-plot_grid(c9.pred.decade, c9.pred.lon, c9.pred.lat, 
                          noeffect, noeffect, noeffect,
                          c9.pred.agriculture, noeffect, noeffect,
                          ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C', 
                                                                'D', 'E', 'F',
                                                                'G', 'H', 'I'))
c9.predictions

pdf("plots/c9_predictions.pdf", height=9, width=9)
grid.draw(c9.predictions)
dev.off()

###
#Adalia

#pull in final model from other analysis, just cut the number of that species in the offset

abi.gam4<-gam(Adalia.bipunctata~offset(log(1+Totalcount))+
                s(lon, sp=0.5)+
                s(Propinvasive, sp=0.5)+
                s(Agriculture, sp=0.5)+
                s(Developed, sp=0.5),  
              data=pre1995, family="nb")
summary(abi.gam4)
AIC(abi.gam4)


#decade
#set decade for rest of analyses
d<-1960

##

#lon
abi.pred<-predict.gam(abi.gam4, newlon, se.fit = T, type="link")
abi.pred<-cbind(newlon,abi.pred)
abi.pred$lower<-abi.pred$fit-2*abi.pred$se.fit
abi.pred$upper<-abi.pred$fit+2*abi.pred$se.fit

abi.pred.lon<-ggplot(data=abi.pred, aes(lon, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey", alpha=0.6)+
  geom_line(col="gray28")+
  theme_classic()+
  xlab(expression(paste("Longitude")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
abi.pred.lon



#prop invasive

abi.pred<-predict.gam(abi.gam4, newPI, se.fit = T, type="link")
abi.pred<-cbind(newPI,abi.pred)
abi.pred$lower<-abi.pred$fit-2*abi.pred$se.fit
abi.pred$upper<-abi.pred$fit+2*abi.pred$se.fit

abi.pred.pi<-ggplot(data=abi.pred, aes(Propinvasive, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="plum1", alpha=0.6)+
  geom_line(col="darkmagenta")+
  theme_classic()+
  xlab(expression(paste("Proportion invasive")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
abi.pred.pi


#agriculture

abi.pred<-predict.gam(abi.gam4, newAgriculture, se.fit = T, type="link")
abi.pred<-cbind(newAgriculture,abi.pred)
abi.pred$lower<-abi.pred$fit-2*abi.pred$se.fit
abi.pred$upper<-abi.pred$fit+2*abi.pred$se.fit

abi.pred.agriculture<-ggplot(data=abi.pred, aes(Agriculture, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="darkolivegreen1", alpha=0.6)+
  geom_line(col="darkolivegreen")+
  theme_classic()+
  xlab(expression(paste("% Agriculture cover")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
abi.pred.agriculture

#Developed
abi.pred<-predict.gam(abi.gam4, newDeveloped, se.fit = T, type="link")
abi.pred<-cbind(newDeveloped,abi.pred)
abi.pred$lower<-abi.pred$fit-2*abi.pred$se.fit
abi.pred$upper<-abi.pred$fit+2*abi.pred$se.fit

abi.pred.developed<-ggplot(data=abi.pred, aes(Developed, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="slategray2", alpha=0.6)+
  geom_line(col="slategray4")+
  theme_classic()+
  xlab(expression(paste("% Developed cover")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
abi.pred.developed



abi.predictions<-plot_grid(noeffect, abi.pred.lon, noeffect, 
                           abi.pred.pi, noeffect, noeffect,
                           abi.pred.agriculture, noeffect, abi.pred.developed,
                           ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C', 
                                                                 'D', 'E', 'F',
                                                                 'G', 'H', 'I'))
abi.predictions

pdf("plots/abi_predictions.pdf", height=9, width=9)
grid.draw(abi.predictions)
dev.off()

#############
#Cstig

#pull in final model from other analysis, just cut the number of that species in the offset

cstig.gam4<-gam(Chilocorus.stigma~offset(log(1+Totalcount))+
                  s(lon, sp=0.5)+
                  s(lat, sp=0.5)+
                  s(Propinvasive, sp=0.5)+
                  s(Forest, sp=0.5),  
                data=lb_all2, family="nb")
summary(cstig.gam4)
AIC(cstig.gam4)

#decade
#set decade for rest of analyses
d<-2000


#lon
cstig.pred<-predict.gam(cstig.gam4, newlon, se.fit = T, type="link")
cstig.pred<-cbind(newlon,cstig.pred)
cstig.pred$lower<-cstig.pred$fit-2*cstig.pred$se.fit
cstig.pred$upper<-cstig.pred$fit+2*cstig.pred$se.fit

cstig.pred.lon<-ggplot(data=cstig.pred, aes(lon, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey", alpha=0.6)+
  geom_line(col="gray28")+
  theme_classic()+
  xlab(expression(paste("Longitude")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
cstig.pred.lon


#lat
cstig.pred<-predict.gam(cstig.gam4, newlat, se.fit = T, type="link")
cstig.pred<-cbind(newlat,cstig.pred)
cstig.pred$lower<-cstig.pred$fit-2*cstig.pred$se.fit
cstig.pred$upper<-cstig.pred$fit+2*cstig.pred$se.fit

cstig.pred.lat<-ggplot(data=cstig.pred, aes(lat, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey", alpha=0.6)+
  geom_line(col="gray28")+
  theme_classic()+
  xlab(expression(paste("Latitude")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
cstig.pred.lat


#prop invasive

cstig.pred<-predict.gam(cstig.gam4, newPI, se.fit = T, type="link")
cstig.pred<-cbind(newPI,cstig.pred)
cstig.pred$lower<-cstig.pred$fit-2*cstig.pred$se.fit
cstig.pred$upper<-cstig.pred$fit+2*cstig.pred$se.fit

cstig.pred.pi<-ggplot(data=cstig.pred, aes(Propinvasive, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="plum1", alpha=0.6)+
  geom_line(col="darkmagenta")+
  theme_classic()+
  xlab(expression(paste("Proportion invasive")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
cstig.pred.pi




#Forest

cstig.pred<-predict.gam(cstig.gam4, newForest, se.fit = T, type="link")
cstig.pred<-cbind(newForest,cstig.pred)
cstig.pred$lower<-cstig.pred$fit-2*cstig.pred$se.fit
cstig.pred$upper<-cstig.pred$fit+2*cstig.pred$se.fit

cstig.pred.forest<-ggplot(data=cstig.pred, aes(Forest, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="lightgreen", alpha=0.6)+
  geom_line(col="darkgreen")+
  theme_classic()+
  xlab(expression(paste("% Forest cover")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
cstig.pred.forest



cstig.predictions<-plot_grid(noeffect, cstig.pred.lon, cstig.pred.lat, 
                             cstig.pred.pi, noeffect, noeffect,
                             noeffect, cstig.pred.forest, noeffect,
                             ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C', 
                                                                   'D', 'E', 'F',
                                                                   'G', 'H', 'I'))
cstig.predictions

pdf("plots/cstig_predictions.pdf", height=9, width=9)
grid.draw(cstig.predictions)
dev.off()

#####################
#ok, things are fussier from here out because we're deling with invasives, or strong responses to individual invasives.

#Ok, first let's do C7 because it's only the one prediction interval we need

c7.gam4<-gam(Coccinella.septempunctata~offset(log(1+Totalcount))+
               s(Agriculture, sp=0.5),  
             data=after1980, family="nb")
summary(c7.gam4)
AIC(c7.gam4)
#Now we've got to adjust prediction intervals a bit because the remaining taxa aren't responding to proportion invasive

d<-2000

c7.pred<-predict.gam(c7.gam4, newAgriculture, se.fit = T, type="link")
c7.pred<-cbind(newAgriculture,c7.pred)
c7.pred$lower<-c7.pred$fit-2*c7.pred$se.fit
c7.pred$upper<-c7.pred$fit+2*c7.pred$se.fit


c7.pred.agriculture<-ggplot(data=c7.pred, aes(Agriculture, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="darkolivegreen1", alpha=0.6)+
  geom_line(col="darkolivegreen")+
  theme_classic()+
  xlab(expression(paste("% Agriculture cover")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
c7.pred.agriculture

c7.predictions<-plot_grid(noeffect, noeffect, noeffect, 
                      noeffect, noeffect, noeffect,
                      c7.pred.agriculture, noeffect, noeffect,
                      ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C', 
                                                            'D', 'E', 'F',
                                                            'G', 'H', 'I'))
c7.predictions

pdf("plots/c7_predictions.pdf", height=9, width=9)
grid.draw(c7.predictions)
dev.off()

##############
#hcon intervals  for decade, lat, c7, ha, dev

#decade
newDecade <- with(lb_all2,
                  data.frame(Decade = seq(min(Decade), max(Decade), length = 100),
                             Totalcount=500,
                             lat=mean(lat), 
                             Coccinella.septempunctata=mean(Coccinella.septempunctata),
                             Harmonia.axyridis=mean(Harmonia.axyridis),
                             Developed=mean(Developed, na.rm=T)))


#lat
newlat <- with(lb_all2,
               data.frame(Decade = d,
                          Totalcount=500, 
                          lat=seq(min(lat), max(lat), length = 100), 
                          Coccinella.septempunctata=mean(Coccinella.septempunctata),
                          Harmonia.axyridis=mean(Harmonia.axyridis),
                          Developed=mean(Developed, na.rm=T)))

#C7
newC7 <- with(lb_all2,
              data.frame(Decade = d,
                         Totalcount=500, 
                         lat=mean(lat), 
                         Coccinella.septempunctata=seq(min(Coccinella.septempunctata), max(Coccinella.septempunctata), length = 100),
                         Harmonia.axyridis=mean(Harmonia.axyridis),
                         Developed=mean(Developed, na.rm=T)))
#HA
newHA <- with(lb_all2,
              data.frame(Decade = d,
                         Totalcount=500, 
                         lat=mean(lat),
                         Coccinella.septempunctata=mean(Coccinella.septempunctata),
                         Harmonia.axyridis=seq(min(Harmonia.axyridis), 15, length = 100),
                         Developed=mean(Developed, na.rm=T)))


#Developed
newDeveloped <- with(lb_all2,
                     data.frame(Decade = d,
                                Totalcount=500,
                                lat=mean(lat), 
                                Coccinella.septempunctata=mean(Coccinella.septempunctata),
                                Harmonia.axyridis=mean(Harmonia.axyridis),
                                Developed=seq(min(Developed, na.rm=T), max(Developed, na.rm=T), length = 100)))




#pull in final model from other analysis, just cut the number of that species in the offset

hcon.gam4<-gam(Hippodamia.convergens~offset(log(1+Totalcount))+
                 s(Decade, sp=0.5, k=4)+
                 s(lat, sp=0.5)+
                 s(log(1+Coccinella.septempunctata), sp=0.5, k=4)+
                 s(log(1+Harmonia.axyridis), sp=0.5, k=4)+
                 s(Developed, sp=0.5),  
               data=lb_all2, family="nb")
summary(hcon.gam4)
AIC(hcon.gam4)


hcon.pred<-predict.gam(hcon.gam4, newDecade, se.fit = T, type="link")
hcon.pred<-cbind(newDecade,hcon.pred)
hcon.pred$lower<-hcon.pred$fit-2*hcon.pred$se.fit
hcon.pred$upper<-hcon.pred$fit+2*hcon.pred$se.fit

#decade
#set decade for rest of analyses
d<-2000

##
hcon.pred.decade<-ggplot(data=hcon.pred, aes(Decade, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey19", alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1929, 2011)+
  xlab(expression(paste("Year")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
hcon.pred.decade


#lat
hcon.pred<-predict.gam(hcon.gam4, newlat, se.fit = T, type="link")
hcon.pred<-cbind(newlat,hcon.pred)
hcon.pred$lower<-hcon.pred$fit-2*hcon.pred$se.fit
hcon.pred$upper<-hcon.pred$fit+2*hcon.pred$se.fit

hcon.pred.lat<-ggplot(data=hcon.pred, aes(lat, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey", alpha=0.6)+
  geom_line(col="gray28")+
  theme_classic()+
  xlab(expression(paste("Latitude")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
hcon.pred.lat


#c7

hcon.pred<-predict.gam(hcon.gam4, newC7, se.fit = T, type="link")
hcon.pred<-cbind(newC7,hcon.pred)
hcon.pred$lower<-hcon.pred$fit-2*hcon.pred$se.fit
hcon.pred$upper<-hcon.pred$fit+2*hcon.pred$se.fit

hcon.pred.c7<-ggplot(data=hcon.pred, aes(Coccinella.septempunctata, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="lightcoral", alpha=0.6)+
  geom_line(col="darkred")+
  theme_classic()+
  xlab(expression(paste("Captures of ", italic("Coccinella septempunctata"))))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
hcon.pred.c7

#harmonia

hcon.pred<-predict.gam(hcon.gam4, newHA, se.fit = T, type="link")
hcon.pred<-cbind(newHA,hcon.pred)
hcon.pred$lower<-hcon.pred$fit-2*hcon.pred$se.fit
hcon.pred$upper<-hcon.pred$fit+2*hcon.pred$se.fit

hcon.pred.ha<-ggplot(data=hcon.pred, aes(Harmonia.axyridis, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="burlywood1", alpha=0.6)+
  geom_line(col="darkorange")+
  theme_classic()+
  xlab(expression(paste("Captures of ", italic("Harmonia axyridis"))))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
hcon.pred.ha


#Developed
hcon.pred<-predict.gam(hcon.gam4, newDeveloped, se.fit = T, type="link")
hcon.pred<-cbind(newDeveloped,hcon.pred)
hcon.pred$lower<-hcon.pred$fit-2*hcon.pred$se.fit
hcon.pred$upper<-hcon.pred$fit+2*hcon.pred$se.fit

hcon.pred.developed<-ggplot(data=hcon.pred, aes(Developed, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="slategray2", alpha=0.6)+
  geom_line(col="slategray4")+
  theme_classic()+
  xlab(expression(paste("% Developed cover")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
hcon.pred.developed



hcon.predictions<-plot_grid(hcon.pred.decade, noeffect, hcon.pred.lat, 
                            noeffect, hcon.pred.c7, hcon.pred.ha,
                            noeffect, noeffect, hcon.pred.developed,
                            ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C', 
                                                                  'D', 'E', 'F',
                                                                  'G', 'H', 'I'))
hcon.predictions

pdf("plots/hcon_predictions.pdf", height=9, width=9)
grid.draw(hcon.predictions)
dev.off()

###########################
# now finally the harmonia predictions

d<-2000
#decade

after1990<-lb_all2[which(lb_all2$Decade>=1990),]


newDecade <- with(after1990,
                  data.frame(Decade = seq(min(Decade), max(Decade), length = 100),
                             Totalcount=500, 
                             lat=mean(lat), 
                             Aphidophagous=mean(Aphidophagous), 
                             Forest=mean(Forest, na.rm=T), 
                             Developed=mean(Developed, na.rm=T)))



#lat
newlat <- with(after1990,
               data.frame(Decade = d,
                          Totalcount=500, 
                          lat=seq(min(lat), max(lat), length = 100), 
                          Aphidophagous=mean(Aphidophagous), 
                          Forest=mean(Forest, na.rm=T), 
                          Developed=mean(Developed, na.rm=T)))

#Aphidophagous
newAp <- with(after1990,
                  data.frame(Decade = d,
                         Totalcount=500, 
                         lat=mean(lat), 
                         Aphidophagous=seq(min(Aphidophagous), max(Aphidophagous), length = 100),
                         Forest=mean(Forest, na.rm=T), 
                         Developed=mean(Developed, na.rm=T)))
                         

#Forest
newForest <- with(after1990,
                  data.frame(Decade = d,
                             Totalcount=500, 
                             lat=mean(lat), 
                             Aphidophagous=mean(Aphidophagous),
                             Forest=seq(min(Forest, na.rm=T), max(Forest, na.rm=T), length = 100), 
                             Developed=mean(Developed, na.rm=T)))
#Developed
newDeveloped <- with(after1990,
                     data.frame(Decade = d,
                                Totalcount=500, 
                                lat=mean(lat), 
                                Aphidophagous=mean(Aphidophagous),
                                Forest=mean(Forest, na.rm=T), 
                                Developed=seq(min(Developed, na.rm=T), max(Developed, na.rm=T), length = 100)))


#ok, harmonia
ha.gam4<-gam(Harmonia.axyridis~offset(log(1+Totalcount))+
               s(Decade, sp=0.5, k=3)+
               s(lat, sp=0.5)+
               s(log(1+Aphidophagous), sp=0.5, k=4)+
               s(Forest, sp=0.5)+
               s(Developed, sp=0.5),  
             data=after1990, family="nb")
summary(ha.gam4)
AIC(ha.gam4)



ha.pred<-predict.gam(ha.gam4, newDecade, se.fit = T, type="link")
ha.pred<-cbind(newDecade,ha.pred)
ha.pred$lower<-ha.pred$fit-2*ha.pred$se.fit
ha.pred$upper<-ha.pred$fit+2*ha.pred$se.fit

#decade
#set decade for rest of analyses
d<-2000

##
ha.pred.decade<-ggplot(data=ha.pred, aes(Decade, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey19", alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1929, 2011)+
  xlab(expression(paste("Year")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
ha.pred.decade
#lat
ha.pred<-predict.gam(ha.gam4, newlat, se.fit = T, type="link")
ha.pred<-cbind(newlat,ha.pred)
ha.pred$lower<-ha.pred$fit-2*ha.pred$se.fit
ha.pred$upper<-ha.pred$fit+2*ha.pred$se.fit

ha.pred.lat<-ggplot(data=ha.pred, aes(lat, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="grey", alpha=0.6)+
  geom_line(col="gray28")+
  theme_classic()+
  xlab(expression(paste("Latitude")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
ha.pred.lat


#aphidophagous

ha.pred<-predict.gam(ha.gam4, newAp, se.fit = T, type="link")
ha.pred<-cbind(newAp,ha.pred)
ha.pred$lower<-ha.pred$fit-2*ha.pred$se.fit
ha.pred$upper<-ha.pred$fit+2*ha.pred$se.fit

ha.pred.ap<-ggplot(data=ha.pred, aes(Aphidophagous, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="palevioletred", alpha=0.6)+
  geom_line(col="maroon4")+
  theme_classic()+
  xlab(expression(paste("Other Aphidophagous")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
ha.pred.ap

#Forest

ha.pred<-predict.gam(ha.gam4, newForest, se.fit = T, type="link")
ha.pred<-cbind(newForest,ha.pred)
ha.pred$lower<-ha.pred$fit-2*ha.pred$se.fit
ha.pred$upper<-ha.pred$fit+2*ha.pred$se.fit

ha.pred.forest<-ggplot(data=ha.pred, aes(Forest, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="lightgreen", alpha=0.6)+
  geom_line(col="darkgreen")+
  theme_classic()+
  xlab(expression(paste("% Forest cover")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
ha.pred.forest

#Developed
ha.pred<-predict.gam(ha.gam4, newDeveloped, se.fit = T, type="link")
ha.pred<-cbind(newDeveloped,ha.pred)
ha.pred$lower<-ha.pred$fit-2*ha.pred$se.fit
ha.pred$upper<-ha.pred$fit+2*ha.pred$se.fit

ha.pred.developed<-ggplot(data=ha.pred, aes(Developed, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="slategray2", alpha=0.6)+
  geom_line(col="slategray4")+
  theme_classic()+
  xlab(expression(paste("% Developed cover")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
ha.pred.developed




ha.predictions<-plot_grid(ha.pred.decade, noeffect, ha.pred.lat, 
                      ha.pred.ap, noeffect, noeffect,
                      noeffect, ha.pred.forest, ha.pred.developed,
                      ncol=3, rel_widths=c(1,1,1), labels=c('A', 'B', 'C', 
                                                            'D', 'E', 'F',
                                                            'G', 'H', 'I'))
ha.predictions

pdf("plots/ha_predictions.pdf", height=12, width=9)
grid.draw(ha.predictions)
dev.off()

######################
#build the big grid fig

#create graphical objects for labels in plots
blankspace<-text_grob(paste(""), color="black")
adalia<-text_grob(paste("Adalia \nbipunctata"), color="black", face="italic", size=11)
cnovo<-text_grob(paste("Coccinella \nnovemnota"), color="black", face="italic", size=11)
colmac<-text_grob(paste("Coleomegilla \nmaculata"), color="black", face="italic", size=11)
hippo<-text_grob(paste("Hippodamia \nconvergens"), color="black", face="italic", size=11)
cstigm<-text_grob(paste("Chilocorus \nstigma"), color="black", face="italic", size=11)
landscape<-text_grob(paste("Landscape parameters"), color="black", face="bold", size=13)
invasion<-text_grob(paste("Invasion parameters"), color="black", face="bold", size=13)
prop.inv<-text_grob(paste("Proportion \ninvasive"), color="black", size=11)
csept<-text_grob(paste("Coccinella \nseptempunctata"), color="black", face="italic", size=11)
haxy<-text_grob(paste("Harmonia \naxyridis"), color="black", face="italic", size=11)
agri<-text_grob(paste("% Agriculture"), color="black", size=11)
fore<-text_grob(paste("% Forest"), color="black", size=11)
devel<-text_grob(paste("% Developed"), color="black", size=11)

titlerow<-plot_grid(blankspace, invasion, landscape,
                    ncol=3, rel_widths=c(1,3,3))
labelrow<-plot_grid(blankspace, prop.inv, csept, haxy, agri, fore, devel,
                    ncol=7, rel_widths=c(1,1,1,1,1,1,1))
adaliarow<-plot_grid(adalia,
                     abi.pred.pi+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                             plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                     blankspace,
                     blankspace, 
                     abi.pred.agriculture+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")), 
                     blankspace, 
                     abi.pred.developed+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                     ncol=7, rel_widths=c(1,1,1,1,1,1,1))

c9row<-plot_grid(cnovo,
                     blankspace,
                     blankspace,
                     blankspace, 
                     c9.pred.agriculture+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                     plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")), 
                     blankspace, 
                     blankspace,
                     ncol=7, rel_widths=c(1,1,1,1,1,1,1))

cmacrow<-plot_grid(colmac,
                     cmac.pred.pi+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                              plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                     blankspace,
                     blankspace, 
                     cmac.pred.agriculture+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                       plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                     blankspace, 
                     blankspace,
                     ncol=7, rel_widths=c(1,1,1,1,1,1,1))
hconrow<-plot_grid(hippo,
                   blankspace,
                   hcon.pred.c7+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                            plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                   hcon.pred.ha+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                            plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")), 
                   blankspace, 
                   blankspace, 
                   hcon.pred.developed+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                   plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                   ncol=7, rel_widths=c(1,1,1,1,1,1,1))

cstigrow<-plot_grid(cstigm,
                    cstig.pred.pi+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                              plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                    blankspace,
                    blankspace,
                    blankspace, 
                    cstig.pred.forest+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                    blankspace, 
                    ncol=7, rel_widths=c(1,1,1,1,1,1,1))


megaplot<-plot_grid(titlerow, labelrow, adaliarow, c9row, cmacrow, hconrow, cstigrow,
                              ncol=1, rel_heights = c(0.2,0.7,1,1,1,1,1))

megaplot


pdf("plots/predictions_megaplot.pdf", height=6, width=7)
grid.draw(megaplot)
dev.off()