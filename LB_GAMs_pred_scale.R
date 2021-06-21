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
                        Totalcount= 500, 
                        lon=mean(lon), 
                        lat=mean(lat), 
                        Propinvasive=mean(Propinvasive),
                        Agriculture=mean(Agriculture, na.rm=T), 
                        Forest=mean(Forest, na.rm=T), 
                        Developed=mean(Developed, na.rm=T)))


#lon
newlon <- with(lb_all2,
                  data.frame(Decade = d,
                             Totalcount= 500, 
                             lon=seq(min(lon), max(lon), length = 100), 
                             lat=mean(lat), 
                             Propinvasive=mean(Propinvasive),
                             Agriculture=mean(Agriculture, na.rm=T), 
                             Forest=mean(Forest, na.rm=T), 
                             Developed=mean(Developed, na.rm=T)))

#lat
newlat <- with(lb_all2,
               data.frame(Decade = d,
                          Totalcount= 500, 
                          lon=mean(lon), 
                          lat=seq(min(lat), max(lat), length = 100), 
                          Propinvasive=mean(Propinvasive),
                          Agriculture=mean(Agriculture, na.rm=T), 
                          Forest=mean(Forest, na.rm=T), 
                          Developed=mean(Developed, na.rm=T)))
#Propinvasive
newPI <- with(lb_all2,
               data.frame(Decade = d,
                          Totalcount= 500, 
                          lon=mean(lon), 
                          lat=mean(lat), 
                          Propinvasive=seq(min(Propinvasive), max(Propinvasive)-0.05, length = 100),
                          Agriculture=mean(Agriculture, na.rm=T), 
                          Forest=mean(Forest, na.rm=T), 
                          Developed=mean(Developed, na.rm=T)))
#Agriculture
newAgriculture <- with(lb_all2,
               data.frame(Decade = d,
                          Totalcount= 500, 
                          lon=mean(lon), 
                          lat=mean(lat), 
                          Propinvasive=mean(Propinvasive),
                          Agriculture=seq(min(Agriculture, na.rm=T), max(Agriculture, na.rm=T), length = 100), 
                          Forest=mean(Forest, na.rm=T), 
                          Developed=mean(Developed, na.rm=T)))
#Forest
newForest <- with(lb_all2,
               data.frame(Decade = d,
                          Totalcount= 500, 
                          lon=mean(lon), 
                          lat=mean(lat), 
                          Propinvasive=mean(Propinvasive),
                          Agriculture=mean(Agriculture, na.rm=T), 
                          Forest=seq(min(Forest, na.rm=T), max(Forest, na.rm=T), length = 100), 
                          Developed=mean(Developed, na.rm=T)))
#Developed
newDeveloped <- with(lb_all2,
               data.frame(Decade = d,
                          Totalcount= 500, 
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
  scale_y_continuous(limits=c(-0.1, NA))
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
  scale_y_continuous(limits=c(-0.1, NA))
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
  scale_y_continuous(limits=c(-0.5, NA))
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
  scale_y_continuous(limits=c(-0.1, NA))
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
cstig.pred.agriculture



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

