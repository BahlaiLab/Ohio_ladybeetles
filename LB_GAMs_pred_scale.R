#let's make all the ladybeetle figures 'back transformed' into the scale of the data being taken, 
# assuming 500 ladybeetles captured per decade. This will essentially remake all the model figures but address 
#the fussier scale issues that make it harder to interpret things

#experimental pred code

#make prediction data for each  different variable. 100 points? yeah

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
                  data.frame(Decade = 2000,
                             Totalcount= 500, 
                             lon=seq(min(lon), max(lon), length = 100), 
                             lat=mean(lat), 
                             Propinvasive=mean(Propinvasive),
                             Agriculture=mean(Agriculture, na.rm=T), 
                             Forest=mean(Forest, na.rm=T), 
                             Developed=mean(Developed, na.rm=T)))

#lat
newlat <- with(lb_all2,
               data.frame(Decade = 2000,
                          Totalcount= 500, 
                          lon=mean(lon), 
                          lat=seq(min(lat), max(lat), length = 100), 
                          Propinvasive=mean(Propinvasive),
                          Agriculture=mean(Agriculture, na.rm=T), 
                          Forest=mean(Forest, na.rm=T), 
                          Developed=mean(Developed, na.rm=T)))
#Propinvasive
newPI <- with(lb_all2,
               data.frame(Decade = 2000,
                          Totalcount= 500, 
                          lon=mean(lon), 
                          lat=mean(lat), 
                          Propinvasive=seq(min(Propinvasive), max(Propinvasive)-0.05, length = 100),
                          Agriculture=mean(Agriculture, na.rm=T), 
                          Forest=mean(Forest, na.rm=T), 
                          Developed=mean(Developed, na.rm=T)))
#Agriculture
newAgriculture <- with(lb_all2,
               data.frame(Decade = 2000,
                          Totalcount= 500, 
                          lon=mean(lon), 
                          lat=mean(lat), 
                          Propinvasive=mean(Propinvasive),
                          Agriculture=seq(min(Agriculture, na.rm=T), max(Agriculture, na.rm=T), length = 100), 
                          Forest=mean(Forest, na.rm=T), 
                          Developed=mean(Developed, na.rm=T)))
#Forest
newForest <- with(lb_all2,
               data.frame(Decade = 2000,
                          Totalcount= 500, 
                          lon=mean(lon), 
                          lat=mean(lat), 
                          Propinvasive=mean(Propinvasive),
                          Agriculture=mean(Agriculture, na.rm=T), 
                          Forest=seq(min(Forest, na.rm=T), max(Forest, na.rm=T), length = 100), 
                          Developed=mean(Developed, na.rm=T)))
#Developed
newDeveloped <- with(lb_all2,
               data.frame(Decade = 2000,
                          Totalcount= 500, 
                          lon=mean(lon), 
                          lat=mean(lat), 
                          Propinvasive=mean(Propinvasive),
                          Agriculture=mean(Agriculture, na.rm=T), 
                          Forest=mean(Forest, na.rm=T), 
                          Developed=seq(min(Developed, na.rm=T), max(Developed, na.rm=T), length = 100)))


##########################################


#pull in final model from other analysis, just cut the number of species in the offset

cmac.gam4<-gam(Coleomegilla.maculata~offset(log(1+Totalcount))+
                 s(Decade, sp=0.5, k=4)+
                 s(lat, sp=0.5)+
                 s(Propinvasive, sp=0.5)+
                 s(Agriculture, sp=0.5),  
               data=lb_all2, family="nb")
summary(cmac.gam4)
AIC(cmac.gam4)
cmac.pred<-predict.gam(cmac.gam4, newDecade, se.fit = T, type="link")
cmac.pred<-cbind(newd,cmac.pred)
cmac.pred$lower<-cmac.pred$fit-2*cmac.pred$se.fit
cmac.pred$upper<-cmac.pred$fit+2*cmac.pred$se.fit


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