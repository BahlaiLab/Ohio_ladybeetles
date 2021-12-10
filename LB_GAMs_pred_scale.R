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

#stuff for creating heatmap scale for correlation coefficients

#Create a function to generate a continuous color palette for 'hot' to 'cool' trends
rbPal <- colorRampPalette(c('blue', 'grey63','red'))
#turn it into a discrete palette,
discrbPal<-rbPal(181)
#lighten the palette for ribbons
library(colorspace)
discrbPallt<-lighten(discrbPal, amount=0.3)
#create a vector where we can extract the index and use it to find the value in the palette
scalePal<-seq(-0.9,0.9, by=0.01)

#cmac space is up to 6
fitspace<-5
#calulate slope for overall 
this.slope<-(max(cmac.pred$Decade)-min(cmac.pred$Decade))*((lm(cmac.pred$fit~cmac.pred$Decade))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

cmac.pred.decade<-ggplot(data=cmac.pred, aes(Decade, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlim(1929, 2011)+
  xlab(expression(paste("Year")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
cmac.pred.decade


#lat
cmac.pred<-predict.gam(cmac.gam4, newlat, se.fit = T, type="link")
cmac.pred<-cbind(newlat,cmac.pred)
cmac.pred$lower<-cmac.pred$fit-2*cmac.pred$se.fit
cmac.pred$upper<-cmac.pred$fit+2*cmac.pred$se.fit

this.slope<-(max(cmac.pred$lat)-min(cmac.pred$lat))*((lm(cmac.pred$fit~cmac.pred$lat))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

cmac.pred.lat<-ggplot(data=cmac.pred, aes(lat, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("Latitude")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
cmac.pred.lat


#prop invasive

cmac.pred<-predict.gam(cmac.gam4, newPI, se.fit = T, type="link")
cmac.pred<-cbind(newPI,cmac.pred)
cmac.pred$lower<-cmac.pred$fit-2*cmac.pred$se.fit
cmac.pred$upper<-cmac.pred$fit+2*cmac.pred$se.fit

this.slope<-(max(cmac.pred$Propinvasive)-min(cmac.pred$Propinvasive))*((lm(cmac.pred$fit~cmac.pred$Propinvasive))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

cmac.pred.pi<-ggplot(data=cmac.pred, aes(Propinvasive, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("Proportion invasive")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
cmac.pred.pi


#agriculture

cmac.pred<-predict.gam(cmac.gam4, newAgriculture, se.fit = T, type="link")
cmac.pred<-cbind(newAgriculture,cmac.pred)
cmac.pred$lower<-cmac.pred$fit-2*cmac.pred$se.fit
cmac.pred$upper<-cmac.pred$fit+2*cmac.pred$se.fit

this.slope<-(max(cmac.pred$Agriculture)-min(cmac.pred$Agriculture))*((lm(cmac.pred$fit~cmac.pred$Agriculture))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

cmac.pred.agriculture<-ggplot(data=cmac.pred, aes(Agriculture, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("% Agriculture cover")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
cmac.pred.agriculture



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

fitspace<-5

this.slope<-(max(c9.pred$Decade)-min(c9.pred$Decade))*((lm(c9.pred$fit~c9.pred$Decade))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

#decade
#set decade for rest of analyses
d<-1960


##
c9.pred.decade<-ggplot(data=c9.pred, aes(Decade, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
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

this.slope<-(max(c9.pred$lon)-min(c9.pred$lon))*((lm(c9.pred$fit~c9.pred$lon))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

c9.pred.lon<-ggplot(data=c9.pred, aes(lon, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("Longitude")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
c9.pred.lon


#lat
c9.pred<-predict.gam(c9.gam4, newlat, se.fit = T, type="link")
c9.pred<-cbind(newlat,c9.pred)
c9.pred$lower<-c9.pred$fit-2*c9.pred$se.fit
c9.pred$upper<-c9.pred$fit+2*c9.pred$se.fit

this.slope<-(max(c9.pred$lat)-min(c9.pred$lat))*((lm(c9.pred$fit~c9.pred$lat))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

c9.pred.lat<-ggplot(data=c9.pred, aes(lat, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("Latitude")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
c9.pred.lat


#agriculture

c9.pred<-predict.gam(c9.gam4, newAgriculture, se.fit = T, type="link")
c9.pred<-cbind(newAgriculture,c9.pred)
c9.pred$lower<-c9.pred$fit-2*c9.pred$se.fit
c9.pred$upper<-c9.pred$fit+2*c9.pred$se.fit

this.slope<-(max(c9.pred$Agriculture)-min(c9.pred$Agriculture))*((lm(c9.pred$fit~c9.pred$Agriculture))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

c9.pred.agriculture<-ggplot(data=c9.pred, aes(Agriculture, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("% Agriculture cover")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
c9.pred.agriculture




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
fitspace<-5
##

#lon
abi.pred<-predict.gam(abi.gam4, newlon, se.fit = T, type="link")
abi.pred<-cbind(newlon,abi.pred)
abi.pred$lower<-abi.pred$fit-2*abi.pred$se.fit
abi.pred$upper<-abi.pred$fit+2*abi.pred$se.fit

this.slope<-(max(abi.pred$lon)-min(abi.pred$lon))*((lm(abi.pred$fit~abi.pred$lon))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

abi.pred.lon<-ggplot(data=abi.pred, aes(lon, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("Longitude")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
abi.pred.lon



#prop invasive

abi.pred<-predict.gam(abi.gam4, newPI, se.fit = T, type="link")
abi.pred<-cbind(newPI,abi.pred)
abi.pred$lower<-abi.pred$fit-2*abi.pred$se.fit
abi.pred$upper<-abi.pred$fit+2*abi.pred$se.fit

this.slope<-(max(abi.pred$Propinvasive)-min(abi.pred$Propinvasive))*((lm(abi.pred$fit~abi.pred$Propinvasive))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

abi.pred.pi<-ggplot(data=abi.pred, aes(Propinvasive, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("Proportion invasive")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
abi.pred.pi


#agriculture

abi.pred<-predict.gam(abi.gam4, newAgriculture, se.fit = T, type="link")
abi.pred<-cbind(newAgriculture,abi.pred)
abi.pred$lower<-abi.pred$fit-2*abi.pred$se.fit
abi.pred$upper<-abi.pred$fit+2*abi.pred$se.fit

this.slope<-(max(abi.pred$Agriculture)-min(abi.pred$Agriculture))*((lm(abi.pred$fit~abi.pred$Agriculture))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

abi.pred.agriculture<-ggplot(data=abi.pred, aes(Agriculture, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("% Agriculture cover")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
abi.pred.agriculture

#Developed
abi.pred<-predict.gam(abi.gam4, newDeveloped, se.fit = T, type="link")
abi.pred<-cbind(newDeveloped,abi.pred)
abi.pred$lower<-abi.pred$fit-2*abi.pred$se.fit
abi.pred$upper<-abi.pred$fit+2*abi.pred$se.fit

this.slope<-(max(abi.pred$Developed)-min(abi.pred$Developed))*((lm(abi.pred$fit~abi.pred$Developed))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

abi.pred.developed<-ggplot(data=abi.pred, aes(Developed, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("% Developed cover")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
abi.pred.developed



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
fitspace<-5

#lon
cstig.pred<-predict.gam(cstig.gam4, newlon, se.fit = T, type="link")
cstig.pred<-cbind(newlon,cstig.pred)
cstig.pred$lower<-cstig.pred$fit-2*cstig.pred$se.fit
cstig.pred$upper<-cstig.pred$fit+2*cstig.pred$se.fit

this.slope<-(max(cstig.pred$lon)-min(cstig.pred$lon))*((lm(cstig.pred$fit~cstig.pred$lon))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

cstig.pred.lon<-ggplot(data=cstig.pred, aes(lon, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("Longitude")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
cstig.pred.lon


#lat
cstig.pred<-predict.gam(cstig.gam4, newlat, se.fit = T, type="link")
cstig.pred<-cbind(newlat,cstig.pred)
cstig.pred$lower<-cstig.pred$fit-2*cstig.pred$se.fit
cstig.pred$upper<-cstig.pred$fit+2*cstig.pred$se.fit

this.slope<-(max(cstig.pred$lat)-min(cstig.pred$lat))*((lm(cstig.pred$fit~cstig.pred$lat))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))


cstig.pred.lat<-ggplot(data=cstig.pred, aes(lat, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("Latitude")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
cstig.pred.lat


#prop invasive

cstig.pred<-predict.gam(cstig.gam4, newPI, se.fit = T, type="link")
cstig.pred<-cbind(newPI,cstig.pred)
cstig.pred$lower<-cstig.pred$fit-2*cstig.pred$se.fit
cstig.pred$upper<-cstig.pred$fit+2*cstig.pred$se.fit

this.slope<-(max(cstig.pred$Propinvasive)-min(cstig.pred$Propinvasive))*((lm(cstig.pred$fit~cstig.pred$Propinvasive))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

cstig.pred.pi<-ggplot(data=cstig.pred, aes(Propinvasive, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("Proportion invasive")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
cstig.pred.pi




#Forest

cstig.pred<-predict.gam(cstig.gam4, newForest, se.fit = T, type="link")
cstig.pred<-cbind(newForest,cstig.pred)
cstig.pred$lower<-cstig.pred$fit-2*cstig.pred$se.fit
cstig.pred$upper<-cstig.pred$fit+2*cstig.pred$se.fit

this.slope<-(max(cstig.pred$Forest)-min(cstig.pred$Forest))*((lm(cstig.pred$fit~cstig.pred$Forest))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

cstig.pred.forest<-ggplot(data=cstig.pred, aes(Forest, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("% Forest cover")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
cstig.pred.forest







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
fitscpace<-5

this.slope<-(max(hcon.pred$Decade)-min(hcon.pred$Decade))*((lm(hcon.pred$fit~hcon.pred$Decade))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))
##
hcon.pred.decade<-ggplot(data=hcon.pred, aes(Decade, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlim(1929, 2011)+
  xlab(expression(paste("Year")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
hcon.pred.decade


#lat
hcon.pred<-predict.gam(hcon.gam4, newlat, se.fit = T, type="link")
hcon.pred<-cbind(newlat,hcon.pred)
hcon.pred$lower<-hcon.pred$fit-2*hcon.pred$se.fit
hcon.pred$upper<-hcon.pred$fit+2*hcon.pred$se.fit

this.slope<-(max(hcon.pred$lat)-min(hcon.pred$lat))*((lm(hcon.pred$fit~hcon.pred$lat))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))


hcon.pred.lat<-ggplot(data=hcon.pred, aes(lat, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("Latitude")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
hcon.pred.lat


#c7

hcon.pred<-predict.gam(hcon.gam4, newC7, se.fit = T, type="link")
hcon.pred<-cbind(newC7,hcon.pred)
hcon.pred$lower<-hcon.pred$fit-2*hcon.pred$se.fit
hcon.pred$upper<-hcon.pred$fit+2*hcon.pred$se.fit

this.slope<-(max(hcon.pred$Coccinella.septempunctata)-min(hcon.pred$Coccinella.septempunctata))*((lm(hcon.pred$fit~hcon.pred$Coccinella.septempunctata))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

hcon.pred.c7<-ggplot(data=hcon.pred, aes(Coccinella.septempunctata, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("Captures of ", italic("Coccinella septempunctata"))))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
hcon.pred.c7

#harmonia

hcon.pred<-predict.gam(hcon.gam4, newHA, se.fit = T, type="link")
hcon.pred<-cbind(newHA,hcon.pred)
hcon.pred$lower<-hcon.pred$fit-2*hcon.pred$se.fit
hcon.pred$upper<-hcon.pred$fit+2*hcon.pred$se.fit

this.slope<-(max(hcon.pred$Harmonia.axyridis)-min(hcon.pred$Harmonia.axyridis))*((lm(hcon.pred$fit~hcon.pred$Harmonia.axyridis))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

hcon.pred.ha<-ggplot(data=hcon.pred, aes(Harmonia.axyridis, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("Captures of ", italic("Harmonia axyridis"))))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
hcon.pred.ha


#Developed
hcon.pred<-predict.gam(hcon.gam4, newDeveloped, se.fit = T, type="link")
hcon.pred<-cbind(newDeveloped,hcon.pred)
hcon.pred$lower<-hcon.pred$fit-2*hcon.pred$se.fit
hcon.pred$upper<-hcon.pred$fit+2*hcon.pred$se.fit

this.slope<-(max(hcon.pred$Developed)-min(hcon.pred$Developed))*((lm(hcon.pred$fit~hcon.pred$Developed))$coefficients[2])/fitspace
colindex<-which.min(abs(scalePal - this.slope))

hcon.pred.developed<-ggplot(data=hcon.pred, aes(Developed, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlab(expression(paste("% Developed cover")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, fitspace))
hcon.pred.developed









######################
#build the big grid fig

#create graphical objects for labels in plots
blankspace<-text_grob(paste(""), color="black")
adalia<-text_grob(paste("Adalia \nbipunctata"), color="black", face="italic", size=11)
cnovo<-text_grob(paste("Coccinella \nnovemnotata"), color="black", face="italic", size=11)
colmac<-text_grob(paste("Coleomegilla \nmaculata"), color="black", face="italic", size=11)
hippo<-text_grob(paste("Hippodamia \nconvergens"), color="black", face="italic", size=11)
cstigm<-text_grob(paste("Chilocorus \nstigma"), color="black", face="italic", size=11)
spatio<-text_grob(paste("Spatio-temporal parameters"), color="black", face="bold", size=13)
landscape<-text_grob(paste("Landscape parameters"), color="black", face="bold", size=13)
invasion<-text_grob(paste("Invasion parameters"), color="black", face="bold", size=13)
prop.inv<-text_grob(paste("Proportion \ninvasive"), color="black", size=11)
csept<-text_grob(paste("Coccinella \nseptempunctata"), color="black", face="italic", size=11)
haxy<-text_grob(paste("Harmonia \naxyridis"), color="black", face="italic", size=11)
agri<-text_grob(paste("% Agriculture"), color="black", size=11)
fore<-text_grob(paste("% Forest"), color="black", size=11)
devel<-text_grob(paste("% Developed"), color="black", size=11)
deca<-text_grob(paste("Decade"), color="black", size=11)
longi<-text_grob(paste("Longitude"), color="black", size=11)
lati<-text_grob(paste("Latitude"), color="black", size=11)


#create a blank plot too for the space holders
blankspace1<-ggplot(data=hcon.pred, aes(Developed, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill="white", alpha=0.6)+
  geom_line(col="white")+
  theme_classic()+
  xlab(NULL)+ylab(NULL)+
  theme(axis.text=element_text(size=5, colour="white"),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt"),
      axis.line=element_line(color="grey"),
      axis.ticks=element_line(color="grey"))+
  annotate(geom="text", label="No effect", color="grey", x=40, y=2, size=2)


titlerow<-plot_grid(blankspace, spatio, invasion, landscape,
                    ncol=4, rel_widths=c(1,3,3,3))
labelrow<-plot_grid(blankspace, deca, longi, lati, prop.inv, csept, haxy, agri, fore, devel,
                    ncol=10, rel_widths=c(1,1,1,1,1,1,1,1,1,1))
adaliarow<-plot_grid(adalia,
                     blankspace1,
                     abi.pred.lon+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                              plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")), 
                     blankspace1,
                     abi.pred.pi+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                             plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                     blankspace1,
                     blankspace1, 
                     abi.pred.agriculture+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")), 
                     blankspace1, 
                     abi.pred.developed+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                     ncol=10, rel_widths=c(1,1,1,1,1,1,1,1,1,1))

c9row<-plot_grid(cnovo,
                     c9.pred.decade+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")), 
                     c9.pred.lon+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                             plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")), 
                     c9.pred.lat+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                             plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")), 
                     blankspace1,
                     blankspace1,
                     blankspace1, 
                     c9.pred.agriculture+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                     plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")), 
                     blankspace1, 
                     blankspace1,
                     ncol=10, rel_widths=c(1,1,1,1,1,1,1,1,1,1))

cmacrow<-plot_grid(colmac,
                     cmac.pred.decade+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")), 
                     blankspace1,
                     cmac.pred.lat+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                               plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")), 
                     cmac.pred.pi+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                              plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                     blankspace1,
                     blankspace1, 
                     cmac.pred.agriculture+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                       plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                     blankspace1, 
                     blankspace1,
                   ncol=10, rel_widths=c(1,1,1,1,1,1,1,1,1,1))
hconrow<-plot_grid(hippo,
                   hcon.pred.decade+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")), 
                   blankspace1,
                   hcon.pred.lat+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                             plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")), 
                   blankspace1,
                   hcon.pred.c7+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                            plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                   hcon.pred.ha+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                            plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt"))+
                     scale_y_continuous(breaks=c(0,1,2,3)), 
                   blankspace1, 
                   blankspace1, 
                   hcon.pred.developed+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                   plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                   ncol=10, rel_widths=c(1,1,1,1,1,1,1,1,1,1))

cstigrow<-plot_grid(cstigm,
                    blankspace1,
                    cstig.pred.lon+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                    cstig.pred.lat+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                               plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")), 
                    cstig.pred.pi+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                              plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                    blankspace1,
                    blankspace1,
                    blankspace1, 
                    cstig.pred.forest+xlab(NULL)+ylab(NULL)+theme(axis.text=element_text(size=5),
                                                                  plot.margin = margin(0.5, 0.5, 0.5, 0.5, "pt")),
                    blankspace1, 
                    ncol=10, rel_widths=c(1,1,1,1,1,1,1,1,1,1))


megaplot<-plot_grid(titlerow, labelrow, adaliarow, c9row, cmacrow, hconrow, cstigrow,
                              ncol=1, rel_heights = c(0.2,0.5,1,1,1,1,1))

megaplot


pdf("plots/predictions_megaplot_colorramp.pdf", height=6, width=10)
grid.draw(megaplot)
dev.off()