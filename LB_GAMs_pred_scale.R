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
                          Propinvasive=seq(min(Propinvasive), max(Propinvasive), length = 100),
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

jitter<-position_jitter(width = 0.1, height = 0.02)

cmac.pred.decade<-ggplot(data=cmac.pred, aes(Decade, fit))+
  geom_point(data=lb_all2, aes(Decade, Coleomegilla.maculata), position = jitter, pch=21, size=1, fill="lightgrey")+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill='paleturquoise', alpha=0.6)+
  geom_line()+
  theme_classic()+
  xlim(1929, 2011)+
  xlab(NULL)+ylab("")+
  scale_y_continuous(trans='pseudo_log',limits=c(-0.1, NA))
cmac.pred.decade





######################################