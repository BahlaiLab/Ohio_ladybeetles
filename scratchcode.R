#scratch code

#stuff for creating heatmap scale for correlation coefficients

#Create a function to generate a continuous color palette for 'hot' to 'cool' trends
rbPal <- colorRampPalette(c('blue', 'black','red'))
#turn it into a discrete palette,
discrbPal<-rbPal(20)
#lighten the palette for ribbons
library(colorspace)
discrbPallt<-lighten(discrbPal, amount=0.3)
#create a vector where we can extract the index and use it to find the value in the palette
scalePal<-seq(-0.95,0.95, by=0.1)


#calulate pearson correlation of this for overall trend
this.corr<-cor(cmac.pred$fit, cmac.pred$Decade)
colindex<-which.min(abs(scalePal - this.corr))

cmac.pred.decade<-ggplot(data=cmac.pred, aes(Decade, fit))+
  geom_ribbon(aes(ymin=lower, ymax=upper), fill=discrbPallt[colindex], alpha=0.6)+
  geom_line(color=discrbPal[colindex])+
  theme_classic()+
  xlim(1929, 2011)+
  xlab(expression(paste("Year")))+ylab("Predicted captures")+
  coord_cartesian(ylim=c(-0.1, NA))
cmac.pred.decade