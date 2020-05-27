#compute county centroids

library(sf)
library(ggplot2)

oh<- st_read("ODOT_shapes/ODOT_County_Boundaries.shp") 

plot(oh)
sf_cent <- st_centroid(oh)

plot(sf_cent)


counties_df<-fortify(sf_cent)
write.csv(counties_df, file="oh_co_centroids.csv")