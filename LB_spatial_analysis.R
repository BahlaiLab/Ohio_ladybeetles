#bring in the data with the specimen info and environmental data integrated 

library(tibble)
#gotta start with naming conventions- one data frame has abbreviated name, one has whole name

COUNTY_CD<-levels(as.factor(counties_df$COUNTY_CD))
County<-levels(as.factor(allcensus$County))

#and LB data
ladybeetle<-read.csv(file="specimen_data/LB_museumData_2020.csv", header=T, stringsAsFactors = T)
museum_name<-levels(as.factor(ladybeetle$County))

namematch<-cbind(COUNTY_CD,County)
#okay, looks like things are lining up, we can use this as a key for merging data


#Want to allign census with County, Decade in ladybeetle data

#first ned to change names of Decade coloumn

names(allcensus)[names(allcensus) == "Year"] <- "Decade"

lb_census<-merge(ladybeetle, allcensus, by=c("County", "Decade"), all.x=T)

#ok, looks like that did the trick, now let's merge in the centroid coordinates.

#first need to index them my the actual county name, not the county abbreviation

counties_df_name<-merge(counties_df, namematch)

#note these data are given in NAD83/ South Ohio projection, to convert to lat and long we can do this:
counties_ll <- st_transform(counties_df_name, "+proj=longlat +ellps=WGS84 +datum=WGS84")

#get the geometry into good ol'd fashioned lat and long

coords <- do.call(rbind, st_geometry(counties_ll$geometry)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

counties_all_in<-cbind(counties_ll,coords)

#ok, now we can merge the cooridinates
lb_allcontext<-merge(lb_census, counties_ll, by="County", all.x=T)




seal_coords <- do.call(rbind, st_geometry(counties_ll$geometry)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))