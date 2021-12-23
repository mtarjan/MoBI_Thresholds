##MoBI Product Thresholding
##November 11, 2021
##M Tarjan

##define thresholds
thresholds<-c(0.99, 0.95, 0.9, 0.75)

##install packages if you don't have them yet (only do this once per machine)
install.packages("raster")
install.packages("rgdal")
install.packages("stringr")

##load required pacakges
library(raster)
library(rgdal)
library(stringr)

##Load MoBI product raster layer
mobi.raster<-raster::raster("H:/MoBI_Final_Results/Final_Tiffs_April2021/RSR_All.tif")

##load state boundaries
#states<-sf::st_read("G:/ellie/ReferenceLayers/US_Boundaries/tl_2017_us_state_prj_select_lower48_select.shp")
states<-rgdal::readOGR(dsn = "G:/ellie/ReferenceLayers/US_Boundaries", layer= "tl_2017_us_state_prj_select_lower48_select")

##check projections of two layers. if needed, convert state coordinate system/projection to MoBI product coordinate system/projection
proj4string(mobi.raster); proj4string(states)
#states.proj <- spTransform(states, CRS("+init=epsg:3157")) # reproject
#proj4string(states.proj)

##check that raster and state files align visually
plot(mobi.raster); plot(states, add=T)

##create a percentile raster for USA
mobi.percentile<-quantile(mobi.raster, probs=thresholds) ##see ?quantile to understand different options for how to calulate percentiles by setting type== and integer
plot(mobi.percentile)

##reclassify raster values based on percentile
##reclassification data.frame
reclass<-cbind(from=c(mobi.percentile, 0), to=c(cellStats(mobi.raster, stat='max'), mobi.percentile), percentile=c(thresholds,NA))

mobi.percentile.raster<-reclassify(x = mobi.raster, rcl = reclass, include.lowest=T)
plot(mobi.percentile.raster)

##write out the resulting raster as tif
#writeRaster(mobi.percentile.raster, "Output/MoBI.product.percentiles.tif", format="GTiff")
writeRaster(mobi.percentile.raster, "Output/MoBI.product.percentiles.asc", format="ascii")

##Create a raster for each state
for (j in 1:length(states$STUSPS)) { #for each state
  ##select the relevant state
  state.temp<-states$STUSPS[j]
  state.shp.temp<-subset(states, STUSPS==state.temp)
  ##clip the raster
  ## crop and mask
  r2 <- crop(mobi.raster, state.shp.temp)
  r3 <- mask(r2, state.shp.temp)
  ##find percentile cutoff values
  r3.percentile<-quantile(r3, probs=thresholds) ##see ?quantile to understand different options for how to calulate percentiles by setting type== and integer
  ##reclassify the raster based on the cutoffs
  reclass<-cbind(from=c(r3.percentile, 0), to=c(cellStats(r3, stat='max'), r3.percentile), percentile=c(thresholds,NA))
  r3.percentile.raster<-reclassify(x = r3, rcl = reclass, include.lowest=T)
  plot(r3.percentile.raster)
  
  ##write out the raster
  writeRaster(r3.percentile.raster, str_c("Output/MoBI.percentiles.",state.temp,".tif"), format="GTiff", overwrite=T)
}

##test reading raster back in
#test<-raster::raster("Output/MoBI.product.percentiles.tif"); plot(test)
