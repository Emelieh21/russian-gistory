## spatially aggregating oblasts/krajs to higher level republics
library(raster)
library(maptools)

# read in original shp
ussr <- shapefile("D:/russian-gistory/data/rlc_boundaries_699/fsuadmla.shp")
plot(ussr)

# aggregate polygons by CNTRY_NAME (these are the republics)
ussr.aggr <- unionSpatialPolygons(ussr, IDs = ussr$CNTRY_NAME)
ussr.aggr$CNTRY_NAME <- getSpPPolygonsIDSlots(ussr.aggr)
plot(ussr.aggr)

# save the new polygons
saveRDS(ussr.aggr, "D:/russian-gistory/data/shp_republics_1990.rds")

# attach the catering values
dat <- read.csv("D:/russian-gistory/data/republics_1985.csv")
ussr.aggr <- merge(ussr.aggr, dat, by="CNTRY_NAME")

#library(RColorBrewer)
#colfunc <- brewer.pal(n = 9, name = "OrRd")

# colors to use in the map 
colfunc <- colorRampPalette(c("#FDD49E","#EF6548","#7F0000"))(9)

# map the data
spplot(ussr.aggr, "VALUE", main = "Republics of 1985's Soviet Union", sub = "Number of state-run catering systems", 
       col.regions = colfunc, cuts = 8)






