# Get unique regions 1967
setwd("D:/russian-gistory/data/")
library(readxl)

dataset <- read_excel("ONDERZOEK OBSHCHEPIT_HISTORISCHE STATISTIEKEN_tabel 2_1957_1967.xls")
localities <- unique(dataset[,c("TER_CODE","LOCALITY_LATIN")])
#write.csv(localities, "localities_1967.csv", row.names = F)

dataset <- read.csv2("localities_1967.csv",stringsAsFactors = F)
summary(duplicated(dataset$GMI_ADMIN))

check <- subset(dataset, duplicated(dataset$GMI_ADMIN))
# st petersburg and moscow make sense, because I assigned the oblast and the city the same code.

shp_republics <- readRDS("shp_republics_1990.rds")
#plot(shp)

library(raster)
shp <- shapefile("D:/russian-gistory/data/rlc_boundaries_699/fsuadmla.shp")

shp_df <- as.data.frame(shp)
shp <- subset(shp, CNTRY_NAME == "Russia")
shp_republics <- subset(shp_republics, CNTRY_NAME != "Russia")

# combining detailed RUS shp's with aggregated shps of republics
shp_republics$GMI_ADMIN <- paste0("CNTRY_",shp_republics$CNTRY_NAME)
shp_republics$ADMIN_NAME <- shp_republics$CNTRY_NAME

shp <- shp[,names(shp_republics)]

shp <- rbind(shp, shp_republics)

check <- as.data.frame(subset(shp, !GMI_ADMIN %in% dataset$GMI_ADMIN))
df <- read_excel("ONDERZOEK OBSHCHEPIT_HISTORISCHE STATISTIEKEN_tabel 2_1957_1967.xls")
rm(shp_republics)

library(tidyr)
library(dplyr)
df_wide <- spread(data = df[,c("TER_CODE","VALUE","HISTCLASS1_LAT")], value = VALUE, key = HISTCLASS1_LAT)
df_wide[is.na(df_wide)] <- 0
df_wide$establishments_total <- df_wide$`Bufety i zakusočnye`+df_wide$`Fabriki-kuhni, stolovye i čajnye`+df_wide$Restoran+df_wide$`Čislo predprijatij obŝestvennogo pitanija gosudarstvennyh organizacij po sojuznym respublikam`

names(df_wide) <- c("TER_CODE","lunchrooms_snackbars","food_drink_republic","factory_canteens_tea",
                    "restaurants","establishments_total")

df_wide <- merge(df_wide, dataset[,c("TER_CODE","GMI_ADMIN")], by="TER_CODE", all.x=T)
df_wide <- df_wide %>%
  group_by(GMI_ADMIN) %>%
  summarise(lunchrooms_snackbars = sum(lunchrooms_snackbars),
            restaurants = sum(restaurants),
            factory_canteens_tea = sum(factory_canteens_tea),
            total = sum(establishments_total))
shp <- merge(shp, df_wide, by="GMI_ADMIN", all.x=T)
saveRDS(shp,"shp_with_1967_data.rds")
shp <- readRDS("shp_with_1967_data.rds")

# colors to use in the map 
colfunc <- colorRampPalette(c("#FDD49E","#EF6548","#7F0000"))(9)

# map the data
spplot(shp, "total", main = "Territories of 1967's Soviet Union", sub = "Number of state-run catering systems", 
       col.regions = colfunc, cuts = 8)

## Attempt to fix coordinate system of shp
mercator <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
proj4string(shp) <- sp::CRS(mercator)

library(leaflet)

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = shp$total)

shp <- spTransform(shp, CRS("+proj=longlat +datum=WGS84"))
leaflet(shp) %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5, 
    popup=~paste("<b>Name:</b>",shp$ADMIN_NAME,"</br>",
                 "<b>Total:</b>",shp$total),
    color = ~pal(shp$total),
    label =~shp$ADMIN_NAME)
