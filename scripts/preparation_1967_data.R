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

options(scipen=999)
library(raster)
shp <- shapefile("D:/russian-gistory/data/rlc_boundaries_699/fsuadmla.shp")

shp_df <- as.data.frame(shp)
shp <- subset(shp, CNTRY_NAME == "Russia")
shp_republics <- subset(shp_republics, CNTRY_NAME != "Russia")

# combining detailed RUS shp's with aggregated shps of republics
shp_republics$GMI_ADMIN <- paste0("CNTRY_",shp_republics$CNTRY_NAME)
shp_republics$ADMIN_NAME <- shp_republics$CNTRY_NAME

library(dplyr)
## Oef... NA's are encoded as -9999...
shp_df$POP_ADMIN <-ifelse(shp_df$POP_ADMIN == -99999,NA,shp_df$POP_ADMIN)

# shp_republics_df <-  shp_df %>%
#   group_by(CNTRY_NAME) %>%
#   summarize(POP_ADMIN = sum(POP_ADMIN), AREA = sum(AREA))
# # Unfortunately some countries with population numbers
# summary(is.na(shp_republics_df$POP_ADMIN))
# missing_countries <- unique(shp_republics_df[is.na(shp_republics_df$POP_ADMIN),"CNTRY_NAME"])
# for (country in missing_countries$CNTRY_NAME){
#   print(country)
#   POP <- as.numeric(readline("Population 1967: "))
#   shp_republics_df[shp_republics_df$CNTRY_NAME == country, "POP_ADMIN"] <- POP
#   message("Saved!")
# }

#saveRDS(shp_republics_df,"republics_pop_1967.rds")

shp_republics_df <- readRDS("republics_pop_1967.rds")
shp_republics_df$AREA <- shp_republics_df$AREA/1000000
shp_republics <- merge(shp_republics, shp_republics_df[,c("CNTRY_NAME",
                                                          "POP_ADMIN",
                                                          "AREA")], 
                       by = "CNTRY_NAME", all.x=T)

shp$AREA <- shp$AREA/1000000

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
df_wide[df_wide == 0] <- NA

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
shp$establishments_p_km2 <- shp$total/shp$AREA
shp$POP_ADMIN <-ifelse(shp$POP_ADMIN == -99999,NA,shp$POP_ADMIN)

shp$establishments_p_inhabitant <- shp$total/shp$POP_ADMIN
names(shp)

saveRDS(shp,"shp_with_1967_data.rds")
shp <- readRDS("shp_with_1967_data.rds")

# colors to use in the map 
colfunc <- colorRampPalette(c("#FDD49E","#EF6548","#7F0000"))(9)

# map the data
spplot(shp, "establishments_p_inhabitant", main = "Territories of 1967's Soviet Union", sub = "Number of state-run catering systems", 
       col.regions = colfunc, cuts = 8)

## Attempt to fix coordinate system of shp
mercator <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
proj4string(shp) <- sp::CRS(mercator)
shp <- spTransform(shp, CRS("+proj=longlat +datum=WGS84"))
saveRDS(shp,"shp_with_1967_data.rds")

library(leaflet)

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = shp$establishments_p_km2,
  na.color = "white")

leaflet(shp) %>%
  addPolygons(
    stroke = TRUE, fillOpacity = 0.9, smoothFactor = 0.5, 
    popup=~paste("<b>Name:</b>",shp$ADMIN_NAME,"</br>",
                 "<b>Total:</b>",shp$total),
    opacity = 1, color = "#444444", weight = 1,
    fillColor = ~pal( shp$establishments_p_km2),
    label =~shp$ADMIN_NAME)
