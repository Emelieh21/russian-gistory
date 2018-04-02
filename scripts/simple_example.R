library(sp)
library(maps)
library(maptools)
library(leaflet)

setwd("D:/russian-gistory/data")

# make sure to use the latest maps package
# it was recently updated at the time of the answer

world <- map("world", fill=TRUE, plot=FALSE)
world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
world_map <- SpatialPolygonsDataFrame(world_map,
                                      data.frame(country=names(world_map), 
                                                 stringsAsFactors=FALSE), 
                                      FALSE)

cnt <- c("Russia")

target <- subset(world_map, country %in% cnt)

leaflet(target) %>% 
  addTiles() %>% 
  addPolygons(weight=1)

library(shiny)
#install.packages("shinyWidgets")
library(shinyWidgets)

dat <- read.csv("test_data.csv")
dat <- subset(dat,duplicated(YEAR)==FALSE)
dat <-dat[order(dat$YEAR),]
dat$YEAR <- as.character(dat$YEAR)

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = dat$VALUE)

ui <- fluidPage(
  sliderTextInput(
    "year","Year:",
    grid = FALSE,
    choices = dat$YEAR),
  leafletOutput("map")
)

server <- function(input, output){
  output$map <- renderLeaflet({
    target$VALUE <- dat[dat$YEAR==input$year,"VALUE"]
      
    library(leaflet)
    leaflet(target) %>%
      addTiles() %>%
      addPolygons(
        stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5,
        color = ~pal(target$VALUE))
  })
}
shinyApp(ui,server)
