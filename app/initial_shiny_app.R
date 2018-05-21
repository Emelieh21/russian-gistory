library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)

dat <- readRDS("shp_with_1967_data.rds")
dat$YEAR <-"1967"
nums <- names(dat)[unlist(lapply(as.data.frame(dat), is.numeric))]
names(nums) <- c("Population",
                 "Area (km2)",
                 "Lunchrooms & Snackbars",
                 "Restaurants",
                 "Factory Canteens & Teahouses",
                 "Total Establishments",
                 "Establishments per km2",
                 "Establishments per inhabitant")

ui <- dashboardPage(skin = "green",
  dashboardHeader(title="Russian GIStory"),
  dashboardSidebar(
    selectInput("measure","Select measure to plot: ", choices = nums, selected = "establishments_p_km2"),
    sliderTextInput(
          "year","Year:",
          grid = FALSE,
          choices = dat$YEAR)
  ),
  dashboardBody(
    em("A project by Olav and Emelie Hofland (2018)"),
    tags$hr(),
    withSpinner(leafletOutput("map"))
  )
)

server <- function(input, output){
  output$map <- renderLeaflet({
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = as.data.frame(dat)[,input$measure],
      na.color = "#d6d5d4")
    
    target <- dat[as.data.frame(dat)$YEAR==input$year,]
    
    library(leaflet)
    leaflet(target) %>%
      addPolygons(
        stroke = TRUE, fillOpacity = 1, smoothFactor = 0.5,
        opacity = 1, color = "#444444", weight = 1,
        popup=~paste("<b>Name:</b>",dat$ADMIN_NAME,"</br>",
                     "<b>Total Establishments:</b>",format(dat$total, big.mark=" ")),
        label =~dat$ADMIN_NAME,
        fillColor = ~pal(as.data.frame(dat)[,input$measure])) %>%
      addLegend("bottomright", pal = pal, values = ~as.data.frame(dat)[,input$measure],
                title = names(nums[nums==input$measure]),
                opacity = 1
      )
  })
}
shinyApp(ui,server)
