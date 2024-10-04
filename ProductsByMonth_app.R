#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

setwd("C:\\Users\\o_kho\\OneDrive - University of Evansville\\2024_Fall\\Stat300\\Urban_Seeds\\App_UrbanSeeds")

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(shinythemes)
library(bslib)
library(imputeTS)
library(data.table) 
# All zip codes
zip_geo <- st_read("cb_2018_us_zcta510_500k\\cb_2018_us_zcta510_500k.shp")
zip_geo_selected <- zip_geo %>% 
  select(ZCTA5CE10, geometry)

# product data: getting the zipps and join the geometry
Data <- read.csv("fully_separated_orders_comb_bag_box.csv")
Data_zip <- unique(substr(Data$Billing.Zip, 1, 5))
Data_zip <- data.frame(ZCTA5CE10 = Data_zip)

my_zip <- left_join(Data_zip, zip_geo_selected, by = "ZCTA5CE10")
names(my_zip) <- c("Zip_Code", "geometry")


# Create a function to determine the season

#Spring: March 20 to June 20
#Summer: June 21 to September 22
#Autumn (Fall): September 23 to December 20
#Winter: December 21 to March 19

get_season <- function(date_value) {
  month <- as.numeric(format(date_value, "%m"))
  day <- as.numeric(format(date_value, "%d"))
  
  ifelse((month == 3 & day >= 20) | (month >= 4 & month <= 6 & !(month == 6 & day > 20)), "Spring",
         ifelse((month == 6 & day >= 21) | (month >= 7 & month <= 9 & !(month == 9 & day > 22)), "Summer",
                ifelse((month == 9 & day >= 23) | (month >= 10 & month <= 12 & !(month == 12 & day > 20)), "Autumn", 
                       "Winter")))
}

# Setting the Date format to the order dates.
Data$Order.Date <- as.Date(Data$Order.Date, format = "%d/%m/%Y")

# setting only first 5 digit as zip code
Data$Billing.Zip <- substr(Data$Billing.Zip, 1, 5)

# selecting the data and adding year, season, month day
product_data <- Data %>% 
  select(ProductID, Order.Date,ProductTotalPrice, Zip_Code = Billing.Zip) %>% 
  mutate(Product_name = factor(ProductID,
                               levels = c(241, 243, 248, 255, 258, 265, 272, 289, 299, 306, 368, 369, 374, 375, 376, 377, 379),
                               labels = c("breakfast bag", "dairy/egg bag", "fresh vegetable bag", "fresh fruit bag", "meat bag", "italian dinner bag", "mexican dinner bag", "grain/bean bag", "flavor box", "holiday dinner box", "local picks bag", "russet potatoes (4 each)", "meat bag (beef)", "meat bag (pork)", "fresh vegetable with picks", "smoked ham", "holiday bag")),
         Year = format(Order.Date, "%Y"),
         Season = get_season(Order.Date),
         Month = format(Order.Date, "%B"),
         Week = format(Order.Date, "%W"),
         Day = format(Order.Date, "%j"))


# Generate a monthly sequence
monthly_sequence <- seq(from = min(Data$Order.Date), to = max(Data$Order.Date), by = "month")

# product data for interactive map
main_prod_data <- Data %>% 
  select(Order.Date, ProductTotalPrice, Zip_Code = Billing.Zip) %>%
  mutate(month_dates= monthly_sequence[findInterval(Order.Date, monthly_sequence)]) %>% 
  select(Zip_Code, month_dates, ProductTotalPrice) %>% 
  group_by(Zip_Code, month_dates) %>% 
  summarise(total_price = sum(ProductTotalPrice, na.rm=T)) %>% 
  arrange(month_dates) %>% 
  distinct()

# adding the geometry
mon_dates <- c()
for(i in monthly_sequence){
  mon_dates = c(mon_dates, rep(i, dim(my_zip)[1]))
}
My_zips <- data.frame(Zip_Code = rep(my_zip$Zip_Code,length(monthly_sequence)),
                      month_dates = as.Date(mon_dates),
                      geometry = rep(my_zip$geometry,length(monthly_sequence)))


main_prod_data <- left_join(My_zips, as.data.frame(main_prod_data), by = c("Zip_Code", "month_dates"))


main_prod_data <- st_as_sf(main_prod_data) %>%
  sf::st_set_crs(4326) %>% 
  sf::st_transform('+proj=longlat +datum=WGS84')

# select the data which was used in Madelyn's app
MonthGRAPH <- product_data %>% 
  select(Product_name, Month) %>% 
  group_by(Product_name, Month) %>% 
  summarise(TotalCount = n())

######################################### interactive map functions
layer_zip <- unique(main_prod_data$Zip_Code)

#### Setting the color range
color_pal1 <- colorRampPalette(colors = c("springgreen4", "yellow3"), space = "Lab")(2)

## Make vector of colors for second bin
#color_pal2 <- colorRampPalette(colors = c("yellow3", "orange"), space = "Lab")(5)

## Make vector of colors for third bin
color_pal3 <- colorRampPalette(colors = c("orange", "red3"), space = "Lab")(15)

## Make vector of colors for fourth bin
#color_pal4 <- colorRampPalette(colors = c("red3", "darkred"), space = "Lab")(5)

## Make vector of colors for last bin
color_pal5 <- colorRampPalette(colors = c("darkred", "black"), space = "Lab")(5)

## Combine the five color palettes
color_pal <- c(color_pal1, color_pal3, color_pal5)#, color_pal4, color_pal5)
#color_pal <- c(color_pal1, color_pal2, color_pal3, color_pal4, color_pal5)
#########

################################## Helper functions  #############################

## https://gist.github.com/addiversitas/d2659ff553f702d60105a97fe46261a0

#helper functions for choropleth animation
setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL, label = NULL,
                           options = NULL){
  
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip, label = label
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors = FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1] # drop layer column
  
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}

setShapeLabel <- function(map, data = getMapData(map), 
                          layerId,
                          label = NULL,
                          options = NULL){
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(label = label
               )))
  # evaluate all options
  options <- evalFormula(options, data = data)
  # make them the same length (by building a data.frame)
  options <- do.call(data.frame, c(options, list(stringsAsFactors = FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1] # drop layer column
  
  leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label);
}

#helper function in JS for choropleth animation
leafletjs <-  tags$head(
  tags$script(HTML('
  
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  style = HTMLWidgets.dataframeToD3(style);
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.setStyle(style[i]);
    }
  });
};
window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};
'
  ))
)

########################################################
# Valid themes are: cerulean, cosmo, cyborg, darkly, flatly, journal, 
# lumen, paper, readable, sandstone, simplex, slate, spacelab, 
# superhero, united, yeti.

ui <- navbarPage(leafletjs, theme = shinytheme("cosmo"),
                 title = "Urban Seeds Data Analysis Portal",
                 tabPanel('Products',
                          fluidRow( column(3,),
                                    column(6,sliderInput(inputId = "dates", "Timeline of Selected Parameter", 
                                                         min = min(monthly_sequence),
                                                         max = max(monthly_sequence),
                                                         value = monthly_sequence[2],
                                                         timeFormat = "%Y-%m",  # Format for Year-Month
                                                         step = 1,               # Step by 1 month
                                                         animate = animationOptions(interval = 1000),
                                    ),
                                    ),
                                    column(3,)        
                          ),
                          fluidRow(
                            column(3,
                                   wellPanel(
                                     selectInput(inputId = "parameter", "Choose the Parameter:",
                                                 choices = c("Total_Order_Price"),
                                                 selected = "Total_Order_Price")
                                   )
                                   ),
                            column(9,
                                   leafletOutput("map_pop"),
                                   )
                          ),
                          fluidRow(
                            column(4, 
                                   uiOutput("productSelect") # Placeholder for dynamic UI input
                                   ),
                            column(8,
                                   plotOutput("productPlot")
                                   )
                          ),
                          fluidRow(
                            column(12,
                                   DT::dataTableOutput("tab1"),
                                   # br(),
                                   # textOutput("test"),
                                   
                            )
                          ),
                )
)
  
  
  
  

###########################################################
server <- function(input, output, session) {
  GRIDrv <- reactiveVal()
  
  
  dates <- reactive({
    main_prod_data %>% 
      filter(month_dates == as.Date(input$dates))

  })
  
  reactive_data <-  reactive({
    switch(input$parameter,
           Total_Order_Price = main_prod_data$total_price
    )
    
  })
  
  reactive_stat <- reactive({
    switch(input$parameter,
           Total_Order_Price = dates()$total_price
    )
  })
  
  # months <- reactive({
  #   main_prod_data$month_dates
  # })
  
  pal_data <- reactive({
    #rdata = reactive_data()
    colorNumeric(palette = color_pal, domain = 0.001:(max(reactive_data(), na.rm = TRUE)+1))
    #colorNumeric(palette = color_pal, domain = rdata)#reactive_data())
  })
  
  popup_msg <- reactive({
    str_c("<strong>", dates()$Zip_Code, #
          "</strong><br /><strong>", dates()$month_dates, "</strong>",
          "<br /> Total_Order_Price: ", ifelse(is.na(dates()$total_price), "(NA)", dates()$total_price))
  })
  
  output$map_pop <- renderLeaflet({
    filtered_data <- main_prod_data %>% 
      filter(month_dates == as.Date(input$dates))
    
    leaflet(width = "100%",
            options = leafletOptions(zoomSnap = 0,
                                     zoomDelta = 0.25)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>% 
      setView(lat = 38.012181, lng = -87.530370,  zoom = 8.7) %>% #41.550835, -86.897873
      addPolygons(data = st_transform(filtered_data, crs = "+init=epsg:4326"),#st_transform(states_map2, crs = "+init=epsg:4326"),
                  group = "state",
                  color = "black",
                  fill = FALSE,
                  weight = 3) %>%
      addPolygons(data = st_transform(filtered_data, crs = "+init=epsg:4326"), #filter(data_new4, YEAR == 2002),#
                  layerId = layer_zip,
                  color = "white",
                  weight = 1,
                  smoothFactor = 0,
                  fillOpacity = 0.7)
  })
  
  
  
  observe({
    leafletProxy("map_pop", data = dates()) %>% 
      setShapeStyle(layerId = layer_zip, 
                    fillColor = ~ suppressWarnings(pal_data()(reactive_stat())))
  })
  
  observe({
    leafletProxy("map_pop", data = dates()) %>%
      setShapeLabel(layerId = layer_zip,
                    label = popup_msg())
  })
  
  observe({ 
    leafletProxy("map_pop") %>% 
      clearControls() %>% 
      addLegend("bottomleft",
                pal = pal_data(),
                values = na.omit(reactive_data()),
                title = str_to_title(str_replace_all(input$parameter, "_", " ")),
                na.label = "",
                opacity = 5)
  })
  
  
  observeEvent(input$map_pop_shape_click, {
    GRIDrv(input$map_pop_shape_click$id)
  })
  
  zip_data <- reactive({
    validate(
      need(GRIDrv() != "", "Please select a climate grid cell to generate analyses.")
    )
    
    DData = main_prod_data %>% 
      subset(Zip_Code == GRIDrv()) 
    DData
  })
  
  output$tab1 <- DT::renderDataTable({
    head(zip_data())
  })
  
  
  
  
  
  
  
  ######################################################
  ################################################
  ################################################
  
  
  
  
  
  # Render plot
  output$productPlot <- renderPlot({

    filtered_data <- MonthGRAPH %>% 
      filter(Product_name %in% input$selectedProducts)
      #filter(ProductID %in% input$selectedProducts)
    
    ggplot(filtered_data, aes(x = Month, y = TotalCount, fill = Product_name)) + #ProductID)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Products Throughout The Months",
           x = "Month",
           y = "Number of Product Sold") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #unique_products <- unique(MonthGRAPH$ProductID)
  
  output$productSelect <- renderUI({
    checkboxGroupInput("selectedProducts", 
                       "Select Products:", 
                       choices = unique(MonthGRAPH$Product_name),# unique_products, 
                       selected = "breakfast bag")
  })
}


# Run the application
shinyApp(ui = ui, server = server)















