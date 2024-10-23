

#setwd("C:\\Users\\o_kho\\OneDrive - University of Evansville\\2024_Fall\\Stat300\\Urban_Seeds\\App_UrbanSeeds")
if ("raster" %in% .packages()) {
  detach("package:raster", unload = TRUE)
}


library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(shinythemes)
library(bslib)
library(imputeTS)
library(data.table) 
library(lubridate)


# All zip codes
url_1 <- "https://github.com/SDS-at-UE/Urban_Seeds-FA24/blob/main/zip_geo_selected.rda?raw=true"
load(url(url_1))

# product data: getting the zipps and join the geometry

url_2 <- "https://github.com/SDS-at-UE/Urban_Seeds-FA24/blob/main/fully_separated_orders_comb_bag_box.csv?raw=true"
Data <- read.csv(url(url_2))
#Data <- read.csv("fully_separated_orders_comb_bag_box.csv")
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
  select(ProductID, Order.Date,Customer.ID,ProductTotalPrice, Zip_Code = Billing.Zip) %>% 
  mutate(Product_name = factor(ProductID,
                               levels = c(241, 243, 248, 255, 258, 265, 272, 289, 299, 306, 368, 369, 374, 375, 376, 377, 379),
                               labels = c("breakfast bag", "dairy/egg bag", "fresh vegetable bag", "fresh fruit bag", "meat bag", "italian dinner bag", "mexican dinner bag", "grain/bean bag", "flavor box", "holiday dinner box", "local picks bag", "russet potatoes (4 each)", "meat bag (beef)", "meat bag (pork)", "fresh vegetable with picks", "smoked ham", "holiday bag")),
         Year = format(Order.Date, "%Y"),
         Season = get_season(Order.Date),
         Month = format(Order.Date, "%B"),
         Week = format(Order.Date, "%W"),
         Day = format(Order.Date, "%j"))


# Generate a monthly sequence
start_year_month <- format(min(Data$Order.Date), "%Y-%m")
end_year_month <- format(max(Data$Order.Date), "%Y-%m")

# Create a sequence of full months between the start and end dates
monthly_sequence <- seq(as.Date(paste0(start_year_month, "-01")), 
                        as.Date(paste0(end_year_month, "-01")), 
                        by = "month")

names(Data)[7] <- c("Zip_Code")

Data <- Data %>% 
  mutate(month_dates= monthly_sequence[findInterval(Order.Date, monthly_sequence)])


# product data for interactive map
main_prod_data <- Data %>% 
  select(Order.Date, ProductTotalPrice, Zip_Code) %>%
  mutate(month_dates= monthly_sequence[findInterval(Order.Date, monthly_sequence)]) %>% 
  select(Zip_Code, month_dates, ProductTotalPrice) %>% 
  group_by(Zip_Code, month_dates) %>% 
  summarise(total_price = ifelse(is.na(sum(ProductTotalPrice, na.rm=T)), 0, sum(ProductTotalPrice, na.rm=T)),
            total_order = sum(!is.na(ProductTotalPrice))) %>% 
  arrange(month_dates) %>% 
  distinct()


# top products and custoemrs:
top <- Data %>%
  group_by(Zip_Code, month_dates) %>%
  
  # Find top 3 products based on number of orders
  group_by(Zip_Code, month_dates, ProductID) %>%
  summarise(order_count = n(), .groups = 'drop') %>%
  arrange(desc(order_count)) %>%
  group_by(Zip_Code, month_dates) %>%
  slice_head(n = 3) %>%
  
  # Collapse the top 3 products into a single string
  summarise(top_products = paste(ProductID, collapse = ", ")) %>%
  
  # Find top 3 customers based on total spending
  left_join(Data %>% group_by(Zip_Code, month_dates, Customer.ID) %>%
              summarise(total_spent = sum(ProductTotalPrice), .groups = 'drop') %>%
              arrange(desc(total_spent)) %>%
              group_by(Zip_Code, month_dates) %>%
              slice_head(n = 3) %>%
              
              # Collapse the top 3 customers into a single string
              summarise(top_customers = paste(Customer.ID, collapse = ", ")),
            by = c("Zip_Code", "month_dates"))


main_prod_data <- left_join(main_prod_data, top, by = c("Zip_Code", "month_dates"))


# adding the geometry
mon_dates <- c()
for(i in monthly_sequence){
  mon_dates = c(mon_dates, rep(i, dim(my_zip)[1]))
}
My_zips <- data.frame(Zip_Code = rep(my_zip$Zip_Code,length(monthly_sequence)),
                      month_dates = as.Date(mon_dates),
                      geometry = rep(my_zip$geometry,length(monthly_sequence)))


main_prod_data <- left_join(My_zips, as.data.frame(main_prod_data), by = c("Zip_Code", "month_dates")) %>% 
  filter(!is.na(month_dates) & !is.na(Zip_Code))

main_prod_data["total_price"][is.na(main_prod_data["total_price"])] <- 0


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
                 tabPanel('Data',
                          titlePanel(
                            fluidRow(
                              column(3, div(style="text-align: center;",img(src = "urbanseeds.jpg", height = "100%", width = "100%")  )),
                              #column(2, span(h5(img(src = "Logo_US_Forest.jpg", height = "30%", width = "30%")  ,"US Forest Service" ))),
                              column(5, div(style = "font-size: 20px;color: black;","This page depicts the data analysis for Urban Seeds: Nourishing Our Community, Evansville, IN ")), 
                              column(2, div(style="text-align: left;",img(src = "UE_logo.jpg", height = "115%", width = "115%"))),
                              column(2, div(style="text-align: top;",img(src = "Stat300_2.jpg", height = '70px', width = '150px')))
                            )
                          ),
                          br(),
                          br(),
                          br(),
                          br(),
                          fluidRow(
                            column(2,),
                            column(8,
                                   fileInput('data_upload', 'Upload the data:',
                                             accept = c('.csv'),
                                             width = '100%'
                                             ),
                                   br(),
                                   textOutput("validation_msg")
                                   ),
                            column(2,)
                            
                          ),
                          br(),
                          fluidRow(
                            column(2,),
                            column(8,
                                   textOutput("table_msg"),
                                   
                                   tableOutput("head_table"),
                                   #DT::dataTableOutput("head_table")
                                   ),
                            column(2,)
                            
                            
                          )
                 ),
                 tabPanel('Time',
                          tabsetPanel(
                            tabPanel("Summary",
                                     
                                     ),
                            tabPanel("Detail",
                                     
                                     )
                          )
                 ),
                 tabPanel('Location',
                          fluidRow( column(3,),
                                    column(6,sliderInput(inputId = "dates", 
                                                         "Timeline of Selected Parameter", 
                                                         min = as.Date(min(monthly_sequence)),
                                                         max = as.Date(max(monthly_sequence)),
                                                         value = as.Date('2021-06-01'),
                                                         timeFormat = "%Y-%m",
                                                         step = 31,  # Consider changing to 'month'
                                                         animate = animationOptions(interval = 2000)),
                                    ),
                                    column(3,)        
                          ),
                          fluidRow(
                            column(3,
                                   wellPanel(
                                     selectInput(inputId = "parameter", "Choose the Parameter:",
                                                 choices = c("Revenue", "Total_number_order"),# 'Top_Products', 'Top_Customers'),
                                                 selected = "Revenue"),
                                     tags$hr(style = "border-top: 2px solid #000;"),
                                     br(),
                                     selectInput(inputId = "year", "Select Year:",
                                                 choices = unique(product_data$Year),
                                                 selected = unique(product_data$Year)[1]),
                                     br(),
                                     
                                     selectInput("selectedProducts", 
                                                 "Select Product(s):", 
                                                 choices = unique(MonthGRAPH$Product_name),# unique_products,
                                                 multiple = T,
                                                 selected = "breakfast bag"),
                                     br(),
                                     selectInput("selectedCustomers", 
                                                 "Select customer(s):", 
                                                 choices = c("",unique(Data$Customer.ID)),# unique_products,
                                                 multiple = T,
                                                 selected = ""),
                                     
                                   )
                                   ),
                            column(6,
                                   leafletOutput("map_pop"),
                                   ),
                            column(3)
                          ),
                          h4("Plots for All Years:"),
                          fluidRow(
                            column(12,
                                   plotOutput("scatter_all_years"),
                                   br(),
                                   plotOutput("ratio_all_years"),
                                   br(),
                                   plotOutput("prod_all_years"),
                                   br(),
                                   plotOutput("price_all_years"),
                                   
                                   
                            )
                          ),
                          tags$hr(style = "border-top: 2px solid #000;"),
                          h4("Plots for the Selected Year:"),
                          fluidRow(
                            column(2, 
                            ),
                            column(8,
                                   plotOutput("scatter_year"),
                                   br(),
                                   plotOutput("ratio_year"),
                                   br(),
                                   plotOutput("prod_year"),
                                   br(),
                                   plotOutput("price_year"),
                            ),
                            column(2)
                          ),
                          tags$hr(style = "border-top: 2px solid #000;"),
                          h4("Plots for Selected Zip Code:"),
                          fluidRow(
                            column(2, 
                            ),
                            column(8,
                                   plotOutput("scatter_zip"),
                                   br(),
                                   plotOutput("ratio_zip"),
                                   br(),
                                   plotOutput("prod_zip"),
                                   br(),
                                   plotOutput("price_zip"),
                            ),
                            column(2)
                          ),
                          tags$hr(style = "border-top: 2px solid #000;"),
                          h4("Plots for the Selected Products:"),
                          fluidRow(
                            column(2, 
                                   ),
                            column(8,
                                   #plotOutput("productPlot")
                                   ),
                            column(2)
                          ),
                          tags$hr(style = "border-top: 2px solid #000;"),
                          h4("Plots for the Selected Customers:"),
                          fluidRow(
                            column(2, 
                            ),
                            column(8,
                                   #plotOutput("productPlot")
                            ),
                            column(2)
                          ),
                          tags$hr(style = "border-top: 2px solid #000;"),
                          h4("Some Cumulative Plots:"),
                          fluidRow(
                            column(2, 
                            ),
                            column(8,
                                   plotOutput("productPlot")
                            ),
                            column(2)
                          ),
                          tags$hr(style = "border-top: 2px solid #000;"),
                          h4("Head of the data for the selected county:"),
                          fluidRow(
                            column(1,),
                            column(10,
                                   DT::dataTableOutput("tab1"),
                                   # br(),
                                   # textOutput("test"),
                                   
                            ),
                            column(1,)
                          ),
                )
)
  
  
  
  

###########################################################
server <- function(input, output, session) {
  GRIDrv <- reactiveVal()
  
  df_main <- reactive({
    req(input$data_upload)
    
    # Read the CSV file using base R's read.csv
    df <- read.csv(input$data_upload$datapath)
    
    # Validate column names
    required_columns <- c("Order.ID", "Customer.ID", "Order.Date", "Billing.Suburb", "Billing.State", "Billing.Zip", "Product.Details")
    if (!all(required_columns %in% names(df))) {
      validate("Uploaded file must contain the following columns: Order.ID, Customer.ID, Order.Date, Billing.Suburb, Billing.State, Billing.Zip, Product.Details")
    }
    print(dim(df))
    return(df)
  })
  
  output$validation_msg <- renderText({
    req(df_main())  # Ensure df_main is valid
    "File uploaded and validated successfully."
  })
  output$table_msg <- renderText({
    req(df_main())  # Ensure df_main is valid
    "Head of the selected variables of the ulpoaded data:"
  })

  
  # output$head_table <- DT::renderDataTable({
  #   head(df_main())
  # })
  
  output$head_table <- renderTable({
    req(df_main())  # Ensure the data exists before rendering
    required_columns <- c("Order.ID", "Customer.ID", "Order.Date", "Billing.Suburb", "Billing.State", "Billing.Zip")
    head(df_main()[,required_columns]) # Return first 6 rows of the dataframe
  })
  
  dates <- reactive({
    # data = main_prod_data %>%
    #   filter(as.Date(month_dates) == as.Date(input$dates)) 
    
    data <- main_prod_data %>%
      filter(floor_date(as.Date(month_dates), "month") == floor_date(as.Date(input$dates), "month"))
    
    
    # Validate that data is not empty
    validate(
      need(nrow(data) > 0, "No data available for the selected month.")
    )
    
    return(data)

  })
  #c("Total_Order_Price", "Total_number_order", 'Top_Products', 'Top_Customers')
  reactive_data <-  reactive({
    switch(input$parameter,
           Revenue = main_prod_data$total_price,
           Total_number_order = main_prod_data$total_order
           #Top_Products = main_prod_data$top_products,
           #Top_Customers = main_prod_data$top_customers
    )
    
  })
  
  reactive_stat <- reactive({
    switch(input$parameter,
           Revenue = dates()$total_price,
           Total_number_order = dates()$total_order
           #Top_Products = dates()$top_products,
           #Top_Customers = dates()$top_customers
    )
  })
  
  # months <- reactive({
  #   main_prod_data$month_dates
  # })
  
  pal_data <- reactive({
    validate(
      need(nrow(dates()) > 0, "No data to apply color palette.")
    )
    
    #rdata = reactive_data()
    colorNumeric(palette = color_pal, domain = reactive_data() + 0.001)
    #colorNumeric(palette = color_pal, domain = 0.001:(max(reactive_data(), na.rm = TRUE)+1))
    #colorNumeric(palette = color_pal, domain = rdata)#reactive_data())
  })
  
  popup_msg <- reactive({
    str_c("<strong>", dates()$Zip_Code, #
          "</strong><br /><strong>", dates()$month_dates, "</strong>",
          "<br /> Revenue: ", ifelse(is.na(dates()$total_price), "(NA)", dates()$total_price),
          "<br /> Total_number_order: ", ifelse(is.na(dates()$total_order), "(NA)", dates()$total_order),
          "<br /> Top_Products: ", ifelse(is.na(dates()$top_products), "(NA)", dates()$top_products),
          "<br /> Top_Customers: ", ifelse(is.na(dates()$top_customers), "(NA)", dates()$top_customers)
          )
  })
  
  output$map_pop <- renderLeaflet({
    # filtered_data <- main_prod_data %>% 
    #   filter(month_dates == as.Date(input$dates)) %>% 
    #   filter(!is.na(month_dates) & !is.na(Zip_Code))
    
    filtered_data <- dates()
    
    validate(
      need(nrow(filtered_data) > 0, "No data available to render map.")
    )
    
    leaflet(width = "100%",
            options = leafletOptions(zoomSnap = 0,
                                     zoomDelta = 0.25)) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>% 
      setView(lat = 38.012181, lng = -87.083593,  zoom = 8) %>% # 38.018259, -87.083593 (87.530370)   41.550835, -86.897873
      addPolygons(data = st_transform(filtered_data, crs = "+init=epsg:4326"),#st_transform(states_map2, crs = "+init=epsg:4326"),
                  #group = "Zip_Code",
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
      need(GRIDrv() != "", "Please select a zip code area to generate analyses.")
    )
    
    DData = main_prod_data %>% 
      subset(Zip_Code == GRIDrv()) 
    DData$total_order[is.na(DData$total_order)] <- 0
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
    filtered_data$Month <- factor(filtered_data$Month, levels = unique(filtered_data$Month))
    ggplot(filtered_data, aes(x = Month, y = TotalCount, fill = Product_name)) + #ProductID)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Products Throughout The Months",
           x = "Month",
           y = "Number of Product Sold") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #unique_products <- unique(MonthGRAPH$ProductID)
  
  # output$productSelect <- renderUI({
  #   checkboxGroupInput("selectedProducts", 
  #                      "Select Products:", 
  #                      choices = unique(MonthGRAPH$Product_name),# unique_products, 
  #                      selected = "breakfast bag")
  # })
  
  plot_data_all <- reactive({
    
    df <- product_data %>%
      select(ProductID, Year, Month, ProductTotalPrice) %>%
      mutate(month_date = as.Date(paste0("01-", Month, "-", Year), format = "%d-%b-%Y")) %>%
      arrange(month_date) %>%
      mutate(month_year = format(month_date, "%B_%Y")) %>%
      group_by(month_date) %>%
      summarise(total_price = sum(ProductTotalPrice, na.rm = TRUE),
                total_order = n(),
                ratio = total_price/total_order)
    
    df$month_year <- format(df$month_date, "%B %Y")
    df$month_year <- factor(df$month_year, levels = unique(df$month_year[order(df$month_date)]))
    
    df
    
  })
  
  plot_data_one <- reactive({
    df <- product_data %>%
      filter(Year == as.numeric(input$year)) %>%
      select(ProductID, Year, Month, ProductTotalPrice) %>%
      mutate(month_date = as.Date(paste0("01-", Month, "-", Year), format = "%d-%b-%Y")) %>%
      arrange(month_date) %>%
      mutate(month_year = format(month_date, "%B_%Y")) %>%
      group_by(month_date) %>%
      summarise(total_price = sum(ProductTotalPrice, na.rm = TRUE),
                total_order = n(),
                ratio = total_price/total_order)
    
    df$month_year <- format(df$month_date, "%B %Y")
    df$month_year <- factor(df$month_year, levels = unique(df$month_year[order(df$month_date)]))
    
    df
  })
  
  
  output$scatter_all_years <- renderPlot({
    df = plot_data_all()
    
    scale_factor <- max(df$total_price) / max(df$total_order)
    
    # Scatter plot with lines connecting points
    ggplot(df, aes(x = month_year)) + 
      # Points for total_order
      geom_point(aes(y = total_order), color = "blue") + 
      geom_line(aes(y = total_order, group = 1), color = "blue") +  # Line for total_order
      
      # Points for Revenue (scaled)
      geom_point(aes(y = total_price / scale_factor), color = "red") + 
      geom_line(aes(y = total_price / scale_factor, group = 1), color = "red") +  # Line for Revenue
      #geom_point(aes(y = total_price ), color = "red") + 
      #geom_line(aes(y = total_price, group = 1), color = "red") +  # Line for Revenue
      
      
      # Scale the y-axis for dual y-axes
      scale_y_continuous(
        name = "Total Orders",
        sec.axis = sec_axis(~ . * scale_factor, name = "Scaled Revenue")
      ) +
      
      # Add labels and titles
      labs(title = "Scatter Plot of Total Orders and Scaled Revenue Over Time",
           x = "Months") +
      
      # Customize axis titles
      theme(axis.title.y = element_text(color = "blue"),
            axis.title.y.right = element_text(color = "red"))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
  })
  
  output$ratio_all_years <- renderPlot({
    df = plot_data_all()
    
    ggplot(data = df)+
      geom_point(aes(x = month_year, y = ratio))+
      geom_line(aes(x = month_year, y = ratio, group = 1)) + 
      labs(title = "Ratio of Revenue and Number of Orders throughout Years",
           x = "Months",
           y = "Ratio") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  output$prod_all_years <- renderPlot({
    ddata <- plot_data_all()

    ggplot(ddata) +
      geom_bar(aes(x = month_year, y = total_order), stat = 'identity') +
      labs(title = "Total Number of Orders throughout Years",
           x = "Months",
           y = "Number of orders") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

  })
  
  output$price_all_years <- renderPlot({
    ddata <- plot_data_all()

    ggplot(ddata) +
      geom_bar(aes(x = month_year, y = total_price), stat = 'identity') +
      labs(title = "Revenue throughout Years",
           x = "Months",
           y = "Revenue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

  })
  
  
  output$scatter_year <- renderPlot({
    df = plot_data_one()
    
    scale_factor <- max(df$total_price) / max(df$total_order)
    
    # Scatter plot with lines connecting points
    ggplot(df, aes(x = month_year)) + 
      # Points for total_order
      geom_point(aes(y = total_order), color = "blue") + 
      geom_line(aes(y = total_order, group = 1), color = "blue") +  # Line for total_order
      
      # Points for Revenue (scaled)
      geom_point(aes(y = total_price / scale_factor), color = "red") + 
      geom_line(aes(y = total_price / scale_factor, group = 1), color = "red") +  # Line for Revenue
      
      # Scale the y-axis for dual y-axes
      scale_y_continuous(
        name = "Total Orders",
        sec.axis = sec_axis(~ . * scale_factor, name = "Scaled Revenue")
      ) +
      
      # Add labels and titles
      labs(title = "Scatter Plot of Total Orders and scaled Revenue over Selected Year",
           x = "Months") +
      
      # Customize axis titles
      theme(axis.title.y = element_text(color = "blue"),
            axis.title.y.right = element_text(color = "red"))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
  })
  
  output$ratio_year <- renderPlot({
    df = plot_data_one()
    
    ggplot(data = df)+
      geom_point(aes(x = month_year, y = ratio))+
      geom_line(aes(x = month_year, y = ratio, group = 1)) + 
      labs(title = "Ratio of Revenue and Number of Orders over Selected Year",
           x = "Months",
           y = "Ratio") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  output$prod_year <- renderPlot({
    ddata <- plot_data_one()

    ggplot(ddata) +
      geom_bar(aes(x = month_year, y = total_order), stat = 'identity') +
      labs(title = "Total Number of Orders throughout the Selected Year",
           x = "Months",
           y = "Number of orders") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

  })
  
  output$price_year <- renderPlot({
    ddata <- plot_data_one()

    ggplot(ddata) +
      geom_bar(aes(x = month_year, y = total_price), stat = 'identity') +
      labs(title = "Revenue throughout the Selected Year",
           x = "Months",
           y = "Revenue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

  })
  
  ################ Plots for Zip codes
  
  output$scatter_zip <- renderPlot({
    df = zip_data()
    
    df$month_year <- format(df$month_dates, "%B %Y")
    df$month_year <- factor(df$month_year, levels = unique(df$month_year[order(df$month_dates)]))

    
    scale_factor <- max(df$total_price) / max(df$total_order)
    
    # Scatter plot with lines connecting points
    ggplot(df, aes(x = month_year)) + 
      # Points for total_order
      geom_point(aes(y = total_order), color = "blue") + 
      geom_line(aes(y = total_order, group = 1), color = "blue") +  # Line for total_order
      
      # Points for Revenue (scaled)
      geom_point(aes(y = total_price / scale_factor), color = "red") + 
      geom_line(aes(y = total_price / scale_factor, group = 1), color = "red") +  # Line for Revenue
      
      # Scale the y-axis for dual y-axes
      scale_y_continuous(
        name = "Total Orders",
        sec.axis = sec_axis(~ . * scale_factor, name = "Scaled Revenue")
      ) +
      
      # Add labels and titles
      labs(title = "Scatter Plot of Total Orders and scaled Revenue over Years in the Selected Zip-Code",
           x = "Months") +
      
      # Customize axis titles
      theme(axis.title.y = element_text(color = "blue"),
            axis.title.y.right = element_text(color = "red"))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
  })
  
  output$ratio_zip <- renderPlot({
    df = zip_data()
    
    df$month_year <- format(df$month_dates, "%B %Y")
    df$month_year <- factor(df$month_year, levels = unique(df$month_year[order(df$month_dates)]))
    df$ratio <- df$total_price / df$total_order
    
    ggplot(data = df)+
      geom_point(aes(x = month_year, y = ratio))+
      geom_line(aes(x = month_year, y = ratio, group = 1)) + 
      labs(title = "Ratio of Revenue and Number of Orders over Years in the Selected Zip-Code",
           x = "Months",
           y = "Ratio") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  output$prod_zip <- renderPlot({
    df = zip_data()
    
    df$month_year <- format(df$month_dates, "%B %Y")
    df$month_year <- factor(df$month_year, levels = unique(df$month_year[order(df$month_dates)]))
 
    ddata = df
    ggplot(ddata) +
      geom_bar(aes(x = month_year, y = total_order), stat = 'identity') +
      labs(title = "Total Number of Orders throughout the Years in the Selected Zip-Code",
           x = "Months",
           y = "Number of orders") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  output$price_zip <- renderPlot({
    df = zip_data()
    
    df$month_year <- format(df$month_dates, "%B %Y")
    df$month_year <- factor(df$month_year, levels = unique(df$month_year[order(df$month_dates)]))
 
    ddata = df
    ggplot(ddata) +
      geom_bar(aes(x = month_year, y = total_price), stat = 'identity') +
      labs(title = "Revenue throughout the Years in the Selected Zip-Code",
           x = "Months",
           y = "Revenue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)


