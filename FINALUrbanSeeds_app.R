setwd("C:/Users/madel/OneDrive/Documents/Urban_Seeds-FA24")
if ("raster" %in% .packages()) {
  detach("package:raster", unload = TRUE)
}


options(timeout=600)

# Check and install necessary packages, Load libraries
list.of.packages <- c('shiny', 'shinythemes', 'tidyverse', 'leaflet', 'sf', 'shinyWidgets', 'httpuv',
                      'bslib', 'imputeTS', 'data.table', 'lubridate', 'plotly', 'reticulate')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

sapply(list.of.packages, library, character.only = TRUE)

reticulate::source_python("walmart.py")
reticulate::source_python("urbanseeds.py")

PYTHON_DEPENDENCIES = c('pip', 'requests', 'BeautifulSoup', 'bs4', 
                        'json','pandas')

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(shinythemes)
library(bslib)
library(imputeTS)
library(data.table) 
library(lubridate)
library(plotly)
library(dplyr)


# All zip codes
url_1 <- "https://github.com/SDS-at-UE/Urban_Seeds-FA24/blob/main/zip_geo_selected.rda?raw=true"
load(url(url_1))

# product data: getting the zipps and join the geometry

# url_2 <- "https://github.com/SDS-at-UE/Urban_Seeds-FA24/blob/main/fully_separated_orders_comb_bag_box.csv?raw=true"
# Data <- read.csv(url(url_2))
# #Data <- read.csv("fully_separated_orders_comb_bag_box.csv")
# Data_zip <- unique(substr(Data$Billing.Zip, 1, 5))
# Data_zip <- data.frame(ZCTA5CE10 = Data_zip)
# 
# my_zip <- left_join(Data_zip, zip_geo_selected, by = "ZCTA5CE10")
# names(my_zip) <- c("Zip_Code", "geometry")
# 
# 
# # Create a function to determine the season
# 
# #Spring: March 20 to June 20
# #Summer: June 21 to September 22
# #Autumn (Fall): September 23 to December 20
# #Winter: December 21 to March 19

get_season <- function(date_value) {
  month <- as.numeric(format(date_value, "%m"))
  day <- as.numeric(format(date_value, "%d"))
  
  ifelse((month == 3 & day >= 20) | (month >= 4 & month <= 6 & !(month == 6 & day > 20)), "Spring",
         ifelse((month == 6 & day >= 21) | (month >= 7 & month <= 9 & !(month == 9 & day > 22)), "Summer",
                ifelse((month == 9 & day >= 23) | (month >= 10 & month <= 12 & !(month == 12 & day > 20)), "Autumn", 
                       "Winter")))
}

load("monthly_sequence.rda")

# # Setting the Date format to the order dates.
# Data$Order.Date <- as.Date(Data$Order.Date, format = "%d/%m/%Y")
# 
# # setting only first 5 digit as zip code
# Data$Billing.Zip <- substr(Data$Billing.Zip, 1, 5)
# 
# # selecting the data and adding year, season, month day
# product_data <- Data %>% 
#   select(ProductID, Order.Date,Customer.ID,ProductTotalPrice, Zip_Code = Billing.Zip) %>% 
#   mutate(Product_name = factor(ProductID,
#                                levels = c(241, 243, 248, 255, 258, 265, 272, 289, 299, 306, 368, 369, 374, 375, 376, 377, 379),
#                                labels = c("breakfast bag", "dairy/egg bag", "fresh vegetable bag", "fresh fruit bag", "meat bag", "italian dinner bag", "mexican dinner bag", "grain/bean bag", "flavor box", "holiday dinner box", "local picks bag", "russet potatoes (4 each)", "meat bag (beef)", "meat bag (pork)", "fresh vegetable with picks", "smoked ham", "holiday bag")),
#          Year = format(Order.Date, "%Y"),
#          Season = get_season(Order.Date),
#          Month = format(Order.Date, "%B"),
#          Week = format(Order.Date, "%W"),
#          Day = format(Order.Date, "%j"))
# 
# 
# # Generate a monthly sequence
# start_year_month <- format(min(Data$Order.Date), "%Y-%m")
# end_year_month <- format(max(Data$Order.Date), "%Y-%m")
# 
# # Create a sequence of full months between the start and end dates
# monthly_sequence <- seq(as.Date(paste0(start_year_month, "-01")), 
#                         as.Date(paste0(end_year_month, "-01")), 
#                         by = "month")
# 
# names(Data)[7] <- c("Zip_Code")
# 
# Data <- Data %>% 
#   mutate(month_dates= monthly_sequence[findInterval(Order.Date, monthly_sequence)])
# 
# 
# # product data for interactive map
# main_prod_data <- Data %>% 
#   select(Order.Date, ProductTotalPrice, Zip_Code) %>%
#   mutate(month_dates= monthly_sequence[findInterval(Order.Date, monthly_sequence)]) %>% 
#   select(Zip_Code, month_dates, ProductTotalPrice) %>% 
#   group_by(Zip_Code, month_dates) %>% 
#   summarise(total_price = ifelse(is.na(sum(ProductTotalPrice, na.rm=T)), 0, sum(ProductTotalPrice, na.rm=T)),
#             total_order = sum(!is.na(ProductTotalPrice))) %>% 
#   arrange(month_dates) %>% 
#   distinct()
# 
# 
# # top products and custoemrs:
# top <- Data %>%
#   group_by(Zip_Code, month_dates) %>%
#   
#   # Find top 3 products based on number of orders
#   group_by(Zip_Code, month_dates, ProductID) %>%
#   summarise(order_count = n(), .groups = 'drop') %>%
#   arrange(desc(order_count)) %>%
#   group_by(Zip_Code, month_dates) %>%
#   slice_head(n = 3) %>%
#   
#   # Collapse the top 3 products into a single string
#   summarise(top_products = paste(ProductID, collapse = ", ")) %>%
#   
#   # Find top 3 customers based on total spending
#   left_join(Data %>% group_by(Zip_Code, month_dates, Customer.ID) %>%
#               summarise(total_spent = sum(ProductTotalPrice), .groups = 'drop') %>%
#               arrange(desc(total_spent)) %>%
#               group_by(Zip_Code, month_dates) %>%
#               slice_head(n = 3) %>%
#               
#               # Collapse the top 3 customers into a single string
#               summarise(top_customers = paste(Customer.ID, collapse = ", ")),
#             by = c("Zip_Code", "month_dates"))
# 
# 
# main_prod_data <- left_join(main_prod_data, top, by = c("Zip_Code", "month_dates"))
# 
# 
# # adding the geometry
# mon_dates <- c()
# for(i in monthly_sequence){
#   mon_dates = c(mon_dates, rep(i, dim(my_zip)[1]))
# }
# My_zips <- data.frame(Zip_Code = rep(my_zip$Zip_Code,length(monthly_sequence)),
#                       month_dates = as.Date(mon_dates),
#                       geometry = rep(my_zip$geometry,length(monthly_sequence)))
# 
# 
# main_prod_data <- left_join(My_zips, as.data.frame(main_prod_data), by = c("Zip_Code", "month_dates")) %>%
#   filter(!is.na(month_dates) & !is.na(Zip_Code))
# 
# main_prod_data["total_price"][is.na(main_prod_data["total_price"])] <- 0
# 
# 
# main_prod_data <- st_as_sf(main_prod_data) %>%
#   sf::st_set_crs(4326) %>%
#   sf::st_transform('+proj=longlat +datum=WGS84')
# 
# # select the data which was used in Madelyn's app
# MonthGRAPH <- product_data %>%
#   select(Product_name, Month) %>%
#   group_by(Product_name, Month) %>%
#   summarise(TotalCount = n())

######################################### interactive map functions


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

#c("#C9BFFA", "#54038A", "#05B58F", "#0D5754")

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
                 tags$style(HTML("
                              .equal-columns { 
                                display: flex; 
                                justify-content: space-between;  /* Ensures spacing between columns */
                              }
                              .equal-columns > div { 
                                flex: 1;                       /* Ensures all columns have equal width */
                                padding: 10px;                 /* Adds space inside the columns */
                                text-align: center;            /* Centers text inside each column */
                                border: 1px solid #ccc;        /* Optional: Adds a border for better visualization */
                              }
                              .navbar-default {
                              background-color: #C9BFFA; /* Change to your desired color */
                              border-color: #C9BFFA;
                              }
                              .navbar-default .navbar-nav > li > a {
                              color: #0D5754; /* Text color for the tabs */
                              }
                              .navbar-default .navbar-brand {
                              color: #0D5754; /* Text color for the title */
                              }
                              body {
                              background-color: #F7F4EB; /* Change to your desired color */
                              }
                            ")),
                 tabPanel("Data",
                          titlePanel(
                            fluidRow(
                              column(3, div(style="text-align: center;", img(src = "urbanseeds_newlogo.png", height = "100%", width = "100%"))),
                              column(4, div(style = "margin-top: 30px; text-align: center; font-size: 20px;color: #54038A;",HTML("<b>This page depicts the data analysis for <br> Urban Seeds: Nourishing Our Community,<br> Evansville, IN</b> "))), 
                              column(2, div(style = "text-align: top;", img(src = "UE_transparent.png", height = "80%", width = "80%"))),
                              column(2, div(style="text-align: center;", img(src = "STAT300_transparent.png", height = "115%", width = "115%")))
                            )
                          ),
                          hr(style = "margin-top: 5px; margin-bottom: 5px; border-width: 2px;; border-color: black;"),
                          fluidRow(
                            column(5,
                                   div(
                                     HTML("<b>Urban Seeds</b> supports the health of our community 
                                      advocating for a quality, robust, and equitable local 
                                      food system, to increase access to nourishing food for all.<br>"),
                                        tags$a(href="https://nourishevv.org/bags/", "Click here!")
                                     , style = "font-size: 19px;color: #0D5754;")
                                   ),
                                   
                            column(4,
                                   div(
                                     HTML("
                                            <b>Contacts:</b><br>
                                            Phone: (812) 777-5375<br>
                                            Email Address: director@urbanseeds.org<br>
                                            Mailing Address: 5444 E Indiana Street, #353<br>
                                            Evansville, IN 47715
                                          "), style = "font-size: 19px;color: #0D5754;"
                                   ),
                                   
                                   ),
                            column(3, 
                                   div(style="text-align: top;",img(src = "urban_seeds.png", height = '150px', width = '250px'))
                                   )
                          ),
                          hr(style = "margin-top: 5px; margin-bottom: 5px; border-width: 2px;; border-color: black;"),
                          fluidRow(
                            column(6,
                                   fileInput('data_upload', 'Upload the data:',
                                             accept = c('.csv'),
                                             width = '100%'
                                             )
                                   
                                   ),
                            column(6,
                                   br(),
                                   br(),
                                   div(textOutput("validation_msg"), style = "color: blue; font-size: 18px;")
                                   #textOutput("validation_msg")
                                   )
                          ),
                          hr(style = "margin-top: 5px; margin-bottom: 5px; border-width: 2px;; border-color: black;"),
                          br(),
                          fluidRow(
                            div(class = "equal-columns",
                                div(
                                  div(style="text-align: center; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                                      img(src = "grapes_icon.png", 
                                          height = '30%', 
                                          width = '30%',
                                          style = "display: block;")),
                                  div(
                                    h4(HTML("<b>Total Revenue:</b>")),
                                    tableOutput("revenue"),
                                    #style = "border-right: 2px solid black; padding-right: 10px;"
                                  )
                                ),
                                div(
                                  div(style="text-align: center; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                                      img(src = "straw_icon.png", 
                                          height = '23%', 
                                          width = '23%',
                                          style = "display: block;")),
                                  div(
                                    h4(HTML("<b>Coupon Amounts:</b>")),
                                    tableOutput("totalcoupon")
                                )),
                                div(
                                  div(style="text-align: center; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                                      img(src = "apple_icon.png", 
                                          height = '30%', 
                                          width = '30%',
                                          style = "display: block;")),
                                  div(
                                    h4(HTML("<b>Total Orders:</b>")),
                                    tableOutput("orders"),
                                    #style = "border-right: 2px solid black; padding-right: 10px;"
                                  )
                                ),
                                div(
                                  div(style="text-align: center; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                                      img(src = "watermelon_icon.png", 
                                          height = '50%', 
                                          width = '50%',
                                          style = "display: block;")),
                                  div(
                                    h4(HTML("<b>Total Customers:</b>")),
                                    tableOutput("customers"),
                                    #style = "border-right: 2px solid black; padding-right: 10px;"
                                  )
                                ),
                                div(
                                  div(style="text-align: center; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                                      img(src = "tom_icon.png", 
                                          height = '35%', 
                                          width = '35%',
                                          style = "display: block;")),
                                  div(
                                    h4(HTML("<b>Average Order Value:</b>")),
                                    tableOutput("average_value")
                                  )
                                )
                            ),
                            
                          ),
                          br(),
                          fluidRow(
                            div(
                              align = "center",
                              class = "footer",
                              wellPanel(
                                helpText(
                                  HTML(
                                    "Special thanks to Maria Marton, Executive Director of Urban Seeds, for her valuable feedback throughout this project.<br>
                                 "
                                  )
                                ),
                                br(),
                                HTML(
                                  '<p><b>This app was developed by</b><p>
                                  <p>Madelyn Tarvin, B.S. in Biochemistry, 2026<p>
                                  <p>Olivia Oliver, B.S. in Biochemistry, 2026<p> 
                                  <p>Theresa Stark, B.S. in Basic Chemistry, 2025<p>
                                  <p>Bakhrom Nasurullaev, B.S. in Computer Science, 2027<p>
                                  <p>Elimdar Idrisov, B.S. in Computer Science, 2027<p> 
                                  <p>at the <a href="https://www.evansville.edu/">University of Evansville</a>, led by <br> Dr. Omid Khormali.<p>'
                                ),
                              
                              )
                            )
                            
                          )
                          
                 ),
                 tabPanel("Interactive Map",
                          tags$head(
                            tags$style(HTML("
                            body {
                            background-color: #F7F4EB; /* Set the background color of the entire app */
                            }
                            
                            #parameter-box {
                            background-color: #C9BFFA; /* Change this to your desired color */
                            padding: 10px;
                            border-radius: 10px;
                            box-shadow: none;
                            margin: 0;
                            border: 2px solid #000;
                            }
                            
                                            "))
                          ),
                          fluidRow( column(3,),
                                    column(9,
                                           sliderInput(inputId = "dates",
                                                       "Timeline of Selected Parameter",
                                                       min = as.Date(min(monthly_sequence)),
                                                       max = as.Date(max(monthly_sequence)),
                                                       value = as.Date('2023-06-01'),
                                                       timeFormat = "%Y-%m",
                                                       step = 31,  # Consider changing to 'month'
                                                       width = '100%',
                                                       animate = animationOptions(interval = 2000)),
                                    ),
                                            
                          ),
                          fluidRow(
                            column(3,
                                   wellPanel(
                                     div(id = "parameter-box",
                                     selectInput(inputId = "parameter", "Choose the Parameter:",
                                                 choices = c("Revenue", "Total_number_order"),# 'Top_Products', 'Top_Customers'),
                                                 selected = "Revenue"),
                                     prettySwitch(
                                       inputId = "create_map",
                                       label = "Create the map!",
                                       status = "success",
                                       fill = TRUE
                                     ),
                                     tags$hr(style = "border-top: 2px solid #000;"),
                                     br(),
                                     ))
                                     
                                     
                                   ),
                            column(9,
                                   leafletOutput("map_pop"),
                            )),
                            #column(3)
                        
                          br(),
                          div(
                            HTML("<b>Some data summeries in the selected Zip Code:</b>"),
                            #style = "border-right: 2px solid black; padding-right: 10px;"
                          ),
                          fluidRow(
                            div(class = "equal-columns",
                                div(
                                  div(
                                    h4(HTML("<b>Total Revenue:</b>")),
                                    tableOutput("revenue_zip"),
                                    #style = "border-right: 2px solid black; padding-right: 10px;"
                                  )
                                ),
                                div(
                                  div(
                                    h4(HTML("<b>Coupon Amounts:</b>")),
                                    tableOutput("totalcoupon_zip")
                                  )),
                                div(
                                  div(
                                    h4(HTML("<b>Total Orders:</b>")),
                                    tableOutput("orders_zip"),
                                    #style = "border-right: 2px solid black; padding-right: 10px;"
                                  )
                                ),
                                div(
                                  div(
                                    h4(HTML("<b>Total Customers:</b>")),
                                    tableOutput("customers_zip"),
                                    #style = "border-right: 2px solid black; padding-right: 10px;"
                                  )
                                ),
                                div(
                                  div(
                                    h4(HTML("<b>Average Order Value:</b>")),
                                    tableOutput("average_value_zip")
                                  )
                                )
                            ) 
                          )
                          
                 ),
                 tabPanel("Product Analysis",
                          tabsetPanel(
                            tabPanel("Price Analysis",
                                     fluidRow(
                                       column(6,
                                                     uiOutput("zipcodeLS")
                                                     
                                                     
                                      ),
                                     column(6,
                                            uiOutput("selectedProductsLs")
                                            
                                            
                                     ),
                                     
                                     ),
                                     fluidRow(column(12,
                                                     plotOutput("price_change"),

                                                     )

                                     ),
                                     #tags$hr(style = "border-top: 2px solid #000;"),
                                     h4("Plots for Selected Zip Code(s):"),
                                     fluidRow(column(2,),
                                              column(8,
                                                     div(
                                                       HTML(
                                                         "<p><b>Scaling the Revenue</b> in the following plot</p>
                                          <p>The revenue is scaled using the formula:</p>
                                          <p style='margin-left: 20px;'>
                                            <code>scale_factor = max(Revenue) / max(Number of Products Sold)</code>
                                          </p>
                                          <p>In the plot, the revenue values are divided by the scale factor to allow comparison with the number of products sold on the same graph.</p>
                                      
                                          <p><b>Interpretation of Line Positions</b></p>
                                          <ul>
                                            <li><b>Lines Overlapping</b> (Revenue and Number of Products Sold are on top of each other):<br>
                                              <span style='margin-left: 20px;'>The revenue is directly proportional to the number of products sold, indicating a relatively constant average price per product over time.</span>
                                            </li>
                                            <li><b>Revenue Line Above</b> the Line for Number of Products Sold:<br>
                                              <span style='margin-left: 20px;'>This suggests a higher average price per product during this period.</span>
                                            </li>
                                            <li><b>Revenue Line Below</b> the Line for Number of Products Sold:<br>
                                              <span style='margin-left: 20px;'>This indicates a lower average price per product during this period.</span>
                                            </li>
                                          </ul>"
                                                       )
                                                     ),
                                                     ),
                                              column(2,)),
                                     fluidRow(
                                       column(1, 
                                              ),
                                       column(10,
                                              plotOutput("scatter_zip"),
                                              br(),
                                              
                                              ),
                                       column(1,
                                              )
                                     ),
                                     fluidRow(column(6,
                                                     plotOutput("price_change2"),
                                                        
                                              ),
                                              column(6,
                                                     plotOutput("price_change3")
                                                     )
                                     
                                     ), 
                                          
                            ),
                            tabPanel("Product(s) Trends",
                                     fluidRow(column(6,
                                                     #uiOutput("year"),
                                                     uiOutput("zipcode")
                                                     
                                                     
                                                     ),
                                              column(6,
                                                     uiOutput("selectedProductsL"),
                                                     
                                                     
                                                     ),
                                              # column(3,
                                              #        uiOutput("selectedCustomersL"),
                                              #        
                                              #        ),
                                              # column(3,
                                              #        #uiOutput("zipcode")
                                              #        )
                                              ),
                                     fluidRow(column(12,
                                                     plotOutput("productPlot") 
                                     )),
                                     fluidRow(column(12,
                                                     
                                                     plotOutput("ZipPlot"),
                                                     br(),
                                                     br(),
                                                     plotOutput("ZipPlot2"),
                                                     )),
                                     hr(style = "margin-top: 5px; margin-bottom: 5px; border-width: 2px;; border-color: black;"),
                                     fluidRow(
                                       column(4,
                                              selectInput("year_pie", "Choose the year:",
                                                          choices = c(2021:2024),
                                                          multiple = T,
                                                          selected = "2022")
                                              ),
                                       column(8,
                                              plotOutput("pieChart1"),
                                               
                                       )
                                     ),
                                     
                                     ),
                            tabPanel("Grouped Product Trends",
                                     h4("Grouped Products"),
                                     fluidRow(column(3,
                                                     #uiOutput("year"),
                                                     uiOutput("zipcode_group")
                                                     
                                                     
                                                     ),
                                                     column(3,
                                                            uiOutput("selectedProduct_group1"),
                                                            
                                                            
                                                     ),
                                              column(3,
                                                     uiOutput("selectedProduct_group2"),
                                                     
                                                     
                                              ),
                                              column(3,
                                                     div(
                                                       prettySwitch(
                                                         inputId = "group_plot",
                                                         label = "Create the plots!",
                                                         status = "success",
                                                         fill = TRUE
                                                       ),
                                                       style = "margin-top: 25px;" # Adjust spacing for alignment
                                                     )
                                                     ),
                                              column(5,
                                                     div(
                                                       HTML(
                                                         "<p>Select zip code and products of interest for each group<p>
                                                         <p>i.e. group 1: 'meat bag, meat bag (beef), meat bag (pork); group 2: 'fresh vegetable bag and fresh vegetable bag with picks'<p>")
                                                     ),)
                                            ),
                                     fluidRow(
                                       column(12,
                                                     plotOutput("productPlot_group") 
                                        )
                                     ),
                                     
                                     
                                     
                                     
                                     )
                          ),
                          
                          
                          
                          
                ),
                tabPanel("Customer Analysis",
                         tabsetPanel(
                           tabPanel("Customer Summeries",
                                    fluidRow(
                                      column(6,
                                             div( 
                                             div(
                                               h4(HTML("<b>Customers in Zip Codes:</b>")),
                                               #style = "border-right: 2px solid black; padding-right: 10px;"
                                             ),
                                             sliderInput("yearRange",
                                                              "Select Year Range",
                                                              min = 2021L,  # Set the minimum year to 2021
                                                              max = 2024L,  # Set the maximum year to 2024
                                                              value = 2021L, #c(2021L, 2022L,2023L,2024L),  # Default to the full range (2021 to 2024)
                                                              step = 1,
                                                              animate = TRUE,
                                                              sep = ""  # Remove comma separation in slider labels
                                             ),
                                             br(),
                                             plotlyOutput("customerPlot"),
                                             style = "border-right: 2px solid black; padding-right: 10px;")
                                              
                                             ),
                                      column(6,
                                             div(
                                               h4(HTML("<b>Customers and Coupons:</b>")),
                                               style = "border-right: 2px solid black; padding-right: 10px;"
                                             ),
                                             tableOutput("coupons_customer"),
                                             br(),
                                             plotOutput("coupon1_customer")
                                             )
                                    ),
                                    fluidRow(column(6,
                                                    div(
                                                    div(
                                                      h4(HTML("<b>Orders and Coupons:</b>")),
                                                      #style = "border-right: 2px solid black; padding-right: 10px;"
                                                    ),
                                                    
                                                    tableOutput("coupons_order"),
                                                    br(),
                                                    plotOutput("coupon1_order"),
                                                    
                                                    style = "border-right: 2px solid black; padding-right: 10px;")
                                                    
                                                    ),
                                            column(6,
                                                   div(
                                                     h4(HTML("<b>Products and Coupons:</b>")),
                                                     style = "border-right: 2px solid black; padding-right: 10px;"
                                                   ),
                                                   tableOutput("coupons"),
                                                   br(),
                                                   plotOutput("coupon1")
                                                   
                                                   )

                                    )
                                    
                                    
                           ),
                           tabPanel("Customer Effects",
                                    fluidRow(column(3,
                                                    selectInput("selectedGroup",
                                                                "Which one is grouped?",
                                                                choices = c("Customers", "Products"),
                                                                selected = "Products"),
                                                    
                                                    
                                    ),
                                    column(3,
                                           uiOutput("selectedProductsT"),
                                           
                                           
                                    ),
                                    column(3,
                                           uiOutput("selectedCustomersT"),
                                           
                                    ),
                                    column(3,
                                           
                                    )),
                                    fluidRow(
                                      column(12,
                                             plotOutput("product_customer_Plot")
                                             )
                                    )
                                    
                           )
                         )
                ),
                tabPanel("Walmart Price",
                         fluidRow(
                           column(8,
                                  br(),
                                  br(),
                                  br(),
                                  div(HTML("Walmart selected product prices!"),
                                      style = "font-size: 19px;color: rgb(76, 38, 131);font-weight: bold;"),
                                  br(),
                                  div(
                                  fluidRow(
                                    column(6,
                                           tags$style(HTML("
                                               #prod_name-label, #zip_name-label {
                                                 font-size: 19px;
                                                 color: rgb(246, 139, 31);
                                                 font-weight: bold;
                                               }
                                             ")),
                                           textInput("prod_name", label = "Insert the name of the product:", placeholder = "Enter product name")
                                    ),
                                    column(6,
                                           textInput("zip_name", label = "Insert the Zip Code:", placeholder = "Enter ZIP code")
                                    ),
                                  ),
                                  div(tableOutput("walmart"), style = "font-family: Arial, sans-serif; font-size: 12px;"),
                                  style = "border-right: 2px solid black; padding-right: 10px;")
                                  ),
                           
                           column(4,
                                  
                                  div(tags$label("Create the tables!", style = "font-size: 19px; color: rgb(52, 101, 127); font-weight: bold;"),
                                    prettySwitch(
                                      inputId = "create_table",
                                      label = "",
                                      status = "success",
                                      fill = TRUE
                                    ),
                                    style = "margin-top: 10px;" # Adjust spacing for alignment
                                  ),
                                  br(),
                                  div(HTML("Urban Seeds product prices!"),
                                      style = "font-size: 19px;color: rgb(76, 38, 131);font-weight: bold;"),
                                  br(),
                                  br(),
                                  div(tableOutput("urbanseeds"), style = "font-family: Arial, sans-serif; font-size: 12px;")
                           )
                         )
                         
                ),
)
  
  
  
  

###########################################################
server <- function(input, output, session) {
  
  output$walmart <- renderTable({
    req(input$prod_name, input$zip_name)
    if(input$create_table){
      #as.data.frame(py_to_r(fetch_walmart_products(as.character(input$prod_name))))
      result <- fetch_walmart_products(as.character(input$prod_name), as.numeric(input$zip_name))

      A <- as.data.frame(py_to_r(unlist(result[1])))
      B <- as.data.frame(py_to_r(unlist(result[2])))
      
      A[A == "{}"] <- NA
      B[B == "{}"] <- NA
      
      A_clean <- na.omit(A)
      B_clean <- na.omit(B)
      
      D <- data.frame(A_clean, B_clean)
      names(D) <- c("Product_Name", "Price")
      D %>% arrange(desc(Price))
    }
  })
  
  output$urbanseeds <- renderTable({
    req(input$prod_name, input$zip_name)
    if(input$create_table){
      #as.data.frame(py_to_r(fetch_walmart_products(as.character(input$prod_name))))
      
      result <- fetch_urbanseeds_products()
      
      # Convert the Python dictionary to an R list
      result <- py_to_r(result)
      
      # Access the product names and prices
      product_names <- result$name
      product_prices <- result$price
      
      # Combine into a data frame
      product_data <- data.frame(Name = product_names, Price = product_prices)
      
      product_data %>% arrange(desc(Price))
      # product_names, product_prices <- fetch_urbanseeds_products()
      # 
      # # Convert Python objects to R
      # product_names <- py_to_r(product_names)
      # product_prices <- py_to_r(product_prices)
      # 
      # # Combine into a data frame in R
      # data.frame(Name = product_names, Price = product_prices) %>% 
      #   arrange(desc(Price))
      # 
      # result <- fetch_urbanseeds_products()
      # 
      # A <- as.data.frame(py_to_r(unlist(result[1])))
      # B <- as.data.frame(py_to_r(unlist(result[2])))
      # 
      # A[A == "{}"] <- NA
      # B[B == "{}"] <- NA
      # 
      # A_clean <- na.omit(A)
      # B_clean <- na.omit(B)
      # 
      # D <- data.frame(A_clean, B_clean)
      # names(D) <- c("Product_Name", "Price")
      # D %>% arrange(desc(Price))
    }
  })
  
  
  GRIDrv <- reactiveVal()
  
  
  #####################################Tab 1
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
  
  revenue_coupon <- reactive({
    ddata <- df_main()
    
    ddata$Order.Date <- as.Date(ddata$Order.Date, format = "%d/%m/%Y")
    ddata$Billing.Zip <- substr(ddata$Billing.Zip, 1, 5)
    
    monthly_sequence <- seq(from = min(ddata$Order.Date), to = max(ddata$Order.Date), by = "month")
    start_year_month <- format(min(ddata$Order.Date), "%Y-%m")
    end_year_month <- format(max(ddata$Order.Date), "%Y-%m")
    
    # Create a sequence of full months between the start and end dates
    monthly_sequence <- seq(as.Date(paste0(start_year_month, "-01")), 
                            as.Date(paste0(end_year_month, "-01")), 
                            by = "month")
    
    names(ddata)[31] <- c("Zip_Code")
    
    
    ddata <- ddata %>% 
      mutate(Year = format(Order.Date, "%Y"),
             Month = format(Order.Date, "%B"),
             month_date = monthly_sequence[findInterval(Order.Date, monthly_sequence)])
    
    ddata
    
  })
  
  potral_datasets <- reactive({
    data_separated <- df_main() %>%
      separate_rows(Product.Details, sep = "\\|")
    data_clean2 <- data_separated %>%
      mutate(
        ProductID = sub(".*Product ID: (\\d+),.*", "\\1", Product.Details),
        ProductQty = sub(".*Product Qty: (\\d+),.*", "\\1", Product.Details),
        ProductSKU = sub(".*Product SKU: ([^,]*),.*", "\\1", Product.Details),
        ProductName = sub(".*Product Name: ([^,]*),.*", "\\1", Product.Details),
        ProductWeight = sub(".*Product Weight: ([^,]*),.*", "\\1", Product.Details),
        ProductVariationDetails = sub(".*Product Variation Details: (.*), Product Unit Price.*", "\\1", Product.Details),
        ProductUnitPrice = sub(".*Product Unit Price: ([^,]*),.*", "\\1", Product.Details),
        ProductTotalPrice = sub(".*Product Total Price: ([^,]*).*", "\\1", Product.Details)
      )
    
    #c(306, "Holiday Dinner Bag", 258 , "Meat Bag") 
    t1 = grep("Holiday Dinner Box", data_clean2$ProductName, value = FALSE)
    data_clean2$ProductName[t1] = "Holiday Dinner Bag"
    
    t2 = grep("Meat Box", data_clean2$ProductName, value = FALSE)
    t3 = grep("Meat Bag$", data_clean2$ProductName, value = FALSE)
    data_clean2$ProductName[c(t2,t3)] = "Meat Bag"
    
    
    data_clean2$ProductVariationList <- strsplit(data_clean2$ProductVariationDetails, ", ")
    
    # Step 4: Handle cases where "ProductVariationDetails" might be NA or empty
    # Correct check for NA applied to the whole list
    data_clean2$ProductVariationList <- lapply(data_clean2$ProductVariationList, function(x) if(all(is.na(x))) list() else x)
    
    # Step 5: Find maximum number of product details in "ProductVariationDetails"
    max_product_details <- max(sapply(data_clean2$ProductVariationList, length))
    
    # Step 6: Create new columns for each product detail
    for (i in 1:max_product_details) {
      data_clean2[[paste0("ProductDetail_", i)]] <- sapply(data_clean2$ProductVariationList, function(x) ifelse(length(x) >= i, x[[i]], NA))
    }
    
    # Step 6: Check the result
    data_clean2 <- data_clean2 %>%
      select(Order.ID, Customer.ID, Order.Date,Coupon.Details,Billing.Suburb,Billing.State,Billing.Zip, ProductID, ProductQty, ProductSKU, ProductName, ProductWeight, ProductUnitPrice, ProductTotalPrice, starts_with("ProductDetail_"))
    
    data_clean2$ProductName <-  gsub("Box", "Bag", data_clean2$ProductName)
    
    Data <- data_clean2
    Data_zip <- unique(Data$Billing.Zip)
    Data_zip <- unique(substr(Data_zip, 1, 5))
    Data_zip <- data.frame(ZCTA5CE10 = Data_zip)

    my_zip <- left_join(Data_zip, zip_geo_selected, by = "ZCTA5CE10")
    names(my_zip) <- c("Zip_Code", "geometry")
    
    
    Data$Order.Date <- as.Date(Data$Order.Date, format = "%d/%m/%Y")
    Data$Billing.Zip <- substr(Data$Billing.Zip, 1, 5)
    
    product_data <- Data %>% 
      select(ProductID, Order.Date, Customer.ID,ProductQty, ProductUnitPrice, ProductTotalPrice, Zip_Code = Billing.Zip) %>% 
      mutate(Product_name = factor(ProductID,
                                   levels = c(241, 243, 248, 255, 258, 265, 272, 289, 299, 306, 368, 369, 374, 375, 376, 377, 379),
                                   labels = c("breakfast bag", "dairy/egg bag", "fresh vegetable bag", "fresh fruit bag", "meat bag", "italian dinner bag", "mexican dinner bag", "grain/bean bag", "flavor box", "holiday dinner box", "local picks bag", "russet potatoes (4 each)", "meat bag (beef)", "meat bag (pork)", "fresh vegetable with picks", "smoked ham", "holiday bag")),
             Year = format(Order.Date, "%Y"),
             Season = get_season(Order.Date),
             Month = format(Order.Date, "%B"),
             Week = format(Order.Date, "%W"),
             Day = format(Order.Date, "%j"))
    
    
    ##############
    monthly_sequence <- seq(from = min(Data$Order.Date), to = max(Data$Order.Date), by = "month")
    start_year_month <- format(min(Data$Order.Date), "%Y-%m")
    end_year_month <- format(max(Data$Order.Date), "%Y-%m")
    
    # Create a sequence of full months between the start and end dates
    monthly_sequence <- seq(as.Date(paste0(start_year_month, "-01")), 
                            as.Date(paste0(end_year_month, "-01")), 
                            by = "month")
    
    names(Data)[7] <- c("Zip_Code")
    
    Data <- Data %>% 
      mutate(month_dates= monthly_sequence[findInterval(Order.Date, monthly_sequence)])
    
    Data$ProductTotalPrice <- as.numeric(Data$ProductTotalPrice)
    t = which(str_length(Data$Coupon.Details)>5)
    s = grep("SNAP",Data$Coupon.Details)
    r = setdiff(t,s)
    
    
    Data$coupon <- ifelse(str_length(Data$Coupon.Details)<=5, "Non-coupon", "SNAP-coupon")
    Data$coupon[r] <- "Other-coupon"
    
    
    main_prod_data <- Data %>% 
      select(Order.Date, ProductTotalPrice, Zip_Code) %>%
      mutate(month_dates= monthly_sequence[findInterval(Order.Date, monthly_sequence)]) %>% 
      select(Zip_Code, month_dates, ProductTotalPrice) %>% 
      group_by(Zip_Code, month_dates) %>% 
      summarise(total_price = ifelse(is.na(sum(as.numeric(ProductTotalPrice), na.rm=T)), 0, sum(as.numeric(ProductTotalPrice), na.rm=T)),
                total_order = sum(!is.na(as.numeric(ProductTotalPrice)))) %>% 
      arrange(month_dates)%>% 
      distinct()
    
    #main_prod_data <- left_join(main_prod_data, my_zip, by = "Zip_Code")
    
    #head(main_prod_data)
    # top products and custoemrs:
    prod_ID_name <- Data %>% 
      dplyr::select(ProductID,ProductName) %>% 
      distinct()
    
    
    top <- Data %>%
      group_by(Zip_Code, month_dates) %>%
      
      # Find top 3 products based on number of orders
      group_by(Zip_Code, month_dates, ProductID) %>%
      summarise(order_count = n_distinct(Order.ID), .groups = 'drop') %>%
      arrange(desc(order_count)) %>%
      group_by(Zip_Code, month_dates) %>%
      slice_head(n = 3) %>%
      left_join(prod_ID_name, by = 'ProductID', keep = NULL) %>%
      # Collapse the top 3 products into a single string
      summarise(top_products = paste(ProductID, collapse = ", "),
                top_products_names = paste(ProductName, collapse = ", ")) %>%
      
      # Find top 3 customers based on total spending
      left_join(Data %>% group_by(Zip_Code, month_dates, Customer.ID) %>%
                  summarise(total_spent = sum(as.numeric(ProductTotalPrice)), .groups = 'drop') %>%
                  arrange(desc(total_spent)) %>%
                  group_by(Zip_Code, month_dates) %>%
                  slice_head(n = 3) %>%
                  
                  # Collapse the top 3 customers into a single string
                  summarise(top_customers = paste(Customer.ID, collapse = ", ")),
                by = c("Zip_Code", "month_dates"))
    
    
    main_prod_data <- left_join(main_prod_data, top, by = c("Zip_Code", "month_dates"))
    

    mon_dates <- c()
    for(i in monthly_sequence){
      mon_dates = c(mon_dates, rep(i, dim(my_zip)[1]))
    }
    My_zips <- data.frame(Zip_Code = rep(my_zip$Zip_Code,length(monthly_sequence)),
                          month_dates = as.Date(mon_dates),
                          geometry = rep(my_zip$geometry,length(monthly_sequence))) %>% 
      arrange(month_dates)
    
    #main_prod_data  <- left_join(as.data.frame(My_zips), as.data.frame(main_prod_data), by = c("Zip_Code", "month_dates"))
    
    main_prod_data <- left_join(My_zips, as.data.frame(main_prod_data), by = c("Zip_Code", "month_dates")) %>%
      filter(!is.na(month_dates) & !is.na(Zip_Code))

    main_prod_data["total_price"][is.na(main_prod_data["total_price"])] <- 0

    main_prod_data <- st_as_sf(main_prod_data) %>%
      sf::st_set_crs(4326) %>%
      sf::st_transform('+proj=longlat +datum=WGS84')
    
    print(dim(main_prod_data))
    
    list(product_data = product_data, fully_seperated = Data, main_prod_data = main_prod_data, monthly_sequence = monthly_sequence)
    
  })
  
  

  # output$dates <- renderUI({
  #   DD = potral_datasets()$main_prod_data
  #   
  #   if (!inherits(DD$month_dates, "Date")) {
  #     DD$month_dates <- as.Date(DD$month_dates)
  #   }
  #   monthly_sequence <- unique(DD$month_dates)
  #   
  #   sliderInput(inputId = "dates",
  #               "Timeline of Selected Parameter",
  #               min = min(monthly_sequence),
  #               max = max(monthly_sequence),
  #               value = as.Date('2021-06-01'),
  #               timeFormat = "%Y-%m",
  #               step = 31,  # Consider changing to 'month'
  #               animate = animationOptions(interval = 2000))
  # })
  
  output$revenue <- renderTable({
    DD <- revenue_coupon()%>% 
      select(Year, Subtotal..inc.tax., Order.Total..inc.tax.) %>% 
      group_by(Year) %>% 
      summarise(revenue = sum(as.numeric(Order.Total..inc.tax.), na.rm = T))
    
    # DD <- potral_datasets()$product_data %>%
    #   dplyr::select(Year, ProductTotalPrice) %>%
    #   group_by(Year) %>%
    #   summarise(Revenue = sum(as.numeric(ProductTotalPrice)))
    as.data.frame(DD)
  })
  
  output$totalcoupon <- renderTable({
    DD <- revenue_coupon()%>% 
      select(Year, Subtotal..inc.tax., Order.Total..inc.tax.) %>% 
      group_by(Year) %>% 
      summarise(coupon = sum(as.numeric(Subtotal..inc.tax.), na.rm = T) - 
                  sum(as.numeric(Order.Total..inc.tax.), na.rm = T))
    
    # DD <- potral_datasets()$product_data %>%
    #   dplyr::select(Year, ProductTotalPrice) %>%
    #   group_by(Year) %>%
    #   summarise(Revenue = sum(as.numeric(ProductTotalPrice)))
    as.data.frame(DD)
  })
  
  output$orders <- renderTable({
    D <- df_main()
    D$Order.Date <- as.Date(D$Order.Date, format = "%d/%m/%Y")
    
    DDD <- D %>% 
      mutate(Year = format(Order.Date, "%Y")) %>% 
      select(Year, Order.ID) %>% 
      group_by(Year) %>% 
      summarise(Total_Orders = n())
    as.data.frame(DDD)
  })
  
  output$customers <- renderTable({
    D <- df_main()
    D$Order.Date <- as.Date(D$Order.Date, format = "%d/%m/%Y")
    
    DDD <- D %>% 
      mutate(Year = format(Order.Date, "%Y")) %>% 
      select(Year, Customer.ID) %>% 
      distinct() %>%
      group_by(Year) %>% 
      summarise(Total_Customers = n())
    as.data.frame(DDD)
  })
  
  output$average_value <- renderTable({
    D <- df_main()
    D$Order.Date <- as.Date(D$Order.Date, format = "%d/%m/%Y")
    
    DDD <- D %>% 
      mutate(Year = format(Order.Date, "%Y")) %>% 
      select(Year, Subtotal..inc.tax.) %>% 
      group_by(Year) %>% 
      summarise(Average_Order_Value = mean(Subtotal..inc.tax.))
    as.data.frame(DDD)
  })
  
  # select the data which was used in Madelyn's app
  MonthGRAPH <- reactive({

    DD <- potral_datasets()$product_data %>%
      dplyr::select(Product_name, Month) %>%
      group_by(Product_name, Month) %>%
      summarise(TotalCount = n())
    
    print(dim(DD))
    return(DD)
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
  
  
  #################################### Tab 2
  

  
  output$selectedProductsL <- renderUI({
    k <- potral_datasets()$product_data
    selectInput("selectedProductsL", 
                "Select Product(s):", 
                choices = unique(k$Product_name),# unique_products,
                multiple = T,
                selected = "breakfast bag")
  })
  
  output$selectedProductsLs <- renderUI({
    k <- potral_datasets()$product_data
    selectInput("selectedProductsLs", 
                "Select Product(s):", 
                choices = unique(k$Product_name),# unique_products,
                multiple = T,
                selected = "breakfast bag")
  })
  
  output$selectedProductsT <- renderUI({
    k <- potral_datasets()$product_data
    selectInput("selectedProductsT", 
                "Select Product(s):", 
                choices = unique(k$Product_name),# unique_products,
                multiple = T,
                selected = "breakfast bag")
  })
  
  output$selectedProduct_group1 <- renderUI({
    k <- potral_datasets()$product_data
    selectInput("selectedProduct_group1", 
                "Select Group 1:", 
                choices = unique(k$Product_name),# unique_products,
                multiple = T,
                selected = "breakfast bag")
  })
  
  output$selectedProduct_group2 <- renderUI({
    k <- potral_datasets()$product_data
    selectInput("selectedProduct_group2", 
                "Select Group 2:", 
                choices = unique(k$Product_name),# unique_products,
                multiple = T,
                selected = "meat bag")
  })
  
  output$year <- renderUI({
    k <- potral_datasets()$product_data
    selectInput(inputId = "year", "Select Year:",
                choices = unique(k$Year),
                selected = unique(k$Year)[1])
  })
  
  output$selectedCustomersL <- renderUI({
    k <- potral_datasets()$fully_seperated
    selectInput("selectedCustomersL", 
                "Select customer(s):", 
                choices = unique(k$Customer.ID),# unique_products,
                multiple = T,
                selected = unique(k$Customer.ID)[1])
  })
  
  output$selectedCustomersLs <- renderUI({
    k <- potral_datasets()$fully_seperated
    selectInput("selectedCustomersLs", 
                "Select customer(s):", 
                choices = "",#unique(k$Customer.ID),# unique_products,
                multiple = T,
                selected = "")#unique(k$Customer.ID)[1])
  })
  
  output$selectedCustomersT <- renderUI({
    k <- potral_datasets()$fully_seperated
    selectInput("selectedCustomersT", 
                "Select customer(s):", 
                choices = sort(unique(k$Customer.ID)),# unique_products,
                multiple = T,
                selected = unique(k$Customer.ID)[1])
  })

  output$zipcode <- renderUI({
    k <- potral_datasets()$fully_seperated
    selectInput("zipcode", 
                "Select Zip code(s):", 
                choices = sort(unique(k$Zip_Code)),# unique_products,
                multiple = T,
                selected = unique(k$Zip_Code)[1])
  })
  
  output$zipcode_group <- renderUI({
    k <- potral_datasets()$fully_seperated
    selectInput("zipcode_group", 
                "Select Zip code(s):", 
                choices = c("All Zip Codes", sort(unique(k$Zip_Code))),# unique_products,
                multiple = T,
                selected = "All Zip Codes")
  })
  
  output$zipcodeLS <- renderUI({
    k <- potral_datasets()$fully_seperated
    selectInput("zipcodeLS", 
                "Select Zip code(s):", 
                choices = c("All_codes", sort(unique(k$Zip_Code))),# unique_products,
                multiple = T,
                selected = "All_codes")
  })
  
  dates <- reactive({
    # data = main_prod_data %>%
    #   filter(as.Date(month_dates) == as.Date(input$dates))

    data <- potral_datasets()$main_prod_data %>%
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
           Revenue = potral_datasets()$main_prod_data[['total_price']],
           Total_number_order = potral_datasets()$main_prod_data[['total_order']]
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
    
    # rdata = reactive_data()
    # if(max(reactive_stat())==0){
    #   colorNumeric(palette = color_pal, domain = 0.01:10000)
    # }else{
    #   colorNumeric(palette = color_pal, domain = rdata+.001)
    # }

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
          "<br /> Top_Products: ", ifelse(is.na(dates()$top_products_names), "(NA)", dates()$top_products_names),
          "<br /> Top_Customers: ", ifelse(is.na(dates()$top_customers), "(NA)", dates()$top_customers)
          )
  })

  layer_zip <- reactive({
    unique(potral_datasets()$main_prod_data[['Zip_Code']])
  })

  output$map_pop <- renderLeaflet({
    if(input$create_map){
      req(pal_data())
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
        setView(lat = 38.012181, lng = -87.083593,  zoom = 8.4) %>% # 38.018259, -87.083593 (87.530370)   41.550835, -86.897873
        addPolygons(data = st_transform(filtered_data, crs = "+init=epsg:4326"),#st_transform(states_map2, crs = "+init=epsg:4326"),
                    #group = "Zip_Code",
                    color = "black",
                    fill = FALSE,
                    weight = 3) %>%
        addPolygons(data = st_transform(filtered_data, crs = "+init=epsg:4326"), #filter(data_new4, YEAR == 2002),#
                    layerId = layer_zip(),
                    color = "white",
                    weight = 1,
                    smoothFactor = 0,
                    fillOpacity = 0.7)
    }
  })

  observe({
    if(input$create_map){
      leafletProxy("map_pop", data = dates()) %>%
        setShapeStyle(layerId = layer_zip(),
                      fillColor = ~ suppressWarnings(pal_data()(reactive_stat())))
    }
  })

  observe({
    if(input$create_map){
      leafletProxy("map_pop", data = dates()) %>%
        setShapeLabel(layerId = layer_zip(),
                      label = popup_msg())
    }
  })

  observe({
    if(input$create_map){
      leafletProxy("map_pop") %>%
        clearControls() %>%
        addLegend("bottomleft",
                  pal = pal_data(),
                  values = na.omit(reactive_data()),
                  title = str_to_title(str_replace_all(input$parameter, "_", " ")),
                  na.label = "",
                  opacity = 5)
    }
  })


  observeEvent(input$map_pop_shape_click, {
    GRIDrv(input$map_pop_shape_click$id)
  })

  zip_data <- reactive({
    validate(
      need(GRIDrv() != "", "Please select a zip code area to generate analyses.")
    )

    DData = potral_datasets()$main_prod_data %>%
      subset(Zip_Code == GRIDrv())
    DData$total_order[is.na(DData$total_order)] <- 0
    DData
  })

  # output$tab1 <- DT::renderDataTable({
  #   head(zip_data())
  # })


  # 
  # 
  
  
  
  ######################################################
  ################################################
  ################################################
  
  
  # 
  # 
  # 
  # Render plot
  # output$productPlot <- renderPlot({
  # 
  #   filtered_data <- MonthGRAPH() %>%
  #     filter(Product_name %in% input$selectedProducts)
  #     #filter(ProductID %in% input$selectedProducts)
  #   filtered_data$Month <- factor(filtered_data$Month, levels = unique(filtered_data$Month))
  #   ggplot(filtered_data, aes(x = Month, y = TotalCount, fill = Product_name)) + #ProductID)) +
  #     geom_bar(stat = "identity", position = "dodge") +
  #     labs(title = "Products Throughout The Months",
  #          x = "Month",
  #          y = "Number of Product Sold") +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # })

  # #unique_products <- unique(MonthGRAPH$ProductID)
  # 
  # # output$productSelect <- renderUI({
  # #   checkboxGroupInput("selectedProducts",
  # #                      "Select Products:",
  # #                      choices = unique(MonthGRAPH$Product_name),# unique_products,
  # #                      selected = "breakfast bag")
  # # })
  # 
  
  
  plot_data_all <- reactive({
    kk <- potral_datasets()$product_data
    kk$ProductTotalPrice = as.numeric(kk$ProductTotalPrice)

    df <- kk %>%
      select(ProductID, Year, Month, ProductTotalPrice) %>%
      mutate(month_date = as.Date(paste0("01-", Month, "-", Year), format = "%d-%b-%Y")) %>%
      arrange(month_date) %>%
      mutate(month_year = format(month_date, "%B_%Y")) %>%
      group_by(month_date) %>%
      summarise(total_price = sum(ProductTotalPrice, na.rm = TRUE),
                total_order = n(),
                #ratio = total_price/total_order
                )

    df$month_year <- format(df$month_date, "%B %Y")
    df$month_year <- factor(df$month_year, levels = unique(df$month_year[order(df$month_date)]))

    df

  })

  plot_data_one <- reactive({
    kk <- potral_datasets()$product_data
    kk$ProductTotalPrice = as.numeric(kk$ProductTotalPrice)

    df <- kk %>%
      filter(Year == as.numeric(input$year)) %>%
      select(ProductID, Year, Month, ProductTotalPrice) %>%
      mutate(month_date = as.Date(paste0("01-", Month, "-", Year), format = "%d-%b-%Y")) %>%
      arrange(month_date) %>%
      mutate(month_year = format(month_date, "%B_%Y")) %>%
      group_by(month_date) %>%
      summarise(total_price = sum(ProductTotalPrice, na.rm = TRUE),
                total_order = n(),
                #ratio = total_price/total_order
                )

    df$month_year <- format(df$month_date, "%B %Y")
    df$month_year <- factor(df$month_year, levels = unique(df$month_year[order(df$month_date)]))

    df
  })


  # output$scatter_all_years <- renderPlot({
  #   df = plot_data_all()
  # 
  #   scale_factor <- max(df$total_price) / max(df$total_order)
  # 
  #   # Scatter plot with lines connecting points
  #   ggplot(df, aes(x = month_year)) +
  #     # Points for total_order
  #     geom_point(aes(y = total_order), color = "blue") +
  #     geom_line(aes(y = total_order, group = 1), color = "blue") +  # Line for total_order
  # 
  #     # Points for Revenue (scaled)
  #     geom_point(aes(y = total_price / scale_factor), color = "red") +
  #     geom_line(aes(y = total_price / scale_factor, group = 1), color = "red") +  # Line for Revenue
  #     #geom_point(aes(y = total_price ), color = "red") +
  #     #geom_line(aes(y = total_price, group = 1), color = "red") +  # Line for Revenue
  #     # Scale the y-axis for dual y-axes
  #     scale_y_continuous(
  #       name = "Number of Product Sold",
  #       sec.axis = sec_axis(~ . * scale_factor, name = "Scaled Revenue")
  #     ) +
  # 
  #     # Add labels and titles
  #     labs(title = "Scatter Plot of Total Orders and Scaled Revenue Over Time",
  #          x = "Months") +
  # 
  #     # Customize axis titles
  #     theme(axis.title.y = element_text(color = "blue"),
  #           axis.title.y.right = element_text(color = "red"))+
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # 
  # 
  # })
  
  ################################################
  output$productPlot <- renderPlot({
    DD = potral_datasets()$product_data
      
    month_names <- c("January", "February", "March", "April", "May", "June", 
                     "July", "August", "September", "October", "November", "December")
    
    # Convert to numeric values (1 to 12)
    DD$Month_num <- match(DD$Month, month.name)
    
    filtered_data <-  DD %>% 
      filter(Product_name %in% input$selectedProductsL) %>% 
      group_by(Product_name,Month_num, Year) %>% 
      summarise(TC = n())
    
    
    ggplot(filtered_data, aes(x = Month_num, y = TC, color = Product_name)) +
      facet_wrap(~ Year) +
      geom_line(size = 3, alpha = 0.7) +
      scale_color_manual(values = c("breakfast bag" = "#C9BFFA", "fresh vegetable bag" = "#05B58F", "fresh fruit bag" = "#54038A", "grain/bean bag" = "#D4DE66", "flavor box" = "#0D5754", "dairy/egg bag" = "#F56E0A", "meat bag" = "deeppink4", "mexican dinner bag" = "lightgreen", "italian dinner bag" = "lightpink", "holiday dinner box" = "lightseagreen", "russet potatoes (4 each)" = "olivedrab3", "local picks bag" = "mediumaquamarine", "fresh vegetable with picks" = "orchid3", "meat bag (beef)" = "purple2", "meat bag (pork)" = "orange2", "holiday bag" = "darkolivegreen1" )) +
      labs(title = "Products Throughout The Months",
           subtitle = "select products of interest", 
           x = "Month",
           y = "Number of Product Sold",
           color = "Product") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_x_continuous(breaks = 1:12, labels = month.name) 
  })
  
  
  ##Under Location -> summary tab
  output$ZipPlot <- renderPlot({
    
    filtered_data2 <- potral_datasets()$product_data %>% 
      filter(Product_name %in% input$selectedProductsL) %>% 
      group_by(Product_name,Zip_Code, Year) %>% 
      summarise(TC = n())
    
    ggplot(filtered_data2, aes(x = Zip_Code, y = TC, fill = Product_name)) +
      facet_wrap(~ Year) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("#C9BFFA", "#05B58F", "#54038A", "#D4DE66", "#0D5754", "#F56E0A", "deeppink4", "lightgreen", "lightpink", "lightseagreen", "olivedrab3", "mediumaquamarine", "orchid3", "purple2", "orange2", "darkolivegreen1" )) +
      labs(title = "Products For Each Zip Code",
           subtitle = "select products of interest",
           x = "Zip Code",
           y = "Number of Product Sold") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
  })
  
  output$ZipPlot2 <- renderPlot({
    
    filtered_data3 <- potral_datasets()$product_data %>% 
      filter(Product_name %in% input$selectedProductsL, Zip_Code %in% input$zipcode) %>% 
      group_by(Product_name,Zip_Code) %>% 
      summarise(TC = n())
    
    ggplot(filtered_data3, aes(x = Zip_Code, y = TC, fill = Product_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("#C9BFFA", "#05B58F", "#54038A", "#D4DE66", "#0D5754", "#F56E0A", "deeppink4", "lightgreen", "lightpink", "lightseagreen", "olivedrab3", "mediumaquamarine", "orchid3", "purple2", "orange2", "darkolivegreen1" )) +
      labs(title = "Products For Each Zip Code",
           subtitle = "select products and zip codes of interest",
           x = "Zip Code",
           y = "Number of Product Sold") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
  })
  
  #selectedProductsLs
  #zipcodeLS
  
  price_change_data <- reactive({
    #require()
    kk <- potral_datasets()$product_data
    kk$ProductTotalPrice = as.numeric(kk$ProductTotalPrice)
    if (!"All_codes" %in% input$zipcodeLS){
      kk = kk %>% filter(Zip_Code %in% input$zipcodeLS)
    }
    
    df <- kk %>%
      select(Product_name, ProductQty,ProductUnitPrice, ProductTotalPrice) %>%
      filter(Product_name %in% input$selectedProductsLs) %>% 
      group_by(Product_name,ProductUnitPrice) %>%
      summarise(total_price = sum(ProductTotalPrice, na.rm = TRUE),
                total_order = sum(as.numeric(ProductQty),na.rm = T)
                #ratio = total_price/total_order
      )
    
    return(df)
    
  })
  
  output$price_change <- renderPlot({
    req(price_change_data())
    
    df = price_change_data()
    
    scale_factor <- max(df$total_price) / max(df$total_order)
    
    # Scatter plot with lines connecting points
    ggplot(df, aes(x = ProductUnitPrice)) +
      # Points for total_order
      geom_point(aes(y = total_order), color = "#05B58F", size = 2) +
      geom_line(aes(y = total_order, group = 1), color = "#05B58F", linewidth = 1) +  # Line for total_order
      
      # Points for Revenue (scaled)
      geom_point(aes(y = total_price / scale_factor), color = "#54038A", size = 2) +
      geom_line(aes(y = total_price / scale_factor, group = 1), color = "#54038A", linewidth = 1) +  # Line for Revenue
      #geom_point(aes(y = total_price ), color = "red") +
      #geom_line(aes(y = total_price, group = 1), color = "red") +  # Line for Revenue
      # Scale the y-axis for dual y-axes
      scale_y_continuous(
        name = "Number of Product Sold",
        sec.axis = sec_axis(~ . * scale_factor, name = "Scaled Revenue")
      ) +
      
      # Add labels and titles
      labs(title = "Scatter Plot of Total Orders and Scaled Revenue Over Time",
           subtitle = "select only one product",
           x = "Product unit prices") +
      
      # Customize axis titles
      theme(axis.title.y = element_text(color = "#05B58F"),
            axis.title.y.right = element_text(color = "#54038A"))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  
  output$price_change2 <- renderPlot({
    req(price_change_data())
    
    df = price_change_data()
    ggplot(df, aes(x = Product_name, y = total_price)) +
      geom_bar(stat = "identity", position = "dodge", fill = "#54038A") +
      labs(title = "Products For Each Zip Code",
           subtitle = "select zip code and products of interest",
           x = "Products",
           y = "Revenue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$price_change3 <- renderPlot({
    req(price_change_data())
    
    df = price_change_data()
    ggplot(df, aes(x = Product_name, y = total_order)) +
      geom_bar(stat = "identity", position = "dodge", fill = "#05B58F") +
      labs(title = "Products For Each Zip Code",
           subtitle = "select zip code and products of interest",
           x = "Products",
           y = "Number of Product Sold") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  TS_data <- reactive({
    
    pdts_separated <- potral_datasets()$fully_seperated
    
    aggregated_data <- pdts_separated %>%
      group_by(Customer.ID) %>%
      summarise(OrderDate = (Order.Date), ZipCode = (Zip_Code), .groups = 'drop')
    
    date <- as.Date(aggregated_data$OrderDate, format = "%d/%m/%Y") 
    Month <- as.numeric(format(date, "%m"))
    Year <- as.numeric(format(date, "%Y"))
    data.month <- cbind(aggregated_data, date, Month, Year)
    
    data.zip_grouped <- data.month %>%
      group_by(Customer.ID = as.factor(Customer.ID), Year, ZipCode) %>%
      summarise(TotalCount = n(), .groups = 'drop') %>%
      ungroup()
    
    pdt_zip_filtered <- data.zip_grouped %>% 
      filter(!is.na(Customer.ID), !is.na(ZipCode), !is.na(Year))
    
  })
  
  output$yearSelect <- renderUI({
    pdt_zip_filtered = TS_data()
    selectInput("selectedyear",
                "Select Year", 
                choices = unique(pdt_zip_filtered$Year),
                selected = unique(pdt_zip_filtered$Year)[1])  # Default to first year
  })
  
  # output$customerPlot <- renderPlotly({
  #   
  #   req(input$selectedyear)
  #   
  #   pdt_zip_filtered = TS_data()
  #   filtered_data <- pdt_zip_filtered %>%
  #     filter(Year == input$selectedyear)
  #   
  #   # Ensure there's data to plot
  #   if (nrow(filtered_data) == 0) {
  #     return(NULL)
  #   }
  #   
  #   total_count <- sum(filtered_data$TotalCount, na.rm = TRUE)
  #   
  #   pie_data <- filtered_data %>%
  #     mutate(Percentage = (TotalCount / total_count) * 100,
  #            Label = paste(ZipCode, "<br>", round(Percentage, 1), "%"))  # Updated label
  #   
  #   # Create the pie chart
  #   p <- plot_ly(pie_data, labels = ~Label, values = ~TotalCount, type = 'pie', 
  #                textinfo = 'text',  # Change to 'text' to only show custom hover text
  #                text = ~Label,      # Use the custom label for hover text
  #                insidetextorientation = 'radial') %>%
  #     layout(title = paste("Customer Distribution by Zip Code in", input$selectedyear),
  #            showlegend = TRUE)
  #   
  #   p
  # })
  
  ###############################################

  # output$ratio_all_years <- renderPlot({
  #   df = plot_data_all()
  # 
  #   ggplot(data = df)+
  #     geom_point(aes(x = month_year, y = ratio))+
  #     geom_line(aes(x = month_year, y = ratio, group = 1)) +
  #     labs(title = "Ratio of Revenue and Number of Orders throughout Years",
  #          x = "Months",
  #          y = "Ratio") +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # 
  # })
  # 
  # output$prod_all_years <- renderPlot({
  #   ddata <- plot_data_all()
  # 
  #   ggplot(ddata) +
  #     geom_bar(aes(x = month_year, y = total_order), stat = 'identity') +
  #     labs(title = "Total Number of Orders throughout Years",
  #          x = "Months",
  #          y = "Number of orders") +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # 
  # })
  # 
  # output$price_all_years <- renderPlot({
  #   ddata <- plot_data_all()
  # 
  #   ggplot(ddata) +
  #     geom_bar(aes(x = month_year, y = total_price), stat = 'identity') +
  #     labs(title = "Revenue throughout Years",
  #          x = "Months",
  #          y = "Revenue") +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # 
  # })
  # 
  # 
  # output$scatter_year <- renderPlot({
  #   df = plot_data_one()
  # 
  #   scale_factor <- max(df$total_price) / max(df$total_order)
  # 
  #   # Scatter plot with lines connecting points
  #   ggplot(df, aes(x = month_year)) +
  #     # Points for total_order
  #     geom_point(aes(y = total_order), color = "blue") +
  #     geom_line(aes(y = total_order, group = 1), color = "blue") +  # Line for total_order
  # 
  #     # Points for Revenue (scaled)
  #     geom_point(aes(y = total_price / scale_factor), color = "red") +
  #     geom_line(aes(y = total_price / scale_factor, group = 1), color = "red") +  # Line for Revenue
  # 
  #     # Scale the y-axis for dual y-axes
  #     scale_y_continuous(
  #       name = "Total Orders",
  #       sec.axis = sec_axis(~ . * scale_factor, name = "Scaled Revenue")
  #     ) +
  # 
  #     # Add labels and titles
  #     labs(title = "Scatter Plot of Total Orders and scaled Revenue over Selected Year",
  #          x = "Months") +
  # 
  #     # Customize axis titles
  #     theme(axis.title.y = element_text(color = "blue"),
  #           axis.title.y.right = element_text(color = "red"))+
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # 
  # 
  # })
  # 
  # output$ratio_year <- renderPlot({
  #   df = plot_data_one()
  # 
  #   ggplot(data = df)+
  #     geom_point(aes(x = month_year, y = ratio))+
  #     geom_line(aes(x = month_year, y = ratio, group = 1)) +
  #     labs(title = "Ratio of Revenue and Number of Orders over Selected Year",
  #          x = "Months",
  #          y = "Ratio") +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # 
  # })
  # 
  # output$prod_year <- renderPlot({
  #   ddata <- plot_data_one()
  # 
  #   ggplot(ddata) +
  #     geom_bar(aes(x = month_year, y = total_order), stat = 'identity') +
  #     labs(title = "Total Number of Orders throughout the Selected Year",
  #          x = "Months",
  #          y = "Number of orders") +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # 
  # })
  # 
  # output$price_year <- renderPlot({
  #   ddata <- plot_data_one()
  # 
  #   ggplot(ddata) +
  #     geom_bar(aes(x = month_year, y = total_price), stat = 'identity') +
  #     labs(title = "Revenue throughout the Selected Year",
  #          x = "Months",
  #          y = "Revenue") +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # 
  # })

  ################ Plots for Zip codes

  output$scatter_zip <- renderPlot({
    req(input$zipcodeLS)
    
    ## price change data:
    dff = potral_datasets()$product_data %>%
      select(Product_name,ProductQty, Year, Month, ProductTotalPrice) %>%
      filter(Product_name %in% input$selectedProductsLs) %>% 
      mutate(month_date = as.Date(paste0("01-", Month, "-", Year), format = "%d-%b-%Y"),
             price = as.numeric(ProductTotalPrice) / as.numeric(ProductQty)) %>%
      arrange(month_date) %>% 
      select(month_date, price) %>% 
      distinct()
    
    dff <- dff %>%
      distinct(month_date, .keep_all = TRUE)
    
    price_changes <- dff %>%
      mutate(price_change = price != lag(price)) %>%  # Compare with previous row
      filter(price_change | is.na(price_change)) %>%  # Keep rows where price changes or the first row
      select(-price_change)
    
    if( "All_codes" %in% input$zipcodeLS){
      df <- potral_datasets()$product_data %>%
        select(Product_name, Year, Month, ProductQty, ProductTotalPrice) %>%
        filter(Product_name %in% input$selectedProductsLs) %>% 
        mutate(month_date = as.Date(paste0("01-", Month, "-", Year), format = "%d-%b-%Y")) %>%
        arrange(month_date) %>%
        #mutate(month_year = format(month_date, "%B_%Y")) %>%
        group_by(month_date) %>%
        summarise(total_price = sum(as.numeric(ProductTotalPrice), na.rm = TRUE),
                  total_order = sum(as.numeric(ProductQty),na.rm = T)
                  #ratio = total_price/total_order
        )
      
      df$month_year <- format(df$month_date, "%B %Y")
      df$month_year <- factor(df$month_year, levels = unique(df$month_year[order(df$month_date)]))
      
    }else{
      df = potral_datasets()$product_data %>%
        filter(Zip_Code %in% input$zipcodeLS) %>% 
        select(Product_name, Year, Month, ProductQty, ProductTotalPrice) %>%
        filter(Product_name %in% input$selectedProductsLs) %>%
        mutate(month_date = as.Date(paste0("01-", Month, "-", Year), format = "%d-%b-%Y")) %>%
        arrange(month_date) %>%
        #mutate(month_year = format(month_date, "%B_%Y")) %>%
        group_by(month_date) %>%
        summarise(total_price = sum(as.numeric(ProductTotalPrice), na.rm = TRUE),
                  total_order = sum(as.numeric(ProductQty),na.rm = T)
                  #ratio = total_price/total_order
        )
      df$month_year <- format(df$month_date, "%B %Y")
      df$month_year <- factor(df$month_year, levels = unique(df$month_year[order(df$month_date)]))
      df$total_order[is.na(df$total_order)] <- 0
    }
    

    
    df$month_date <- as.Date(df$month_date)
    price_changes$month_date <- as.Date(price_changes$month_date)
    
    scale_factor <- max(df$total_price, na.rm = TRUE) / max(df$total_order, na.rm = TRUE)
    
    ggplot(df, aes(x = month_date)) +
      # Line and points for total_order
      geom_line(aes(y = total_order), color = "#05B58F", linewidth = 1) +
      geom_point(aes(y = total_order), color = "#05B58F", size = 2) +
      
      # Line and points for total_price (scaled)
      geom_line(aes(y = total_price / scale_factor), color = "#54038A", linewidth = 1) +
      geom_point(aes(y = total_price / scale_factor), color = "#54038A", size = 2) +
      
      # Add vertical dashed lines for price changes
      geom_vline(
        data = price_changes,
        aes(xintercept = month_date),  # `month_date` must be a Date object
        linetype = "dashed",
        color = "black"
      ) +
      
      # Add labels for price changes
      geom_text(
        data = price_changes,
        aes(
          x = month_date,
          y = max(df$total_price / scale_factor) * 0.8,  # Adjust label position
          label = paste0("$", round(price, 2))
        ),
        angle = 45,  # Rotate the text vertically
        hjust = -0.2,  # Adjust alignment
        color = "black",
        size = 5  # Enlarge the font size of the price labels
      ) +
      
      # Scale the y-axis for dual y-axes
      scale_y_continuous(
        name = "Number of Products Sold",
        sec.axis = sec_axis(~ . * scale_factor, name = "Scaled Revenue")
      ) +
      
      # Customize x-axis to label all `month_date`
      scale_x_date(
        breaks = df$month_date,
        labels = format(df$month_date, "%b %Y"),  # Format as "Month Year"
        expand = expansion(mult = c(0.02, 0.02))  # Add slight padding
      ) +
      
      # Add titles and labels
      labs(
        title = "Monthly Total Orders and Scaled Revenue with Price Changes",
        x = "Month",
        y = "Number of Products Sold"
      ) +
      
      # Customize the theme
      theme_minimal() +
      theme(
        axis.title.y = element_text(color = "#05B58F"),
        axis.title.y.right = element_text(color = "#54038A"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels and enlarge font
        plot.title = element_text(hjust = 0.5, size = 14)  # Center the title and enlarge
      ) 
    
    
    
    # # Scatter plot with lines connecting points
    # ggplot(df, aes(x = month_year)) +
    #   # Points for total_order
    #   geom_point(aes(y = total_order), color = "blue") +
    #   geom_line(aes(y = total_order, group = 1), color = "blue") +  # Line for total_order
    # 
    #   # Points for Revenue (scaled)
    #   geom_point(aes(y = total_price / scale_factor), color = "red") +
    #   geom_line(aes(y = total_price / scale_factor, group = 1), color = "red") +  # Line for Revenue
    # 
    #   # Scale the y-axis for dual y-axes
    #   scale_y_continuous(
    #     name = "Number of Product Sold",
    #     sec.axis = sec_axis(~ . * scale_factor, name = "Scaled Revenue")
    #   ) +
    # 
    #   # Add labels and titles
    #   labs(title = "Scatter Plot of Total Orders and scaled Revenue over Years in the Selected Zip-Code",
    #        x = "Months") +
    # 
    #   # Customize axis titles
    #   theme(axis.title.y = element_text(color = "blue"),
    #         axis.title.y.right = element_text(color = "red"))+
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1))


  })

  # output$ratio_zip <- renderPlot({
  #   df = zip_data()
  # 
  #   df$month_year <- format(df$month_dates, "%B %Y")
  #   df$month_year <- factor(df$month_year, levels = unique(df$month_year[order(df$month_dates)]))
  #   df$ratio <- df$total_price / df$total_order
  # 
  #   ggplot(data = df)+
  #     geom_point(aes(x = month_year, y = ratio))+
  #     geom_line(aes(x = month_year, y = ratio, group = 1)) +
  #     labs(title = "Ratio of Revenue and Number of Orders over Years in the Selected Zip-Code",
  #          x = "Months",
  #          y = "Ratio") +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # 
  # })
  # 
  # output$prod_zip <- renderPlot({
  #   df = zip_data()
  # 
  #   df$month_year <- format(df$month_dates, "%B %Y")
  #   df$month_year <- factor(df$month_year, levels = unique(df$month_year[order(df$month_dates)]))
  # 
  #   ddata = df
  #   ggplot(ddata) +
  #     geom_bar(aes(x = month_year, y = total_order), stat = 'identity') +
  #     labs(title = "Total Number of Orders throughout the Years in the Selected Zip-Code",
  #          x = "Months",
  #          y = "Number of orders") +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # 
  # })
  # 
  # output$price_zip <- renderPlot({
  #   df = zip_data()
  # 
  #   df$month_year <- format(df$month_dates, "%B %Y")
  #   df$month_year <- factor(df$month_year, levels = unique(df$month_year[order(df$month_dates)]))
  # 
  #   ddata = df
  #   ggplot(ddata) +
  #     geom_bar(aes(x = month_year, y = total_price), stat = 'identity') +
  #     labs(title = "Revenue throughout the Years in the Selected Zip-Code",
  #          x = "Months",
  #          y = "Revenue") +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # 
  # })
  
  output$pieChart1 <- renderPlot({
    filtered_data2 <- potral_datasets()$product_data %>%
      filter(Zip_Code %in% input$zipcode, Year %in% input$year_pie) %>%
      group_by(Year, Product_name) %>%
      summarise(TotalCount = n()) %>%
      ungroup() %>%
      group_by(Year) %>%
      mutate(Percentage = TotalCount / sum(TotalCount) * 100)
    
    ggplot(filtered_data2, aes(x = "", y = TotalCount, fill = Product_name)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      scale_fill_manual(values = c("#C9BFFA", "#05B58F", "#54038A", "#D4DE66", "#0D5754", "#F56E0A", "deeppink4", "lightgreen", "lightpink", "lightseagreen", "olivedrab3", "mediumaquamarine", "orchid3", "purple2", "orange2", "darkolivegreen1" )) +
      labs(title = "Percentage of Products Sold in the selected Zip Code(s) and Year(s):",
           subtitle = "select only one zip code and one year",
           fill = "Product Name") +
      theme_void() +
      geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                position = position_stack(vjust = 0.5))
  })
  
  # output$revenue_zip <- renderTable({
  #   req(zip_data())
  #   DD <- potral_datasets()$product_data %>%
  #     filter(Zip_Code == GRIDrv()) %>%
  #     group_by(Year, ProductTotalPrice) %>%
  #     group_by(Year) %>%
  #     summarise(Revenue = sum(as.numeric(ProductTotalPrice)))
  #   as.data.frame(DD)
  # })
  
  output$revenue_zip <- renderTable({
    DD <- revenue_coupon()%>% 
      filter(Zip_Code == GRIDrv()) %>%
      select(Year, Subtotal..inc.tax., Order.Total..inc.tax.) %>% 
      group_by(Year) %>% 
      summarise(revenue = sum(as.numeric(Order.Total..inc.tax.), na.rm = T))
    
    # DD <- potral_datasets()$product_data %>%
    #   dplyr::select(Year, ProductTotalPrice) %>%
    #   group_by(Year) %>%
    #   summarise(Revenue = sum(as.numeric(ProductTotalPrice)))
    as.data.frame(DD)
  })
  
  output$totalcoupon_zip <- renderTable({
    DD <- revenue_coupon()%>% 
      filter(Zip_Code == GRIDrv()) %>%
      select(Year, Subtotal..inc.tax., Order.Total..inc.tax.) %>% 
      group_by(Year) %>% 
      summarise(coupon = sum(as.numeric(Subtotal..inc.tax.), na.rm = T) - 
                  sum(as.numeric(Order.Total..inc.tax.), na.rm = T))
    
    # DD <- potral_datasets()$product_data %>%
    #   dplyr::select(Year, ProductTotalPrice) %>%
    #   group_by(Year) %>%
    #   summarise(Revenue = sum(as.numeric(ProductTotalPrice)))
    as.data.frame(DD)
  })
  
  
  output$orders_zip <- renderTable({
    req(zip_data())
    D <- df_main()
    D$Order.Date <- as.Date(D$Order.Date, format = "%d/%m/%Y")
    
    DDD <- D %>% #potral_datasets()$fully_seperated %>% 
      filter(Billing.Zip == GRIDrv()) %>%#filter(Zip_Code == GRIDrv()) %>% 
      mutate(Year = format(Order.Date, "%Y")) %>% 
      select(Year, Order.ID) %>% 
      group_by(Year) %>% 
      summarise(Total_Orders = n())
    as.data.frame(DDD)
  })
  
  output$customers_zip <- renderTable({
    req(zip_data())
    # D <- df_main()
    # D$Order.Date <- as.Date(D$Order.Date, format = "%d/%m/%Y")
    
    DDD <- potral_datasets()$fully_seperated %>% 
      filter(Zip_Code == GRIDrv()) %>%
      mutate(Year = format(Order.Date, "%Y")) %>% 
      select(Year, Customer.ID) %>% 
      distinct() %>% 
      group_by(Year) %>% 
      summarise(Total_Customers = n())
    as.data.frame(DDD)
  })
  
  output$average_value_zip <- renderTable({
    req(zip_data())
    # D <- df_main()
    # D$Order.Date <- as.Date(D$Order.Date, format = "%d/%m/%Y")
    
    DDD <- potral_datasets()$fully_seperated %>% 
      filter(Zip_Code == GRIDrv()) %>%
      mutate(Year = format(Order.Date, "%Y")) %>% 
      select(Year, ProductUnitPrice) %>% 
      group_by(Year) %>% 
      summarise(Average_Order_Value = mean(as.numeric(ProductUnitPrice)))
    as.data.frame(DDD)
  })
  
  
  customer_data <- reactive({
    # Preprocess data and aggregate
    aggregated_data <- potral_datasets()$fully_seperated %>%
      group_by(Customer.ID) %>%
      summarise(OrderDate = (Order.Date), ZipCode = (Zip_Code), .groups = 'drop')
    
    date <- as.Date(aggregated_data$OrderDate, format = "%d/%m/%Y") 
    Month <- as.numeric(format(date, "%m"))
    Year <- as.numeric(format(date, "%Y"))
    data.month <- cbind(aggregated_data, date, Month, Year)
    
    # Group by Year and ZipCode
    data.zip_grouped <- data.month %>%
      group_by(Customer.ID = as.factor(Customer.ID), Year, ZipCode) %>%
      summarise(TotalCount = n(), .groups = 'drop') %>%
      ungroup()
    
    # Filter out missing values
    pdt_zip_filtered <- data.zip_grouped %>% 
      filter(!is.na(Customer.ID), !is.na(ZipCode), !is.na(Year))
    
    pdt_zip_filtered
    
  })
  
  output$customerPlot <- renderPlotly({
    req(input$yearRange)
    # Get filtered data
    data <- customer_data() %>%
      filter(Year == input$yearRange)
    
    # Ensure there's data to plot
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    total_count <- sum(data$TotalCount, na.rm = TRUE)
    
    # Create a data frame with labels and percentages
    pie_data <- data %>%
      mutate(Percentage = (TotalCount / total_count) * 100,
             Label = paste(ZipCode, "<br>", round(Percentage, 1), "%"))  # Updated label
    
    # Filter out zip codes with 0 customers
    pie_data <- pie_data %>% filter(TotalCount > 0)
    
    # Create the pie chart using plotly without showing the labels on the pie itself
    p <- plot_ly(pie_data, labels = ~Label, values = ~TotalCount, type = 'pie', 
                 textinfo = 'none',  # Hide text labels on the pie
                 hoverinfo = 'text', # Only show hover text when hovering
                 text = ~Label,      # Use the custom label for hover text
                 insidetextorientation = 'radial') %>%
      layout(title = paste("Customer Distribution by Zip Code in", 
                           input$yearRange),  # Format year without commas
             showlegend = TRUE)
    
    p
  })
  
  output$productPlot_group <- renderPlot({
    #req(input$selectedProduct_group1, input$selectedProduct_group2)
    if(input$group_plot){
      if("All Zip Codes" %in% input$zipcode_group){
        DD = potral_datasets()$product_data
      }else{
        
        DD = potral_datasets()$product_data %>% 
          filter(Zip_Code %in% input$zipcode_group)
      }
      
      month_names <- c("January", "February", "March", "April", "May", "June", 
                       "July", "August", "September", "October", "November", "December")
      
      # Convert to numeric values (1 to 12)
      DD$Month_num <- match(DD$Month, month.name)
      
      print(head(DD))
      
      
      filtered_data1 <-  DD %>% 
        filter(Product_name %in% as.character(input$selectedProduct_group1)) %>% 
        mutate(group_1 = "group1") %>% 
        group_by(Year, Month_num, group_1) %>% 
        summarise(TC = n(), .groups = "drop")

      
      filtered_data2 <-  DD %>% 
        filter(Product_name %in% as.character(input$selectedProduct_group2)) %>% 
        mutate(group_2 = "group2") %>% 
        group_by(Year, Month_num, group_2) %>% 
        summarise(TC = n(), .groups = "drop")

      filtered_data <- filtered_data1 %>% 
        left_join(filtered_data2, by = c("Year", "Month_num"))

      
      ggplot(filtered_data) +
        geom_line(aes(x = Month_num, y = TC.x, color = "Group 1"), size = 1.2, alpha = 0.7) +
        geom_line(aes(x = Month_num, y = TC.y, color = "Group 2"), size = 1.2, alpha = 0.7) +
        scale_color_manual(
          values = c(
            "Group 1" = "#D4DE66",  
            "Group 2" = "#05B58F")) +
        facet_wrap(~ Year) +
        labs(title = "Monthly Trends for total product sold by Year",
             x = "Month",
             y = "Values",
             color = "Group") +
        scale_x_continuous(breaks = 1:12, labels = month.name) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  #coupon_product
  output$coupons <- renderTable({
    
    D <- potral_datasets()$fully_seperated %>% 
      mutate(Year = year(month_dates)) %>% 
      select(Year, coupon) %>% 
      group_by(Year, coupon) %>% 
      summarise(coupons = n())
    
    D %>%
      group_by(Year, coupon) %>%
      summarise(total_coupons = sum(coupons), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = coupon, values_from = total_coupons, values_fill = 0)
    
  })
  #coupon_product
  output$coupon1 <- renderPlot({
    D <- potral_datasets()$fully_seperated %>% 
      mutate(Year = year(month_dates)) %>% 
      select(Year, coupon) %>% 
      group_by(Year, coupon) %>% 
      summarise(coupons = n())
    
    ggplot(D, aes(x = Year, y = coupons, fill = coupon)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("#05B58F", "#D4DE66", "#0D5754")) +
      labs(title = "Number of Coupons by Year and Coupon Groups",
           x = "Year",
           y = "Total Coupons",
           fill = "Coupon Groups") +
      theme_minimal()
    
  })
  
  #coupon_customer
  output$coupons_customer <- renderTable({
    
    
    D <- potral_datasets()$fully_seperated %>% 
      mutate(Year = year(month_dates)) %>% 
      select(Year,Customer.ID, coupon) %>% 
      group_by(Year, Customer.ID, coupon) %>% 
      distinct() 
    
    priority <- c("SNAP-coupon", "Other-coupon", "Non-coupon")
    
    data_aggregated <- D %>%
      group_by(Year, Customer.ID) %>%
      mutate(Coupon = factor(coupon, levels = priority)) %>%  # Factorize with priority order
      summarise(Coupon = first(sort(coupon)), .groups = "drop")  # Pick the highest-priority coupon
    
    # Create the two-way table
    two_way_table <- data_aggregated %>%
      count(Year, Coupon) %>%    # Count customers per year and coupon category
      pivot_wider(names_from = Coupon, values_from = n, values_fill = 0)  # Pivot to wide format
    
    # View the result
    two_way_table
    
  })
  
  #coupon_customer
  output$coupon1_customer <- renderPlot({
    D <- potral_datasets()$fully_seperated %>% 
      mutate(Year = year(month_dates)) %>% 
      select(Year,Customer.ID, coupon) %>% 
      group_by(Year, Customer.ID, coupon) %>% 
      distinct() 
    
    priority <- c("SNAP-coupon", "Other-coupon", "Non-coupon")
    
    data_aggregated <- D %>%
      group_by(Year, Customer.ID) %>%
      mutate(Coupon = factor(coupon, levels = priority)) %>%  # Factorize with priority order
      summarise(Coupon = first(sort(coupon)), .groups = "drop")  # Pick the highest-priority coupon
    
    # Create the two-way table
    two_way_table <- data_aggregated %>%
      count(Year, Coupon) %>%    # Count customers per year and coupon category
      pivot_wider(names_from = Coupon, values_from = n, values_fill = 0)  # Pivot to wide format
    
    data_long <- two_way_table %>%
      pivot_longer(
        cols = -Year,
        names_to = "Coupon_Type",
        values_to = "Count"
      )
    
    # Create the side-by-side bar graph
    ggplot(data_long, aes(x = factor(Year), y = Count, fill = Coupon_Type)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = c("#05B58F", "#0D5754")) +
      labs(
        title = "Coupon Usage by Year",
        x = "Year",
        y = "Number of Customers",
        fill = "Coupon Type"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels if needed
        legend.position = "top"                             # Optional: Place legend on top
      )
    
  })
  
  #coupon_order
  output$coupons_order <- renderTable({
    
    D <- potral_datasets()$fully_seperated %>% 
      mutate(Year = year(month_dates)) %>% 
      select(Year,Order.ID, coupon) %>% 
      group_by(Year, Order.ID, coupon) %>% 
      distinct() %>% 
      select(Year, coupon) %>% 
      group_by(Year, coupon) %>% 
      summarise(coupons = n())
    
    D %>%
      group_by(Year, coupon) %>%
      summarise(total_coupons = sum(coupons), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = coupon, values_from = total_coupons, values_fill = 0)
    
  })
  
  #coupon_order
  output$coupon1_order <- renderPlot({
    D <- potral_datasets()$fully_seperated %>% 
      mutate(Year = year(month_dates)) %>% 
      select(Year,Order.ID, coupon) %>% 
      group_by(Year, Order.ID, coupon) %>% 
      distinct() %>% 
      select(Year, coupon) %>% 
      group_by(Year, coupon) %>% 
      summarise(coupons = n())
    
    ggplot(D, aes(x = Year, y = coupons, fill = coupon)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("#05B58F", "#D4DE66", "#0D5754")) +
      labs(title = "Number of Coupons by Year and Coupon Groups",
           x = "Year",
           y = "Total Coupons",
           fill = "Coupon Groups") +
      theme_minimal()
    
  })
  
  
  output$product_customer_Plot <- renderPlot({
    DD = potral_datasets()$product_data
    
    month_names <- c("January", "February", "March", "April", "May", "June", 
                     "July", "August", "September", "October", "November", "December")
    
    # Convert to numeric values (1 to 12)
    DD$Month_num <- match(DD$Month, month.name)
    
    if(input$selectedGroup == "Products"){
      filtered_data <-  DD %>% 
        filter(Product_name %in% input$selectedProductsT,
               Customer.ID %in% input$selectedCustomersT) %>% 
        group_by(Customer.ID,Month_num, Year) %>% 
        summarise(TC = n())
      filtered_data$Customer.ID = as.factor(filtered_data$Customer.ID)
      
      ggplot(filtered_data, aes(x = Month_num, y = TC, color = Customer.ID)) +
        geom_line(size = 1, alpha = 0.7) +
        geom_point(size = 3) +
        facet_wrap(~ Year) +
        labs(title = "Customer(s) Purchase Throughout The Months",
             x = "Month",
             y = "Number of Product(s) purshaced by customer(s)",
             color = "Customer") +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        scale_x_continuous(breaks = 1:12, labels = month.name) 
    
      } else if(input$selectedGroup == "Customers"){
      filtered_data <-  DD %>% 
        filter(Product_name %in% input$selectedProductsT,
               Customer.ID %in% input$selectedCustomersT) %>% 
        group_by(Product_name,Month_num, Year) %>% 
        summarise(TC = n())
      
      #filtered_data$Customer.ID = as.factor(filtered_data$Customer.ID)
      
      ggplot(filtered_data, aes(x = Month_num, y = TC, group = interaction(Product_name, Year))) +
        geom_line(aes(color = Product_name, linetype = Product_name), size = 1.2, alpha = 0.7) + # Different linetypes for overlapping lines
        geom_point(aes(color = Product_name, shape = Product_name), size = 3, alpha = 0.9) + # Different shapes for points
        facet_wrap(~ Year) + # Separate facets for each year
        labs(
          title = "Products Sold by Customer(s) Throughout The Months",
          x = "Month",
          y = "Number of Products Sold by Customer(s)",
          color = "Product",
          linetype = "Product",
          shape = "Product"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1), # Improve x-axis label readability
          legend.position = "bottom" # Move legend to bottom for better spacing
        ) +
        scale_x_continuous(
          breaks = 1:12, 
          labels = month.name # Label months with full names
        )
      
      
      
      # ggplot(filtered_data, aes(x = Month_num, y = TC, color = Product_name)) +
      #   geom_line(size = 1, alpha = 0.7) +
      #   geom_point(size = 3) +
      #   facet_wrap(~ Year) +
      #   labs(title = "Ptoducts Sold by customer(s) Throughout The Months",
      #        x = "Month",
      #        y = "Number products sold by customer(s)",
      #        color = "Customer") +
      #   theme_minimal() + 
      #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      #   scale_x_continuous(breaks = 1:12, labels = month.name) 
    }
    
  })
  
  
  
}


# Run the application
shinyApp(ui = ui, server = server)


