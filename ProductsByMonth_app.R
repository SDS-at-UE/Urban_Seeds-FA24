#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(readxl)
library(dplyr) 

ui <- fluidPage(
  titlePanel("Products Throughout The Months"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("productSelect") # Placeholder for dynamic UI input
    ),
    mainPanel(
      plotOutput("productPlot")
    )
  )
)

server <- function(input, output, session) {
 
  pdts_separated <- read.csv("G:/My Drive/a.UE24-25/STAT300/separated_orders_byproduct.csv")
  aggregated_data <- pdts_separated %>%
    group_by(ProductID) %>%
    summarise(OrderDate = (Order.Date), .groups = 'drop')
  
  aggregated_data
  
  date <- as.Date(aggregated_data$OrderDate, format = "%d/%m/%Y") 
  Month <- as.numeric(format(date, "%m"))
  data.month <- cbind(aggregated_data, Month)
  data.month
  
  data.month_grouped <- data.month %>%
    group_by(ProductID, Month) %>%
    summarize(TotalCount = n()) %>%
    ungroup()
  data.month_grouped
 
  product_types <- unique(aggregated_data$ProductID)
  
  
  MonthNames <- filter(data.month_grouped, Month == c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  MonthNames <- MonthNames[, -which(names(MonthNames) == "OrderDate")]
  
  MonthNames <- filter(data.month_grouped, Month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  
  MonthNamesDistinct <- MonthNames %>%
    distinct(ProductID, TotalCount, Month)
  
  MonthNamesDistinct$Month <- factor(MonthNamesDistinct$Month, 
                                    levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                                    labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  MonthNamesDistinct$ProductID <- factor(MonthNamesDistinct$ProductID,
                                        levels = c(241, 243, 248, 255, 258, 265, 272, 289, 299, 306, 368, 369, 374, 375, 376, 377, 379),
                                        labels = c("breakfast bag", "dairy/egg bag", "fresh vegetable bag", "fresh fruit bag", "meat bag", "italian dinner bag", "mexican dinner bag", "grain/bean bag", "flavor box", "holiday dinner box", "local picks bag", "russet potatoes (4 each)", "meat bag (beef)", "meat bag (pork)", "fresh vegetable with picks", "smoked ham", "holiday bag"))
  
  MonthGRAPH <- MonthNamesDistinct %>% 
    filter(!is.na(ProductID), !is.na(Month))
  
  # Render plot
  output$productPlot <- renderPlot({
    filtered_data <- MonthGRAPH %>% 
      filter(ProductID %in% input$selectedProducts)
    
    ggplot(filtered_data, aes(x = Month, y = TotalCount, fill = ProductID)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Products Throughout The Months",
           x = "Month",
           y = "Number of Product Sold") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  unique_products <- unique(MonthGRAPH$ProductID)
  
  output$productSelect <- renderUI({
    checkboxGroupInput("selectedProducts", 
                       "Select Products:", 
                       choices = unique_products, 
                       selected = "breakfast bag")
  })
}

# Run the application
shinyApp(ui = ui, server = server)















