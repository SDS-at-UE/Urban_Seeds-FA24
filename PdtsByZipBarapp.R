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
      uiOutput("productSelect")),
    mainPanel(
      plotOutput("productPlot"))
  )
)


server <- function(input, output, session) {
  
  pdts_separated <- read.csv("C:/Users/madel/OneDrive/Documents/Urban_Seeds-FA24/separated_orders_byproduct.csv")
  aggregated_data <- pdts_separated %>%
    group_by(ProductID) %>%
    summarise(OrderDate = (Order.Date), ZipCode = (Billing.Zip), .groups = 'drop')
  
  date <- as.Date(aggregated_data$OrderDate, format = "%d/%m/%Y") 
  Month <- as.numeric(format(date, "%m"))
  Year <- as.numeric(format(date, "%Y"))
  data.month <- cbind(aggregated_data, date, Month, Year)
  data.month
  
  data.zip_grouped <- data.month %>%
    group_by(ProductID, Year, ZipCode) %>%
    summarize(TotalCount = n()) %>%
    ungroup()
  data.zip_grouped
  
  product_types <- unique(aggregated_data$ProductID)
  
  ZipDistinct <- data.zip_grouped %>%
    distinct(ProductID, TotalCount, Year, ZipCode)
  
  ZipDistinct$ProductID <- factor(ZipDistinct$ProductID,
                                         levels = c(241, 243, 248, 255, 258, 265, 272, 289, 299, 306, 368, 369, 374, 375, 376, 377, 379),
                                         labels = c("breakfast bag", "dairy/egg bag", "fresh vegetable bag", "fresh fruit bag", "meat bag", "italian dinner bag", "mexican dinner bag", "grain/bean bag", "flavor box", "holiday dinner box", "local picks bag", "russet potatoes (4 each)", "meat bag (beef)", "meat bag (pork)", "fresh vegetable with picks", "smoked ham", "holiday bag"))
 ZipDistinct$Year <- as.numeric(ZipDistinct$Year)
  
  pdt_zip_filtered <- ZipDistinct %>% 
    filter(!is.na(ProductID), !is.na(ZipCode), !is.na(Year))
  
  # Render plot
  output$productPlot <- renderPlot({
    
    filtered_data <- pdt_zip_filtered %>%
      filter(ProductID %in% input$selectedProducts)
    
    ggplot(filtered_data, aes(x = ZipCode, y = TotalCount, fill = ProductID)) +
      facet_wrap(~ Year) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Products For Each Zip Code",
           x = "Zip Code",
           y = "Number of Product Sold") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
   
  })
  
  output$productSelect <- renderUI ({
    selectInput("selectedProducts",
                "select products", 
                choices = unique(pdt_zip_filtered$ProductID),
                selected = "breakfast bag",
                multiple = TRUE)
  })
  
  
  
}
# Run the application
shinyApp(ui = ui, server = server)