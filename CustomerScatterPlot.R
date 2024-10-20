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
library(dplyr)

ui <- fluidPage(
  titlePanel("Customers Throughout The Months"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("yearSelect"),
      uiOutput("customerSelect")
    ),
    mainPanel(
      plotOutput("customerPlot")
    )
  )
)

server <- function(input, output, session) {
  
  pdts_separated <- read.csv("C:/Users/18166/Downloads/fully_separated_orders.csv")
  
  aggregated_data <- pdts_separated %>%
    group_by(Customer.ID) %>%
    summarise(OrderDate = (Order.Date), .groups = 'drop')
  
  aggregated_data$OrderDate <- as.Date(aggregated_data$OrderDate, format = "%d/%m/%Y")
  
  data.month <- aggregated_data %>%
    mutate(Month = as.numeric(format(OrderDate, "%m")),
           Year = as.numeric(format(OrderDate, "%Y")))
  
  data.month_grouped <- data.month %>%
    group_by(Customer.ID, Month, Year) %>%
    summarize(TotalCount = n(), .groups = 'drop')
  
  MonthGRAPH_filtered <- data.month_grouped %>%
    distinct(Customer.ID, TotalCount, Month, Year) %>%
    mutate(Month = factor(Month, 
                          levels = 1:12, 
                          labels = month.abb)) %>%
    filter(!is.na(Customer.ID), !is.na(Month), !is.na(Year))
  
  output$customerPlot <- renderPlot({
    req(input$selectedcustomers, input$yearRange)
    
    filtered_data <- MonthGRAPH_filtered %>%
      filter(Customer.ID %in% input$selectedcustomers, Year %in% input$yearRange)
    
    # Convert Customer.ID to a factor
    filtered_data$Customer.ID <- as.factor(filtered_data$Customer.ID)
    
    if (nrow(filtered_data) == 0) {
      ggplot() + 
        labs(title = "No Data Available", x = "", y = "") +
        theme_minimal()
    } else {
      ggplot(filtered_data, aes(x = Month, y = TotalCount, color = as.factor(Year), shape = Customer.ID)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(title = "Customers Throughout The Months",
             x = "Month",
             y = "Number of Orders",
             color = "Year",
             shape = "Customer") +
        theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  output$customerSelect <- renderUI({
    selectInput("selectedcustomers",
                "Select Customers", 
                choices = unique(MonthGRAPH_filtered$Customer.ID),
                selected = unique(MonthGRAPH_filtered$Customer.ID)[1],
                multiple = TRUE)
  })
  
  output$yearSelect <- renderUI({
    selectInput("yearRange", 
                "Select Year(s):", 
                choices = unique(MonthGRAPH_filtered$Year),
                selected = unique(MonthGRAPH_filtered$Year)[1],
                multiple = TRUE)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)