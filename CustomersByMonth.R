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
  titlePanel("Customers Throughout The Months"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("CustomerSelect") # Placeholder for dynamic UI input
    ),
    mainPanel(
      plotOutput("CustomerPlot")
    )
  )
)


server <- function(input, output, session) {
  
  pdts_separated <- read.csv("C:/Users/18166/Downloads/fully_separated_orders.csv")
  aggregated_data <- pdts_separated %>%
    group_by(Customer.ID) %>%
    summarise(OrderDate = (Order.Date), .groups = 'drop')
  
  aggregated_data
  
  date <- as.Date(aggregated_data$OrderDate, format = "%d/%m/%Y") 
  Month <- as.numeric(format(date, "%m"))
  data.month <- cbind(aggregated_data, Month)
  data.month
  
  data.month_grouped <- data.month %>%
    group_by(Customer.ID, Month) %>%
    summarize(TotalCount = n()) %>%
    ungroup()
  data.month_grouped
  
  product_types <- unique(aggregated_data$Customer.ID)
  
  
  MonthNames <- filter(data.month_grouped, Month == c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  MonthNames <- MonthNames[, -which(names(MonthNames) == "OrderDate")]
  
  MonthNames <- filter(data.month_grouped, Month %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
  
  MonthNamesDistinct <- MonthNames %>%
    distinct(Customer.ID, TotalCount, Month)
  
  MonthNamesDistinct$Month <- factor(MonthNamesDistinct$Month, 
                                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                                     labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

  
  MonthGRAPH <- MonthNamesDistinct %>% 
    filter(!is.na(Customer.ID), !is.na(Month))
  
  # Render plot
  output$CustomerPlot <- renderPlot({
    filtered_data <- MonthGRAPH %>% 
      filter(Customer.ID %in% input$selectedCustomers)
    
    ggplot(filtered_data, aes(x = Month, y = TotalCount, fill = Customer.ID)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Customers Throughout The Months",
           x = "Month",
           y = "Number of Customers") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  unique_products <- unique(MonthGRAPH$Customer.ID)
  
  output$CustomerSelect <- renderUI({
    checkboxGroupInput("selectedCustomers", 
                       "Select Customer:", 
                       choices = unique_products, 
                       selected = "6")
  })
}

# Run the application
shinyApp(ui = ui, server = server)





