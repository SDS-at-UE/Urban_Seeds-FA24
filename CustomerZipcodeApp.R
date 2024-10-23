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
  titlePanel("Customers in Each Zipcode"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("customerSelect"),
      uiOutput("zipcodeSelect")
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
    summarise(OrderDate = (Order.Date), ZipCode = (Billing.Zip), .groups = 'drop')
  
  date <- as.Date(aggregated_data$OrderDate, format = "%d/%m/%Y") 
  Month <- as.numeric(format(date, "%m"))
  Year <- as.numeric(format(date, "%Y"))
  data.month <- cbind(aggregated_data, date, Month, Year)
  
  data.zip_grouped <- data.month %>%
    group_by(Customer.ID, Year, ZipCode) %>%
    summarise(TotalCount = n(), .groups = 'drop') %>%
    ungroup()
  
  pdt_zip_filtered <- data.zip_grouped %>% 
    filter(!is.na(Customer.ID), !is.na(ZipCode), !is.na(Year))
  
  # Render customer selection
  output$customerSelect <- renderUI({
    selectInput("selectedcustomers",
                "Select Customers", 
                choices = unique(pdt_zip_filtered$Customer.ID),
                selected = unique(pdt_zip_filtered$Customer.ID)[1],
                multiple = TRUE)
  })
  
  # Render zipcode selection
  output$zipcodeSelect <- renderUI({
    selectInput("selectedzipcodes",
                "Select Zip Codes", 
                choices = unique(pdt_zip_filtered$ZipCode),
                selected = unique(pdt_zip_filtered$ZipCode)[1],
                multiple = TRUE)
  })
  
  # Render plot
  output$customerPlot <- renderPlot({
    req(input$selectedcustomers, input$selectedzipcodes)  # Ensure both are selected
    
    filtered_data <- pdt_zip_filtered %>%
      filter(Customer.ID %in% input$selectedcustomers,
             ZipCode %in% input$selectedzipcodes)
    
    ggplot(filtered_data, aes(x = ZipCode, y = TotalCount, fill = Customer.ID)) +
      facet_wrap(~ Year) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Customers for Each Zipcode",
           x = "Zip Code",
           y = "Number of Customers") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

