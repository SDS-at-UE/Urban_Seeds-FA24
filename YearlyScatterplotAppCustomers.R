library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Customers in Each Zipcode"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("yearSelect")  # Select input for year
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
  
  # Render year selection
  output$yearSelect <- renderUI({
    selectInput("selectedyear",
                "Select Year", 
                choices = unique(pdt_zip_filtered$Year),
                selected = unique(pdt_zip_filtered$Year)[1])  # Default to first year
  })
  
  # Render plot
  output$customerPlot <- renderPlot({
    req(input$selectedyear)  # Ensure a year is selected
    
    filtered_data <- pdt_zip_filtered %>%
      filter(Year == input$selectedyear)
    
    ggplot(filtered_data, aes(x = ZipCode, y = TotalCount, color = Customer.ID)) +
      geom_point(size = 3) +
      labs(title = paste("Customers for Each Zipcode in", input$selectedyear),
           x = "Zip Code",
           y = "Number of Customers") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
