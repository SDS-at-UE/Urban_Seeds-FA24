library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

ui <- fluidPage(
  titlePanel("Customer Distribution by Zipcode"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("yearSelect")  # Selector for year
    ),
    mainPanel(
      plotlyOutput("customerPlot")  # Output for the interactive pie chart
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
  output$customerPlot <- renderPlotly({
    req(input$selectedyear)
    
    filtered_data <- pdt_zip_filtered %>%
      filter(Year == input$selectedyear)
    
    # Ensure there's data to plot
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    total_count <- sum(filtered_data$TotalCount, na.rm = TRUE)
    
    pie_data <- filtered_data %>%
      mutate(Percentage = (TotalCount / total_count) * 100,
             Label = paste(ZipCode, "<br>", round(Percentage, 1), "%"))  # Updated label
    
    # Create the pie chart
    p <- plot_ly(pie_data, labels = ~Label, values = ~TotalCount, type = 'pie', 
                 textinfo = 'text',  # Change to 'text' to only show custom hover text
                 text = ~Label,      # Use the custom label for hover text
                 insidetextorientation = 'radial') %>%
      layout(title = paste("Customer Distribution by Zip Code in", input$selectedyear),
             showlegend = TRUE)
    
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)

