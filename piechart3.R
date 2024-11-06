library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

ui <- fluidPage(
  titlePanel("Customer Distribution by Zipcode"),
  sidebarLayout(
    sidebarPanel(
      # Slider input for selecting a year range (from 2021 to 2024)
      sliderInput("yearRange",
                  "Select Year Range",
                  min = 2021,  # Set the minimum year to 2021
                  max = 2024,  # Set the maximum year to 2024
                  value = c(2021, 2024),  # Default to the full range (2021 to 2024)
                  step = 1,
                  animate = TRUE,
                  sep = ""  # Remove comma separation in slider labels
      )
    ),
    mainPanel(
      plotlyOutput("customerPlot")  # Output for the interactive pie chart
    )
  )
)

server <- function(input, output, session) {
  
  # Read data
  pdts_separated <- read.csv("C:/Users/18166/Downloads/fully_separated_orders.csv")
  
  # Preprocess data and aggregate
  aggregated_data <- pdts_separated %>%
    group_by(Customer.ID) %>%
    summarise(OrderDate = (Order.Date), ZipCode = (Billing.Zip), .groups = 'drop')
  
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
  
  # Reactive expression to filter data based on year range
  filtered_data <- reactive({
    pdt_zip_filtered %>%
      filter(Year >= input$yearRange[1], Year <= input$yearRange[2])
  })
  
  # Render the plot based on the selected year range
  output$customerPlot <- renderPlotly({
    # Get filtered data
    data <- filtered_data()
    
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
      layout(title = paste("Customer Distribution by Zip Code from", 
                           input$yearRange[1], "to", input$yearRange[2]),  # Format year without commas
             showlegend = TRUE)
    
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)



