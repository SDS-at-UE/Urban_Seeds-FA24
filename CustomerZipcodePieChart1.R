library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)  # Load tidyr for replace_na()

ui <- fluidPage(
  titlePanel("Customer Distribution by Zipcode"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("yearSelect")  # Selector for year
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
    summarise(OrderDate = first(Order.Date), ZipCode = first(Billing.Zip), .groups = 'drop')
  
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
    
    # Create a data frame of all zip codes
    all_zipcodes <- unique(pdt_zip_filtered$ZipCode)
    pie_data <- data.frame(ZipCode = all_zipcodes) %>%
      left_join(filtered_data %>%
                  group_by(ZipCode) %>%
                  summarise(TotalCount = sum(TotalCount), .groups = 'drop'), 
                by = "ZipCode") %>%
      mutate(TotalCount = replace_na(TotalCount, 0),  # Replace NA with 0
             Percentage = (TotalCount / sum(TotalCount, na.rm = TRUE)) * 100)
    
    # Create a pie chart
    ggplot(pie_data, aes(x = "", y = TotalCount, fill = ZipCode)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = paste("Customer Distribution by Zip Code in", input$selectedyear),
           fill = "Zip Code") +
      theme_void() +
      geom_text(aes(label = ifelse(TotalCount > 0, paste0(round(Percentage, 1), "%"), "")),
                position = position_stack(vjust = 0.5))
  })
}

# Run the application
shinyApp(ui = ui, server = server)


