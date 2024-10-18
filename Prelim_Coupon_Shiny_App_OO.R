library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)

# Define UI
ui <- fluidPage(
  titlePanel("Coupon Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      uiOutput("dateRangeUI")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", tableOutput("summary")),
        tabPanel("Product Sales", plotOutput("productPlotCoupon"), plotOutput("productPlotNonCoupon")),
        tabPanel("Customer Returns", plotOutput("returnPlot")),
        tabPanel("Customer Map", leafletOutput("map")),
        tabPanel("Time Graph", plotOutput("timeGraph")),
        tabPanel("T-Test Result", verbatimTextOutput("ttestResult"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  data <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, header = input$header)
    
    # Convert Coupon_Details.x to binary code
    if ("Coupon_Details.x" %in% colnames(df)) {
      df$Coupon.Binary <- ifelse(df$Coupon_Details.x == "", 0, 1)
    } else {
      stop("Coupon_Details.x column not found in the uploaded file.")
    }
    
    df$Order.Date <- as.Date(df$Order.Date, format = "%d/%m/%Y")
    df
  })
  
  output$dateRangeUI <- renderUI({
    df <- data()
    sliderInput("dateRange", "Select Date Range:",
                min = min(df$Order.Date, na.rm = TRUE),
                max = max(df$Order.Date, na.rm = TRUE),
                value = c(min(df$Order.Date, na.rm = TRUE), max(df$Order.Date, na.rm = TRUE)),
                timeFormat = "%Y-%m-%d")
  })
  
  filteredData <- reactive({
    df <- data()
    if (!is.null(input$dateRange)) {
      df <- df %>% filter(Order.Date >= input$dateRange & Order.Date <= input$dateRange)
    }
    df
  })
  
  output$summary <- renderTable({
    df <- filteredData()
    summary_stats <- df %>%
      group_by(Coupon.Binary) %>%
      summarise(
        Mean_Total_Quantity = mean(ProductQty, na.rm = TRUE),
        Median_Total_Quantity = median(ProductQty, na.rm = TRUE),
        SD_Total_Quantity = sd(ProductQty, na.rm = TRUE),
        Mean_ProductOrderPrice = mean(ProductTotalPrice, na.rm = TRUE),
        Median_ProductOrderPrice = median(ProductTotalPrice, na.rm = TRUE),
        SD_ProductOrderPrice = sd(ProductTotalPrice, na.rm = TRUE)
      )
    summary_stats
  })
  
  output$productPlotCoupon <- renderPlot({
    df <- filteredData() %>% filter(Coupon.Binary == 1)
    ggplot(df, aes(x = ProductName, y = ProductQty)) +
      geom_bar(stat = "identity", fill = "blue") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
      labs(title = "Total Product Sales (Coupon Users)", x = "Product Name", y = "Total Quantity Sold")
  })
  
  output$productPlotNonCoupon <- renderPlot({
    df <- filteredData() %>% filter(Coupon.Binary == 0)
    ggplot(df, aes(x = ProductName, y = ProductQty)) +
      geom_bar(stat = "identity", fill = "red") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
      labs(title = "Total Product Sales (Non-Coupon Users)", x = "Product Name", y = "Total Quantity Sold")
  })
  
  output$returnPlot <- renderPlot({
    df <- filteredData()
    return_summary <- df %>%
      group_by(Customer.ID, Coupon.Binary) %>%
      summarise(Returns = n_distinct(Order.ID))
    
    ggplot(return_summary, aes(x = as.factor(Coupon.Binary), y = Returns, fill = as.factor(Coupon.Binary))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Customer Returns", x = "Used Coupon", y = "Number of Returns", fill = "Used Coupon")
  })
  
  output$map <- renderLeaflet({
    df <- filteredData()
    df <- df %>% filter(!is.na(Billing.Zip))
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(~Billing.Zip, popup = ~paste("Customer ID:", Customer.ID, "<br>", "Used Coupon:", Coupon.Binary))
  })
  
  output$timeGraph <- renderPlot({
    df <- filteredData()
    time_graph <- df %>%
      group_by(Order.Date) %>%
      summarise(Total_Quantity = sum(ProductQty, na.rm = TRUE))
    
    ggplot(time_graph, aes(x = Order.Date, y = Total_Quantity)) +
      geom_line() +
      labs(title = "Total Products Purchased Over Time", x = "Order Date", y = "Total Quantity Purchased")
  })
  
  output$ttestResult <- renderPrint({
    df <- filteredData()
    coupon_users <- df %>% filter(Coupon.Binary == 1) %>% pull(ProductTotalPrice)
    non_coupon_users <- df %>% filter(Coupon.Binary == 0) %>% pull(ProductTotalPrice)
    
    t_test <- t.test(coupon_users, non_coupon_users)
    
    cat("T-test result:\n")
    cat("t-statistic =", t_test$statistic, "\n")
    cat("p-value =", t_test$p.value, "\n")
    
    if (t_test$p.value < 0.05) {
      cat("There is a significant difference between the purchasing behaviors of coupon and non-coupon users.\n")
    } else {
      cat("There is no significant difference between the purchasing behaviors of coupon and non-coupon users.\n")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
