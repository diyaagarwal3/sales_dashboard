# app.R
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)

# ---- Load data ----
sales_data <- read.csv(
  "C:\\Users\\Lenovo\\Downloads\\sales_data.csv",
  stringsAsFactors = FALSE
)
sales_data$Sale_Date <- as.Date(sales_data$Sale_Date)

# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Sales Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Details",  tabName = "details",  icon = icon("table"))
    ),
    dateRangeInput(
      "date_range", "Sale Date Range:",
      start = min(sales_data$Sale_Date),
      end   = max(sales_data$Sale_Date)
    ),
    selectInput(
      "category", "Product Category:",
      choices  = c("All", sort(unique(sales_data$Product_Category))),
      selected = "All", multiple = TRUE
    ),
    selectInput(
      "region", "Region:",
      choices  = c("All", sort(unique(sales_data$Region))),
      selected = "All", multiple = TRUE
    ),
    selectInput(
      "rep", "Sales Rep:",
      choices  = c("All", sort(unique(sales_data$Sales_Rep))),
      selected = "All", multiple = TRUE
    )
  ),
  dashboardBody(
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("totalSales"),
                valueBoxOutput("totalQty"),
                valueBoxOutput("avgDiscount")
              ),
              fluidRow(
                box(
                  title = "Sales Over Time", status = "primary",
                  solidHeader = TRUE, width = 6,
                  plotOutput("salesTimePlot", height = 250)
                ),
                box(
                  title = "Sales by Category", status = "info",
                  solidHeader = TRUE, width = 6,
                  plotOutput("salesCatPlot", height = 250)
                )
              )
      ),
      # Details tab
      tabItem(tabName = "details",
              fluidRow(
                box(
                  title = "Filtered Data", status = "warning",
                  solidHeader = TRUE, width = 12,
                  DTOutput("dataTable")
                )
              )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  data_filtered <- reactive({
    df <- sales_data %>%
      filter(Sale_Date >= input$date_range[1],
             Sale_Date <= input$date_range[2])
    
    if (!("All" %in% input$category)) {
      df <- df %>% filter(Product_Category %in% input$category)
    }
    if (!("All" %in% input$region)) {
      df <- df %>% filter(Region %in% input$region)
    }
    if (!("All" %in% input$rep)) {
      df <- df %>% filter(Sales_Rep %in% input$rep)
    }
    df
  })
  
  output$totalSales <- renderValueBox({
    valueBox(
      dollar(sum(data_filtered()$Sales_Amount)),
      "Total Sales", icon = icon("dollar-sign"), color = "green"
    )
  })
  output$totalQty <- renderValueBox({
    valueBox(
      sum(data_filtered()$Quantity_Sold),
      "Total Quantity Sold", icon = icon("boxes"), color = "blue"
    )
  })
  output$avgDiscount <- renderValueBox({
    valueBox(
      percent(mean(data_filtered()$Discount, na.rm = TRUE)),
      "Average Discount", icon = icon("percent"), color = "purple"
    )
  })
  
  output$salesTimePlot <- renderPlot({
    df_ts <- data_filtered() %>%
      group_by(Sale_Date) %>%
      summarise(DailySales = sum(Sales_Amount), .groups = "drop")
    ggplot(df_ts, aes(Sale_Date, DailySales)) +
      geom_line() +
      labs(x = NULL, y = "Sales") +
      scale_y_continuous(labels = dollar) +
      theme_minimal()
  })
  
  output$salesCatPlot <- renderPlot({
    df_cat <- data_filtered() %>%
      group_by(Product_Category) %>%
      summarise(TotalSales = sum(Sales_Amount), .groups = "drop") %>%
      arrange(TotalSales)
    ggplot(df_cat, aes(reorder(Product_Category, TotalSales), TotalSales)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Sales") +
      scale_y_continuous(labels = dollar) +
      theme_minimal()
  })
  
  output$dataTable <- renderDT({
    datatable(
      data_filtered(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

# ---- Run App ----
shinyApp(ui, server)
