library(shiny)
library(tidyquant)
library(ggplot2)
library(plotly)
library(dplyr)
library(zoo)

ui <- fluidPage(
  titlePanel("Interactive Stock Market Dashboard"),
  p("Select a stock ticker and date range to visualize trends and metrics"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Stock Selection"),
      textInput("ticker", "Enter Stock Ticker:", "AAPL"),
      dateInput("start_date", "Start Date", value = Sys.Date() - 365),
      dateInput("end_date", "End Date", value = Sys.Date()),
      sliderInput("ma_window", "Moving Average Window (days)", min = 5, max = 50, value = 20),
      hr(),
      p("Built with Shiny, tidyquant, and Plotly"),
      p("Source: Yahoo Finance")
    ),
    
    mainPanel(
      h3(textOutput("metrics_title")),
      fluidRow(
        column(4, verbatimTextOutput("metrics_title")),
        column(4, verbatimTextOutput("latest_price")),
        column(4, verbatimTextOutput("price_change")),
        column(4, verbatimTextOutput("volatility"))
      ),
      h3("Stock Price Trend"), 
      plotlyOutput("price_plot", height = "400px"),
      h3("Trading Volume"), 
      plotlyOutput("volume_plot", height = "400px"),
      checkboxInput("show_data", "Show Raw Data", FALSE),
      tableOutput("raw_data")
    )
  )
)

server <- function(input, output, session) {
  
  stock_data <- reactive({
    tryCatch({
      df <- tq_get(toupper(input$ticker),
                   get = "stock.prices",
                   from = input$start_date,
                   to = input$end_date) %>%
        mutate(date = as.Date(date)) %>%
        mutate(ma = rollmean(close, k = input$ma_window, fill = NA, align = "right"))
      return(df)
    }, error = function(e) {
      showNotification(paste("Error fetching data for", input$ticker, ":", e$message), type = "error")
      return(NULL)
    })
  })
  
  output$metrics_title <- renderText({
    paste("Stock Metrics for", toupper(input$ticker))
  })
  
  output$latest_price <- renderPrint({
    df <- stock_data()
    if (!is.null(df) && nrow(df) > 0) {
      latest <- tail(df$close, 1)
      sprintf("Latest Closing Price: $ %.2f", latest)
    } else {
      "No data available"
    }
  }) 
  
  output$price_change <- renderPrint({
    df <- stock_data()
    if (!is.null(df) && nrow(df) > 0) {
      change <- tail(df$close, 1) - head(df$close, 1)
      sprintf("Price Change: $%.2f %s", abs(change),
              ifelse(change >= 0, "(+)", "(-)"))
    } else {
      "No data available"
    }
  })
  
  output$volatility <- renderPrint({
    df <- stock_data()
    if (!is.null(df) && nrow(df) > 0) {
      #vol <- sd(diff(df$close) / lag(df$close, 1), na.rm = TRUE) * 100
      returns <- diff(df$close) / head(df$close, -1)
      vol <- sd(returns, na.rm = TRUE) * 100
      sprintf("Volatility (Std Dev %%): %.2f%%", vol)
    } else {
      "No data available"
    }
  })
  
  output$price_plot <- renderPlotly({
    df <- stock_data()
    if (!is.null(df) && nrow(df) > 0) {
      ma_label <- paste(input$ma_window, "Day MA")
      color_map <- setNames(c("blue", "red"), c("Close Price", ma_label))
      
      p <- ggplot(df, aes(x = date)) +
        geom_line(aes(y = close, color = "Close Price")) +
        geom_line(aes(y = ma, color = ma_label), linetype = "dashed") +
        labs(
          title = paste(toupper(input$ticker), "Stock Price and Moving Average"),
          x = "Date",
          y = "Price (USD)"
        ) +
        theme_minimal() +
        scale_color_manual(values = color_map)
      
      ggplotly(p)
    } else {
      plot_ly() %>%
        add_text(x = 0.5, y = 0.5, text = "No data available", textposition = "middle center")
    }
  })
  
  output$volume_plot <- renderPlotly({
    df <- stock_data()
    if (!is.null(df) && nrow(df) > 0) {
      p <- ggplot(df, aes(x = date, y = volume)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = "Trading Volume", x = "Date", y = "Volume") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      plot_ly() %>%
        add_text(x = 0.5, y = 0.5, text = "No data available", textposition = "middle center")
    }
  })
  
  output$raw_data <- renderTable({
    if (input$show_data) {
      df <- stock_data()
      if (!is.null(df)) {
        df %>% select(date, open, high, low, close, ma, volume)
      } else {
        data.frame(Message = "No data available")
      }
    }
  })
}

shinyApp(ui, server)