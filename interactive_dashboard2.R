# Load required libraries
library(shiny)
library(tidyquant)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Stock Market Dashboard"),
  p("Select a stock ticker and date range to visualize trends and metrics."),
  
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
        column(4, verbatimTextOutput("latest_price")),
        column(4, verbatimTextOutput("price_change")),
        column(4, verbatimTextOutput("volatility"))
      ),
      h3("Stock Price with Bollinger Bands"),
      plotlyOutput("price_plot", height = "400px"),
      h3("Trading Volume"),
      plotlyOutput("volume_plot", height = "400px"),
      h3("Relative Strength Index (RSI)"),
      plotlyOutput("rsi_plot", height = "400px"),
      h3("Candlestick Chart"),
      plotlyOutput("candlestick_plot", height = "400px"),
      checkboxInput("show_data", "Show Raw Data", FALSE),
      tableOutput("raw_data")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive function to retrieve and process stock data
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
  
  # Metrics
  output$metrics_title <- renderText({
    paste("Stock Metrics for", toupper(input$ticker))
  })
  
  output$latest_price <- renderPrint({
    df <- stock_data()
    if (!is.null(df) && nrow(df) > 0) {
      latest <- tail(df$close, 1)
      sprintf("Latest Closing Price: $%.2f", latest)
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
      vol <- sd(diff(df$close) / lag(df$close, 1), na.rm = TRUE) * 100
      sprintf("Volatility (Std Dev %%): %.2f%%", vol)
    } else {
      "No data available"
    }
  })
  
  # Price plot with Bollinger Bands
  output$price_plot <- renderPlotly({
    df <- stock_data()
    if (!is.null(df) && nrow(df) > 0) {
      df_bb <- tryCatch({
        df %>%
          tq_transmute(select = close, mutate_fun = BBands, n = 20, sd = 2,
                       col_rename = c("bb_lower", "bb_mavg", "bb_upper"))
      }, error = function(e) {
        showNotification(paste("Error calculating Bollinger Bands:", e$message), type = "error")
        return(NULL)
      })
      
      if (is.null(df_bb) || nrow(df_bb) == 0 || !all(c("bb_lower", "bb_upper") %in% colnames(df_bb))) {
        showNotification("Bollinger Bands calculation failed. Displaying price and MA only.", type = "warning")
        df_plot <- df
      } else {
        if (nrow(df) == nrow(df_bb)) {
          df_plot <- df %>% mutate(bb_lower = df_bb$bb_lower, bb_upper = df_bb$bb_upper)
        } else {
          showNotification("Row mismatch in Bollinger Bands data. Displaying price and MA only.", type = "warning")
          df_plot <- df
        }
      }
      
      p <- ggplot(df_plot, aes(x = date)) +
        geom_line(aes(y = close, color = "Close Price")) +
        geom_line(aes(y = ma, color = paste0(input$ma_window, " Day MA")), linetype = "dashed") +
        { if ("bb_lower" %in% colnames(df_plot) && "bb_upper" %in% colnames(df_plot))
          geom_ribbon(aes(ymin = bb_lower, ymax = bb_upper), fill = "grey", alpha = 0.3) } +
        labs(title = paste(toupper(input$ticker), 
                           if ("bb_lower" %in% colnames(df_plot)) "Stock Price with Bollinger Bands" else "Stock Price"),
             x = "Date", y = "Price (USD)") +
        theme_minimal() +
        scale_color_manual(values = setNames(c("blue", "red"), 
                                             c("Close Price", paste0(input$ma_window, " Day MA"))))
      
      ggplotly(p)
    } else {
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No data available", textposition = "middle center")
    }
  })
  
  # Volume plot
  output$volume_plot <- renderPlotly({
    df <- stock_data()
    if (!is.null(df) && nrow(df) > 0) {
      p <- ggplot(df, aes(x = date, y = volume)) +
        geom_bar(stat = "identity", fill = "blue") +
        labs(title = paste(toupper(input$ticker), "Trading Volume"),
             x = "Date", y = "Volume") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No data available", textposition = "middle center")
    }
  })
  
  # RSI plot
  output$rsi_plot <- renderPlotly({
    df <- stock_data()
    if (!is.null(df) && nrow(df) > 0) {
      df_rsi <- df %>%
        tq_transmute(select = close, mutate_fun = RSI, n = 14, col_rename = "rsi")
      
      p <- ggplot(df_rsi, aes(x = date, y = rsi)) +
        geom_line(color = "purple") +
        geom_hline(yintercept = 70, linetype = "dashed", color = "red", show.legend = TRUE) +
        geom_hline(yintercept = 30, linetype = "dashed", color = "green", show.legend = TRUE) +
        labs(title = paste(toupper(input$ticker), "Relative Strength Index (RSI)"),
             x = "Date", y = "RSI (14-day)") +
        theme_minimal()
      
      ggplotly(p)
    } else {
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No data available", textposition = "middle center")
    }
  })
  
  # Candlestick chart
  output$candlestick_plot <- renderPlotly({
    df <- stock_data()
    if (!is.null(df) && nrow(df) > 0) {
      plot_ly(data = df, type = "candlestick", x = ~date,
              open = ~open, high = ~high, low = ~low, close = ~close,
              name = toupper(input$ticker)) %>%
        layout(title = paste(toupper(input$ticker), "Candlestick Chart"),
               xaxis = list(title = "Date"),
               yaxis = list(title = "Price (USD)"),
               template = "plotly_dark")
    } else {
      plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No data available", textposition = "middle center")
    }
  })
  
  # Raw data table
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

# Run the app
shinyApp(ui = ui, server = server)
