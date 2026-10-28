library(shiny)
library(readxl)
library(DT)
library(ggplot2)

ui <- navbarPage(
  "Production Planning",
  
  tabPanel("Learning Curve",
           fluidPage(
             h3("Learning Curve Module Placeholder")
           )
  ),
  
  tabPanel("Moving Average",
           sidebarLayout(
             sidebarPanel(
               fileInput("file1", "Upload an Excel file (.xlsx)",
                         accept = c(".xlsx", ".xls")),
               
               uiOutput("col_select_ui"),
               
               radioButtons("n_choice", "How should N be chosen?",
                            choices = c("I will choose N" = "manual", 
                                        "Find the best N" = "auto")),
               
               conditionalPanel(
                 condition = "input.n_choice == 'manual'",
                 numericInput("n_val", "N (number of past periods)", value = 3, min = 1, step = 1)
               ),
               
               actionButton("calculate", "Run Moving Average", class = "btn-primary", style = "margin-top: 15px;")
             ),
             
             mainPanel(
               uiOutput("plot_title_ui"),
               plotOutput("maPlot", height = "350px"),
               br(),
               h4("Error measures"),
               DTOutput("errorTable"),
               br(),
               h4("Forecasts by period"),
               DTOutput("forecastTable")
             )
           )
  )
)

server <- function(input, output, session) {
  
  raw_data <- reactive({
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    req(ext %in% c("xlsx", "xls"))
    read_excel(input$file1$datapath)
  })
  
  output$col_select_ui <- renderUI({
    df <- raw_data()
    req(df)
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    selectInput("demand_col", "Demand column", choices = numeric_cols)
  })
  
  ma_results <- eventReactive(input$calculate, {
    req(raw_data(), input$demand_col)
    demand <- as.numeric(raw_data()[[input$demand_col]])
    n_obs <- length(demand)
    
    chosen_n <- input$n_val
    if (input$n_choice == "auto") {
      best_mse <- Inf
      best_n <- 1
      max_possible_n = min(10, n_obs - 2)
      if (max_possible_n < 1) max_possible_n <- 1
      
      for (n in 1:max_possible_n) {
        fc_temp <- rep(NA, n_obs)
        for (i in (n + 1):n_obs) {
          fc_temp[i] <- mean(demand[(i - n):(i - 1)])
        }
        valid_idx <- (n + 1):n_obs
        mse_temp <- mean((demand[valid_idx] - fc_temp[valid_idx])^2, na.rm = TRUE)
        if (mse_temp < best_mse) {
          best_mse <- mse_temp
          best_n <- n
        }
      }
      chosen_n <- best_n
    }
    
    req(chosen_n < n_obs)
    
    # Calculate Forecasts with exact n_obs length (NA padding for initial periods)
    forecasts <- rep(NA, n_obs)
    for (i in (chosen_n + 1):n_obs) {
      forecasts[i] <- mean(demand[(i - chosen_n):(i - 1)])
    }
    
    next_forecast <- mean(demand[(n_obs - chosen_n + 1):n_obs])
    
    # Errors calculation with exact n_obs length
    errors <- rep(NA, n_obs)
    valid_idx <- (chosen_n + 1):n_obs
    errors[valid_idx] <- forecasts[valid_idx] - demand[valid_idx]
    
    mad <- mean(abs(errors[valid_idx]))
    mse <- mean(errors[valid_idx]^2)
    mape <- mean(abs(errors[valid_idx] / demand[valid_idx])) * 100
    
    eval_range <- paste0(chosen_n + 1, "-", n_obs)
    
    df_res <- data.frame(
      Period = 1:n_obs,
      Demand = demand,
      Forecast = round(forecasts, 2),
      Error = round(errors, 2)
    )
    
    list(
      n = chosen_n,
      next_forecast = round(next_forecast, 2),
      df = df_res,
      mad = round(mad, 2),
      mse = round(mse, 2),
      mape = round(mape, 2),
      eval_range = eval_range
    )
  })
  
  output$plot_title_ui <- renderUI({
    res <- ma_results()
    req(res)
    h4(paste0("MA(", res$n, ") — next-period forecast: ", res$next_forecast))
  })
  
  output$maPlot <- renderPlot({
    res <- ma_results()
    req(res)
    df <- res$df
    
    # Gelecek dönem (Next-period) için tek noktalık bir veri çerçevesi oluşturuyoruz
    n_obs <- nrow(df)
    next_period <- n_obs + 1
    next_fc <- res$next_forecast
    next_df <- data.frame(Period = next_period, Forecast = next_fc)
    
    ggplot() +
      # Gerçek Talep (Actual Demand) - Yuvarlak nokta ve düz çizgi
      geom_line(data = df, aes(x = Period, y = Demand, color = "Actual demand"), linetype = "solid", linewidth = 0.8) +
      geom_point(data = df, aes(x = Period, y = Demand, color = "Actual demand"), shape = 16, size = 2) +
      
      # Tahmin (Forecast) - Üçgen nokta ve kesik çizgi
      geom_line(data = df, aes(x = Period, y = Forecast, color = "Forecast"), linetype = "dashed", linewidth = 0.8) +
      geom_point(data = df, aes(x = Period, y = Forecast, color = "Forecast"), shape = 17, size = 2) +
      
      # Gelecek Dönem Tahmini (Next-period forecast) - Büyük turuncu üçgen
      geom_point(data = next_df, aes(x = Period, y = Forecast, color = "Next-period forecast"), shape = 17, size = 4) +
      
      # Renkleri ve etiketleri ayarlama
      scale_color_manual(
        name = NULL,
        values = c("Actual demand" = "#555555", 
                   "Forecast" = "#2980b9", 
                   "Next-period forecast" = "#d35400"),
        breaks = c("Actual demand", "Forecast", "Next-period forecast")
      ) +
      labs(title = paste0("Actual demand and MA(", res$n, ") forecast"),
           y = "Demand", x = "Period") +
      theme_bw() + # Beyaz arka planlı temiz tema
      theme(
        legend.position = c(0.15, 0.85), # Lejantı sol üst köşeye grafiğin içine alır
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5) # Başlığı ortalar
      )
  })
  
  output$errorTable <- renderDT({
    res <- ma_results()
    req(res)
    err_df <- data.frame(
      Measure = c("MAD", "MSE", "MAPE (%)"),
      Value = c(res$mad, res$mse, res$mape),
      `Periods evaluated` = c(res$eval_range, res$eval_range, res$eval_range)
    )
    datatable(err_df, options = list(dom = 't', pageLength = 3), rownames = FALSE)
  })
  
  output$forecastTable <- renderDT({
    res <- ma_results()
    req(res)
    table_df <- res$df[, c("Period", "Demand", "Forecast", "Error")]
    colnames(table_df) <- c("Period", "Demand (D)", "Forecast (F)", "Error (e = F - D)")
    datatable(table_df, options = list(pageLength = 5, dom = 'tp'), rownames = FALSE)
  })
}

shinyApp(ui, server)