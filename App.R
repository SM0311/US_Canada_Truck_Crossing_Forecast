library(shiny)
library(tsibble)
library(fable)
library(fabletools)
library(ggplot2)
library(plotly)
library(DT)
library(bslib)
library(lubridate)
library(shinyjs)
library(ggthemes)  # For additional themes

# Load and clean the data
df=read.csv("Border_Crossing_Entry_Data.csv")

df_clean <- df |> distinct()
df_clean <- df_clean |>
  distinct(Port.Name, State, Port.Code, Border, Date, Measure, Value, Latitude, Longitude, Point, .keep_all = TRUE)

# Check for duplicates and missing values
sum(duplicated(df_clean))
sum(is.na(df_clean))

US_Canada <- df_clean |>
  filter(Border == "US-Canada Border" & Measure == "Trucks")

df1 <- US_Canada |>
  select(State, Measure, Value, Date)

# Aggregate data for each state
agg_data <- aggregate(df1$Value, by = list(df1$State, df1$Measure), FUN = sum)
agg_data <- agg_data |>
  rename(State = Group.1, Measure = Group.2, Value = x)

# Filter Michigan data and aggregate by month
US_Canada_MI <- US_Canada |>
  filter(State == "Michigan")

agg_data_mi <- aggregate(US_Canada_MI$Value, by = list(US_Canada_MI$Date), FUN = sum)
agg_data_mi <- agg_data_mi |>
  rename(Date = Group.1, Value = x)

# Convert the Date to year-month format
agg_data_mi <- agg_data_mi |>
  mutate(Date = yearmonth(Date))

# Convert the data to a tsibble
MI_TimeSeries_Plot <- as_tsibble(agg_data_mi, index = Date)

# Define the UI
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs
  theme = bs_theme(bootswatch = "cerulean"),
  
  # Inline CSS to adjust font sizes
  tags$style(HTML("
    body {
      font-size: 14px;
    }
    .container-fluid {
      font-size: 14px;
    }
    .sidebarPanel {
      font-size: 14px;
    }
    .mainPanel {
      font-size: 14px;
    }
    .selectize-input {
      font-size: 14px;
    }
    .shiny-input-container {
      font-size: 14px;
    }
    .shiny-output-error-validation {
      font-size: 14px;
    }
    .shiny-output-error {
      font-size: 14px;
    }
    /* CSS for smaller font size in visualization components */
    .plotly .legend .text {
      font-size: 10px;
    }
    .plotly .xaxislayer-above .tick text,
    .plotly .yaxislayer-above .tick text {
      font-size: 10px;
    }
    .plotly .title {
      font-size: 12px;
    }
    .plotly .annotation text {
      font-size: 8px;
    }
  ")),
  
  titlePanel("Forecasting Truck Crossings at the US-Canada Border from Michigan"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("model1", "Select Model 1:", choices = c("ETS", "ARIMA", "SARIMA", "Multiple Linear Regression")),
      uiOutput("model1_params"),  # UI for model 1 parameters
      selectInput("model2", "Select Model 2:", choices = c("ETS", "ARIMA", "SARIMA", "Multiple Linear Regression")),
      uiOutput("model2_params"),  # UI for model 2 parameters
      numericInput("horizon", "Forecast Months:", value = 12, min = 1),
      dateRangeInput("date_range", "Select Date Range:", start = min(MI_TimeSeries_Plot$Date), end = max(MI_TimeSeries_Plot$Date)),
      textInput("feedback", "Your Feedback:"),
      actionButton("submit_feedback", "Submit Feedback")
    ),
    
    mainPanel(
      fluidRow(
        column(6, plotlyOutput("plot")),
        column(6, plotlyOutput("decomposition_plot"))  # Added plot for decomposition
      ),
      textOutput("total_forecast"),
      DTOutput("summary_table"),
      downloadButton("downloadData", "Download Forecast Data"),
      uiOutput("feedback_message")  # UI for feedback message
    )
  )
)

# Define the server
server <- function(input, output, session) {
  
  output$model1_params <- renderUI({
    if (input$model1 %in% c("ARIMA", "SARIMA")) {
      tagList(
        numericInput("p1", "AR Order (p):", value = 2, min = 0),
        numericInput("d1", "Differencing Order (d):", value = 1, min = 0),
        numericInput("q1", "MA Order (q):", value = 2, min = 0)
      )
    } else {
      NULL
    }
  })
  
  output$model2_params <- renderUI({
    if (input$model2 %in% c("ARIMA", "SARIMA")) {
      tagList(
        numericInput("p2", "AR Order (p):", value = 2, min = 0),
        numericInput("d2", "Differencing Order (d):", value = 1, min = 0),
        numericInput("q2", "MA Order (q):", value = 2, min = 0)
      )
    } else {
      NULL
    }
  })
  
  fit_model <- function(model_name, p, d, q) {
    if (model_name == "ETS") {
      return(MI_TimeSeries_Plot |>
               model(ETS(Value ~ error("A") + trend("Ad") + season("A"))))
    } else if (model_name == "ARIMA") {
      return(MI_TimeSeries_Plot |>
               model(ARIMA(Value ~ pdq(p, d, q))))
    } else if (model_name == "SARIMA") {
      return(MI_TimeSeries_Plot |>
               model(ARIMA(Value ~ pdq(p, d, q) + PDQ(0, 1, 2))))
    } else if (model_name == "Multiple Linear Regression") {
      return(MI_TimeSeries_Plot |>
               model(TSLM(Value ~ trend() + season())))
    }
  }
  
  forecast_data <- reactive({
    model1 <- fit_model(input$model1, input$p1, input$d1, input$q1)
    model2 <- fit_model(input$model2, input$p2, input$d2, input$q2)
    forecast1 <- forecast(model1, h = input$horizon)
    forecast2 <- forecast(model2, h = input$horizon)
    list(forecast1 = forecast1, forecast2 = forecast2)
  })
  
  output$plot <- renderPlotly({
    forecasts <- forecast_data()
    p <- autoplot(forecasts$forecast1) +
      autolayer(forecasts$forecast2, series = "Model 2", color = "red") +
      labs(title = "Forecast of Truck Crossings from Michigan at US-Canada Border") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 10),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            plot.title = element_text(size = 12)) +
      scale_color_manual(values = c("blue", "red"))
    ggplotly(p)
  })
  
  output$decomposition_plot <- renderPlotly({
    forecasts <- forecast_data()
    
    # Decomposition for Model 1
    decomposed_model1 <- MI_TimeSeries_Plot |>
      model(ETS(Value ~ error("A") + trend("Ad") + season("A"))) |>
      components()
    
    p_decomp1 <- autoplot(decomposed_model1, facets = FALSE) +
      labs(title = "Decomposition of Forecasts for Model 1") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            plot.title = element_text(size = 12))
    
    ggplotly(p_decomp1)
  })
  
  output$total_forecast <- renderText({
    forecasts <- forecast_data()
    total_forecast1 <- sum(forecasts$forecast1$.mean, na.rm = TRUE)
    total_forecast2 <- sum(forecasts$forecast2$.mean, na.rm = TRUE)
    paste("Total Forecast for Model 1:", total_forecast1, "\nTotal Forecast for Model 2:", total_forecast2)
  })
  
  output$summary_table <- renderDT({
    forecasts <- forecast_data()
    summary_table <- data.frame(
      Model = c("Model 1", "Model 2"),
      Total_Forecast = c(sum(forecasts$forecast1$.mean, na.rm = TRUE), sum(forecasts$forecast2$.mean, na.rm = TRUE))
    )
    datatable(summary_table, options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("forecast_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      forecasts <- forecast_data()
      forecast_df <- data.frame(
        Date = rep(forecasts$forecast1$.index, 2),
        Forecast = c(forecasts$forecast1$.mean, forecasts$forecast2$.mean),
        Model = rep(c("Model 1", "Model 2"), each = length(forecasts$forecast1$.mean))
      )
      write.csv(forecast_df, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$submit_feedback, {
    feedback <- input$feedback
    output$feedback_message <- renderUI({
      if (nzchar(feedback)) {
        tagList(
          h4("Thank you for your feedback!"),
          p("Your feedback has been received:")
        )
      } else {
        h4("Please enter your feedback before submitting.")
      }
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)



