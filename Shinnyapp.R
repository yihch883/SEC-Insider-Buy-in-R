library(shiny)

library(shiny)

# Sample data frames (replace these with your actual data frames)


ui <- fluidPage(
  titlePanel("Insider Information"),
  sidebarLayout(
    sidebarPanel(
      # Multiple checkboxes input
      checkboxGroupInput("checkboxes", "Select options:", 
                         choices = c("Insider buy", "Insider sell"),
                         selected = c()),
      # Numeric input for filtering data
      numericInput("input_value", "Enter a value:", value = 0)
    ),
    mainPanel(
      # Output elements go here
      verbatimTextOutput("output_text")
    )
  )
)

ui <- fluidPage(
  titlePanel("Insider Information"),
  sidebarLayout(
    sidebarPanel(
      # Multiple checkboxes input
      checkboxGroupInput("checkboxes", "Select options:", 
                         choices = c("Insider buy", "Insider sell"),
                         selected = c()),
      # Dynamic numeric input for filtering data
      uiOutput("input_value")
    ),
    mainPanel(
      # Output elements go here
      verbatimTextOutput("output_text")
    )
  )
)

server <- function(input, output) {
  # Dynamic numeric input widget
  output$input_value <- renderUI({
    numericInput("input_value", "Enter a value:", value = 0)
  })
  
  # Reactive expression to filter and display data based on the selected checkboxes and input value
  filtered_data <- reactive({
    result_df <- data.frame() # Create an empty data frame to store the filtered results
    
    if ("Insider buy" %in% input$checkboxes) {
      filtered_rows <- subset(insider_buy_df, Value > input$input_value)
      result_df <- rbind(result_df, filtered_rows) # Append the filtered rows to the result data frame
    }
    
    if ("Insider sell" %in% input$checkboxes) {
      filtered_rows <- subset(insider_sell_df, Value < input$input_value)
      result_df <- rbind(result_df, filtered_rows) # Append the filtered rows to the result data frame
    }
    result_df <- result_df[, (names(result_df) %in% c("Ticker","Trade_Date","Value"))]
    

    # Return the final data frame containing all filtered results
    return(result_df)
  })
  
  # Reactive expression to print the filtered data frame
  output$output_text <- renderPrint({
    filtered_data()
  })
}

##Run The App
shinyApp(ui, server)

