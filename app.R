# Load necessary libraries
library(shiny)
library(tidyverse)
library(pROC)
library(caret)

# Define UI
ui <- fluidPage(
  titlePanel("Logistic Regression Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "cleaned_data.csv", accept = ".csv"),
      uiOutput("independent_vars"),
      actionButton("run_model", "Run Logistic Regression")
    ),
    
    mainPanel(
      h3("Model Summary"),
      verbatimTextOutput("model_summary"),
      h3("ROC Curve"),
      plotOutput("roc_curve"),
      h3("Confusion Matrix"),
      verbatimTextOutput("confusion_matrix"),
      h3("Optimized Threshold"),
      verbatimTextOutput("optimized_threshold")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Load data and convert Result to 0 and 1
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df$Result <- ifelse(df$Result == 2, 1, 0) # Convert 2 to 1 and 1 to 0
    df
  })
  
  # Render UI for independent variable selection
  output$independent_vars <- renderUI({
    req(data())
    selectInput("independent_vars", "Select Independent Variables", 
                choices = names(data())[names(data()) != "Result"], 
                multiple = TRUE)
  })
  
  # Run logistic regression
  model <- eventReactive(input$run_model, {
    req(data(), input$independent_vars)
    formula <- as.formula(paste("Result ~", paste(input$independent_vars, collapse = "+")))
    glm(formula, data = data(), family = binomial)
  })
  
  # Display model summary
  output$model_summary <- renderPrint({
    req(model())
    summary(model())
  })
  
  # Plot ROC curve
  output$roc_curve <- renderPlot({
    req(model(), data())
    prob <- predict(model(), type = "response")
    roc_obj <- roc(data()$Result, prob)
    plot(roc_obj, main = "ROC Curve")
  })
  
  # Calculate optimized threshold
  optimized_threshold <- reactive({
    req(model(), data())
    prob <- predict(model(), type = "response")
    roc_obj <- roc(data()$Result, prob)
    coords(roc_obj, "best", ret = "threshold")[1,1]
  })
  
  # Display optimized threshold
  output$optimized_threshold <- renderText({
    req(optimized_threshold())
    paste("Optimized Threshold:", round(optimized_threshold(), 3))
  })
  
  # Display confusion matrix
  output$confusion_matrix <- renderPrint({
    req(model(), data(), optimized_threshold())
    prob <- predict(model(), type = "response")
    pred <- ifelse(prob > optimized_threshold(), 1, 0)
    confusionMatrix(factor(pred, levels = c(0, 1)), factor(data()$Result, levels = c(0, 1)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)