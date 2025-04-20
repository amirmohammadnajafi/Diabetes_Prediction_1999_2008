
# بارگذاری کتابخانه‌ها
library(shiny)
library(dplyr)
library(gRain)
library(bnlearn)
library(igraph)
library(tidyr)

# Set maximum upload file size (100 MB)
options(shiny.maxRequestSize = 100 * 1024^2)

# Define the UI of the application
ui <- fluidPage(
  titlePanel("Bayesian Network Prediction"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload Data", accept = ".csv"),
      actionButton("predict", "Run Prediction")
    ),
    mainPanel(
      textOutput("status"),       # Display prediction status
      dataTableOutput("predictions")  # Show prediction results in an interactive table
  )
)

# Define the Server logic of the application
server <- function(input, output) {
  
  # Function to run prediction using a Bayesian Network
  predict_bayesian <- function(file_data, model_path = "D:/project_esame_aml/fit_params_hill.rds") {
    # Load and preprocess input data
    data <- read.csv(file_data)
    data <- subset(data, select = -c(patient_nbr, examide, citoglipton,
                                     glimepiride.pioglitazone, metformin.rosiglitazone))
    data <- subset(data, gender != "Unknown/Invalid")
    data <- data.frame(lapply(data, as.factor))
    
    # Load the fitted Bayesian model from file
    fit_params_hill <- readRDS(model_path)
    
    # Convert the model to gRain format and propagate beliefs
    grain_model <- as.grain(fit_params_hill)
    grain_prop  <- propagate(grain_model)
    
    # Predict 'readmitted' for the uploaded dataset
    pred_vec <- predict(grain_prop,
                        response = "readmitted",
                        newdata = data)
    
    # Combine original data with the prediction results
    result_df <- cbind(data, Prediction = pred_vec)
    return(result_df)
  }
  
  # React to the prediction button click
  observeEvent(input$predict, {
    req(input$file1)
    
    # Show initial prediction status
    output$status <- renderText("Prediction is in progress...")
    
    # Call the prediction function
    predictions_data <- predict_bayesian(input$file1$datapath)
    
    # Display the prediction results in a data table
    output$predictions <- renderDataTable({
      predictions_data
    }, options = list(pageLength = 10, autoWidth = TRUE))
    
    # Update status after completion
    output$status <- renderText("Prediction completed!")
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
