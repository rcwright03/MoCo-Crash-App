library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(corrplot)
library(DT)
library(plotly)
library(shinycssloaders)
library(reshape2)
source('helpers.R')

ui <- dashboardPage(
  dashboardHeader(
    title = "MoCo Crash Classification",
    # custom css to make the whole title fit
    tags$li(class = "dropdown",
            tags$style(".main-header .logo {
                         font-size: 14px; /* Adjust the font size as needed */
                         font-weight: bold;
                       }")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "aboutTab", icon=icon('info-circle')),
      menuItem("Data Exploration", tabName="dataTab", icon=icon('search')),
      menuItem("Data Preprocessing", tabName = "preprocessingTab", icon=icon('clipboard')),
      menuItem("Model Selection and Tuning", tabName="selectionTab", icon=icon('arrow-pointer')),
      menuItem("Model Visualization", tabName="visualizationTab", icon=icon("chart-simple"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName="aboutTab",
        # talk about the dataset and why the project exists
        fluidRow(
          box(
            title="Dataset Description", solidHeader=TRUE, width=12, status='info',
            helpText("This app uses car crash data from the Montgomery County Crash Reporting - Drivers Dataset, which
                     can be found here: https://data.montgomerycountymd.gov/Public-Safety/Crash-Reporting-Drivers-Data/mmzv-x632/about_data. 
                     The original dataset has 39 columns, including but not limited to the surface conditions, weather, light, speed limit, and vehicle body type. 
                     Many features were removed from the dataset due to being unnecessary and adding noise. The full list of removed features
                     includes the following: (LIST FEATURES THAT WERE REMOVED).",
                     br(), br(), 
                     "The purpose of this app is to determine which models are most effective at classifying crashes by injury severity, report type 
                     (property damage, injury, fatal), and vehicle damage extent. This can be used to help determine how to help those who get in car
                     crashes before reports are taken.",
                     br(), br(),
                     "This app takes users through the data science lifecycle, with pages that make data exploration, data preprocessing, 
                     model selection and tuning, and model visualization seamless.")
                     
          )
        )
      ),
      tabItem(
        tabName="dataTab",
        # add some graphs for the dataset
        fluidRow(
          box(
            width=12,
            helpText("You may select from several features to view their distrbution. You may also
                     view a correlation plot of all the dataset's features to understand how they
                     interact with each one another. This is particularly useful for determining
                     which features to keep and which features to remove in data preprocessing and
                     model training.")
          )
        ),
        fluidRow(
          box(
            title='Interactive Data Table', solidHeader=TRUE, width=12, status='success',
            DTOutput('grouped_crash_table')
          )
        ),
        fluidRow(
          box(
            title = "Correlation Matrix", solidHeader=TRUE, width=12, status='warning',
            # correlation matrix
            plotOutput("correlationPlot"),
          )
        ),
        fluidRow(
          box(
            title='Feature Exploration', solidHeader=TRUE, width=12, status='primary',
            # users can select features and see their distribution
            selectInput('featureSelectInput', 'Select Feature to See Distribution: ', choices=c(
              'Crash_Quarter' = 'crashQuarter',
              'Time_of_day' = 'timeOfDay',
              'Route_Type_Grouped' = 'routeType',
              'Weather_Grouped' = 'weather',
              'Surface_Condition_Grouped' = 'surfaceCondition',
              'Light_Grouped' = 'light',
              'Traffic_Control_Grouped' = 'trafficControl',
              'Driver_Substance_Grouped' = 'driverSubstanceAbuse',
              'Driver_Distracted_Grouped' = 'driverDistractedBy',
              'First_Impact_Grouped' = 'vehicleFirstImpactLocation',
              'Vehicle_Movement_Grouped' = 'vehicleMovement',
              'Vehicle_Body_Type_Grouped' = 'vehicleBodyType',
              'Collision_Type_Grouped' = 'collisionType',
              'Speed_Limit_Grouped' = 'speedLimit',
              "ACRS_Report_Type" = 'acrsReportType',
              'Injury_Severity' = 'injurySeverity',
              'Vehicle_Damage_Extent' = 'vehicleDamageExtent',
              'Parked_Vehicle' = 'parkedVehicle'
            ),
            selected='crashQuarter'),
            # distribution plot
            plotlyOutput("groupedDistributionPlot")
          )
        )
      ),
      tabItem(
        tabName="preprocessingTab",
        fluidRow(
          box(
            title='Data Preprocessing Options', solidHeader=TRUE, width=12, status='info',
            sliderInput("trainingInput", "Select Training Set Size (%):", min=50, max=90, value=80, step=5),
            checkboxInput("missingCheck", "Handle Missing, N/A, and Unknown Values (mode imputation)", FALSE),
            selectInput("variableSelection", "Select Target Variable (ACRS Report Type by default):", choices=c(
              "ACRS_Report_Type" = 'ACRS_Report_Type',
              "Injury_Severity" = 'Injury_Severity',
              "Vehicle_Damage_Extent" = 'Vehicle_Damage_Extent'
            )),
            actionButton("resetDataButton", "Reset Options", icon=icon('arrow-rotate-right'), class='btn-warning'),
            br(), br(),
            actionButton("preprocessDataButton", "Preprocess Data", icon=icon("save"), class='btn-success'),
            span(
              style="margin-left: 15px;",
              htmlOutput("preprocess_status")
            )
          )
        ),
        fluidRow(
          box(
            title='Preprocessing Results', solidHeader=TRUE, width=12, status='success',
            # paste output of results
            # number of rows kept and number of rows removed
            # graph of data that can be altered and features can be selected and modified
            verbatimTextOutput("preprocess_summary")
        )),
        fluidRow(
          box(
            title="Training Data View", solidHeader=TRUE, width=12, status='warning',
            DTOutput('training_data_table'),
          )
        ),
        fluidRow(
          box(
            title="Testing Data View", solidHeader=TRUE, width=12, status='info',
            DTOutput('testing_data_table'),
          )
        ),
        fluidRow(
          box(
            title="Training/Testing Data Comparison", solidHeader=TRUE, width=12, status='success',
            selectInput('comparisonFeatureSelectInput', 'Select Feature to See Distribution: ', choices=c(
              'Crash_Quarter' = 'crashQuarter',
              'Time_of_day' = 'timeOfDay',
              'Route_Type_Grouped' = 'routeType',
              'Weather_Grouped' = 'weather',
              'Surface_Condition_Grouped' = 'surfaceCondition',
              'Light_Grouped' = 'light',
              'Traffic_Control_Grouped' = 'trafficControl',
              'Driver_Substance_Grouped' = 'driverSubstanceAbuse',
              'Driver_Distracted_Grouped' = 'driverDistractedBy',
              'First_Impact_Grouped' = 'vehicleFirstImpactLocation',
              'Vehicle_Movement_Grouped' = 'vehicleMovement',
              'Vehicle_Body_Type_Grouped' = 'vehicleBodyType',
              'Collision_Type_Grouped' = 'collisionType',
              'Speed_Limit_Grouped' = 'speedLimit',
              "ACRS_Report_Type" = 'acrsReportType',
              'Injury_Severity' = 'injurySeverity',
              'Vehicle_Damage_Extent' = 'vehicleDamageExtent',
              'Parked_Vehicle' = 'parkedVehicle'
            ),
            selected='crashQuarter'),
            plotlyOutput('training_data_histogram'),
            plotlyOutput('testing_data_histogram')
          )
        )
      ),
      tabItem(
        tabName="selectionTab",
        fluidRow(
          box(
            title="Model Selection", solidHeader=TRUE, width=12, status='info',
            selectInput('modelInput', 'Select Classification Model:', choices=c(
              'Random Forest' = 'randomForest',
              'Logistic Regression' = 'logisticRegression',
              'Naive Bayes' = 'naiveBayes',
              'KNN' = 'knn',
              'Support Vector Machine' = 'svm'
            )),
            # add little blurbs talking about how each model works
            conditionalPanel(
              condition = "input.modelInput == 'randomForest'",
              sliderInput('numTrees', 'Number of Trees:', min=100, max=500, value=300, step=50),
              sliderInput('varPerSplit', 'Mtry (square root of number of variables at each split):', min=1, max=15, value=4, step=1),
              helpText(
                HTML("<strong>Note: a larger number of trees may result in a longer training time.</strong>")
                     )
            ),
            conditionalPanel(
              condition = "input.modelInput == 'logisticRegression'"
            ),
            conditionalPanel(
              condition = "input.modelInput == 'naiveBayes'",
              checkboxInput('kFoldInput', 'Use K-Fold Cross-Validation (5 folds):', TRUE)
            ),
            conditionalPanel(
              condition = "input.modelInput == 'knn'",
              sliderInput('kInput', 'Select K Value:', min=1, max=5, value=3, step=1),
              helpText(
                HTML("<strong>Note: a larger K value may result in a longer training time.</strong>")
              )
            ),
            conditionalPanel(
              condition = "input.modelInput == 'svm'",
              selectInput('kernelInput', 'Select Kernel Type:', choices=c(
                'Linear' = 'linear',
                'Radial' = 'radial',
                'Polynomial' = 'polynomial'
              )),
              sliderInput('costParam', 'Cost Parameter:', min=0.1, max=10, value=1, step=0.1),
              helpText(
                HTML("<strong>Note: SVMs may require a very long training time (> 20 mins). They are not recommended for viewing results.</strong>")
              )
            ),
            actionButton('trainButton', 'Train Model', icon=icon('gear'), class='btn-success'),
            span(
              style="margin-left: 15px;",
              htmlOutput("model_status")
            )
          )
        ),
        fluidRow(
          box(
            title='Model Training Results', solidHeader=TRUE, width=12, status='success',
            verbatimTextOutput('modelTextSummary'),
            plotOutput('cmPlot'),
            
            # display another plot for model results, depending on the model used
            conditionalPanel(
              condition = "input.modelInput == 'randomForest'",
              br(),
              plotOutput('featureImportancePlot')
            ),
            conditionalPanel(
              condition = "input.modelInput == 'logisticRegression'",
              br(),
              DTOutput('featureCoefficientTable')
            ),
            conditionalPanel(
              condition = "input.modelInput == 'naiveBayes'",
              br(),
              DTOutput('featureProbabilityTable')
            ),
            conditionalPanel(
              condition = "input.modelInput == 'knn'",
              br(),
              plotOutput('distanceToNeighbors')
            )
          )
        )
      ),
      tabItem(
        tabName="visualizationTab",
        fluidRow(
          box(
            title='Model Feature Evaluation', solidHeader=TRUE, width=12, status='success',
            # display correlation matrix
            # let users enter features to determine output and create visualization
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  rv <- reactiveValues(
    train_data = NULL,
    test_data = NULL,
    model = NULL,
    target_var = NULL,
    model_results = NULL,
    #feature_importance = NULL
  )
  
  output$grouped_crash_table <- renderDT({
    datatable(grouped_crash_df_rem,
              options=list(pagelength=10,
                           scrollX=TRUE,
                           autowidth=TRUE,
                           search=list(regex=FALSE, caseInsensitive=TRUE),
                           searchCols=NULL),
              filter='top',
              caption=htmltools::tags$caption(
                style='caption-side: top; text-align: left; font-size: 14px;', 'This table uses grouped
              features to simplify the search process. Search globally (located top right) or use column
              filters.')
              )
  })
  output$correlationPlot <- renderPlot({
    grouped_heatmap
  })
  output$groupedDistributionPlot <- renderPlotly({
    selected_column <- feature_column_map[[input$featureSelectInput]]
    create_distribution_plot(grouped_crash_df_rem, selected_column)
  })
  observeEvent(input$resetDataButton, {
    updateSliderInput(inputId="trainingInput", value=80)
    updateCheckboxInput(inputId="missingCheck", value=FALSE)
    updateCheckboxInput(inputId="validationCheck", value=FALSE)
    updateSelectInput(inputId="variableSelection", selected='reportType')
    # clear preprocess summary and update preprocess status
    output$preprocess_summary <- renderPrint({
      cat("")
      })
    output$preprocess_status <- renderUI({
      tags$span(
        style = "color: orange; font-weight: bold;",
        "Options resetted"
      )
    })
  })
  observeEvent(input$preprocessDataButton, {
    withProgress(message="Processing data...", value=0, {
      incProgress(0.1)
      impute <- input$missingCheck
      trainingSize <- input$trainingInput
      incProgress(0.2)
      splits <- processData(trainingSize, impute)
      incProgress(0.5)
      rv$train_data <- splits$training
      rv$test_data <- splits$testing
      
      # preprocess status text
      output$preprocess_status <- renderUI({
        tags$span(
          style = "color: green; font-weight: bold;",
          "✓ Data successfully preprocessed"
        )
      })
      incProgress(0.2)
    })
    output$preprocess_summary <- renderPrint({
      # display preprocess stats
      # (nrows in training set, nrows in test set, etc.)
      req(rv$train_data, rv$test_data)
      cat("Data Summary: \n")
      cat("Imputation used: ", impute, "\n")
      cat("Train/Test split: ", trainingSize, "/", 100-trainingSize, "\n")
      cat("Total dataset size: ", nrow(rv$train_data)+nrow(rv$test_data), " entries\n")
      cat("Training set size: ", nrow(rv$train_data), " entries\n")
      cat("Testing set size: ", nrow(rv$test_data), " entries\n")
      cat("")
    })
    output$training_data_table <- renderDT({
      datatable(rv$train_data,
                options=list(pagelength=10,
                             scrollX=TRUE,
                             autowidth=TRUE,
                             search=list(regex=FALSE, caseInsensitive=TRUE),
                             searchCols=NULL),
                filter='top')
    })
    output$training_data_histogram <- renderPlotly({
      selected_column <- feature_column_map[[input$comparisonFeatureSelectInput]]
      create_distribution_plot(rv$train_data, selected_column)
    })
    output$testing_data_table <- renderDT({
      datatable(rv$test_data,
                options=list(pagelength=10,
                             scrollX=TRUE,
                             autowidth=TRUE,
                             search=list(regex=FALSE, caseInsensitive=TRUE),
                             searchCols=NULL),
                filter='top')
    })
    output$testing_data_histogram <- renderPlotly({
      selected_column <- feature_column_map[[input$comparisonFeatureSelectInput]]
      create_distribution_plot(rv$test_data, selected_column)
    })
  })
  observeEvent(input$trainButton, {
    rv$model <- input$modelInput
    rv$target_var <- input$variableSelection
    
    # progress bar for visualization
    withProgress(message='Training model...', value=0, {
      incProgress(0.2)
      if(rv$model == 'randomForest'){
        # create random forest model
        rv$model_results <- createRF(rv$target_var, input$numTrees, input$varPerSplit, rv$train_data, rv$test_data)
        output$featureImportancePlot <- renderPlot({
          req(res$model)
          varImpPlot(res$model, main='Feature Importance Table for Random Forest Model')
        })
      } else if (rv$model == 'logisticRegression') {
        rv$model_results <- createLogReg(rv$target_var, rv$train_data, rv$test_data)
        output$featureCoefficientTable <- renderDT({
          req(res$featureCoefficients)
          datatable(res$featureCoefficients,
                    options=list(pagelength=10,
                                 scrollX=TRUE,
                                 autowidth=TRUE,
                                 search=list(regex=FALSE, caseInsensitive=TRUE),
                                 searchCols=NULL),
                    filter='top')
        })
      } else if (rv$model == 'naiveBayes') {
        useKFold <- input$kFoldInput
        rv$model_results <- createNaiveBayes(rv$target_var, useKFold, rv$train_data, rv$test_data)
        output$featureProbabilityTable <- renderDT({
          req(res$feature_probs)
          datatable(res$feature_probs,
                    options=list(pagelength=10,
                                 scrollX=TRUE,
                                 autowidth=TRUE,
                                 search=list(regex=FALSE, caseInsensitive=TRUE),
                                 searchCols=NULL),
                    filter='top')
        })
      } else if (rv$model == 'knn') {
        rv$model_results <- createKNN(rv$target_var, input$kInput, rv$train_data, rv$test_data)
        output$distanceToNeighbors <- renderPlot({
          req(res$distances)
          ggplot(res$distances, aes(x=distance, fill=prediction)) +
            geom_density(alpha = 0.5) +
            labs(
              title = paste("Nearest Neighbor Distance:", rv$targetVar),
              x = "Distance to Nearest Neighbor",
              y = "Density",
              fill = "Prediction"
            ) +
            theme_minimal()
        })
      } else if (rv$model == 'svm') {
        rv$model_results <- createSVM(rv$target_var, input$kernelInput, input$costParam, rv$train_data, rv$test_data)
      }
      incProgress(0.8)
    })
    
    res <- rv$model_results
    output$modelTextSummary <- renderPrint({
      cat("Accuracy:", round(as.numeric(res$accuracy, 3)), "\n")
      cat("Kappa:", round(as.numeric(res$kappa, 3)), "\n")
      cat("Macro Precision:", round(res$macro_metrics["precision"], 3), "\n")
      cat("Macro Recall:", round(res$macro_metrics["recall"], 3), "\n")
      cat("Macro F1:", round(res$macro_metrics["f1"], 3))
    })
    
    output$model_status <- renderUI({
      tags$span(
        style = "color: green; font-weight: bold;",
        "✓ Model successfully trained"
      )
    })
    
    output$cmPlot <- renderPlot({
      req(res$cm_df)
      ggplot(res$cm_df, aes(x=Reference, y=Prediction, fill=Freq)) +
        geom_tile(color = 'white') +
        geom_text(aes(label=Freq), vjust=1) +
        scale_fill_gradient(low='white', high='#006D2C') +
        theme_bw() +
        labs(title="Confusion Matrix Heatmap")
    })
    
  })
}

shinyApp(ui, server)