library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

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
            title='Feature Exploration', solidHeader=TRUE, width=12, status='info',
            helpText("Fortnite battle pass")
          )
        )
      ),
      tabItem(
        tabName="preprocessingTab",
        fluidRow(
          box(
            title='Data Preprocessing Options', solidHeader=TRUE, width=12, status='info',
            sliderInput("trainingInput", "Select Training Set Size (%):", min=50, max=90, value=80, step=5),
            sliderInput('totalDataInput', 'Select Total Data Size (# Rows):', min=5000, max=200000, value=20000, step=5000),
            checkboxInput("outlierCheck", "Remove Outliers", FALSE),
            checkboxInput("normalizeCheck", "Normalize Numerical Values", FALSE),
            checkboxInput("missingCheck", "Handle Missing Values (Imputation)", FALSE),
            checkboxInput("validationCheck", "Use Validation Set", FALSE),
            selectInput("variableSelection", "Select Target Variable:", choices=c(
              "ACRS Report Type" = 'reportType',
              "Injury Severity" = 'injurySeverity',
              "Vehicle Damage Extent" = 'damageExtent'
            )),
            actionButton("resetDataButton", "Reset Options", icon=icon('arrow-rotate-right'), class='btn-warning'),
            br(), br(),
            actionButton("preprocessDataButton", "Preprocess Data", icon=icon("save"), class='btn-success')
          )
        ),
        fluidRow(
          box(
            title='Preprocessing Results', solidHeader=TRUE, width=12, status='success',
            # paste output of results
            # number of rows kept and number of rows removed
            # graph of data that can be altered and features can be selected and modified
            
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
              'Support Vector Machine' == 'svm'
            )),
            # add little blurbs talking about how each model works
            conditionalPanel(
              condition = "input.modelInput == 'randomForest'",
              sliderInput('numTrees', 'Number of Trees:', min=100, max=500, value=300, step=50),
              sliderInput('varPerSplit', 'Number of Variables per Split:', min=1, max=8, value=4, step=1)
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
              sliderInput('kInput', 'Select K Value:', min=1, max=10, value=3, step=1)
            ),
            conditionalPanel(
              condition = "input.modelInput == 'svm'",
              selectInput('kernelInput', 'Select Kernel Type:', choices=c(
                'Linear' == 'linear',
                'Radial' == 'radial',
                'Polynomial' == 'polynomial'
              )),
              sliderInput('costParam', 'Cost Parameter:', min=0.1, max=10, value=1, step=0.1)
            ),
            actionButton('trainButton', 'Train Model', icon=icon('gear'), class='btn-success')
          )
        )
      ),
      tabItem(
        tabName="visualizationTab"
      )
    )
  )
)

server <- function(input, output) {
  
}

shinyApp(ui, server)