library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(
    title = ""
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
        # add some graphs for the dataset
        fluidRow(
          box(
            helpText("Fortnite battle pass")
          )
        )
      ),
      tabItem(
        tabName="dataTab"
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