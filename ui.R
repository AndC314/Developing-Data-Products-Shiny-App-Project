
library(shiny)
library(datasets)
library(tidyverse)
library(leaps)
library(data.table)
library(ggthemes)
library(summarytools)
library(ggcorrplot)

fluidPage(title = "An app for multilinear regression", 
                tabsetPanel(              
                    tabPanel(title = "Normal data",
                             # Application title
                             titlePanel("Descriptive Analytics and Multi-linear Regression Tool"),
                             
                             #sidebarLayout(
                                 sidebarPanel(
                                     selectInput('dataset', 'Choose the dataset:',
                                                 choices = c('air quality', 'mtcars', 'earth quakes', 'swiss')),
                                     numericInput("obs", "Number of observations to view:", 10),
                                    hr(),
                                     uiOutput('response'),
                                     checkboxInput("pred_type", 'Click here if you want to choose predictors yourself!'),
                                     conditionalPanel(
                                         condition = "input.pred_type == true",
                                         uiOutput('predictors')
                                     ),
                                     conditionalPanel(
                                         condition = "input.pred_type == false",
                                         helpText(" If this is not selected then the predictors are chosen ",
                                                  "using an all subsets regression. The algorithm picks
                                                  the variables that maximize adjusted R^2 and BIC.
                                                  R^2 and BIC importance weighted 50/50.") ) ,
                                    hr(),
                                     h5('Author: Armin K'),
                                     hr(),
                                     hr(),
                                     helpText('The framework and basic utility of this 
                                                                Shiny app was inspired by the below dashboard:'),
                                     helpText(   a("Linear Regression Simulation",     
                                                       href="https://venkadeshwarank.shinyapps.io/Linear_Regression_Simulation/", 
                                                       target="_blank"  ))
                                     ) ,
                                 
                                 # Show a plot of the generated distribution
                                 mainPanel(
                                     #HTML('<hr style="color: purple;" size="20" >')
                                     #, hr(),
                                     h3('What this Shiny App can do for you:'),
                                     p('Use this app to run basic descriptive analytics and a multi-linear regression. 
                                        The regression dataset and target variables are chosen by the user, while the predictors
                                        are either automatically selected or chosen manually by the user. 
                                        This online tool inludes the following built-in datasets:
                                        1. airquality 2. mtcars 3. earth quakes and 4. swiss
                                        ...all part of the', strong("datasets"), ' package'),
                                     p('You will be able to see a detailed summary of your dataset, along with a ', strong('correlation matrix'), 
                                       'of all variables. The scatterplots between your target and explanatory variables are based on either the 
                                        automatic or manual selection of predictors, and hence may not include all variables.'),
                                     br(),
                                     h4('How to use this app:'),
                                     p('1. Select the dataset you want to play with. Default is set to', strong('airquality')),
                                     p('2. Select the number of observations you want to display below'),
                                     p('4. Select your target variable. Default is set to the first column of your dataset'),
                                     p('5. If you want to specify the predictor variables yourself, select the ', strong('predictor checkbox'),' to the left.',
                                            'By default, the predictor(s) is automatically selected using an ', strong('all subsets regression')),
                                     
                                     hr(),
                                     
                                     
                                     h4("Initial data header"),
                                     tableOutput("view") )) , 
                    
                    tabPanel(title = "Data Summary",
                                 h4("Detailed summary of our selected data set"),
                             br(),
                                p('This summary includes all variables, not just the ones used in our regression and scatterplots.'),
                                br(),
                                 verbatimTextOutput("summary")),
                    
                    tabPanel(title = "Correlation Matrix",
                             h4("Correlation matrix of all variables"),
                             p('This summary includes all variables, not just the ones used in our regression and scatterplots.'),
                             plotOutput("corrplt")),
                    
        
                    
                    tabPanel(title = "Scatterplot",
                             h4("Scatterplot of Target variable and selected predictors"),
                             h5("Unless predictors are manually specified on the 1st tab, 
                                        predictors are automatically selected using all subsets regression"),
                             br(),
                             plotOutput('plt') ),
                    
                    tabPanel(title = "Regression model output",
                             h4("Summary of our regression model"),
                             verbatimTextOutput('model') )  )  )



