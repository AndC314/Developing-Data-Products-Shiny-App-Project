
library(shiny)
library(datasets)
library(tidyverse)
library(leaps)
library(data.table)
library(ggthemes)
library(summarytools)
library(ggcorrplot)


shinyServer(function(input, output) {
    
    # Return the requested dataset
    datasetInput <- reactive({
        switch (input$dataset,
                'mtcars' = mtcars,
                'air quality' = airquality,
                'swiss' = swiss,
                'earth quakes' = quakes
        )
    })
    
    #in the original code, this is the data frame of the regressors only.
    #it is either picked from best subset regression or manually selected by the user
    #we will change this to be only the names of the predictors (not the data frame)
    #replace "xcol" with "variables_selected" in the code
    
    variables_selected <- reactive({
        df <- datasetInput()
        y = toString(input$resp)
        #---filter data set so that the target variable is not NA
        df = df %>% dplyr::filter_at(vars(matches(y)), all_vars(!is.na(.)))
        #---filter data set to exclude the target variable
        df_x = df %>% dplyr::select(-one_of(y))
        
        
        
        x_variables <- names(df_x)
        
        
        #best predictors picked from all subset regression
        if(input$pred_type == F){
            #---do all subsets regression
            f <- as.formula(
                paste(y, 
                      paste(x_variables, collapse = " + "), 
                      sep = " ~ "))
            
            leaps <- eval(bquote(   regsubsets(.(f), data = df, nbest = 10,nvmax = 15) ))
            
            
            a = summary(leaps)
            b = data.frame(a$which[,-1])
            rownames(b) <- NULL
            
            adj_rsq = data.frame(adj_rsq = a$adjr2)
            bic = data.frame(BIC = a$bic)
            
            model_summaries = bind_cols(b,adj_rsq, bic) %>%
                mutate(adj_rsq_rank = row_number(desc(adj_rsq)),
                       BIC_rank = row_number(BIC)) %>% 
                mutate(model_score = adj_rsq_rank*BIC_rank) %>%
                mutate(model_rank = row_number(model_score)) %>%
                dplyr::filter(model_rank == 1) %>% 
                dplyr::select(-adj_rsq, -BIC, -adj_rsq_rank, -BIC_rank ,-model_score, -model_rank) %>%
                dplyr::select_if(function(col)  sum(col) > 0)
            
            regressors_selected <- names(model_summaries)
            
            
            #otherwise the user specified the variables       
        } else {
            
            regressors_selected <- input$predx
            
        }
        
        regressors_selected
    })
    
    # Show the first "n" observations
    output$view <- renderTable({
        head(datasetInput(), n = input$obs)
    })
    
    # Generate a summary of the dataset
    output$summary <- renderPrint({
        dataset <- datasetInput()
        print(dfSummary(dataset, graph.magnif = 0.8, method = 'render', 
                        graph.col = FALSE,
                        omit.headings = TRUE,
                        bootstrap.css = FALSE))
    })    
    
    
    
    #Correlation matrix 
    output$corrplt <- renderPlot({
        df <- datasetInput()
        df2 = na.omit(df)
        corrdata = cor(df2)
        ggcorrplot(corrdata, hc.order = TRUE, type = "lower",
                   lab = TRUE)
    })
    
    output$response <- renderUI({
        resp <- names(datasetInput())
        selectInput("resp", "Choose Target Variable", choices = resp)
    })
    
    
    output$predictors <- renderUI({
        predx <- setdiff(names(datasetInput()), input$resp)
        checkboxGroupInput("predx", "Choose Predictor Variable(s)", choices = predx)
    })
    
    
    
    
    output$model <- renderPrint({
        if(input$pred_type == T){
            validate(need(input$predx, 'Select at least 1 predictor variable to generate regression model!'))
            y = toString(input$resp)
            df <- datasetInput()
            df = df %>% dplyr::filter_at(vars(matches(y)), all_vars(!is.na(.)))
            cat('Linear regression is performed on variable "',y,'" , using predictors: ',
                paste(variables_selected(),collapse=', ') )
            
            g <- as.formula(
                paste(y, 
                      paste(variables_selected(), collapse = " + "), 
                      sep = " ~ "))
            
            fit <- eval(bquote(   lm(.(g), data = df )))
            
            summary(fit)
            
        } else {
            y = toString(input$resp)
            df <- datasetInput()
            df = df %>% dplyr::filter_at(vars(matches(y)), all_vars(!is.na(.)))
            cat('Linear regression is performed on variable "',y,'" , using predictors: ',
                paste(variables_selected(),collapse=', ') )
            
            g <- as.formula(
                paste(y, 
                      paste(variables_selected(), collapse = " + "), 
                      sep = " ~ "))
            
            fit <- eval(bquote(   lm(.(g), data = df )))
            
            summary(fit)
        }
        
        
    })
    
    
    
    
    output$plt <- renderPlot({
        
        if(input$pred_type == T){
            validate(need(input$predx, 'Select at least 1 predictor variable to generate the plots below!'))
            
            df <- datasetInput()
            y_var = toString(input$resp)
            df2 = df %>% dplyr::filter_at(vars(matches(y_var)), all_vars(!is.na(.)))
            
            x_var = paste0(variables_selected())
            df_filtered_names = c(x_var, y_var)
            df_filtered = df2 %>% dplyr::select(one_of(df_filtered_names))
            
            df_filtered2 = df_filtered %>% tidyr::gather(predictors, value, -y_var) 
            setnames(df_filtered2, 1, 'target_variable')
            
            
            tit <- paste('Scatterplot of ',y_var, ' vs its predictors')
            g <- ggplot(df_filtered2, aes(x=target_variable, y=value, color=predictors)) + geom_point()
            g <- g + geom_smooth(method = 'lm') 
            g <- g + facet_wrap(~predictors, scales = 'free_y')
            g <- g + ylab('Predictor values') + ggtitle(tit) + xlab(paste0(y_var)) + 
                guides(color = FALSE) + theme_economist_white()
            g
            
            
        } else {
            df <- datasetInput()
            y_var = toString(input$resp)
            df2 = df %>% dplyr::filter_at(vars(matches(y_var)), all_vars(!is.na(.)))
            
            x_var = paste0(variables_selected())
            df_filtered_names = c(x_var, y_var)
            df_filtered = df2 %>% dplyr::select(one_of(df_filtered_names))
            
            df_filtered2 = df_filtered %>% tidyr::gather(predictors, value, -y_var) 
            setnames(df_filtered2, 1, 'target_variable')
            
            
            tit <- paste('Scatterplot of ',y_var, ' vs its predictors')
            g <- ggplot(df_filtered2, aes(x=target_variable, y=value, color=predictors)) + geom_point()
            g <- g + geom_smooth(method = 'lm') 
            g <- g + facet_wrap(~predictors, scales = 'free_y')
            g <- g + ylab('Predictor values') + ggtitle(tit) + xlab(paste0(y_var)) + 
                guides(color = FALSE) + theme_economist_white()
            g
            
        }
        
    })
    
    
    
})



